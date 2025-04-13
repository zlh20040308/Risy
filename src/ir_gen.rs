// src/ir_gen.rs
use super::ast::*;
use super::context::IrContext;
use super::context::SymbolValue;

impl CompUnit {
    pub fn to_ir(&self) -> String {
        let mut ctx = IrContext::new();
        self.func_def.to_ir(&mut ctx)
    }
}

impl FuncDef {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        let body = self.block.to_ir(ctx);
        format!(
            "fun @{}(): {} {{\n{}\n}}",
            self.ident,
            self.func_type.to_ir(),
            body
        )
    }
}

impl FuncType {
    pub fn to_ir(&self) -> &'static str {
        "i32"
    }
}

impl Block {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        ctx.enter_scope();

        for item in &self.block_items {
            if let BlockItem::Decl(decl) = &**item {
                if let Decl::Const(const_decl) = &**decl {
                    const_decl.analyze(ctx).expect("Semantic error");
                }
            }
        }

        println!("{}", serde_json::to_string_pretty(&ctx).unwrap());

        let mut ir_code = String::new();
        for item in &self.block_items {
            ir_code.push_str(&item.to_ir(ctx));
        }

        ctx.exit_scope();
        format!("%entry:\n{}", ir_code)
    }
}

impl Stmt {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        match self {
            Stmt::Assign(l_val, exp) => {
                let (exp_code, exp_val) = exp.to_ir(ctx);
                let var_name = l_val.to_ir(ctx);

                format!("{}  store {}, {}\n", exp_code, exp_val, var_name)
            }
            Stmt::Return(exp) => {
                let (exp_code, exp_val_raw) = exp.to_ir(ctx);
                let (load_code, exp_val) = ctx.load_if_needed(exp_val_raw);
                format!("{}{}  ret {}", exp_code, load_code, exp_val)
            }
        }
    }
}

impl Exp {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        self.lor_exp.eval(ctx)
    }

    pub fn to_ir(&self, ctx: &mut IrContext) -> (String, String) {
        self.lor_exp.to_ir(ctx)
    }
}

impl Decl {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        match self {
            Decl::Var(decl) => decl.to_ir(ctx),
            _ => String::new(),
        }
    }
}

impl VarDecl {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        let mut code = String::new();
        for def in &self.var_defs {
            let def_code = def.to_ir(ctx);
            code.push_str(&def_code);
        }
        code
    }
}

impl ConstDecl {
    pub fn analyze(&self, ctx: &mut IrContext) -> Result<(), String> {
        for def in &self.const_defs {
            let value = def.const_init_val.eval(ctx)?; // 编译期求值
            ctx.insert(&def.ident, SymbolValue::Const(value))?; // 插入符号表
        }
        Ok(())
    }
}

impl VarDef {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        match self {
            VarDef::Ident(name) => {
                let ir_name = ctx.generate_named(name);
                match ctx.insert(name, SymbolValue::Var(ir_name.clone())) {
                    Ok(_) => {
                        format!("  {} = alloc i32\n", ir_name)
                    }
                    Err(e) => panic!("{}", e),
                }
            }
            VarDef::Init(name, init_value) => {
                let (code, value) = init_value.to_ir(ctx);
                let ir_name = ctx.generate_named(name);
                match ctx.insert(name, SymbolValue::Var(ir_name.clone())) {
                    Ok(_) => {
                        format!(
                            "{}  {} = alloc i32\n  store {}, {}\n",
                            code, ir_name, value, ir_name
                        )
                    }
                    Err(e) => panic!("{}", e),
                }
            }
        }
    }
}

impl ConstInitVal {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        self.const_exp.eval(ctx)
    }
}

impl LVal {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        match ctx.lookup(&self.ident) {
            Some(SymbolValue::Const(val)) => Ok(*val),
            Some(SymbolValue::Var(_)) => Err(format!(
                "Cannot evaluate variable '{}' at compile-time",
                self.ident
            )),
            None => Err(format!(
                "Identifier '{}' not found in current scope",
                self.ident
            )),
        }
    }

    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        match ctx.lookup(&self.ident) {
            Some(SymbolValue::Var(ir_name)) => ir_name.clone(),
            Some(SymbolValue::Const(value)) => value.to_string(),
            None => panic!("Identifier '{}' not found", self.ident),
        }
    }
}

impl InitVal {
    pub fn to_ir(&self, ctx: &mut IrContext) -> (String, String) {
        self.exp.to_ir(ctx)
    }
}

impl ConstExp {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        // println!("ConstExp");
        self.exp.eval(ctx)
    }
}

impl BlockItem {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        match self {
            BlockItem::Decl(decl) => decl.to_ir(ctx),
            BlockItem::Stmt(stmt) => stmt.to_ir(ctx),
        }
    }
}

impl RelExp {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        match self {
            RelExp::AddExp(add) => add.eval(ctx),
            RelExp::Rel(lhs, op, rhs) => {
                let lval = lhs.eval(ctx)?;
                let rval = rhs.eval(ctx)?;
                Ok(match op {
                    RelOp::Less => (lval < rval) as i32,
                    RelOp::Greater => (lval > rval) as i32,
                    RelOp::LessEq => (lval <= rval) as i32,
                    RelOp::GreaterEq => (lval >= rval) as i32,
                })
            }
        }
    }
    pub fn to_ir(&self, ctx: &mut IrContext) -> (String, String) {
        match self {
            RelExp::AddExp(add) => add.to_ir(ctx),
            RelExp::Rel(lhs, op, rhs) => {
                let (l_code, l_val_raw) = lhs.to_ir(ctx);
                let (r_code, r_val_raw) = rhs.to_ir(ctx);

                let (l_load_code, l_val) = ctx.load_if_needed(l_val_raw);
                let (r_load_code, r_val) = ctx.load_if_needed(r_val_raw);

                if let (Ok(l_num), Ok(r_num)) = (l_val.parse::<i32>(), r_val.parse::<i32>()) {
                    let result = match op {
                        RelOp::Less => l_num < r_num,
                        RelOp::Greater => l_num > r_num,
                        RelOp::LessEq => l_num <= r_num,
                        RelOp::GreaterEq => l_num >= r_num,
                    };
                    return (
                        format!("{}{}{}{}", l_code, l_load_code, r_code, r_load_code),
                        (result as i32).to_string(),
                    );
                }

                let dst = ctx.next_temp();
                let inst = match op {
                    RelOp::Less => format!("  {} = lt {}, {}", dst, l_val, r_val),
                    RelOp::Greater => format!("  {} = gt {}, {}", dst, l_val, r_val),
                    RelOp::LessEq => format!("  {} = le {}, {}", dst, l_val, r_val),
                    RelOp::GreaterEq => format!("  {} = ge {}, {}", dst, l_val, r_val),
                };
                (
                    format!(
                        "{}{}{}{}{}\n",
                        l_code, l_load_code, r_code, r_load_code, inst
                    ),
                    dst,
                )
            }
        }
    }
}

impl EqExp {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        match self {
            EqExp::RelExp(rel) => rel.eval(ctx),
            EqExp::Eq(lhs, op, rhs) => {
                let lval = lhs.eval(ctx)?;
                let rval = rhs.eval(ctx)?;
                Ok(match op {
                    EqOp::Equal => (lval == rval) as i32,
                    EqOp::NotEqual => (lval != rval) as i32,
                })
            }
        }
    }
    pub fn to_ir(&self, ctx: &mut IrContext) -> (String, String) {
        match self {
            EqExp::RelExp(rel) => rel.to_ir(ctx),
            EqExp::Eq(lhs, op, rhs) => {
                let (l_code, l_val_raw) = lhs.to_ir(ctx);
                let (r_code, r_val_raw) = rhs.to_ir(ctx);

                let (l_load_code, l_val) = ctx.load_if_needed(l_val_raw);
                let (r_load_code, r_val) = ctx.load_if_needed(r_val_raw);

                if let (Ok(l_num), Ok(r_num)) = (l_val.parse::<i32>(), r_val.parse::<i32>()) {
                    let result = match op {
                        EqOp::Equal => (l_num == r_num) as i32,
                        EqOp::NotEqual => (l_num != r_num) as i32,
                    };
                    return (
                        format!("{}{}{}{}", l_code, l_load_code, r_code, r_load_code),
                        result.to_string(),
                    );
                }

                let dst = ctx.next_temp();
                let inst = match op {
                    EqOp::Equal => format!("  {} = eq {}, {}", dst, l_val, r_val),
                    EqOp::NotEqual => format!("  {} = ne {}, {}", dst, l_val, r_val),
                };
                (
                    format!(
                        "{}{}{}{}{}\n",
                        l_code, l_load_code, r_code, r_load_code, inst
                    ),
                    dst,
                )
            }
        }
    }
}

impl LAndExp {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        // println!("LAndExp");
        match self {
            LAndExp::EqExp(eq) => eq.eval(ctx),
            LAndExp::LAnd(lhs, rhs) => {
                let lval = lhs.eval(ctx)?;
                let rval = rhs.eval(ctx)?;
                Ok(if lval != 0 && rval != 0 { 1 } else { 0 })
            }
        }
    }
    pub fn to_ir(&self, ctx: &mut IrContext) -> (String, String) {
        match self {
            LAndExp::EqExp(eq) => eq.to_ir(ctx),

            LAndExp::LAnd(lhs, rhs) => {
                let (l_code, l_val_raw) = lhs.to_ir(ctx);
                let (r_code, r_val_raw) = rhs.to_ir(ctx);

                let (l_load_code, l_val) = ctx.load_if_needed(l_val_raw);
                let (r_load_code, r_val) = ctx.load_if_needed(r_val_raw);

                if let (Ok(l_num), Ok(r_num)) = (l_val.parse::<i32>(), r_val.parse::<i32>()) {
                    let result = if (l_num != 0) && (r_num != 0) { 1 } else { 0 };
                    return (
                        format!("{}{}{}{}", l_code, l_load_code, r_code, r_load_code),
                        result.to_string(),
                    );
                }

                let l_nonzero = ctx.next_temp();
                let r_nonzero = ctx.next_temp();
                let dst = ctx.next_temp();

                let code = format!(
                    "{}{}{}{}  {} = ne {}, 0\n  {} = ne {}, 0\n  {} = and {}, {}\n",
                    l_code,
                    l_load_code,
                    r_code,
                    r_load_code,
                    l_nonzero,
                    l_val,
                    r_nonzero,
                    r_val,
                    dst,
                    l_nonzero,
                    r_nonzero
                );

                (code, dst)
            }
        }
    }
}

impl LOrExp {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        match self {
            LOrExp::LAndExp(land) => land.eval(ctx),
            LOrExp::LOr(lhs, rhs) => {
                let lval = lhs.eval(ctx)?;
                let rval = rhs.eval(ctx)?;
                Ok(if lval != 0 || rval != 0 { 1 } else { 0 })
            }
        }
    }
    pub fn to_ir(&self, ctx: &mut IrContext) -> (String, String) {
        match self {
            LOrExp::LAndExp(land) => land.to_ir(ctx),

            LOrExp::LOr(lhs, rhs) => {
                let (l_code, l_val_raw) = lhs.to_ir(ctx);
                let (r_code, r_val_raw) = rhs.to_ir(ctx);

                let (l_load_code, l_val) = ctx.load_if_needed(l_val_raw);
                let (r_load_code, r_val) = ctx.load_if_needed(r_val_raw);

                if let (Ok(l_num), Ok(r_num)) = (l_val.parse::<i32>(), r_val.parse::<i32>()) {
                    let result = if (l_num != 0) || (r_num != 0) { 1 } else { 0 };
                    return (
                        format!("{}{}{}{}", l_code, l_load_code, r_code, r_load_code),
                        result.to_string(),
                    );
                }

                let l_nonzero = ctx.next_temp();
                let r_nonzero = ctx.next_temp();
                let dst = ctx.next_temp();

                let code = format!(
                    "{}{}{}{}  {} = ne {}, 0\n  {} = ne {}, 0\n  {} = or {}, {}\n",
                    l_code,
                    l_load_code,
                    r_code,
                    r_load_code,
                    l_nonzero,
                    l_val,
                    r_nonzero,
                    r_val,
                    dst,
                    l_nonzero,
                    r_nonzero
                );

                (code, dst)
            }
        }
    }
}

impl AddExp {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        match self {
            AddExp::MulExp(mul) => mul.eval(ctx),
            AddExp::Binary(lhs, op, rhs) => {
                let lval = lhs.eval(ctx)?;
                let rval = rhs.eval(ctx)?;
                Ok(match op {
                    BinOp::Add => lval + rval,
                    BinOp::Minus => lval - rval,
                    _ => unreachable!("Invalid binary operator in AddExp"),
                })
            }
        }
    }

    pub fn to_ir(&self, ctx: &mut IrContext) -> (String, String) {
        match self {
            AddExp::MulExp(mul) => mul.to_ir(ctx),
            AddExp::Binary(lhs, op, rhs) => {
                let (l_code, l_val_raw) = lhs.to_ir(ctx);
                let (r_code, r_val_raw) = rhs.to_ir(ctx);

                let (l_load, l_val) = ctx.load_if_needed(l_val_raw);
                let (r_load, r_val) = ctx.load_if_needed(r_val_raw);

                // 常数折叠
                if let (Ok(l_num), Ok(r_num)) = (l_val.parse::<i32>(), r_val.parse::<i32>()) {
                    let result = match op {
                        BinOp::Add => l_num + r_num,
                        BinOp::Minus => l_num - r_num,
                        _ => unreachable!(),
                    };
                    return (
                        format!("{}{}{}{}", l_code, l_load, r_code, r_load),
                        result.to_string(),
                    );
                }

                let dst = ctx.next_temp();
                let inst = match op {
                    BinOp::Add => format!("  {} = add {}, {}\n", dst, l_val, r_val),
                    BinOp::Minus => format!("  {} = sub {}, {}\n", dst, l_val, r_val),
                    _ => panic!("unexpected binop in AddExp"),
                };

                (
                    format!("{}{}{}{}{}", l_code, l_load, r_code, r_load, inst),
                    dst,
                )
            }
        }
    }
}

impl MulExp {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        match self {
            MulExp::Unary(unary) => unary.eval(ctx),
            MulExp::Binary(lhs, op, rhs) => {
                let lval = lhs.eval(ctx)?;
                let rval = rhs.eval(ctx)?;
                Ok(match op {
                    BinOp::Mul => lval * rval,
                    BinOp::Div => lval / rval,
                    BinOp::Mod => lval % rval,
                    _ => unreachable!("Invalid binary operator in MulExp"),
                })
            }
        }
    }
    pub fn to_ir(&self, ctx: &mut IrContext) -> (String, String) {
        match self {
            MulExp::Unary(u) => u.to_ir(ctx),
            MulExp::Binary(lhs, op, rhs) => {
                let (l_code, l_val_raw) = lhs.to_ir(ctx);
                let (r_code, r_val_raw) = rhs.to_ir(ctx);

                let (l_load_code, l_val) = ctx.load_if_needed(l_val_raw);
                let (r_load_code, r_val) = ctx.load_if_needed(r_val_raw);

                if let (Ok(l_num), Ok(r_num)) = (l_val.parse::<i32>(), r_val.parse::<i32>()) {
                    let result = match op {
                        BinOp::Mul => l_num * r_num,
                        BinOp::Div => l_num / r_num,
                        BinOp::Mod => l_num % r_num,
                        _ => unreachable!(),
                    };
                    return (
                        format!("{}{}{}{}", l_code, l_load_code, r_code, r_load_code),
                        result.to_string(),
                    );
                }

                let dst = ctx.next_temp();
                let inst = match op {
                    BinOp::Mul => format!("  {} = mul {}, {}", dst, l_val, r_val),
                    BinOp::Div => format!("  {} = div {}, {}", dst, l_val, r_val),
                    BinOp::Mod => format!("  {} = mod {}, {}", dst, l_val, r_val),
                    _ => panic!("unexpected binop in MulExp"),
                };

                (
                    format!(
                        "{}{}{}{}{}\n",
                        l_code, l_load_code, r_code, r_load_code, inst
                    ),
                    dst,
                )
            }
        }
    }
}

impl UnaryExp {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        match self {
            UnaryExp::Primary(primary) => primary.eval(ctx),
            UnaryExp::UnaryOp(op, expr) => {
                let val = expr.eval(ctx)?;
                Ok(match op {
                    UnaryOp::Plus => val,
                    UnaryOp::Minus => -val,
                    UnaryOp::Not => (val == 0) as i32,
                })
            }
        }
    }
    pub fn to_ir(&self, ctx: &mut IrContext) -> (String, String) {
        match self {
            UnaryExp::Primary(p) => p.to_ir(ctx),
            UnaryExp::UnaryOp(op, exp) => {
                let (code, val_raw) = exp.to_ir(ctx);
                if let Ok(num) = val_raw.parse::<i32>() {
                    let result = match op {
                        UnaryOp::Plus => num,
                        UnaryOp::Minus => -num,
                        UnaryOp::Not => (num == 0) as i32,
                    };
                    return (format!("{}\n", code), result.to_string());
                }

                let (load_code, val) = ctx.load_if_needed(val_raw);

                let dst = ctx.next_temp();
                let inst = match op {
                    UnaryOp::Plus => return (format!("{}{}", code, load_code), val),
                    UnaryOp::Minus => format!("  {} = sub 0, {}", dst, val),
                    UnaryOp::Not => format!("  {} = eq 0, {}", dst, val),
                };

                (format!("{}{}{}\n", code, load_code, inst), dst)
            }
        }
    }
}

impl PrimaryExp {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        match self {
            PrimaryExp::Number(n) => Ok(*n),
            PrimaryExp::LVal(lval) => lval.eval(ctx),
            PrimaryExp::Paren(exp) => exp.eval(ctx),
        }
    }
    pub fn to_ir(&self, ctx: &mut IrContext) -> (String, String) {
        match self {
            PrimaryExp::Number(n) => (String::new(), format!("{}", n)),
            PrimaryExp::LVal(lval) => (String::new(), lval.to_ir(ctx)),
            PrimaryExp::Paren(inner) => inner.to_ir(ctx),
        }
    }
}
