// src/ir_gen.rs
use super::ast::*;
use super::context::IrContext;
use super::context::SymbolValue;

/// 判断一段 IR 是否已经以 `ret` 或 `jump` 结束
fn is_terminated(code: &str) -> bool {
    code.trim_end()
        .lines()
        .rev()
        .find(|line| !line.trim().is_empty())
        .map_or(false, |line| {
            let line = line.trim_start();
            line.starts_with("ret") || line.starts_with("jump")
        })
}

impl CompUnit {
    pub fn to_ir(&self) -> String {
        let mut ctx = IrContext::new();
        self.func_def.to_ir(&mut ctx)
    }
}

impl FuncDef {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        let body = self.block.to_ir(ctx);
        let mut code = String::new();
        code.push_str(&format!(
            "fun @{}(): {} {{\n",
            self.ident,
            self.func_type.to_ir()
        ));
        code.push_str("%entry:\n");
        code.push_str(&body);
        code.push_str("}");
        code
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

        let mut ir_code = String::new();
        for item in &self.block_items {
            let item_code = item.to_ir(ctx);
            ir_code.push_str(&item_code);

            if is_terminated(&item_code) {
                break;
            }
        }

        println!("{}", serde_json::to_string_pretty(&ctx).unwrap());
        ctx.exit_scope();

        ir_code
    }
}

impl Stmt {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        match self {
            Stmt::Matched(matched) => matched.to_ir(ctx),
            Stmt::Unmatched(unmatched) => unmatched.to_ir(ctx),
        }
    }
}

impl MatchedStmt {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        match self {
            MatchedStmt::If(cond, then_body, else_body) => {
                let mut code = String::new();
                let (cond_code, cond_val) = cond.to_ir(ctx);
                let then_label = ctx.next_then();
                let else_label = ctx.next_else();
                let end_label = ctx.next_end();

                code.push_str(&cond_code);
                let cond_val = if let Ok(cond_num) = cond_val.parse::<i32>() {
                    let temp = ctx.next_temp();
                    code.push_str(&format!("  {} = add 0, {}\n", temp, cond_num));
                    temp
                } else {
                    cond_val
                };
                code.push_str(&format!(
                    "  br {}, {}, {}\n",
                    cond_val, then_label, else_label
                ));

                // then
                code.push_str(&format!("{}:\n", then_label));
                let then_code = then_body.to_ir(ctx);
                code.push_str(&then_code);
                if !is_terminated(&then_code) {
                    code.push_str(&format!("  jump {}\n", end_label));
                }

                // else
                code.push_str(&format!("{}:\n", else_label));
                let else_code = else_body.to_ir(ctx);
                code.push_str(&else_code);
                if !is_terminated(&else_code) {
                    code.push_str(&format!("  jump {}\n", end_label));
                }

                // end
                code.push_str(&format!("{}:\n", end_label));
                code
            }

            MatchedStmt::NonIf(stmt) => stmt.to_ir(ctx),
        }
    }
}

impl UnmatchedStmt {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        match self {
            UnmatchedStmt::Else(cond, then_body) => {
                let mut code = String::new();
                let (cond_code, cond_val) = cond.to_ir(ctx);
                let then_label = ctx.next_then();
                let end_label = ctx.next_end();

                code.push_str(&cond_code);
                let cond_val = if let Ok(cond_num) = cond_val.parse::<i32>() {
                    let temp = ctx.next_temp();
                    code.push_str(&format!("  {} = add 0, {}\n", temp, cond_num));
                    temp
                } else {
                    cond_val
                };
                code.push_str(&format!(
                    "  br {}, {}, {}\n",
                    cond_val, then_label, end_label
                ));

                code.push_str(&format!("{}:\n", then_label));
                let then_code = then_body.to_ir(ctx);
                code.push_str(&then_code);
                if !is_terminated(&then_code) {
                    code.push_str(&format!("  jump {}\n", end_label));
                }

                code.push_str(&format!("{}:\n", end_label));
                code
            }

            UnmatchedStmt::NonElse(cond, then_body, else_body) => {
                let mut code = String::new();
                let (cond_code, cond_val) = cond.to_ir(ctx);
                let then_label = ctx.next_then();
                let else_label = ctx.next_else();
                let end_label = ctx.next_end();

                code.push_str(&cond_code);
                let cond_val = if let Ok(cond_num) = cond_val.parse::<i32>() {
                    let temp = ctx.next_temp();
                    code.push_str(&format!("  {} = add 0, {}\n", temp, cond_num));
                    temp
                } else {
                    cond_val
                };
                code.push_str(&format!(
                    "  br {}, {}, {}\n",
                    cond_val, then_label, else_label
                ));

                // then
                code.push_str(&format!("{}:\n", then_label));
                let then_code = then_body.to_ir(ctx);
                code.push_str(&then_code);
                if !is_terminated(&then_code) {
                    code.push_str(&format!("  jump {}\n", end_label));
                }

                // else
                code.push_str(&format!("{}:\n", else_label));
                let else_code = else_body.to_ir(ctx);
                code.push_str(&else_code);
                if !is_terminated(&else_code) {
                    code.push_str(&format!("  jump {}\n", end_label));
                }

                // end
                code.push_str(&format!("{}:\n", end_label));
                code
            }
        }
    }
}

impl NonIfStmt {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        match self {
            NonIfStmt::Assign(l_val, exp) => {
                let (exp_code, exp_val) = exp.to_ir(ctx);
                let var_name = l_val.to_ir(ctx);
                format!("{}  store {}, {}\n", exp_code, exp_val, var_name)
            }
            NonIfStmt::Block(block) => {
                let code = block.to_ir(ctx);
                format!("{}", code)
            }
            NonIfStmt::Exp(exp) => {
                if let Some(exp) = exp {
                    let (exp_code, _) = exp.to_ir(ctx);
                    exp_code
                } else {
                    String::new()
                }
            }
            NonIfStmt::Return(exp) => {
                if let Some(exp) = exp {
                    let (exp_code, exp_val_raw) = exp.to_ir(ctx);
                    let (load_code, exp_val) = ctx.load_if_needed(exp_val_raw);
                    format!("{}{}  ret {}\n", exp_code, load_code, exp_val)
                } else {
                    format!("  ret\n")
                }
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
            Decl::Var(var_decl) => var_decl.to_ir(ctx),
            Decl::Const(const_decl) => {
                const_decl.analyze(ctx).expect("Semantic error");
                String::new()
            }
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

                let mut code = String::new();

                // 拼接左右表达式的 IR 代码
                code.push_str(&l_code);
                code.push_str(&l_load_code);
                code.push_str(&r_code);
                code.push_str(&r_load_code);

                // 判断是否为非零（相当于布尔值）
                code.push_str(&format!("  {} = ne {}, 0\n", l_nonzero, l_val));
                code.push_str(&format!("  {} = ne {}, 0\n", r_nonzero, r_val));

                // 逻辑与运算
                code.push_str(&format!("  {} = and {}, {}\n", dst, l_nonzero, r_nonzero));

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

                let mut code = String::new();

                // 拼接左值与右值的表达式代码
                code.push_str(&l_code);
                code.push_str(&l_load_code);
                code.push_str(&r_code);
                code.push_str(&r_load_code);

                // 生成非零判断的中间指令
                code.push_str(&format!("  {} = ne {}, 0\n", l_nonzero, l_val));
                code.push_str(&format!("  {} = ne {}, 0\n", r_nonzero, r_val));

                // 生成逻辑或运算
                code.push_str(&format!("  {} = or {}, {}\n", dst, l_nonzero, r_nonzero));

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
