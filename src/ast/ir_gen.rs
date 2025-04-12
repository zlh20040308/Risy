use super::context::IrGenContext;
use super::node::*;
use super::symbol_table::SymbolTable;

impl CompUnit {
    pub fn to_ir(&self) -> String {
        let mut ctx = IrGenContext::new();
        let mut symtab = SymbolTable::new();
        self.func_def.to_ir(&mut ctx, &mut symtab)
    }
}

impl FuncDef {
    pub fn to_ir(&self, ctx: &mut IrGenContext, symtab: &mut SymbolTable) -> String {
        let body = self.block.to_ir(ctx, symtab);
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
    pub fn to_ir(&self, ctx: &mut IrGenContext, symtab: &mut SymbolTable) -> String {
        symtab.enter_scope();

        // 阶段1：处理所有声明（建立符号表）
        for item in &self.block_items {
            if let BlockItem::Decl(decl) = &**item {
                decl.analyze(symtab).expect("语义错误");
            }
        }
        println!("{:?}", symtab);

        // 阶段2：生成IR（此时所有常量已被替换）
        let mut ir_code = String::new();
        for item in &self.block_items {
            if let BlockItem::Stmt(stmt) = &**item {
                ir_code.push_str(&stmt.to_ir(ctx, symtab));
            }
        }

        symtab.exit_scope();
        format!("%entry:\n{}", ir_code)
    }
}

impl Stmt {
    pub fn to_ir(&self, ctx: &mut IrGenContext, symtab: &mut SymbolTable) -> String {
        let (code, result) = self.exp.to_ir(ctx, symtab);
        format!("{}  ret {}", code, result)
    }
}

impl Exp {
    pub fn eval(&self, symtab: &SymbolTable) -> Result<i32, String> {
        // 委托给逻辑或表达式
        self.lor_exp.eval(symtab)
    }
    pub fn to_ir(&self, ctx: &mut IrGenContext, symtab: &mut SymbolTable) -> (String, String) {
        self.lor_exp.to_ir(ctx, symtab)
    }
}

impl Decl {
    pub fn analyze(&self, symtab: &mut SymbolTable) -> Result<(), String> {
        self.const_decl.analyze(symtab)
    }
    pub fn to_ir(&self, ctx: &mut IrGenContext, symtab: &mut SymbolTable) -> (String, String) {
        for def in &self.const_decl.const_defs {}
        self.const_decl.to_ir(ctx, symtab)
    }
}

impl ConstDecl {
    pub fn analyze(&self, symtab: &mut SymbolTable) -> Result<(), String> {
        for def in &self.const_defs {
            let value = def.const_init_val.eval(symtab)?; // 编译期求值
            symtab.insert(def.ident.clone(), value)?; // 插入符号表
        }
        Ok(())
    }

    pub fn to_ir(&self, ctx: &mut IrGenContext, symtab: &mut SymbolTable) -> (String, String) {
        let mut code = String::new();
        for def in &self.const_defs {
            let (def_code, _) = def.to_ir(ctx, symtab);
            code.push_str(&def_code);
        }
        (code, String::new())
    }
}

impl ConstDef {
    pub fn to_ir(&self, ctx: &mut IrGenContext, symtab: &mut SymbolTable) -> (String, String) {
        let (val_code, val_str) = self.const_init_val.to_ir(ctx, symtab);
        let temp = ctx.next_temp();
        let code = format!(
            "{}  {} = add {}, 0\n", // 生成一个中间值（仿照 mov 指令）
            val_code, temp, val_str
        );
        symtab
            .insert(self.ident.clone(), val_str.parse::<i32>().unwrap())
            .unwrap();
        (code, temp)
    }
}

impl ConstInitVal {
    pub fn eval(&self, symtab: &SymbolTable) -> Result<i32, String> {
        // 根据定义，ConstInitVal 只包含 const_exp
        self.const_exp.eval(symtab)
    }
    pub fn to_ir(&self, ctx: &mut IrGenContext, symtab: &mut SymbolTable) -> (String, String) {
        self.const_exp.to_ir(ctx, symtab)
    }
}

impl LVal {
    pub fn eval(&self, symtab: &SymbolTable) -> Result<i32, String> {
        symtab
            .lookup(&self.ident)
            .ok_or_else(|| format!("Undefined constant: {}", self.ident))
    }
    pub fn to_ir(&self, ctx: &mut IrGenContext, symtab: &mut SymbolTable) -> (String, String) {
        let val = symtab.lookup(&self.ident).expect("Undefined identifier");
        (String::new(), val.to_string())
    }
}

impl ConstExp {
    pub fn eval(&self, symtab: &SymbolTable) -> Result<i32, String> {
        self.exp.eval(symtab)
    }
    pub fn to_ir(&self, ctx: &mut IrGenContext, symtab: &mut SymbolTable) -> (String, String) {
        self.exp.to_ir(ctx, symtab)
    }
}

impl BlockItem {
    pub fn to_ir(&self, ctx: &mut IrGenContext, symtab: &mut SymbolTable) -> String {
        match self {
            BlockItem::Decl(decl) => {
                let (code, _) = decl.to_ir(ctx, symtab);
                code
            }
            BlockItem::Stmt(stmt) => stmt.to_ir(ctx, symtab),
        }
    }
}

impl RelExp {
    pub fn eval(&self, symtab: &SymbolTable) -> Result<i32, String> {
        match self {
            RelExp::AddExp(add) => add.eval(symtab),
            RelExp::Rel(lhs, op, rhs) => {
                let lval = lhs.eval(symtab)?;
                let rval = rhs.eval(symtab)?;
                Ok(match op {
                    RelOp::Less => (lval < rval) as i32,
                    RelOp::Greater => (lval > rval) as i32,
                    RelOp::LessEq => (lval <= rval) as i32,
                    RelOp::GreaterEq => (lval >= rval) as i32,
                })
            }
        }
    }
    pub fn to_ir(&self, ctx: &mut IrGenContext, symtab: &mut SymbolTable) -> (String, String) {
        match self {
            RelExp::AddExp(add) => add.to_ir(ctx, symtab),
            RelExp::Rel(lhs, op, rhs) => {
                let (l_code, l_val) = lhs.to_ir(ctx, symtab);
                let (r_code, r_val) = rhs.to_ir(ctx, symtab);
                if let (Ok(l_num), Ok(r_num)) = (l_val.parse::<i32>(), r_val.parse::<i32>()) {
                    let result = match op {
                        RelOp::Less => l_num < r_num,
                        RelOp::Greater => l_num > r_num,
                        RelOp::LessEq => l_num <= r_num,
                        RelOp::GreaterEq => l_num >= r_num,
                    };
                    return (format!("{}{}", l_code, r_code), result.to_string());
                }
                let dst = ctx.next_temp();
                let inst = match op {
                    RelOp::Less => format!("  {} = lt {}, {}", dst, l_val, r_val),
                    RelOp::Greater => format!("  {} = gt {}, {}", dst, l_val, r_val),
                    RelOp::LessEq => format!("  {} = le {}, {}", dst, l_val, r_val),
                    RelOp::GreaterEq => format!("  {} = ge {}, {}", dst, l_val, r_val),
                };
                (format!("{}{}{}\n", l_code, r_code, inst), dst)
            }
        }
    }
}

impl EqExp {
    pub fn eval(&self, symtab: &SymbolTable) -> Result<i32, String> {
        match self {
            EqExp::RelExp(rel) => rel.eval(symtab),
            EqExp::Eq(lhs, op, rhs) => {
                let lval = lhs.eval(symtab)?;
                let rval = rhs.eval(symtab)?;
                Ok(match op {
                    EqOp::Equal => (lval == rval) as i32,
                    EqOp::NotEqual => (lval != rval) as i32,
                })
            }
        }
    }
    pub fn to_ir(&self, ctx: &mut IrGenContext, symtab: &mut SymbolTable) -> (String, String) {
        match self {
            EqExp::RelExp(rel) => rel.to_ir(ctx, symtab),
            EqExp::Eq(lhs, op, rhs) => {
                let (l_code, l_val) = lhs.to_ir(ctx, symtab);
                let (r_code, r_val) = rhs.to_ir(ctx, symtab);
                if let (Ok(l_num), Ok(r_num)) = (l_val.parse::<i32>(), r_val.parse::<i32>()) {
                    let result = match op {
                        EqOp::Equal => (l_num == r_num) as i32,
                        EqOp::NotEqual => (l_num != r_num) as i32,
                    };
                    return (format!("{}{}", l_code, r_code), result.to_string());
                }
                let dst = ctx.next_temp();
                let inst = match op {
                    EqOp::Equal => format!("  {} = eq {}, {}", dst, l_val, r_val),
                    EqOp::NotEqual => format!("  {} = ne {}, {}", dst, l_val, r_val),
                };
                (format!("{}{}{}\n", l_code, r_code, inst), dst)
            }
        }
    }
}

impl LAndExp {
    pub fn eval(&self, symtab: &SymbolTable) -> Result<i32, String> {
        match self {
            LAndExp::EqExp(eq) => eq.eval(symtab),
            LAndExp::LAnd(lhs, rhs) => {
                let lval = lhs.eval(symtab)?;
                let rval = rhs.eval(symtab)?;
                Ok(if lval != 0 && rval != 0 { 1 } else { 0 })
            }
        }
    }
    pub fn to_ir(&self, ctx: &mut IrGenContext, symtab: &mut SymbolTable) -> (String, String) {
        match self {
            LAndExp::EqExp(eq) => eq.to_ir(ctx, symtab),

            LAndExp::LAnd(lhs, rhs) => {
                let (l_code, l_val) = lhs.to_ir(ctx, symtab);
                let (r_code, r_val) = rhs.to_ir(ctx, symtab);
                println!("l_val = {}, r_val = {}", l_val, r_val);
                if let (Ok(l_num), Ok(r_num)) = (l_val.parse::<i32>(), r_val.parse::<i32>()) {
                    let result = if (l_num != 0) && (r_num != 0) { 1 } else { 0 };
                    return (format!("{}{}", l_code, r_code), result.to_string());
                }
                let l_nonzero = ctx.next_temp();
                let r_nonzero = ctx.next_temp();
                let dst = ctx.next_temp();

                let code = format!(
                    "{}{}  {} = ne {}, 0\n  {} = ne {}, 0\n  {} = and {}, {}\n",
                    l_code, r_code, l_nonzero, l_val, r_nonzero, r_val, dst, l_nonzero, r_nonzero
                );

                (code, dst)
            }
        }
    }
}

impl LOrExp {
    pub fn eval(&self, symtab: &SymbolTable) -> Result<i32, String> {
        match self {
            LOrExp::LAndExp(land) => land.eval(symtab),
            LOrExp::LOr(lhs, rhs) => {
                let lval = lhs.eval(symtab)?;
                let rval = rhs.eval(symtab)?;
                Ok(if lval != 0 || rval != 0 { 1 } else { 0 })
            }
        }
    }
    pub fn to_ir(&self, ctx: &mut IrGenContext, symtab: &mut SymbolTable) -> (String, String) {
        match self {
            LOrExp::LAndExp(land) => land.to_ir(ctx, symtab),

            LOrExp::LOr(lhs, rhs) => {
                let (l_code, l_val) = lhs.to_ir(ctx, symtab);
                let (r_code, r_val) = rhs.to_ir(ctx, symtab);
                if let (Ok(l_num), Ok(r_num)) = (l_val.parse::<i32>(), r_val.parse::<i32>()) {
                    let result = if (l_num != 0) || (r_num != 0) { 1 } else { 0 };
                    return (format!("{}{}", l_code, r_code), result.to_string());
                }

                let l_nonzero = ctx.next_temp();
                let r_nonzero = ctx.next_temp();
                let dst = ctx.next_temp();

                let code = format!(
                    "{}{}  {} = ne {}, 0\n  {} = ne {}, 0\n  {} = or {}, {}\n",
                    l_code, r_code, l_nonzero, l_val, r_nonzero, r_val, dst, l_nonzero, r_nonzero
                );

                (code, dst)
            }
        }
    }
}

impl AddExp {
    pub fn eval(&self, symtab: &SymbolTable) -> Result<i32, String> {
        match self {
            AddExp::MulExp(mul) => mul.eval(symtab),
            AddExp::Binary(lhs, op, rhs) => {
                let lval = lhs.eval(symtab)?;
                let rval = rhs.eval(symtab)?;
                Ok(match op {
                    BinOp::Add => lval + rval,
                    BinOp::Minus => lval - rval,
                    _ => unreachable!("Invalid binary operator in AddExp"),
                })
            }
        }
    }

    pub fn to_ir(&self, ctx: &mut IrGenContext, symtab: &mut SymbolTable) -> (String, String) {
        match self {
            AddExp::MulExp(mul) => mul.to_ir(ctx, symtab),
            AddExp::Binary(lhs, op, rhs) => {
                let (l_code, l_val) = lhs.to_ir(ctx, symtab);
                let (r_code, r_val) = rhs.to_ir(ctx, symtab);
                println!("l_val = {}, r_val = {}", l_val, r_val);
                if let (Ok(l_num), Ok(r_num)) = (l_val.parse::<i32>(), r_val.parse::<i32>()) {
                    let result = match op {
                        BinOp::Add => l_num + r_num,
                        BinOp::Minus => l_num - r_num,
                        _ => unreachable!(),
                    };
                    return (format!("{}{}", l_code, r_code), result.to_string());
                }
                let dst = ctx.next_temp();
                let inst = match op {
                    BinOp::Add => format!("  {} = add {}, {}", dst, l_val, r_val),
                    BinOp::Minus => format!("  {} = sub {}, {}", dst, l_val, r_val),
                    _ => panic!("unexpected binop in AddExp"),
                };
                (format!("{}{}{}\n", l_code, r_code, inst), dst)
            }
        }
    }
}

impl MulExp {
    pub fn eval(&self, symtab: &SymbolTable) -> Result<i32, String> {
        match self {
            MulExp::Unary(unary) => unary.eval(symtab),
            MulExp::Binary(lhs, op, rhs) => {
                let lval = lhs.eval(symtab)?;
                let rval = rhs.eval(symtab)?;
                Ok(match op {
                    BinOp::Mul => lval * rval,
                    BinOp::Div => lval / rval,
                    BinOp::Mod => lval % rval,
                    _ => unreachable!("Invalid binary operator in MulExp"),
                })
            }
        }
    }
    pub fn to_ir(&self, ctx: &mut IrGenContext, symtab: &mut SymbolTable) -> (String, String) {
        match self {
            MulExp::Unary(u) => u.to_ir(ctx, symtab),
            MulExp::Binary(lhs, op, rhs) => {
                let (l_code, l_val) = lhs.to_ir(ctx, symtab);
                let (r_code, r_val) = rhs.to_ir(ctx, symtab);
                if let (Ok(l_num), Ok(r_num)) = (l_val.parse::<i32>(), r_val.parse::<i32>()) {
                    let result = match op {
                        BinOp::Mul => l_num * r_num,
                        BinOp::Div => l_num / r_num,
                        BinOp::Mod => l_num % r_num,
                        _ => unreachable!(),
                    };
                    return (format!("{}{}", l_code, r_code), result.to_string());
                }

                let dst = ctx.next_temp();
                let inst = match op {
                    BinOp::Mul => format!("  {} = mul {}, {}", dst, l_val, r_val),
                    BinOp::Div => format!("  {} = div {}, {}", dst, l_val, r_val),
                    BinOp::Mod => format!("  {} = mod {}, {}", dst, l_val, r_val),
                    _ => panic!("unexpected binop in MulExp"),
                };
                (format!("{}{}{}\n", l_code, r_code, inst), dst)
            }
        }
    }
}

impl UnaryExp {
    pub fn eval(&self, symtab: &SymbolTable) -> Result<i32, String> {
        match self {
            UnaryExp::Primary(primary) => primary.eval(symtab),
            UnaryExp::UnaryOp(op, expr) => {
                let val = expr.eval(symtab)?;
                Ok(match op {
                    UnaryOp::Plus => val,
                    UnaryOp::Minus => -val,
                    UnaryOp::Not => (val == 0) as i32,
                })
            }
        }
    }
    pub fn to_ir(&self, ctx: &mut IrGenContext, symtab: &mut SymbolTable) -> (String, String) {
        match self {
            UnaryExp::Primary(p) => p.to_ir(ctx, symtab),
            UnaryExp::UnaryOp(op, exp) => {
                let (code, val) = exp.to_ir(ctx, symtab);
                if let (Ok(num)) = (val.parse::<i32>()) {
                    let result = match op {
                        UnaryOp::Plus => num,
                        UnaryOp::Minus => -num,
                        UnaryOp::Not => (num == 0) as i32,
                    };
                    return (format!("{}\n", code), result.to_string());
                }
                let dst = ctx.next_temp();
                let inst = match op {
                    UnaryOp::Plus => return (code, val),
                    UnaryOp::Minus => format!("  {} = sub 0, {}", dst, val),
                    UnaryOp::Not => format!("  {} = eq 0, {}", dst, val),
                };
                (format!("{}{}\n", code, inst), dst)
            }
        }
    }
}

impl PrimaryExp {
    pub fn eval(&self, symtab: &SymbolTable) -> Result<i32, String> {
        match self {
            PrimaryExp::Number(n) => Ok(*n),
            PrimaryExp::LVal(lval) => lval.eval(symtab),
            PrimaryExp::Paren(exp) => exp.eval(symtab),
        }
    }
    pub fn to_ir(&self, ctx: &mut IrGenContext, symtab: &mut SymbolTable) -> (String, String) {
        match self {
            PrimaryExp::Number(n) => (String::new(), format!("{}", n)),
            PrimaryExp::LVal(lval) => lval.to_ir(ctx, symtab),
            PrimaryExp::Paren(inner) => inner.to_ir(ctx, symtab),
        }
    }
}
