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
            "fun @{}(){}{{\n",
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
        match self {
            FuncType::Int => ": i32 ",
            FuncType::Void => " ",
        }
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
            Stmt::Open(open) => open.to_ir(ctx),
            Stmt::Closed(closed) => closed.to_ir(ctx),
        }
    }
}

impl OpenStmt {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        match self {
            OpenStmt::If(cond, then_stmt) => {
                let mut code = String::new();
                let (cond_code, cond_val) = cond.to_ir(ctx);
                let then_label = ctx.generate_label("then");
                let end_label = ctx.generate_label("end");
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
                let then_code = then_stmt.to_ir(ctx);
                code.push_str(&then_code);
                if !is_terminated(&then_code) {
                    code.push_str(&format!("  jump {}\n", end_label));
                }
                code.push_str(&format!("{}:\n", end_label));
                code
            }
            OpenStmt::Else(cond, then_stmt, else_stmt) => {
                let mut code = String::new();
                let (cond_code, cond_val) = cond.to_ir(ctx);
                let then_label = ctx.generate_label("then");
                let else_label = ctx.generate_label("else");
                let end_label = ctx.generate_label("end");
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
                let then_code = then_stmt.to_ir(ctx);
                code.push_str(&then_code);
                if !is_terminated(&then_code) {
                    code.push_str(&format!("  jump {}\n", end_label));
                }
                // else
                code.push_str(&format!("{}:\n", else_label));
                let else_code = else_stmt.to_ir(ctx);
                code.push_str(&else_code);
                if !is_terminated(&else_code) {
                    code.push_str(&format!("  jump {}\n", end_label));
                }
                // end
                code.push_str(&format!("{}:\n", end_label));
                code
            }
            OpenStmt::While(cond, body) => {
                let mut code = String::new();
                let entry_label = ctx.generate_label("while_entry");
                let body_label = ctx.generate_label("while_body");
                let end_label = ctx.generate_label("end");
                ctx.enter_loop(entry_label.clone(), end_label.clone());
                code.push_str(&format!("  jump {}\n", entry_label));
                code.push_str(&format!("{}:\n", entry_label));
                let (cond_code, cond_val) = cond.to_ir(ctx);
                code.push_str(&cond_code);
                code.push_str(&format!(
                    "  br {}, {}, {}\n",
                    cond_val, body_label, end_label
                ));
                code.push_str(&format!("{}:\n", body_label));
                let body_code = body.to_ir(ctx);
                code.push_str(&body_code);
                if !is_terminated(&code) {
                    code.push_str(&format!("  jump {}\n", entry_label));
                }
                // end
                code.push_str(&format!("{}:\n", end_label));
                ctx.exit_loop();
                code
            }
        }
    }
}

impl ClosedStmt {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        match self {
            ClosedStmt::Else(cond, then_stmt, else_stmt) => {
                let mut code = String::new();
                let (cond_code, cond_val) = cond.to_ir(ctx);
                let then_label = ctx.generate_label("then");
                let else_label = ctx.generate_label("else");
                let end_label = ctx.generate_label("end");
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
                let then_code = then_stmt.to_ir(ctx);
                code.push_str(&then_code);
                if !is_terminated(&then_code) {
                    code.push_str(&format!("  jump {}\n", end_label));
                }
                // else
                code.push_str(&format!("{}:\n", else_label));
                let else_code = else_stmt.to_ir(ctx);
                code.push_str(&else_code);
                if !is_terminated(&else_code) {
                    code.push_str(&format!("  jump {}\n", end_label));
                }
                // end
                code.push_str(&format!("{}:\n", end_label));
                code
            }
            ClosedStmt::Simple(stmt) => stmt.to_ir(ctx),
            ClosedStmt::While(cond, body) => {
                let mut code = String::new();
                let entry_label = ctx.generate_label("while_entry");
                let body_label = ctx.generate_label("while_body");
                let end_label = ctx.generate_label("end");
                ctx.enter_loop(entry_label.clone(), end_label.clone());
                code.push_str(&format!("  jump {}\n", entry_label));

                code.push_str(&format!("{}:\n", entry_label));
                let (cond_code, cond_val) = cond.to_ir(ctx);
                code.push_str(&cond_code);
                code.push_str(&format!(
                    "  br {}, {}, {}\n",
                    cond_val, body_label, end_label
                ));
                code.push_str(&format!("{}:\n", body_label));

                let body_code = body.to_ir(ctx);
                code.push_str(&body_code);
                if !is_terminated(&code) {
                    code.push_str(&format!("  jump {}\n", entry_label));
                }
                // end
                code.push_str(&format!("{}:\n", end_label));
                ctx.exit_loop();
                code
            }
        }
    }
}

impl SimpleStmt {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        match self {
            SimpleStmt::Assign(lval, exp) => {
                let (exp_code, exp_val) = exp.to_ir(ctx);
                if let SymbolValue::Var(ir_name) = ctx.lookup(&lval.ident).unwrap() {
                    format!("{}  store {}, {}\n", exp_code, exp_val, ir_name)
                } else {
                    panic!("LVal is not a Var")
                }
            }
            SimpleStmt::Eval(exp) => {
                if let Some(exp) = exp {
                    let (exp_code, _) = exp.to_ir(ctx);
                    exp_code
                } else {
                    String::new()
                }
            }
            SimpleStmt::Break => {
                let label = ctx.current_loop_labels().unwrap().exit.clone();
                format!("  jump {}\n", label)
            }
            SimpleStmt::Continue => {
                let label = ctx.current_loop_labels().unwrap().entry.clone();
                format!("  jump {}\n", label)
            }
            SimpleStmt::Return(exp) => {
                if let Some(exp) = exp {
                    let (exp_code, exp_val) = exp.to_ir(ctx);
                    format!("{}  ret {}\n", exp_code, exp_val)
                } else {
                    format!("  ret\n")
                }
            }
            SimpleStmt::Block(block) => {
                let code = block.to_ir(ctx);
                format!("{}", code)
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
            Some(SymbolValue::Const(val)) => Ok(val),
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

    pub fn to_ir(&self, ctx: &mut IrContext) -> (String, String) {
        match ctx.lookup(&self.ident) {
            Some(SymbolValue::Var(ir_name)) => {
                let temp = ctx.next_temp();
                let code = format!("  {} = load {}\n", temp, ir_name);
                (code, temp)
            }
            Some(SymbolValue::Const(value)) => {
                let temp = ctx.next_temp();
                let code = format!("  {} = add 0, {}\n", temp, value);
                (code, temp)
            }
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
                let (l_code, l_val) = lhs.to_ir(ctx);
                let (r_code, r_val) = rhs.to_ir(ctx);
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
                let (l_code, l_val) = lhs.to_ir(ctx);
                let (r_code, r_val) = rhs.to_ir(ctx);
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
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        match self {
            LAndExp::EqExp(eq) => eq.eval(ctx),
            LAndExp::LAnd(lhs, rhs) => {
                let lval = lhs.eval(ctx)?;
                if lval == 0 {
                    return Ok(0); // 短路，直接返回
                }
                let rval = rhs.eval(ctx)?;
                Ok(if rval != 0 { 1 } else { 0 })
            }
        }
    }
    pub fn to_ir(&self, ctx: &mut IrContext) -> (String, String) {
        match self {
            LAndExp::EqExp(eq) => eq.to_ir(ctx),
            LAndExp::LAnd(lhs, rhs) => {
                let result = ctx.next_temp();
                let mut code = String::new();
                code.push_str(&format!("  {} = alloc i32\n", result));
                code.push_str(&format!("  store 0, {}\n", result));
                let (lhs_code, lhs_val) = lhs.to_ir(ctx);
                code.push_str(&lhs_code);
                let rhs_label = ctx.generate_label("rhs_label");
                let true_label = ctx.generate_label("true_label");
                let end_label = ctx.generate_label("end_label");
                code.push_str(&format!("  br {}, {}, {}\n", lhs_val, rhs_label, end_label));
                code.push_str(&format!("{}:\n", rhs_label));
                let (rhs_code, rhs_val) = rhs.to_ir(ctx);
                code.push_str(&rhs_code);
                code.push_str(&format!(
                    "  br {}, {}, {}\n",
                    rhs_val, true_label, end_label
                ));
                code.push_str(&format!("{}:\n", true_label));
                code.push_str(&format!("  store {}, {}\n", rhs_val, result));
                code.push_str(&format!("  jump {}\n", end_label));
                code.push_str(&format!("{}:\n", end_label));
                let save = ctx.next_temp();
                let dst = ctx.next_temp();
                code.push_str(&format!("  {} = load {}\n", save, result));
                code.push_str(&format!("  {} = ne 0, {}\n", dst, save));
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
                if lval != 0 {
                    return Ok(1); // 短路，直接返回
                }
                let rval = rhs.eval(ctx)?;
                Ok(if rval != 0 { 1 } else { 0 })
            }
        }
    }
    pub fn to_ir(&self, ctx: &mut IrContext) -> (String, String) {
        match self {
            LOrExp::LAndExp(land) => land.to_ir(ctx),
            LOrExp::LOr(lhs, rhs) => {
                let result = ctx.next_temp();
                let mut code = String::new();
                code.push_str(&format!("  {} = alloc i32\n", result));
                code.push_str(&format!("  store 1, {}\n", result));
                let (lhs_code, lhs_val) = lhs.to_ir(ctx);
                code.push_str(&lhs_code);
                let rhs_label = ctx.generate_label("rhs_label");
                let true_label = ctx.generate_label("true_label");
                let end_label = ctx.generate_label("end_label");
                code.push_str(&format!("  br {}, {}, {}\n", lhs_val, end_label, rhs_label));
                code.push_str(&format!("{}:\n", rhs_label));
                let (rhs_code, rhs_val) = rhs.to_ir(ctx);
                code.push_str(&rhs_code);
                code.push_str(&format!(
                    "  br {}, {}, {}\n",
                    rhs_val, end_label, true_label
                ));
                code.push_str(&format!("{}:\n", true_label));
                code.push_str(&format!("  store {}, {}\n", rhs_val, result));
                code.push_str(&format!("  jump {}\n", end_label));
                code.push_str(&format!("{}:\n", end_label));
                let save = ctx.next_temp();
                let dst = ctx.next_temp();
                code.push_str(&format!("  {} = load {}\n", save, result));
                code.push_str(&format!("  {} = ne 0, {}\n", dst, save));
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
                let (l_code, l_val) = lhs.to_ir(ctx);
                let (r_code, r_val) = rhs.to_ir(ctx);
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
                let (l_code, l_val) = lhs.to_ir(ctx);
                let (r_code, r_val) = rhs.to_ir(ctx);
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
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        match self {
            UnaryExp::Primary(primary) => primary.eval(ctx),
            UnaryExp::Call(_ident, _args) => {
                todo!()
            }
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
            UnaryExp::Call(_ident, _args) => {
                todo!()
            }
            UnaryExp::UnaryOp(op, exp) => {
                let (exp_code, exp_val) = exp.to_ir(ctx);
                let mut code = String::new();
                code.push_str(&exp_code);
                match op {
                    UnaryOp::Plus => (code, exp_val),
                    UnaryOp::Minus => {
                        let temp = ctx.next_temp();
                        code.push_str(&format!("  {} = sub 0, {}\n", temp, exp_val));
                        (code, temp)
                    }
                    UnaryOp::Not => {
                        let temp = ctx.next_temp();
                        code.push_str(&format!("  {} = eq 0, {}\n", temp, exp_val));
                        (code, temp)
                    }
                }
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
            PrimaryExp::Number(n) => {
                let temp = ctx.next_temp();
                let code = format!("  {} = add 0, {}\n", temp.clone(), n);
                (code, temp)
            }
            PrimaryExp::LVal(lval) => lval.to_ir(ctx),
            PrimaryExp::Paren(inner) => inner.to_ir(ctx),
        }
    }
}
