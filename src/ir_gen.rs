use std::process::exit;

// src/ir_gen.rs
use super::ast::*;
use super::context::IrContext;
use super::context::SymbolValue;

#[derive(Debug)]
pub struct IrResult {
    pub code: String,          // 生成的IR代码
    pub value: Option<String>, // 可能的值（None表示void）
}

impl IrResult {
    pub fn new(code: String, value: Option<String>) -> Self {
        Self { code, value }
    }
}

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

// 提前声明 SysY 库函数
pub fn register_sysy_library(ctx: &mut IrContext, code: &mut String) {
    // decl @getint(): i32
    ctx.insert(
        "getint",
        SymbolValue::Func {
            name: "getint".to_string(),
            params: Vec::new(),
            ret_type: Type::Int,
        },
    )
    .expect("Failed to insert symbol");
    // decl @getch(): i32
    ctx.insert(
        "getch",
        SymbolValue::Func {
            name: "getch".to_string(),
            params: Vec::new(),
            ret_type: Type::Int,
        },
    )
    .expect("Failed to insert symbol");
    // decl @putint(i32)
    ctx.insert(
        "putint",
        SymbolValue::Func {
            name: "putint".to_string(),
            params: Vec::new(),
            ret_type: Type::Void,
        },
    )
    .expect("Failed to insert symbol");
    // decl @putch(i32)
    ctx.insert(
        "putch",
        SymbolValue::Func {
            name: "putch".to_string(),
            params: Vec::new(),
            ret_type: Type::Void,
        },
    )
    .expect("Failed to insert symbol");
    // decl @starttime()
    ctx.insert(
        "starttime",
        SymbolValue::Func {
            name: "starttime".to_string(),
            params: Vec::new(),
            ret_type: Type::Void,
        },
    )
    .expect("Failed to insert symbol");
    // decl @stoptime()
    ctx.insert(
        "stoptime",
        SymbolValue::Func {
            name: "stoptime".to_string(),
            params: Vec::new(),
            ret_type: Type::Void,
        },
    )
    .expect("Failed to insert symbol");

    code.push_str("decl @getint(): i32\n");
    code.push_str("decl @getch(): i32\n");
    code.push_str("decl @putint(i32)\n");
    code.push_str("decl @putch(i32)\n");
    code.push_str("decl @starttime()\n");
    code.push_str("decl @stoptime()\n");
    code.push_str("\n");
}

impl CompUnit {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        let mut result = String::new();
        // 提前声明 SysY 库函数
        register_sysy_library(ctx, &mut result);
        // 生成各个 item ir
        for item in &self.items {
            match &**item {
                TopLevel::Decl(decl) => {
                    result.push_str(&decl.to_ir(ctx)); // 处理全局变量和常量
                }
                TopLevel::Func(func_def) => {
                    ctx.insert(
                        &func_def.ident,
                        SymbolValue::Func {
                            name: func_def.ident.clone(),
                            params: func_def.params.clone(),
                            ret_type: func_def.func_type.clone(),
                        },
                    )
                    .expect("Failed to insert symbol");
                    result.push_str(&func_def.to_ir(ctx)); // 处理函数定义
                }
            }
        }
        result
    }
}

impl FuncDef {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        let mut code = String::new();
        code.push_str(&format!(
            "fun @{}({}){}{{\n",
            self.ident,
            self.params
                .iter()
                .map(|param| format!("@{}: i32", param.ident.clone()))
                .collect::<Vec<_>>()
                .join(", "),
            self.func_type.to_return()
        ));
        code.push_str("%entry:\n");
        ctx.enter_scope();
        let mut body = String::new();
        for param in &self.params {
            ctx.insert(
                &param.ident,
                SymbolValue::LocalVar(format!("%{}", param.ident)),
            )
            .expect("Failed to insert symbol");
            body.push_str(&format!("  %{} = alloc i32\n", param.ident));
            body.push_str(&format!("  store @{}, %{}\n", param.ident, param.ident));
        }
        body.push_str(&self.block.to_ir(ctx));
        ctx.exit_scope();
        code.push_str(&body);
        if !is_terminated(&body) {
            code.push_str("  ret\n");
        }
        code.push_str("}\n\n");
        code
    }
}

impl Type {
    fn to_return(&self) -> String {
        match self {
            Self::Int => String::from(": i32 "),
            Self::Void => String::from(" "),
        }
    }
}

impl Block {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        let mut ir_code = String::new();
        for item in &self.block_items {
            let item_code = item.to_ir(ctx);
            ir_code.push_str(&item_code);

            if is_terminated(&item_code) {
                break;
            }
        }
        // println!("{}", serde_json::to_string_pretty(&ctx).unwrap());
        ir_code
    }
}

impl Stmt {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        match self {
            Self::Open(open) => open.to_ir(ctx),
            Self::Closed(closed) => closed.to_ir(ctx),
        }
    }
}

impl OpenStmt {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        match self {
            Self::If(cond, then_stmt) => {
                let mut code = String::new();
                let cond_result = cond.to_ir(ctx);
                let cond_code = cond_result.code;
                let cond_val = cond_result
                    .value
                    .expect("Condition expression must have a value");
                let then_label = ctx.generate_label("then");
                let end_label = ctx.generate_label("end");
                code.push_str(&cond_code);
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
            Self::Else(cond, then_stmt, else_stmt) => {
                let mut code = String::new();
                let cond_result = cond.to_ir(ctx);
                let cond_code = cond_result.code;
                let cond_val = cond_result
                    .value
                    .expect("Condition expression must have a value");
                let then_label = ctx.generate_label("then");
                let else_label = ctx.generate_label("else");
                let end_label = ctx.generate_label("end");
                code.push_str(&cond_code);
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
            Self::While(cond, body) => {
                let mut code = String::new();
                let entry_label = ctx.generate_label("while_entry");
                let body_label = ctx.generate_label("while_body");
                let end_label = ctx.generate_label("end");
                ctx.enter_loop(entry_label.clone(), end_label.clone());
                code.push_str(&format!("  jump {}\n", entry_label));
                code.push_str(&format!("{}:\n", entry_label));
                let cond_result = cond.to_ir(ctx);
                let cond_code = cond_result.code;
                let cond_val = cond_result
                    .value
                    .expect("Condition expression must have a value");
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
            Self::Else(cond, then_stmt, else_stmt) => {
                let mut code = String::new();
                let cond_result = cond.to_ir(ctx);
                let cond_code = cond_result.code;
                let cond_val = cond_result
                    .value
                    .expect("Condition expression must have a value");
                let then_label = ctx.generate_label("then");
                let else_label = ctx.generate_label("else");
                let end_label = ctx.generate_label("end");
                code.push_str(&cond_code);
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
            Self::Simple(stmt) => stmt.to_ir(ctx),
            Self::While(cond, body) => {
                let mut code = String::new();
                let entry_label = ctx.generate_label("while_entry");
                let body_label = ctx.generate_label("while_body");
                let end_label = ctx.generate_label("end");
                ctx.enter_loop(entry_label.clone(), end_label.clone());
                code.push_str(&format!("  jump {}\n", entry_label));

                code.push_str(&format!("{}:\n", entry_label));
                let cond_result = cond.to_ir(ctx);
                let cond_code = cond_result.code;
                let cond_val = cond_result
                    .value
                    .expect("Condition expression must have a value");
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
            Self::Assign(lval, exp) => {
                let exp_result = exp.to_ir(ctx);
                let exp_code = exp_result.code;
                let exp_val = exp_result
                    .value
                    .expect("Condition expression must have a value");
                match ctx.lookup(&lval.ident).unwrap() {
                    SymbolValue::LocalVar(ir_name) => {
                        format!("{}  store {}, {}\n", exp_code, exp_val, ir_name)
                    }
                    SymbolValue::GlobalVar { name, .. } => {
                        format!("{}  store {}, {}\n", exp_code, exp_val, name)
                    }
                    _ => {
                        panic!("{} is not a LVar", lval.ident)
                    }
                }
            }
            Self::Eval(exp) => {
                if let Some(exp) = exp {
                    let exp_result = exp.to_ir(ctx);
                    let exp_code = exp_result.code;
                    exp_code
                } else {
                    String::new()
                }
            }
            Self::Break => {
                let label = ctx.current_loop_labels().unwrap().exit.clone();
                format!("  jump {}\n", label)
            }
            Self::Continue => {
                let label = ctx.current_loop_labels().unwrap().entry.clone();
                format!("  jump {}\n", label)
            }
            Self::Return(exp) => {
                if let Some(exp) = exp {
                    let exp_result = exp.to_ir(ctx);
                    let exp_code = exp_result.code;
                    let exp_val = exp_result
                        .value
                        .expect("Condition expression must have a value");
                    format!("{}  ret {}\n", exp_code, exp_val)
                } else {
                    format!("  ret\n")
                }
            }
            Self::Block(block) => {
                ctx.enter_scope();
                let code = block.to_ir(ctx);
                ctx.exit_scope();
                format!("{}", code)
            }
        }
    }
}

impl Exp {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        self.lor_exp.eval(ctx)
    }

    pub fn to_ir(&self, ctx: &mut IrContext) -> IrResult {
        self.lor_exp.to_ir(ctx)
    }
}

impl Decl {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        match self {
            Self::Var(var_decl) => var_decl.to_ir(ctx),
            Self::Const(const_decl) => {
                const_decl.analyze(ctx);
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
    pub fn analyze(&self, ctx: &mut IrContext) {
        for def in &self.const_defs {
            let value = def.const_init_val.eval(ctx); // 编译期求值
            ctx.insert(&def.ident, SymbolValue::Const(value))
                .unwrap_or_else(|e| {
                    eprintln!("{}", e);
                    exit(1);
                });
        }
    }
}

impl VarDef {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        match self {
            Self::Ident(name) => {
                let ir_name = ctx.generate_named(name);
                if ctx.is_global() {
                    match ctx.insert(
                        name,
                        SymbolValue::GlobalVar {
                            name: ir_name.clone(),
                            init: 0,
                        },
                    ) {
                        Ok(_) => {
                            format!("global {}= alloc i32, zeroinit\n", ir_name)
                        }
                        Err(e) => panic!("{}", e),
                    }
                } else {
                    match ctx.insert(name, SymbolValue::LocalVar(ir_name.clone())) {
                        Ok(_) => {
                            format!("  {} = alloc i32\n", ir_name)
                        }
                        Err(e) => panic!("{}", e),
                    }
                }
            }
            Self::Init(name, init_value) => {
                let ir_name = ctx.generate_named(name);
                if ctx.is_global() {
                    let init_val = init_value.eval(ctx).expect("");
                    match ctx.insert(
                        name,
                        SymbolValue::GlobalVar {
                            name: ir_name.clone(),
                            init: init_val,
                        },
                    ) {
                        Ok(_) => {
                            format!("global {}= alloc i32, {}\n", ir_name, init_val)
                        }
                        Err(e) => panic!("{}", e),
                    }
                } else {
                    let init_result = init_value.to_ir(ctx);
                    let init_code = init_result.code;
                    let init_value = init_result
                        .value
                        .expect("Condition expression must have a value");
                    match ctx.insert(name, SymbolValue::LocalVar(ir_name.clone())) {
                        Ok(_) => {
                            format!(
                                "{}  {} = alloc i32\n  store {}, {}\n",
                                init_code, ir_name, init_value, ir_name
                            )
                        }
                        Err(e) => panic!("{}", e),
                    }
                }
            }
        }
    }
}

impl InitVal {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        self.exp.eval(ctx)
    }
}

impl ConstInitVal {
    pub fn eval(&self, ctx: &mut IrContext) -> i32 {
        self.const_exp.eval(ctx)
    }
}

impl LVal {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        match ctx.lookup(&self.ident) {
            Some(SymbolValue::Const(val)) => Ok(val),
            Some(SymbolValue::GlobalVar { name: _, init }) => Ok(init),
            Some(SymbolValue::LocalVar(_)) => Err(format!(
                "Cannot evaluate local variable '{}' at compile-time",
                self.ident
            )),
            Some(SymbolValue::Func { .. }) => Err(format!(
                "Cannot evaluate function '{}' as an lvalue",
                self.ident
            )),
            None => Err(format!(
                "Identifier '{}' not found in current scope",
                self.ident
            )),
        }
    }

    pub fn to_ir(&self, ctx: &mut IrContext) -> IrResult {
        match ctx.lookup(&self.ident) {
            Some(SymbolValue::LocalVar(ir_name)) => {
                let temp = ctx.next_temp();
                let code = format!("  {} = load {}\n", temp, ir_name);
                IrResult::new(code, Some(temp))
            }
            Some(SymbolValue::GlobalVar { name: ir_name, .. }) => {
                let temp = ctx.next_temp();
                let code = format!("  {} = load {}\n", temp, ir_name);
                IrResult::new(code, Some(temp))
            }
            Some(SymbolValue::Const(value)) => {
                let temp = ctx.next_temp();
                let code = format!("  {} = add 0, {}\n", temp, value);
                IrResult::new(code, Some(temp))
            }
            Some(SymbolValue::Func { .. }) => {
                panic!("Cannot use function '{}' as a variable", self.ident)
            }
            None => panic!("Identifier '{}' not found", self.ident),
        }
    }
}

impl InitVal {
    pub fn to_ir(&self, ctx: &mut IrContext) -> IrResult {
        self.exp.to_ir(ctx)
    }
}

impl ConstExp {
    pub fn eval(&self, ctx: &mut IrContext) -> i32 {
        match self.exp.eval(ctx) {
            Ok(val) => val,
            Err(e) => {
                eprintln!("{}", e);
                exit(1);
            }
        }
    }
}

impl BlockItem {
    pub fn to_ir(&self, ctx: &mut IrContext) -> String {
        match self {
            Self::Decl(decl) => decl.to_ir(ctx),
            Self::Stmt(stmt) => stmt.to_ir(ctx),
        }
    }
}

impl RelExp {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        match self {
            Self::AddExp(add) => add.eval(ctx),
            Self::Rel(lhs, op, rhs) => {
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
    pub fn to_ir(&self, ctx: &mut IrContext) -> IrResult {
        match self {
            Self::AddExp(add) => add.to_ir(ctx),
            Self::Rel(lhs, op, rhs) => {
                let lhs_result = lhs.to_ir(ctx);
                let l_code = lhs_result.code;
                let l_val = lhs_result
                    .value
                    .expect("Condition expression must have a value");
                let rhs_result = rhs.to_ir(ctx);
                let r_code = rhs_result.code;
                let r_val = rhs_result
                    .value
                    .expect("Condition expression must have a value");
                let dst = ctx.next_temp();
                let inst = match op {
                    RelOp::Less => format!("  {} = lt {}, {}", dst, l_val, r_val),
                    RelOp::Greater => format!("  {} = gt {}, {}", dst, l_val, r_val),
                    RelOp::LessEq => format!("  {} = le {}, {}", dst, l_val, r_val),
                    RelOp::GreaterEq => format!("  {} = ge {}, {}", dst, l_val, r_val),
                };
                IrResult::new(format!("{}{}{}\n", l_code, r_code, inst), Some(dst))
            }
        }
    }
}

impl EqExp {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        match self {
            Self::RelExp(rel) => rel.eval(ctx),
            Self::Eq(lhs, op, rhs) => {
                let lval = lhs.eval(ctx)?;
                let rval = rhs.eval(ctx)?;
                Ok(match op {
                    EqOp::Equal => (lval == rval) as i32,
                    EqOp::NotEqual => (lval != rval) as i32,
                })
            }
        }
    }
    pub fn to_ir(&self, ctx: &mut IrContext) -> IrResult {
        match self {
            Self::RelExp(rel) => rel.to_ir(ctx),
            Self::Eq(lhs, op, rhs) => {
                let lhs_result = lhs.to_ir(ctx);
                let l_code = lhs_result.code;
                let l_val = lhs_result
                    .value
                    .expect("Condition expression must have a value");
                let rhs_result = rhs.to_ir(ctx);
                let r_code = rhs_result.code;
                let r_val = rhs_result
                    .value
                    .expect("Condition expression must have a value");
                let dst = ctx.next_temp();
                let inst = match op {
                    EqOp::Equal => format!("  {} = eq {}, {}", dst, l_val, r_val),
                    EqOp::NotEqual => format!("  {} = ne {}, {}", dst, l_val, r_val),
                };
                IrResult::new(format!("{}{}{}\n", l_code, r_code, inst), Some(dst))
            }
        }
    }
}

impl LAndExp {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        match self {
            Self::EqExp(eq) => eq.eval(ctx),
            Self::LAnd(lhs, rhs) => {
                let lval = lhs.eval(ctx)?;
                if lval == 0 {
                    return Ok(0); // 短路，直接返回
                }
                let rval = rhs.eval(ctx)?;
                Ok(if rval != 0 { 1 } else { 0 })
            }
        }
    }
    pub fn to_ir(&self, ctx: &mut IrContext) -> IrResult {
        match self {
            Self::EqExp(eq) => eq.to_ir(ctx),
            Self::LAnd(lhs, rhs) => {
                let result = ctx.next_temp();
                let mut code = String::new();
                code.push_str(&format!("  {} = alloc i32\n", result));
                code.push_str(&format!("  store 0, {}\n", result));
                let lhs_result = lhs.to_ir(ctx);
                let l_code = lhs_result.code;
                let l_val = lhs_result
                    .value
                    .expect("Condition expression must have a value");

                code.push_str(&l_code);
                let rhs_label = ctx.generate_label("rhs_label");
                let true_label = ctx.generate_label("true_label");
                let end_label = ctx.generate_label("end_label");
                code.push_str(&format!("  br {}, {}, {}\n", l_val, rhs_label, end_label));
                code.push_str(&format!("{}:\n", rhs_label));
                let rhs_result = rhs.to_ir(ctx);
                let r_code = rhs_result.code;
                let r_val = rhs_result
                    .value
                    .expect("Condition expression must have a value");
                code.push_str(&r_code);
                code.push_str(&format!("  br {}, {}, {}\n", r_val, true_label, end_label));
                code.push_str(&format!("{}:\n", true_label));
                code.push_str(&format!("  store {}, {}\n", r_val, result));
                code.push_str(&format!("  jump {}\n", end_label));
                code.push_str(&format!("{}:\n", end_label));
                let save = ctx.next_temp();
                let dst = ctx.next_temp();
                code.push_str(&format!("  {} = load {}\n", save, result));
                code.push_str(&format!("  {} = ne 0, {}\n", dst, save));
                IrResult::new(code, Some(dst))
            }
        }
    }
}

impl LOrExp {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        match self {
            Self::LAndExp(land) => land.eval(ctx),
            Self::LOr(lhs, rhs) => {
                let lval = lhs.eval(ctx)?;
                if lval != 0 {
                    return Ok(1); // 短路，直接返回
                }
                let rval = rhs.eval(ctx)?;
                Ok(if rval != 0 { 1 } else { 0 })
            }
        }
    }
    pub fn to_ir(&self, ctx: &mut IrContext) -> IrResult {
        match self {
            Self::LAndExp(land) => land.to_ir(ctx),
            Self::LOr(lhs, rhs) => {
                let result = ctx.next_temp();
                let mut code = String::new();
                code.push_str(&format!("  {} = alloc i32\n", result));
                code.push_str(&format!("  store 1, {}\n", result));
                let lhs_result = lhs.to_ir(ctx);
                let l_code = lhs_result.code;
                let l_val = lhs_result
                    .value
                    .expect("Condition expression must have a value");
                code.push_str(&l_code);
                let rhs_label = ctx.generate_label("rhs_label");
                let true_label = ctx.generate_label("true_label");
                let end_label = ctx.generate_label("end_label");
                code.push_str(&format!("  br {}, {}, {}\n", l_val, end_label, rhs_label));
                code.push_str(&format!("{}:\n", rhs_label));
                let rhs_result = rhs.to_ir(ctx);
                let r_code = rhs_result.code;
                let r_val = rhs_result
                    .value
                    .expect("Condition expression must have a value");
                code.push_str(&r_code);
                code.push_str(&format!("  br {}, {}, {}\n", r_val, end_label, true_label));
                code.push_str(&format!("{}:\n", true_label));
                code.push_str(&format!("  store {}, {}\n", r_val, result));
                code.push_str(&format!("  jump {}\n", end_label));
                code.push_str(&format!("{}:\n", end_label));
                let save = ctx.next_temp();
                let dst = ctx.next_temp();
                code.push_str(&format!("  {} = load {}\n", save, result));
                code.push_str(&format!("  {} = ne 0, {}\n", dst, save));
                IrResult::new(code, Some(dst))
            }
        }
    }
}

impl AddExp {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        match self {
            Self::MulExp(mul) => mul.eval(ctx),
            Self::Binary(lhs, op, rhs) => {
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

    pub fn to_ir(&self, ctx: &mut IrContext) -> IrResult {
        match self {
            Self::MulExp(mul) => mul.to_ir(ctx),
            Self::Binary(lhs, op, rhs) => {
                let lhs_result = lhs.to_ir(ctx);
                let l_code = lhs_result.code;
                let l_val = lhs_result
                    .value
                    .expect("Condition expression must have a value");
                let rhs_result = rhs.to_ir(ctx);
                let r_code = rhs_result.code;
                let r_val = rhs_result
                    .value
                    .expect("Condition expression must have a value");
                let dst = ctx.next_temp();
                let inst = match op {
                    BinOp::Add => format!("  {} = add {}, {}", dst, l_val, r_val),
                    BinOp::Minus => format!("  {} = sub {}, {}", dst, l_val, r_val),
                    _ => panic!("unexpected binop in AddExp"),
                };
                IrResult::new(format!("{}{}{}\n", l_code, r_code, inst), Some(dst))
            }
        }
    }
}

impl MulExp {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        match self {
            Self::Unary(unary) => unary.eval(ctx),
            Self::Binary(lhs, op, rhs) => {
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
    pub fn to_ir(&self, ctx: &mut IrContext) -> IrResult {
        match self {
            Self::Unary(u) => u.to_ir(ctx),
            Self::Binary(lhs, op, rhs) => {
                let lhs_result = lhs.to_ir(ctx);
                let l_code = lhs_result.code;
                let l_val = lhs_result
                    .value
                    .expect("Condition expression must have a value");
                let rhs_result = rhs.to_ir(ctx);
                let r_code = rhs_result.code;
                let r_val = rhs_result
                    .value
                    .expect("Condition expression must have a value");
                let dst = ctx.next_temp();
                let inst = match op {
                    BinOp::Mul => format!("  {} = mul {}, {}", dst, l_val, r_val),
                    BinOp::Div => format!("  {} = div {}, {}", dst, l_val, r_val),
                    BinOp::Mod => format!("  {} = mod {}, {}", dst, l_val, r_val),
                    _ => panic!("unexpected binop in MulExp"),
                };
                IrResult::new(format!("{}{}{}\n", l_code, r_code, inst), Some(dst))
            }
        }
    }
}

impl UnaryExp {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        match self {
            Self::Primary(primary) => primary.eval(ctx),
            Self::Call(ident, _) => Err(format!(
                "Cannot evaluate function call '{}' at compile-time",
                ident
            )),
            Self::UnaryOp(op, expr) => {
                let val = expr.eval(ctx)?;
                Ok(match op {
                    UnaryOp::Plus => val,
                    UnaryOp::Minus => -val,
                    UnaryOp::Not => (val == 0) as i32,
                })
            }
        }
    }
    pub fn to_ir(&self, ctx: &mut IrContext) -> IrResult {
        match self {
            Self::Primary(exp) => exp.to_ir(ctx),
            Self::Call(ident, args) => {
                let mut code = String::new();
                let mut ir_args = Vec::new();
                for arg in args {
                    let arg_result = arg.to_ir(ctx);
                    let arg_code = arg_result.code;
                    let arg_val = arg_result
                        .value
                        .expect("Condition expression must have a value");
                    code.push_str(&arg_code);
                    ir_args.push(arg_val);
                }
                match ctx.get_func_type(ident) {
                    Ok(Type::Void) => {
                        code.push_str(&format!("  call @{}({})\n", ident, &ir_args.join(", ")));
                        IrResult::new(code, None)
                    }
                    Ok(Type::Int) => {
                        let temp = ctx.next_temp();
                        code.push_str(&format!(
                            "  {} = call @{}({})\n",
                            temp,
                            ident,
                            &ir_args.join(", ")
                        ));
                        IrResult::new(code, Some(temp))
                    }
                    Err(err_msg) => {
                        eprintln!("Error: {}", err_msg);
                        exit(1);
                    }
                }
            }
            Self::UnaryOp(op, exp) => {
                let exp_result = exp.to_ir(ctx);
                let exp_code = exp_result.code;
                let exp_val = exp_result
                    .value
                    .expect("Condition expression must have a value");
                let mut code = String::new();
                code.push_str(&exp_code);
                match op {
                    UnaryOp::Plus => IrResult::new(code, Some(exp_val)),
                    UnaryOp::Minus => {
                        let temp = ctx.next_temp();
                        code.push_str(&format!("  {} = sub 0, {}\n", temp, exp_val));
                        IrResult::new(code, Some(temp))
                    }
                    UnaryOp::Not => {
                        let temp = ctx.next_temp();
                        code.push_str(&format!("  {} = eq 0, {}\n", temp, exp_val));
                        IrResult::new(code, Some(temp))
                    }
                }
            }
        }
    }
}

impl PrimaryExp {
    pub fn eval(&self, ctx: &mut IrContext) -> Result<i32, String> {
        match self {
            Self::Number(n) => Ok(*n),
            Self::LVal(lval) => lval.eval(ctx),
            Self::Paren(exp) => exp.eval(ctx),
        }
    }
    pub fn to_ir(&self, ctx: &mut IrContext) -> IrResult {
        match self {
            Self::Number(n) => {
                let temp = ctx.next_temp();
                let code = format!("  {} = add 0, {}\n", temp.clone(), n);
                IrResult::new(code, Some(temp))
            }
            Self::LVal(lval) => lval.to_ir(ctx),
            Self::Paren(inner) => inner.to_ir(ctx),
        }
    }
}
