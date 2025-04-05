// ast.rs

#[derive(Debug, Clone)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

#[derive(Debug, Clone)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub enum FuncType {
    Int,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmt: Stmt,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub exp: Exp,
}

#[derive(Debug, Clone)]
pub struct Exp {
    pub unary_exp: UnaryExp,
}

#[derive(Debug, Clone)]
pub enum PrimaryExp {
    Number(i32),
    Paren(Box<Exp>),
}

#[derive(Debug, Clone)]
pub enum UnaryExp {
    Primary(PrimaryExp),
    UnaryOp(UnaryOp, Box<UnaryExp>),
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}

pub struct IrGenContext {
    counter: usize,
}

impl IrGenContext {
    pub fn new() -> Self {
        IrGenContext { counter: 0 }
    }

    pub fn next_temp(&mut self) -> String {
        let name = format!("%{}", self.counter);
        self.counter += 1;
        name
    }
}

impl CompUnit {
    pub fn to_ir(&self) -> String {
        let mut ctx = IrGenContext::new();
        self.func_def.to_ir(&mut ctx)
    }
}

impl FuncDef {
    pub fn to_ir(&self, ctx: &mut IrGenContext) -> String {
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
    pub fn to_ir(&self, ctx: &mut IrGenContext) -> String {
        let code = self.stmt.to_ir(ctx);
        format!("%entry:\n{}", code)
    }
}

impl Stmt {
    pub fn to_ir(&self, ctx: &mut IrGenContext) -> String {
        let (code, result) = self.exp.to_ir(ctx);
        format!("{}  ret {}", code, result)
    }
}

impl Exp {
    pub fn to_ir(&self, ctx: &mut IrGenContext) -> (String, String) {
        self.unary_exp.to_ir(ctx)
    }
}

impl UnaryExp {
    pub fn to_ir(&self, ctx: &mut IrGenContext) -> (String, String) {
        match self {
            UnaryExp::Primary(p) => p.to_ir(ctx),
            UnaryExp::UnaryOp(op, exp) => {
                let (code, val) = exp.to_ir(ctx);
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
    pub fn to_ir(&self, ctx: &mut IrGenContext) -> (String, String) {
        match self {
            PrimaryExp::Number(n) => {
                // 不生成 SSA，直接返回数字字面量
                (String::new(), format!("{}", n))
            }
            PrimaryExp::Paren(inner) => inner.to_ir(ctx),
        }
    }
}
