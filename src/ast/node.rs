use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct CompUnit {
    pub func_def: Box<FuncDef>,
}

#[derive(Debug, Clone, Serialize)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Box<Block>,
}

#[derive(Debug, Clone, Serialize)]
pub enum FuncType {
    Int,
}

#[derive(Debug, Clone, Serialize)]
pub struct Block {
    pub stmt: Box<Stmt>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Stmt {
    pub exp: Box<Exp>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Exp {
    pub lor_exp: Box<LOrExp>,
}

#[derive(Debug, Clone, Serialize)]
pub enum RelExp {
    AddExp(Box<AddExp>),
    Rel(Box<RelExp>, RelOp, Box<AddExp>),
}

#[derive(Debug, Clone, Serialize)]
pub enum EqExp {
    RelExp(Box<RelExp>),
    Eq(Box<EqExp>, EqOp, Box<RelExp>),
}

#[derive(Debug, Clone, Serialize)]
pub enum LAndExp {
    EqExp(Box<EqExp>),
    LAnd(Box<LAndExp>, Box<EqExp>),
}

#[derive(Debug, Clone, Serialize)]
pub enum LOrExp {
    LAndExp(Box<LAndExp>),
    LOr(Box<LOrExp>, Box<LAndExp>),
}

#[derive(Debug, Clone, Serialize)]
pub enum AddExp {
    MulExp(Box<MulExp>),
    Binary(Box<AddExp>, BinOp, Box<MulExp>),
}

#[derive(Debug, Clone, Serialize)]
pub enum MulExp {
    Unary(Box<UnaryExp>),
    Binary(Box<MulExp>, BinOp, Box<UnaryExp>),
}

#[derive(Debug, Clone, Serialize)]
pub enum PrimaryExp {
    Number(i32),
    Paren(Box<Exp>),
}

#[derive(Debug, Clone, Serialize)]
pub enum UnaryExp {
    Primary(Box<PrimaryExp>),
    UnaryOp(UnaryOp, Box<UnaryExp>),
}

#[derive(Debug, Clone, Serialize)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}

#[derive(Debug, Clone, Serialize)]
pub enum BinOp {
    Add,
    Minus,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, Serialize)]
pub enum EqOp {
    Equal,    // "=="
    NotEqual, // "!="
}

#[derive(Debug, Clone, Serialize)]
pub enum RelOp {
    Less,      // "<"
    Greater,   // ">"
    LessEq,    // "<="
    GreaterEq, // ">="
}
