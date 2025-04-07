#[derive(Debug, Clone)]
pub struct CompUnit {
    pub func_def: Box<FuncDef>,
}

#[derive(Debug, Clone)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Box<Block>,
}

#[derive(Debug, Clone)]
pub enum FuncType {
    Int,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmt: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub exp: Box<Exp>,
}

#[derive(Debug, Clone)]
pub struct Exp {
    pub add_exp: Box<AddExp>,
}

#[derive(Debug, Clone)]
pub enum AddExp {
    MulExp(Box<MulExp>),
    Binary(Box<AddExp>, BinOp, Box<MulExp>),
}

#[derive(Debug, Clone)]
pub enum MulExp {
    Unary(Box<UnaryExp>),
    Binary(Box<MulExp>, BinOp, Box<UnaryExp>),
}

#[derive(Debug, Clone)]
pub enum PrimaryExp {
    Number(i32),
    Paren(Box<Exp>),
}

#[derive(Debug, Clone)]
pub enum UnaryExp {
    Primary(Box<PrimaryExp>),
    UnaryOp(UnaryOp, Box<UnaryExp>),
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Minus,
    Mul,
    Div,
    Mod,
}
