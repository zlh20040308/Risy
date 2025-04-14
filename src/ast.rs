// src/ast.rs
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
    pub block_items: Vec<Box<BlockItem>>,
}

#[derive(Debug, Clone, Serialize)]
pub enum Stmt {
    Matched(Box<MatchedStmt>),
    Unmatched(Box<UnmatchedStmt>),
}

#[derive(Debug, Clone, Serialize)]
pub enum MatchedStmt {
    If(Box<Exp>, Box<MatchedStmt>, Box<MatchedStmt>),
    NonIf(Box<NonIfStmt>),
}

#[derive(Debug, Clone, Serialize)]
pub enum UnmatchedStmt {
    Else(Box<Exp>, Box<Stmt>),
    NonElse(Box<Exp>, Box<MatchedStmt>, Box<UnmatchedStmt>),
}

#[derive(Debug, Clone, Serialize)]
pub enum NonIfStmt {
    Assign(Box<LVal>, Box<Exp>),
    Exp(Option<Box<Exp>>),
    Block(Box<Block>),
    Return(Option<Box<Exp>>),
}

#[derive(Debug, Clone, Serialize)]
pub struct Exp {
    pub lor_exp: Box<LOrExp>,
}

#[derive(Debug, Clone, Serialize)]
pub enum Decl {
    Const(Box<ConstDecl>),
    Var(Box<VarDecl>),
}

#[derive(Debug, Clone, Serialize)]
pub struct VarDecl {
    pub btype: String,
    pub var_defs: Vec<Box<VarDef>>,
}

#[derive(Debug, Clone, Serialize)]
pub struct ConstDecl {
    pub btype: String,
    pub const_defs: Vec<Box<ConstDef>>,
}

#[derive(Debug, Clone, Serialize)]
pub enum VarDef {
    Ident(String),
    Init(String, Box<InitVal>),
}

#[derive(Debug, Clone, Serialize)]
pub struct ConstDef {
    pub ident: String,
    pub const_init_val: Box<ConstInitVal>,
}

#[derive(Debug, Clone, Serialize)]
pub struct ConstInitVal {
    pub const_exp: Box<ConstExp>,
}

#[derive(Debug, Clone, Serialize)]
pub struct LVal {
    pub ident: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct InitVal {
    pub exp: Box<Exp>,
}

#[derive(Debug, Clone, Serialize)]
pub struct ConstExp {
    pub exp: Box<Exp>,
}

#[derive(Debug, Clone, Serialize)]
pub enum BlockItem {
    Decl(Box<Decl>),
    Stmt(Box<Stmt>),
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
    LVal(Box<LVal>),
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
