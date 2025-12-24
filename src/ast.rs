// src/ast.rs
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub enum TopLevel {
    Decl(Box<Decl>),
    Func(Box<FuncDef>),
}

#[derive(Debug, Clone, Serialize)]
pub struct CompUnit {
    pub items: Vec<Box<TopLevel>>,
}

#[derive(Debug, Clone, Serialize)]
pub struct FuncDef {
    pub func_type: Type,
    pub ident: String,
    pub params: Vec<FuncFParam>,
    pub block: Box<Block>,
}

#[derive(Debug, Clone, Serialize)]
pub enum Type {
    Void,
    Int,
}

#[derive(Debug, Clone, Serialize)]
pub struct FuncFParam {
    pub func_type: Type,
    pub ident: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct Block {
    pub block_items: Vec<Box<BlockItem>>,
}

#[derive(Debug, Clone, Serialize)]
pub enum Stmt {
    Open(Box<OpenStmt>),
    Closed(Box<ClosedStmt>),
}

#[derive(Debug, Clone, Serialize)]
pub enum OpenStmt {
    If(Box<Exp>, Box<Stmt>),
    Else(Box<Exp>, Box<ClosedStmt>, Box<OpenStmt>),
    While(Box<Exp>, Box<OpenStmt>),
}

#[derive(Debug, Clone, Serialize)]
pub enum ClosedStmt {
    Simple(Box<SimpleStmt>),
    Else(Box<Exp>, Box<ClosedStmt>, Box<ClosedStmt>),
    While(Box<Exp>, Box<ClosedStmt>),
}

#[derive(Debug, Clone, Serialize)]
pub enum SimpleStmt {
    Assign(Box<LVal>, Box<Exp>),
    Eval(Option<Box<Exp>>),
    Return(Option<Box<Exp>>),
    Break,
    Continue,
    Block(Box<Block>),
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
    pub btype: Type,
    pub var_defs: Vec<Box<VarDef>>,
}

#[derive(Debug, Clone, Serialize)]
pub struct ConstDecl {
    pub btype: Type,
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
    Call(String, Vec<Box<Exp>>),
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
