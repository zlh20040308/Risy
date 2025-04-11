use super::context::IrGenContext;
use super::node::*;

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
        self.lor_exp.to_ir(ctx)
    }
}

impl RelExp {
    pub fn to_ir(&self, ctx: &mut IrGenContext) -> (String, String) {
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
    pub fn to_ir(&self, ctx: &mut IrGenContext) -> (String, String) {
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
    pub fn to_ir(&self, ctx: &mut IrGenContext) -> (String, String) {
        match self {
            LAndExp::EqExp(eq) => eq.to_ir(ctx),

            LAndExp::LAnd(lhs, rhs) => {
                let (l_code, l_val) = lhs.to_ir(ctx);
                let (r_code, r_val) = rhs.to_ir(ctx);

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
    pub fn to_ir(&self, ctx: &mut IrGenContext) -> (String, String) {
        match self {
            LOrExp::LAndExp(land) => land.to_ir(ctx),

            LOrExp::LOr(lhs, rhs) => {
                let (l_code, l_val) = lhs.to_ir(ctx);
                let (r_code, r_val) = rhs.to_ir(ctx);

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
    pub fn to_ir(&self, ctx: &mut IrGenContext) -> (String, String) {
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
    pub fn to_ir(&self, ctx: &mut IrGenContext) -> (String, String) {
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
            PrimaryExp::Number(n) => (String::new(), format!("{}", n)),
            PrimaryExp::Paren(inner) => inner.to_ir(ctx),
        }
    }
}
