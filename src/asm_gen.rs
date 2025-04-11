use koopa::ir::{BinaryOp, FunctionData, Program, Value, ValueKind};
use log::info;
use std::collections::HashMap;

/// 寄存器分配器：记录每个 SSA 值对应的虚拟寄存器名（如 t0、t1）
struct RegAllocator {
    map: HashMap<Value, String>,
    pool: Vec<String>, // 可用寄存器池
    count: usize,      // 用于记录是否超出了寄存器池
}

impl RegAllocator {
    fn new() -> Self {
        let mut pool = vec![];

        for i in 0..=6 {
            pool.push(format!("t{}", i));
        }

        for i in 0..=7 {
            pool.push(format!("a{}", i));
        }

        Self {
            map: HashMap::new(),
            pool,
            count: 0,
        }
    }

    fn alloc(&mut self, value: Value) -> String {
        if let Some(name) = self.map.get(&value) {
            return name.clone();
        }

        let reg = self.alloc_new();
        self.map.insert(value, reg.clone());
        reg
    }

    fn alloc_tmp(&mut self) -> String {
        self.alloc_new()
    }

    fn get(&self, value: &Value) -> Option<&String> {
        self.map.get(value)
    }

    fn alloc_new(&mut self) -> String {
        let reg = if self.count < self.pool.len() {
            self.pool[self.count].clone()
        } else {
            panic!("Registers are gone");
        };
        self.count += 1;
        reg
    }

    /// 自动判断是立即数或 SSA 值并返回对应寄存器名
    fn resolve_value(&mut self, val: Value, func: &FunctionData, asm: &mut String) -> String {
        match func.dfg().value(val).kind() {
            ValueKind::Integer(imm) => {
                if imm.value() == 0 {
                    "x0".to_string()
                } else {
                    let tmp = self.alloc_tmp();
                    asm.push_str(&format!("  li {}, {}\n", tmp, imm.value()));
                    tmp
                }
            }
            _ => self.alloc(val),
        }
    }
}

pub trait GenerateAsm {
    fn generate(&self) -> String;
}

impl GenerateAsm for Program {
    fn generate(&self) -> String {
        let mut asm = String::new();
        asm.push_str("  .text\n");
        for &func in self.func_layout() {
            let func_data = self.func(func);
            asm.push_str(&func_data.generate());
        }
        asm
    }
}

impl GenerateAsm for FunctionData {
    fn generate(&self) -> String {
        let mut asm = String::new();
        let func_name = &self.name()[1..];
        asm.push_str(&format!("  .globl {}\n", func_name));
        asm.push_str(&format!("{}:\n", func_name));

        let mut reg_alloc = RegAllocator::new();

        for (&_bb, bb_node) in self.layout().bbs() {
            for &inst in bb_node.insts().keys() {
                let value = self.dfg().value(inst);
                info!("{:?}", value);
                match value.kind() {
                    ValueKind::Binary(bin) => {
                        let lhs = reg_alloc.resolve_value(bin.lhs(), self, &mut asm);
                        let rhs = reg_alloc.resolve_value(bin.rhs(), self, &mut asm);
                        let rd = reg_alloc.alloc(inst);

                        match bin.op() {
                            BinaryOp::Add => {
                                asm.push_str(&format!("  add {}, {}, {}\n", rd, lhs, rhs));
                            }
                            BinaryOp::Sub => {
                                asm.push_str(&format!("  sub {}, {}, {}\n", rd, lhs, rhs));
                            }
                            BinaryOp::Mul => {
                                asm.push_str(&format!("  mul {}, {}, {}\n", rd, lhs, rhs));
                            }
                            BinaryOp::Div => {
                                asm.push_str(&format!("  div {}, {}, {}\n", rd, lhs, rhs));
                            }
                            BinaryOp::Mod => {
                                asm.push_str(&format!("  rem {}, {}, {}\n", rd, lhs, rhs));
                            }
                            BinaryOp::And => {
                                asm.push_str(&format!("  and {}, {}, {}\n", rd, lhs, rhs));
                            }
                            BinaryOp::Eq => {
                                asm.push_str(&format!("  xor {}, {}, {}\n", rd, lhs, rhs));
                                asm.push_str(&format!("  seqz {}, {}\n", rd, rd));
                            }
                            BinaryOp::NotEq => {
                                asm.push_str(&format!("  xor {}, {}, {}\n", rd, lhs, rhs));
                                asm.push_str(&format!("  snez {}, {}\n", rd, rd));
                            }
                            BinaryOp::Or => {
                                asm.push_str(&format!("  or {}, {}, {}\n", rd, lhs, rhs));
                            }
                            BinaryOp::Ge => {
                                asm.push_str(&format!("  slt {}, {}, {}\n", rd, lhs, rhs));
                                asm.push_str(&format!("  seqz {}, {}\n", rd, rd));
                            }
                            BinaryOp::Lt => {
                                asm.push_str(&format!("  slt {}, {}, {}\n", rd, lhs, rhs));
                            }
                            BinaryOp::Gt => {
                                asm.push_str(&format!("  slt {}, {}, {}\n", rd, rhs, lhs));
                            }
                            BinaryOp::Le => {
                                asm.push_str(&format!("  slt {}, {}, {}\n", rd, rhs, lhs));
                                asm.push_str(&format!("  seqz {}, {}\n", rd, rd));
                            }
                            _ => {
                                panic!("Unhandled binary op: {:?}", bin.op());
                            }
                        }
                    }

                    ValueKind::Return(ret) => {
                        if let Some(val) = ret.value() {
                            match self.dfg().value(val).kind() {
                                ValueKind::Integer(int_val) => {
                                    asm.push_str(&format!("  li a0, {}\n", int_val.value()));
                                }
                                _ => {
                                    if let Some(src) = reg_alloc.get(&val) {
                                        asm.push_str(&format!("  mv a0, {}\n", src));
                                    } else {
                                        panic!("Return value not found");
                                    }
                                }
                            }
                        }
                        asm.push_str("  ret\n");
                    }

                    _ => {
                        panic!("Unhandled instruction: {:?}\n", value.kind());
                    }
                }
            }
        }

        asm
    }
}
