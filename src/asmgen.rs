// codegen.rs
use koopa::ir::{BinaryOp, FunctionData, Program, Value, ValueKind};

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

        // 添加 t0 - t6
        for i in 0..=6 {
            pool.push(format!("t{}", i));
        }

        // 添加 a0 - a7
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

        let reg = if self.count < self.pool.len() {
            self.pool[self.count].clone()
        } else {
            // 如果寄存器池用光了，就 fallback 到栈或生成额外寄存器名（可拓展）
            format!("x_tmp{}", self.count - self.pool.len())
        };

        self.map.insert(value, reg.clone());
        self.count += 1;
        reg
    }

    fn get(&self, value: &Value) -> Option<&String> {
        self.map.get(value)
    }
}


/// Trait：定义“可以生成汇编”的类型
pub trait GenerateAsm {
    fn generate(&self) -> String;
}

/// 为 Program 实现汇编生成逻辑
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

/// 为 FunctionData 实现汇编生成逻辑
impl GenerateAsm for FunctionData {
    fn generate(&self) -> String {
        let mut asm = String::new();
        let func_name = &self.name()[1..]; // 去掉 '@'
        asm.push_str(&format!("  .globl {}\n", func_name));
        asm.push_str(&format!("{}:\n", func_name));

        let mut reg_alloc = RegAllocator::new();

        for (&_bb, bb_node) in self.layout().bbs() {
            for &inst in bb_node.insts().keys() {
                let value = self.dfg().value(inst);
                match value.kind() {
                    ValueKind::Integer(i) => {
                        let rd = reg_alloc.alloc(inst);
                        asm.push_str(&format!("  li {}, {}\n", rd, i.value()));
                    }

                    ValueKind::Binary(bin) => {
                        let lhs_id = bin.lhs();
                        let rhs_id = bin.rhs();

                        let lhs = reg_alloc.get(&lhs_id).map(Clone::clone);
                        let rhs = reg_alloc.get(&rhs_id).map(Clone::clone);

                        let rd = reg_alloc.alloc(inst);
                        let lhs = lhs.unwrap_or_else(|| reg_alloc.alloc(lhs_id));
                        let rhs = rhs.unwrap_or_else(|| reg_alloc.alloc(rhs_id));

                        match bin.op() {
                            BinaryOp::Add => {
                                asm.push_str(&format!("  add {}, {}, {}\n", rd, lhs, rhs))
                            }
                            BinaryOp::Sub => {
                                asm.push_str(&format!("  sub {}, {}, {}\n", rd, lhs, rhs))
                            }
                            BinaryOp::Mul => {
                                asm.push_str(&format!("  mul {}, {}, {}\n", rd, lhs, rhs))
                            }
                            BinaryOp::Div => {
                                asm.push_str(&format!("  div {}, {}, {}\n", rd, lhs, rhs))
                            }
                            BinaryOp::Mod => {
                                asm.push_str(&format!("  rem {}, {}, {}\n", rd, lhs, rhs))
                            }
                            BinaryOp::Eq => {
                                let tmp = format!("t{}", reg_alloc.count);
                                reg_alloc.count += 1;
                                asm.push_str(&format!("  xor {}, {}, {}\n", tmp, lhs, rhs));
                                asm.push_str(&format!("  seqz {}, {}\n", rd, tmp));
                            }
                            _ => {
                                asm.push_str(&format!("  # Unhandled binary op: {:?}\n", bin.op()))
                            }
                        }
                    }

                    ValueKind::Return(ret) => {
                        if let Some(val) = ret.value() {
                            if let Some(src) = reg_alloc.get(&val) {
                                asm.push_str(&format!("  mv a0, {}\n", src));
                            } else {
                                asm.push_str("  # Return value not found\n");
                            }
                        }
                        asm.push_str("  ret\n");
                    }

                    _ => {
                        asm.push_str(&format!("  # Unhandled instruction: {:?}\n", value.kind()));
                    }
                }
            }
        }

        asm
    }
}
