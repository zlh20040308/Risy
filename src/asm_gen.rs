// src/asm_gen.rs
use koopa::ir::{BasicBlock, BinaryOp, FunctionData, Program, Value, ValueKind};
use std::{any::Any, collections::HashMap};

struct RegAllocator {
    pool: Vec<String>, // 可用寄存器池
}

impl RegAllocator {
    fn new() -> Self {
        let mut pool = vec![];

        for i in 0..=6 {
            pool.push(format!("t{}", i));
        }

        Self { pool }
    }

    fn alloc(&mut self) -> String {
        self.pool.pop().unwrap()
    }
}

#[derive(Debug, Clone)]
pub struct StackAllocator {
    map: HashMap<Value, i32>, // SSA 值 -> 栈偏移（相对于 sp）
    offset: i32,              // 当前栈帧已分配的大小（负数）
}

impl StackAllocator {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            offset: 0,
        }
    }

    /// 为一个 SSA 值分配栈空间（默认 4 字节对齐）
    pub fn alloc(&mut self, value: Value) {
        if self.map.contains_key(&value) {
            return;
        }
        self.map.insert(value, self.offset);
        self.offset += 4; // 每次分配 4 字节
    }

    /// 获取某个 SSA 值对应的偏移量
    pub fn get_offset(&self, value: &Value) -> Option<i32> {
        self.map.get(value).copied()
    }

    /// 返回整个栈帧大小
    pub fn frame_size(&self) -> i32 {
        self.offset
    }

    fn scan_allocs(&mut self, func: &FunctionData) {
        for (&_bb, bb_node) in func.layout().bbs() {
            for &inst in bb_node.insts().keys() {
                let value_data = func.dfg().value(inst);
                if value_data.ty().is_unit() {
                    continue;
                }
                self.alloc(inst);
            }
        }
        // sp 寄存器用来保存栈指针, 它的值必须是 16 字节对齐的
        self.offset = (self.offset + 15) & !15;
    }

    fn resolve_value(
        &self,
        val: Value,
        func: &FunctionData,
        asm: &mut String,
        reg_alloc: &mut RegAllocator,
    ) -> String {
        match func.dfg().value(val).kind() {
            ValueKind::Integer(imm) => {
                if imm.value() == 0 {
                    "x0".to_string()
                } else {
                    let reg = reg_alloc.alloc();
                    asm.push_str(&format!("  li {}, {}\n", reg, imm.value()));
                    reg
                }
            }
            _ => {
                let dst = reg_alloc.alloc();
                asm.push_str(&format!(
                    "  lw {}, {}(sp)\n",
                    dst,
                    self.get_offset(&val).unwrap()
                ));
                dst
            }
        }
    }
}

pub struct LabelTable {
    labels: HashMap<BasicBlock, String>,
    counter: usize,
}

impl LabelTable {
    pub fn new() -> Self {
        Self {
            labels: HashMap::new(),
            counter: 0,
        }
    }

    pub fn get_or_assign(&mut self, block: BasicBlock) -> String {
        if let Some(label) = self.labels.get(&block) {
            return label.clone();
        }

        let label = format!("L_{}", self.counter);
        self.counter += 1;
        self.labels.insert(block, label.clone());
        label
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

        let mut stack_alloc = StackAllocator::new();
        let mut label_table = LabelTable::new();
        stack_alloc.scan_allocs(self);
        println!("{:?}", stack_alloc);
        asm.push_str(&format!("  addi sp, sp, -{}\n", stack_alloc.frame_size()));

        for (&bb, bb_node) in self.layout().bbs() {
            asm.push_str(&format!("{}:\n", label_table.get_or_assign(bb)));
            for &inst in bb_node.insts().keys() {
                let mut reg_alloc = RegAllocator::new();
                let value = self.dfg().value(inst);
                println!("{:?}", value);
                match value.kind() {
                    ValueKind::Binary(bin) => {
                        let lhs =
                            stack_alloc.resolve_value(bin.lhs(), self, &mut asm, &mut reg_alloc);
                        let rhs =
                            stack_alloc.resolve_value(bin.rhs(), self, &mut asm, &mut reg_alloc);

                        let rd = reg_alloc.alloc();

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
                        asm.push_str(&format!(
                            "  sw {}, {}(sp)\n",
                            rd,
                            stack_alloc.get_offset(&inst).unwrap()
                        ));
                    }

                    ValueKind::Return(ret) => {
                        if let Some(val) = ret.value() {
                            match self.dfg().value(val).kind() {
                                ValueKind::Integer(int_val) => {
                                    asm.push_str(&format!("  li a0, {}\n", int_val.value()));
                                }
                                _ => {
                                    let dst = stack_alloc.resolve_value(
                                        val,
                                        self,
                                        &mut asm,
                                        &mut reg_alloc,
                                    );
                                    asm.push_str(&format!("  mv a0, {}\n", dst));
                                }
                            }
                        }
                        asm.push_str(&format!("  addi sp, sp, {}\n", stack_alloc.frame_size()));
                        asm.push_str("  ret\n");
                    }
                    ValueKind::Store(store) => {
                        let store_dest = store.dest();
                        let store_value = store.value();
                        let dest_offset = stack_alloc.get_offset(&store_dest).unwrap();

                        let value_data = self.dfg().value(store_value);
                        let temp = reg_alloc.alloc();

                        match value_data.kind() {
                            ValueKind::Integer(int_val) => {
                                // 立即数，直接加载立即数
                                asm.push_str(&format!("  li {}, {}\n", temp, int_val.value()));
                            }
                            _ => {
                                // 正常变量，从栈中加载
                                let value_offset = stack_alloc.get_offset(&store_value).expect(
                                    &format!("store_value not allocated: {:?}", store_value),
                                );
                                asm.push_str(&format!("  lw {}, {}(sp)\n", temp, value_offset));
                            }
                        }
                        asm.push_str(&format!("  sw {}, {}(sp)\n", temp, dest_offset));
                    }
                    ValueKind::Load(load) => {
                        let src = load.src();
                        let src_offset = stack_alloc.get_offset(&src).unwrap();
                        let dest_offset = stack_alloc.get_offset(&inst).unwrap();
                        let temp = reg_alloc.alloc();
                        asm.push_str(&format!("  lw {}, {}(sp)\n", temp, src_offset));
                        asm.push_str(&format!("  sw {}, {}(sp)\n", temp, dest_offset));
                    }
                    ValueKind::Branch(branch) => {
                        // # if 的条件判断部分
                        // lw t0, 4(sp)
                        // bnez t0, then
                        // j else
                        let temp = reg_alloc.alloc();
                        let true_bb_label = label_table.get_or_assign(branch.true_bb());
                        let false_bb_label = label_table.get_or_assign(branch.false_bb());

                        let cond = branch.cond();
                        let cond_offset = stack_alloc.get_offset(&cond).unwrap();
                        asm.push_str(&format!("  lw {}, {}(sp)\n", temp, cond_offset));
                        asm.push_str(&format!("  bnez {}, {}\n", temp, true_bb_label));
                        asm.push_str(&format!("  j {}\n", false_bb_label));
                    }
                    ValueKind::Jump(jump) => {
                        let target_bb_label = label_table.get_or_assign(jump.target());
                        asm.push_str(&format!("  j {}\n", target_bb_label));
                    }
                    ValueKind::Alloc(_) => {}
                    _ => {
                        panic!("Unhandled instruction: {:?}\n", value.kind());
                    }
                }
            }
        }

        asm
    }
}
