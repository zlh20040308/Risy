// src/asm_gen.rs
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Program, Value, ValueKind};
use std::cmp::max;
use std::collections::HashMap;

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
    map: HashMap<Value, usize>, // SSA 值 -> 栈偏移（相对于 sp）
    offset: usize,              // 当前栈帧已分配的大小（负数）
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
    pub fn get_offset(&self, value: &Value) -> Option<usize> {
        self.map.get(value).copied()
    }

    /// 返回整个栈帧大小
    pub fn get_frame_size(&self) -> usize {
        self.offset
    }

    fn scan_allocs(&mut self, func: &FunctionData) {
        // 为函数参数超过 8 个的情况余留空间
        let mut max_params_num = 0;
        for (&_, bb_node) in func.layout().bbs() {
            for &inst in bb_node.insts().keys() {
                let value_data = func.dfg().value(inst);
                if let ValueKind::Call(call) = value_data.kind() {
                    max_params_num = max(call.args().len(), max_params_num)
                }
            }
        }
        self.offset += if max_params_num > 8 {
            (max_params_num - 8) * 4
        } else {
            0
        };
        // 局部变量
        for (&_, bb_node) in func.layout().bbs() {
            for &inst in bb_node.insts().keys() {
                let value_data = func.dfg().value(inst);
                // 计算局部变量所需要的栈帧大小并记录每个变量的偏移量
                if !value_data.ty().is_unit() {
                    self.alloc(inst);
                }
            }
        }
        // 用 8 字节用于存放 ra 与 sp
        self.offset += 8;
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
                    asm.push_str(&format!("  li {},{}\n", reg, imm.value()));
                    reg
                }
            }
            _ => {
                let dst = reg_alloc.alloc();
                asm.push_str(&format!(
                    "  lw {},{}(sp)\n",
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

pub struct AsmContext {
    global_value_name: HashMap<Value, String>,
    func_name: HashMap<Function, String>,
    label_table: LabelTable,
}

impl AsmContext {
    pub fn new() -> Self {
        Self {
            global_value_name: HashMap::new(),
            func_name: HashMap::new(),
            label_table: LabelTable::new(),
        }
    }

    pub fn get_global_value_name(&self, global_value: &Value) -> &String {
        &self.global_value_name[global_value]
    }

    pub fn set_global_value_name(&mut self, global_value: Value, name: &str) {
        self.global_value_name
            .insert(global_value, name.to_string());
    }

    pub fn get_func_name(&self, func: &Function) -> &String {
        &self.func_name[func]
    }

    pub fn set_func_name(&mut self, func: Function, name: &str) {
        self.func_name.insert(func, name.to_string());
    }
}

pub trait GenerateAsm {
    fn generate(&self, ctx: &mut AsmContext) -> String;
}

impl GenerateAsm for Program {
    fn generate(&self, ctx: &mut AsmContext) -> String {
        let mut asm = String::new();
        // 声明全局变量
        for global_value in self.inst_layout() {
            let global_value_data = self.borrow_value(*global_value);
            let global_value_name = global_value_data
                .name()
                .as_ref()
                .and_then(|name| name.strip_prefix('@'))
                .map(|s| s.to_string())
                .unwrap_or_default();
            ctx.set_global_value_name(*global_value, &global_value_name);
            asm.push_str(&format!("  .data\n"));
            asm.push_str(&format!("  .globl {}\n", global_value_name));
            asm.push_str(&format!("{}:\n", global_value_name));
            if let ValueKind::GlobalAlloc(global_alloc) = global_value_data.kind() {
                let init = global_alloc.init();
                match self.borrow_value(init).kind() {
                    ValueKind::Integer(init_val) => {
                        asm.push_str(&format!("  .word {}\n", init_val.value()));
                    }
                    ValueKind::ZeroInit(_) => {
                        asm.push_str(&format!("  .zero 4\n"));
                    }
                    _ => {}
                }
            }
        }

        for &func in self.func_layout() {
            ctx.set_func_name(func, &self.func(func).name()[1..]);
        }

        for &func in self.func_layout() {
            if self.func(func).layout().entry_bb() == None {
                continue;
            }
            asm.push_str(&self.func(func).generate(ctx));
        }
        asm
    }
}

impl GenerateAsm for FunctionData {
    fn generate(&self, ctx: &mut AsmContext) -> String {
        let mut asm = String::new();
        let func_name = &self.name()[1..];
        let mut stack_alloc = StackAllocator::new();

        asm.push_str("  .text\n");
        asm.push_str(&format!("  .globl {}\n", func_name));
        asm.push_str(&format!("{}:\n", func_name));

        stack_alloc.scan_allocs(self);
        asm.push_str(&format!("  addi sp,sp,-{}\n", stack_alloc.get_frame_size()));

        // 将 ra, sp 保存到栈上对应的位置上
        asm.push_str(&format!(
            "  sw	ra,{}(sp)\n",
            stack_alloc.get_frame_size() - 4
        ));
        asm.push_str(&format!(
            "  sw	s0,{}(sp)\n",
            stack_alloc.get_frame_size() - 8
        ));
        // 设置新的帧指针
        asm.push_str(&format!("  addi	s0,sp,{}\n", stack_alloc.get_frame_size()));

        for (&bb, bb_node) in self.layout().bbs() {
            asm.push_str(&format!("{}:\n", ctx.label_table.get_or_assign(bb)));
            for &inst in bb_node.insts().keys() {
                let mut reg_alloc = RegAllocator::new();
                let value_data = self.dfg().value(inst);
                match value_data.kind() {
                    ValueKind::Binary(bin) => {
                        let lhs =
                            stack_alloc.resolve_value(bin.lhs(), self, &mut asm, &mut reg_alloc);
                        let rhs =
                            stack_alloc.resolve_value(bin.rhs(), self, &mut asm, &mut reg_alloc);

                        let rd = reg_alloc.alloc();

                        match bin.op() {
                            BinaryOp::Add => {
                                asm.push_str(&format!("  add {},{},{}\n", rd, lhs, rhs));
                            }
                            BinaryOp::Sub => {
                                asm.push_str(&format!("  sub {},{},{}\n", rd, lhs, rhs));
                            }
                            BinaryOp::Mul => {
                                asm.push_str(&format!("  mul {},{},{}\n", rd, lhs, rhs));
                            }
                            BinaryOp::Div => {
                                asm.push_str(&format!("  div {},{},{}\n", rd, lhs, rhs));
                            }
                            BinaryOp::Mod => {
                                asm.push_str(&format!("  rem {},{},{}\n", rd, lhs, rhs));
                            }
                            BinaryOp::And => {
                                asm.push_str(&format!("  and {},{},{}\n", rd, lhs, rhs));
                            }
                            BinaryOp::Eq => {
                                asm.push_str(&format!("  xor {},{},{}\n", rd, lhs, rhs));
                                asm.push_str(&format!("  seqz {},{}\n", rd, rd));
                            }
                            BinaryOp::NotEq => {
                                asm.push_str(&format!("  xor {},{},{}\n", rd, lhs, rhs));
                                asm.push_str(&format!("  snez {},{}\n", rd, rd));
                            }
                            BinaryOp::Or => {
                                asm.push_str(&format!("  or {},{},{}\n", rd, lhs, rhs));
                            }
                            BinaryOp::Ge => {
                                asm.push_str(&format!("  slt {},{},{}\n", rd, lhs, rhs));
                                asm.push_str(&format!("  seqz {},{}\n", rd, rd));
                            }
                            BinaryOp::Lt => {
                                asm.push_str(&format!("  slt {},{},{}\n", rd, lhs, rhs));
                            }
                            BinaryOp::Gt => {
                                asm.push_str(&format!("  slt {},{},{}\n", rd, rhs, lhs));
                            }
                            BinaryOp::Le => {
                                asm.push_str(&format!("  slt {},{},{}\n", rd, rhs, lhs));
                                asm.push_str(&format!("  seqz {},{}\n", rd, rd));
                            }
                            _ => {
                                panic!("Unhandled binary op: {:?}", bin.op());
                            }
                        }
                        asm.push_str(&format!(
                            "  sw {},{}(sp)\n",
                            rd,
                            stack_alloc.get_offset(&inst).unwrap()
                        ));
                    }

                    ValueKind::Return(ret) => {
                        if let Some(val) = ret.value() {
                            match self.dfg().value(val).kind() {
                                ValueKind::Integer(int_val) => {
                                    asm.push_str(&format!("  li a0,{}\n", int_val.value()));
                                }
                                _ => {
                                    let dst = stack_alloc.resolve_value(
                                        val,
                                        self,
                                        &mut asm,
                                        &mut reg_alloc,
                                    );
                                    asm.push_str(&format!("  mv a0,{}\n", dst));
                                }
                            }
                        }
                        asm.push_str(&format!(
                            "  lw	ra,{}(sp)\n",
                            stack_alloc.get_frame_size() - 4
                        ));
                        asm.push_str(&format!(
                            "  lw	s0,{}(sp)\n",
                            stack_alloc.get_frame_size() - 8
                        ));
                        asm.push_str(&format!("  addi sp,sp,{}\n", stack_alloc.get_frame_size()));
                        asm.push_str("  ret\n");
                    }
                    ValueKind::Store(store) => {
                        let store_dest = store.dest();
                        let store_value = store.value();
                        let value_data = self.dfg().value(store_value);
                        let temp = reg_alloc.alloc();
                        //  把要存的数字放在暂存寄存器中
                        match value_data.kind() {
                            ValueKind::Integer(int_val) => {
                                // 立即数，直接加载立即数
                                asm.push_str(&format!("  li {},{}\n", temp, int_val.value()));
                            }
                            ValueKind::FuncArgRef(arg) => {
                                if arg.index() < 8 {
                                    asm.push_str(&format!("  mv	{},a{}\n", temp, arg.index()));
                                } else {
                                    asm.push_str(&format!(
                                        "  lw	{},{}(s0)\n",
                                        temp,
                                        (arg.index() - 8) * 4
                                    ));
                                }
                            }
                            _ => {
                                // 正常变量，从栈中加载
                                let value_offset = stack_alloc.get_offset(&store_value).expect(
                                    &format!("store_value not allocated: {:?}", value_data),
                                );
                                asm.push_str(&format!("  lw {},{}(sp)\n", temp, value_offset));
                            }
                        }

                        match stack_alloc.get_offset(&store_dest) {
                            Some(dest_offset) => {
                                // dest 是局部变量
                                asm.push_str(&format!("  sw {},{}(sp)\n", temp, dest_offset));
                            }
                            None => {
                                // dest 是全局变量
                                let dest_name = ctx.get_global_value_name(&store_dest);
                                let dest_base = reg_alloc.alloc();

                                asm.push_str(&format!("  la {},{}\n", dest_base, dest_name));
                                asm.push_str(&format!("  sw {},0({})\n", temp, dest_base));
                            }
                        }
                    }
                    ValueKind::Load(load) => {
                        let src = load.src();
                        let dest_offset = stack_alloc.get_offset(&inst).unwrap();
                        let temp = reg_alloc.alloc();
                        match stack_alloc.get_offset(&src) {
                            Some(src_offset) => {
                                asm.push_str(&format!("  lw {},{}(sp)\n", temp, src_offset));
                                asm.push_str(&format!("  sw {},{}(sp)\n", temp, dest_offset));
                            }
                            None => {
                                let src_name = ctx.get_global_value_name(&src);
                                asm.push_str(&format!("  la {},{}\n", temp, src_name));
                                asm.push_str(&format!("  lw {},0({})\n", temp, temp));
                                asm.push_str(&format!("  sw {},{}(sp)\n", temp, dest_offset));
                            }
                        }
                    }
                    ValueKind::Branch(branch) => {
                        let temp = reg_alloc.alloc();
                        let true_bb_label = ctx.label_table.get_or_assign(branch.true_bb());
                        let false_bb_label = ctx.label_table.get_or_assign(branch.false_bb());

                        let cond = branch.cond();
                        let cond_offset = stack_alloc
                            .get_offset(&cond)
                            .expect(&format!("store_value not allocated: {:?}", cond));
                        asm.push_str(&format!("  lw {},{}(sp)\n", temp, cond_offset));
                        asm.push_str(&format!("  bnez {},{}\n", temp, true_bb_label));
                        asm.push_str(&format!("  j {}\n", false_bb_label));
                    }
                    ValueKind::Jump(jump) => {
                        let target_bb_label = ctx.label_table.get_or_assign(jump.target());
                        asm.push_str(&format!("  j {}\n", target_bb_label));
                    }
                    ValueKind::Call(call) => {
                        // 传参
                        let temp = reg_alloc.alloc();
                        for (index, arg) in call.args().into_iter().enumerate() {
                            let arg_offest = stack_alloc
                                .get_offset(arg)
                                .expect(&format!("store_value not allocated: {:?}", arg));
                            asm.push_str(&format!("  lw {},{}(sp)\n", temp, arg_offest));
                            if index < 8 {
                                asm.push_str(&format!("  mv a{},{}\n", index, temp));
                            } else {
                                // 如果参数大于 8 个， 则存放在栈中
                                asm.push_str(&format!("  sw {},{}(sp)\n", temp, (index - 8) * 4));
                            }
                        }
                        // 调用
                        let func_name = ctx.get_func_name(&call.callee());
                        asm.push_str(&format!("  call	{}\n", func_name));

                        // 处理结果
                        if !value_data.ty().is_unit() {
                            asm.push_str(&format!(
                                "  sw	a0,{}(sp)\n",
                                stack_alloc.get_offset(&inst).unwrap()
                            ));
                        }
                    }
                    ValueKind::Alloc(_) => {}
                    _ => {
                        panic!("Unhandled instruction: {:?}\n", value_data.kind());
                    }
                }
            }
        }
        asm.push_str(&format!("\n"));
        asm
    }
}
