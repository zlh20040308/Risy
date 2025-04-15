// src/context.rs
use serde::Serialize;
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize)]
pub enum SymbolValue {
    Const(i32),
    Var(String),
}

#[derive(Debug, Clone, Serialize)]
pub struct LoopLabels {
    pub entry: String, // continue 的目标
    pub exit: String,  // break 的目标
}

#[derive(Debug, Clone, Serialize)]
pub struct IrContext {
    temp_counter: usize,
    then_counter: usize,
    else_counter: usize,
    while_entry_counter: usize,
    while_body_counter: usize,
    end_counter: usize,
    named_counter: HashMap<String, usize>,
    symbol_table: Vec<HashMap<String, SymbolValue>>,
    loop_stack: Vec<LoopLabels>,
}

impl IrContext {
    pub fn new() -> Self {
        Self {
            temp_counter: 0,
            then_counter: 0,
            else_counter: 0,
            while_entry_counter: 0,
            while_body_counter: 0,
            end_counter: 0,
            named_counter: HashMap::new(),
            symbol_table: vec![HashMap::new()],
            loop_stack: vec![],
        }
    }

    pub fn next_temp(&mut self) -> String {
        let name = format!("%{}", self.temp_counter);
        self.temp_counter += 1;
        name
    }

    pub fn next_then(&mut self) -> String {
        let label = format!("%then_{}", self.then_counter);
        self.then_counter += 1;
        label
    }

    pub fn next_end(&mut self) -> String {
        let label = format!("%end_{}", self.end_counter);
        self.end_counter += 1;
        label
    }

    pub fn next_else(&mut self) -> String {
        let label = format!("%else_{}", self.else_counter);
        self.else_counter += 1;
        label
    }

    pub fn next_while_entry(&mut self) -> String {
        let label = format!("%while_entry_{}", self.while_entry_counter);
        self.while_entry_counter += 1;
        label
    }

    pub fn next_while_body(&mut self) -> String {
        let label = format!("%while_body_{}", self.while_body_counter);
        self.while_body_counter += 1;
        label
    }

    pub fn generate_named(&mut self, name: &str) -> String {
        let id = self.named_counter.entry(name.to_string()).or_insert(1);
        let ir_name = format!("@{}_{}", name, *id);
        *id += 1;
        ir_name
    }

    /// 进入新作用域
    pub fn enter_scope(&mut self) {
        self.symbol_table.push(HashMap::new());
    }

    /// 退出当前作用域
    pub fn exit_scope(&mut self) {
        self.symbol_table.pop();
    }

    /// 插入一个符号，要求当前作用域中没有重定义
    pub fn insert(&mut self, name: &str, value: SymbolValue) -> Result<(), String> {
        let current_scope = self
            .symbol_table
            .last_mut()
            .ok_or("No active scope".to_string())?;

        if current_scope.contains_key(name) {
            return Err(format!("Symbol '{}' redefined", name));
        }

        current_scope.insert(name.to_string(), value);
        Ok(())
    }

    /// 查找符号（从内向外）
    pub fn lookup(&self, name: &str) -> Option<&SymbolValue> {
        for scope in self.symbol_table.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Some(v);
            }
        }
        None
    }

    pub fn load_if_needed(&mut self, val: String) -> (String, String) {
        if val.starts_with('@') {
            let tmp = self.next_temp();
            let code = format!("  {} = load {}\n", tmp, val);
            (code, tmp)
        } else {
            (String::new(), val)
        }
    }

    pub fn enter_loop(&mut self, entry: String, exit: String) {
        self.loop_stack.push(LoopLabels { entry, exit });
    }

    pub fn exit_loop(&mut self) {
        self.loop_stack.pop();
    }

    pub fn current_loop_labels(&self) -> Option<&LoopLabels> {
        self.loop_stack.last()
    }
}
