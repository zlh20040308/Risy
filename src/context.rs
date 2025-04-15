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
    label_counter: HashMap<String, usize>,
    named_counter: HashMap<String, usize>,
    symbol_table: Vec<HashMap<String, SymbolValue>>,
    loop_stack: Vec<LoopLabels>,
}

impl IrContext {
    pub fn new() -> Self {
        Self {
            temp_counter: 0,
            label_counter: HashMap::new(),
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

    pub fn generate_label(&mut self, prefix: &str) -> String {
        let counter = self.label_counter.entry(prefix.to_string()).or_insert(0);
        let label = format!("%{}_{}", prefix, *counter);
        *counter += 1;
        label
    }

    /// 生成用户命名变量，如 @x_1、@y_2
    pub fn generate_named(&mut self, name: &str) -> String {
        let id = self.named_counter.entry(name.to_string()).or_insert(1);
        let ir_name = format!("@{}_{}", name, *id);
        *id += 1;
        ir_name
    }

    pub fn enter_scope(&mut self) {
        self.symbol_table.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.symbol_table.pop();
    }

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

    pub fn lookup(&self, name: &str) -> Option<SymbolValue> {
        for scope in self.symbol_table.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Some(v.clone());
            }
        }
        None
    }

    pub fn enter_loop(&mut self, entry: String, exit: String) {
        self.loop_stack.push(LoopLabels { entry, exit });
    }

    pub fn exit_loop(&mut self) {
        self.loop_stack.pop();
    }

    pub fn current_loop_labels(&self) -> Option<LoopLabels> {
        self.loop_stack.last().cloned()
    }
}
