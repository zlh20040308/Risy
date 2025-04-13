// src/context.rs
use serde::Serialize;
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize)]
pub enum SymbolValue {
    Const(i32),
    Var(String),
}

#[derive(Debug, Clone, Serialize)]
pub struct IrContext {
    temp_counter: usize,
    named_counter: HashMap<String, usize>,
    pub symbol_table: Vec<HashMap<String, SymbolValue>>,
}

impl IrContext {
    pub fn new() -> Self {
        Self {
            temp_counter: 0,
            named_counter: HashMap::new(),
            symbol_table: vec![HashMap::new()],
        }
    }

    pub fn next_temp(&mut self) -> String {
        let name = format!("%{}", self.temp_counter);
        self.temp_counter += 1;
        name
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
}
