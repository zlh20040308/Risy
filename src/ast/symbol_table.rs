use std::collections::HashMap;

#[derive(Debug)]
pub struct SymbolTable {
    scopes: Vec<HashMap<String, i32>>, // 每一层作用域是一个 HashMap
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![HashMap::new()], // 全局作用域
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert(&mut self, name: String, value: i32) -> Result<(), String> {
        let current_scope = self.scopes.last_mut().unwrap();
        if current_scope.contains_key(&name) {
            return Err(format!("Symbol '{}' redefined", name));
        }
        current_scope.insert(name, value);
        Ok(())
    }

    pub fn lookup(&self, name: &str) -> Option<i32> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Some(*v);
            }
        }
        None
    }
}
