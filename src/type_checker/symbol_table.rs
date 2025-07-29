// use struct

// pub fn ()

use crate::types::types::Type;

use std::collections::HashMap;

pub struct SymbolTable {
    scopes: Vec<HashMap<String, Option<Type>>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }
    pub fn declare_and_define(&mut self, name: &str, ty: Type) -> Result<(), String> {
        let current = self.scopes.last_mut().unwrap();
        if current.contains_key(name) {
            return Err(format!(
                "Variable '{}' already declared in this scope",
                name
            ));
        }
        current.insert(name.to_string(), Some(ty));
        Ok(())
    }

    pub fn define(&mut self, name: &str, ty: Type) -> Result<(), String> {
        let current = self.scopes.last_mut().unwrap();
        if current.contains_key(name) {
            let x = current.get(name);
            if let Some(x) = x {
                if &None == x {
                    current.insert(name.to_string(), Some(ty));
                }
                return Err(format!(
                    "Variable '{}' already defined in this scope",
                    name
                ));
            }
        }
        current.insert(name.to_string(), None);
        Ok(())
    }

    pub fn declare(&mut self, name: &str) -> Result<(), String> {
        let current = self.scopes.last_mut().unwrap();
        if current.contains_key(name) {
            return Err(format!(
                "Variable '{}' already declared in this scope",
                name
            ));
        }
        current.insert(name.to_string(), None);
        Ok(())
    }

    pub fn lookup(&self, name: &str) -> &Option<Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(my_type) = scope.get(name) {
                return my_type;
            }
        }
        return &None;
    }
}
