use crate::ast::Identifier;
use crate::object::Object;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    inner: HashMap<Identifier, Object>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            inner: HashMap::new(),
            outer: None,
        }
    }

    pub fn get(&self, ident: &Identifier) -> Option<&Object> {
        if let result @ Some(_) = self.inner.get(&ident) {
            result
        } else if let Some(ref outer) = self.outer {
            outer.get(&ident)
        } else {
            None
        }
    }

    pub fn set(&mut self, ident: Identifier, obj: Object) {
        self.inner.insert(ident, obj);
    }

    pub fn extend(&self) -> Environment {
        let mut new_env = Environment::new();
        new_env.outer = Some(Box::new(self.clone()));
        new_env
    }
}
