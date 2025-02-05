use std::collections::HashMap; // Add this import for HashMap
use crate::frontend::ast::{Expression, Program}; // Ensure this import is present

pub struct Environment {
    global_env: Option<Box<Environment>>, // Pointer to another Environment
    parent_env: Option<Box<Environment>>, // Pointer to another Environment
    values: HashMap<String, Expression>, // Map from string to Expression
}

impl Environment {
    pub fn new(parent_env: Option<Box<Environment>>) -> Self {
        let global_env = match parent_env {
            Some(parent) => match parent.global_env {
                Some(global) => Some(global),
                None => Some(parent),
            },
            None => None,
        };
        Environment {
            global_env,
            parent_env,
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Expression) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&Expression> {
        self.values.get(name).or_else(|| {
            self.parent_env.as_ref().and_then(|parent| parent.get(name))
        })
    }

    pub fn assign(&mut self, name: String, value: Expression) {
        if self.values.contains_key(&name) {
            self.values.insert(name, value);
        } else {
            self.parent_env.as_mut().unwrap().assign(name, value);
        }
    }
}