use crate::{compiler_debug};
use std::collections::HashMap; // Add this import for HashMap
use crate::backend::interpreter::Value;
use std::fmt; // Add this import for formatting
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Environment {
    global_env: Option<Rc<RefCell<Environment>>>, // Mutable global environment
    parent_env: Option<Rc<RefCell<Environment>>>, // Mutable parent environment
    values: HashMap<String, Value>, // Map from string to Value
    depth: usize,
}

impl Environment {
    pub fn new_global() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Environment {
            global_env: None,
            parent_env: None,
            values: HashMap::new(),
            depth: 0,
        }))
    }

    pub fn new(parent_env: Rc<RefCell<Environment>>) -> Rc<RefCell<Self>> {
        let global_env = {
            let parent = parent_env.borrow();
            match &parent.global_env {
                Some(global) => Some(Rc::clone(global)),
                None => Some(Rc::clone(&parent_env)),
            }
        };

        Rc::new(RefCell::new(Environment {
            global_env,
            parent_env: Some(Rc::clone(&parent_env)),
            values: HashMap::new(),
            depth: parent_env.borrow().depth + 1,
        }))
    }

    /*
    pub fn env_at_distance(self, distance: usize) -> Rc<RefCell<Environment>> {
        let mut current = self;
        if distance == 0 {
            return Rc::new(RefCell::new(current));
        }
        for _ in 0..distance {
            current = current.parent_env.as_mut().unwrap();
        }
        Rc::clone(current)
    }

    pub fn env_at_depth(self, depth: usize) -> Rc<RefCell<Environment>> {
        let mut current = self;
        while current.depth > depth {
            current = current.parent_env.as_mut().unwrap();
        }
        Rc::clone(current)
    }
    */

    pub fn get(&self, name: &str) -> Option<Value> {
        self.values.get(name).cloned().or_else(|| {
            self.parent_env.as_ref().and_then(|parent| {
                parent.borrow().get(name)
            })
        })
    }

    pub fn exists(&self, name: &str) -> bool {
        self.values.contains_key(name) || 
        self.parent_env.as_ref().map_or(false, |parent| {
            parent.borrow().exists(name)
        })
    }

    // TODO: distance might be in the wrong direction. 
    // Local scope distance is "levels deep" from the global environment
    // Not levels away from the current environment
    pub fn assign(&mut self, depth: usize, name: String, value: Value) {
        if self.depth == depth {
            compiler_debug!("Env.assign| Assigning {:?} = {:?} at {:?} depth", name, value, depth);
            self.define(name, value);
            compiler_debug!("Env.assign| Environment after assignment: {}", self);
        } else if let Some(ref parent) = self.parent_env {
            parent.borrow_mut().assign(depth, name, value);
        }
    }

    // Define a variable in the current environment
    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }
    /*
    pub fn print_environment(&self, indent: &str) {
        compiler_debug!("{:?}", self.values);
        if let Some(ref parent) = self.parent_env {
            compiler_debug!("\n{}{:?}", indent, parent);
            parent.print_environment(&format!("  {}", indent));
        }
    }
    */
}

impl fmt::Display for Environment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Depth: {} | ", self.depth)?;
        writeln!(f, "Keys: {:?}", self.values.keys().collect::<Vec<_>>())?; // Display only the keys
        if let Some(ref parent) = self.parent_env {
            write!(f, "{}", parent.borrow())?;
        }
        Ok(())
    }
}