use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub offset: usize,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<Field>,
    pub size: usize,
    name_to_index: HashMap<String, usize>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Struct(Struct),
    // Add more as needed
}

impl Field {
    pub fn new(name: &str, offset: usize, ty: Type) -> Self {
        Field {
            name: name.to_string(),
            offset,
            ty,
        }
    }
}

impl Struct {
    pub fn new(name: &str, fields: Vec<Field>, size: usize) -> Self {
        let name_to_index = fields.iter().enumerate().map(|(i, f)| (f.name.clone(), i)).collect();
        Struct {
            name: name.to_string(),
            fields,
            size,
            name_to_index,
        }
    }

    pub fn get_field(&self, name: &str) -> Option<&Field> {
        self.name_to_index.get(name).map(|&i| &self.fields[i])
    }
}
