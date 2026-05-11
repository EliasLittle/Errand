use std::collections::HashMap;
use tracing::instrument;

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
    Int32,
    Float,
    Bool,
    String,
    Struct(Struct),
    // Add more as needed
}

impl Field {
    #[instrument(
        skip(ty),
        fields(name = name, offset = offset),
        name = "backend.structs.field_new",
        target = "backend",
        level = "trace"
    )]
    pub fn new(name: &str, offset: usize, ty: Type) -> Self {
        Field {
            name: name.to_string(),
            offset,
            ty,
        }
    }
}

impl Struct {
    #[instrument(
        skip(fields),
        fields(name = name, field_count = fields.len(), size = size),
        name = "backend.structs.struct_new",
        target = "backend",
        level = "trace"
    )]
    pub fn new(name: &str, fields: Vec<Field>, size: usize) -> Self {
        let name_to_index = fields
            .iter()
            .enumerate()
            .map(|(i, f)| (f.name.clone(), i))
            .collect();
        Struct {
            name: name.to_string(),
            fields,
            size,
            name_to_index,
        }
    }

    #[instrument(
        skip(self),
        fields(struct_name = self.name.as_str(), field_name = name),
        name = "backend.structs.get_field",
        target = "backend",
        level = "trace"
    )]
    pub fn get_field(&self, name: &str) -> Option<&Field> {
        self.name_to_index.get(name).map(|&i| &self.fields[i])
    }
}
