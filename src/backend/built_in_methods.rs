use crate::{cranelift_log};
// built_in_methods.rs

use cranelift_frontend::FunctionBuilder;
use cranelift_codegen::ir::{types, AbiParam, Value, MemFlags, InstBuilder, StackSlotData, StackSlotKind};
use cranelift_module::Module;
use crate::backend::structs::Struct as BackendStruct;

pub fn emit_new(
    builder: &mut FunctionBuilder,
    struct_info: &BackendStruct,
    compiled_args: &[Value],
    malloc_func: Option<(cranelift_module::FuncId, &mut cranelift_object::ObjectModule, &mut cranelift_codegen::ir::Function)>,
) -> Value {
    let struct_size = struct_info.size as i64;
    let struct_ptr = if struct_size <= 128 {
        // Stack allocate
        let slot = builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            struct_size as u32,
            0,
        ));
        builder.ins().stack_addr(types::I64, slot, 0)
    } else {
        // Heap allocate
        let (malloc_id, module, func) = malloc_func.expect("malloc function required for heap allocation");
        let malloc_func_ref = module.declare_func_in_func(malloc_id, func);
        let malloc_sig_ref = func.dfg.ext_funcs[malloc_func_ref].signature;
        let params = &mut func.dfg.signatures[malloc_sig_ref].params;
        params.clear();
        params.push(AbiParam::new(types::I64));
        let struct_size_val = builder.ins().iconst(types::I64, struct_size);
        let call_inst = builder.ins().call(malloc_func_ref, &[struct_size_val]);
        let results = builder.inst_results(call_inst);
        results[0]
    };
    // Store each field value at the correct offset
    for (i, field) in struct_info.fields.iter().enumerate() {
        let value = compiled_args[i + 1]; // +1 to skip type arg
        let offset = field.offset as i32;
        let cranelift_type = match &field.ty {
            crate::backend::structs::Type::Int => types::I64, // or types::I32 if your Int is 32-bit
            crate::backend::structs::Type::Float => types::F64, // or types::F32 if your Float is 32-bit
            crate::backend::structs::Type::Bool => types::I8, // use I8 for bool
            crate::backend::structs::Type::String => types::I64, // pointer to string
            _ => types::I64, // fallback for unsupported types
        };
        // If the value is not already the correct type, cast it
        let store_value = if builder.func.dfg.value_type(value) != cranelift_type {
            let src_type = builder.func.dfg.value_type(value);
            match (src_type, cranelift_type) {
                // Integer widening
                (t1, t2) if t1.is_int() && t2.is_int() && t2.bits() > t1.bits() => builder.ins().uextend(cranelift_type, value),
                // Integer narrowing
                (t1, t2) if t1.is_int() && t2.is_int() && t2.bits() < t1.bits() => builder.ins().ireduce(cranelift_type, value),
                // Float widening
                (t1, t2) if t1.is_float() && t2.is_float() && t2.bits() > t1.bits() => builder.ins().fpromote(cranelift_type, value),
                // Float narrowing
                (t1, t2) if t1.is_float() && t2.is_float() && t2.bits() < t1.bits() => builder.ins().fdemote(cranelift_type, value),
                // Otherwise, use as-is (may error at runtime if truly incompatible)
                _ => value,
            }
        } else {
            value
        };
        // Debug print for address and offset
        cranelift_log!("Storing field '{}' at struct_ptr: {:?}, offset: {}", field.name, struct_ptr, offset);
        builder.ins().store(MemFlags::new(), store_value, struct_ptr, offset);
    }
    struct_ptr
}

pub fn emit_printf(
    builder: &mut FunctionBuilder,
    compiled_args: &[Value],
    printf_func: Option<(cranelift_module::FuncId, &mut cranelift_object::ObjectModule, &mut cranelift_codegen::ir::Function)>,
) -> Value {
    let (printf_id, _module, func) = printf_func.expect("printf function required");
    let func_ref = _module.declare_func_in_func(printf_id, func);
    let sig_ref = func.dfg.ext_funcs[func_ref].signature;
    let params = &mut func.dfg.signatures[sig_ref].params;
    params.clear();
    for _ in 0..compiled_args.len() {
        params.push(AbiParam::new(types::I64));
    }
    let call_inst = builder.ins().call(func_ref, &compiled_args);
    let results = builder.inst_results(call_inst);
    if let Some(&return_value) = results.first() {
        return return_value;
    }
    builder.ins().iconst(types::I32, 0)
}

pub fn emit_mem_load(
    builder: &mut FunctionBuilder,
    ptr: Value,
    offset: Value,
) -> Value {
    let ptr_with_offset = builder.ins().iadd(ptr, offset);
    builder.ins().load(types::I64, MemFlags::new(), ptr_with_offset, 0)
}

pub fn emit_mem_store(
    builder: &mut FunctionBuilder,
    ptr: Value,
    offset: Value,
    value: Value,
) {
    let ptr_with_offset = builder.ins().iadd(ptr, offset);
    builder.ins().store(MemFlags::new(), value, ptr_with_offset, 0);
}

pub fn emit_getfield(
    builder: &mut FunctionBuilder,
    struct_registry: &std::collections::HashMap<String, BackendStruct>,
    struct_ptr: Value,
    field_symbol: &str,
    struct_type: &str, // The type name of the struct (needed to look up the layout)
) -> Value {
    // Look up the struct layout
    cranelift_log!("Getting field: {:?} of type: {:?}", field_symbol, struct_type);
    let struct_info = struct_registry.get(struct_type).expect("Struct type not found in registry");
    cranelift_log!("Struct info: {:?}", struct_info);
    let field = struct_info.fields.iter().find(|f| f.name == field_symbol)
        .expect("Field not found in struct");
    let offset = field.offset as i32;
    cranelift_log!("Pointer: {:?}", struct_ptr);
    cranelift_log!("Offset: {:?}", offset);
    let cranelift_type = match &field.ty {
        crate::backend::structs::Type::Int => types::I64, // or types::I32 if your Int is 32-bit
        crate::backend::structs::Type::Float => types::F64, // or types::F32 if your Float is 32-bit
        crate::backend::structs::Type::Bool => types::I8, // use I8 for bool
        crate::backend::structs::Type::String => types::I64, // pointer to string
        _ => types::I64, // fallback for unsupported types
    };
    builder.ins().load(cranelift_type, MemFlags::new(), struct_ptr, offset)
}

pub fn emit_ffi(
    builder: &mut FunctionBuilder,
    func_name: &str,
    compiled_args: &[Value],
    module: &mut cranelift_object::ObjectModule,
    func: &mut cranelift_codegen::ir::Function,
) -> Value {
    use cranelift_module::{Module, FuncOrDataId};
    let func_id = match module.get_name(func_name) {
        Some(FuncOrDataId::Func(id)) => id,
        _ => {
            // If not found, declare it as an imported function with all I64 params and I64 return
            let mut sig = module.make_signature();
            for _ in 0..compiled_args.len() {
                sig.params.push(AbiParam::new(types::I64));
            }
            sig.returns.push(AbiParam::new(types::I64));
            module.declare_function(func_name, cranelift_module::Linkage::Import, &sig).unwrap()
        }
    };
    let func_ref = module.declare_func_in_func(func_id, func);
    let sig_ref = func.dfg.ext_funcs[func_ref].signature;
    let params = &mut func.dfg.signatures[sig_ref].params;
    params.clear();
    for _ in 0..compiled_args.len() {
        params.push(AbiParam::new(types::I64));
    }
    let call_inst = builder.ins().call(func_ref, compiled_args);
    let results = builder.inst_results(call_inst);
    if let Some(&return_value) = results.first() {
        return return_value;
    }
    builder.ins().iconst(types::I64, 0)
} 