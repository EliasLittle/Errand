use std::collections::HashMap;

use crate::backend::preir::{Instr, LiteralPl, PreIR, FuncData, StructData, RegionData, instr_index};
use crate::frontend::ast::{BinaryOperator, UnaryOperator, Parameter};
use crate::backend::errand_builtins::{add_builtin_data_constructors, add_builtin_functions, type_expr_to_errand_type};

/// Type inference for Errand's PreIR using DK Worklist Algorithm
/// Adapted from System-F-ω to work with imperative instruction sequences

pub type TyVar = String;
pub type TmVar = String;

/// Simplified type system for Errand
#[derive(Debug, Clone, PartialEq)]
pub enum ErrandType {
    /// Struct type (simplified)
    // Struct(String),
    /// Function type: T1 -> T2
    Arrow(Box<ErrandType>, Box<ErrandType>),
    /// Type variable: α
    Var(String),
    /// Existential type variable: ^α
    ETVar(String),
    /// Type constant: Int, Bool, String, Unit (primitive types)
    Con(String),
    /// Forall type: ∀α. T
    Forall(String, Box<ErrandType>),
    /// Product type: T1 × T2
    Product(Vec<ErrandType>),
    /// Sum type: T1 + T2
    Sum(Vec<ErrandType>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum WorklistEntry {
    /// Type variable binding: α
    TVar(TyVar, TyVarKind),
    /// Term variable binding: x : T
    Var(TmVar, ErrandType),
    /// Judgment: Sub A B | Inf e ⊢ A | Chk e ⇐ A
    Judgment(Judgment),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyVarKind {
    /// Universal type variable: α
    Universal,
    /// Existential type variable: ^α
    Existential,
    /// Solved existential: ^α = τ
    Solved(ErrandType),
    /// Marker: ►α (for scoping)
    Marker,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Judgment {
    /// Subtyping: A <: B
    Sub { left: ErrandType, right: ErrandType },
    /// Type inference: e ⊢ A
    Inf { instr: Instr, ty: ErrandType },
    /// Type checking: e ⇐ A
    Chk { instr: Instr, ty: ErrandType },
    /// Instruction sequence typing
    InstrSeq { instrs: Vec<instr_index>, ty: ErrandType },
    // TODO: InfApp: A • e ⊢ C
    // InfApp {
    //     func_ty: ErrandType,
    //     arg: Instr,
    //     result_ty: ErrandType,
    // },
}

#[derive(Debug, Clone)]
pub struct Worklist {
    entries: Vec<WorklistEntry>,
    next_var: usize,
}

impl Default for Worklist {
    fn default() -> Self {
        Self::new()
    }
}

impl Worklist {
    pub fn new() -> Self {
        Worklist {
            entries: Vec::new(),
            next_var: 0,
        }
    }

    pub fn fresh_var(&mut self) -> TyVar {
        let var = format!("α{}", self.next_var);
        self.next_var += 1;
        var
    }

    pub fn fresh_evar(&mut self) -> TyVar {
        let var = format!("^α{}", self.next_var);
        self.next_var += 1;
        var
    }

    pub fn push(&mut self, entry: WorklistEntry) {
        self.entries.push(entry);
    }

    pub fn pop(&mut self) -> Option<WorklistEntry> {
        self.entries.pop()
    }

    pub fn find_var(&self, name: &str) -> Option<&ErrandType> {
        for entry in self.entries.iter().rev() {
            if let WorklistEntry::Var(var_name, ty) = entry {
                if var_name == name {
                    return Some(ty);
                }
            }
        }
        None
    }

    pub fn solve_evar(&mut self, name: &str, ty: ErrandType) -> Result<(), ErrandTypeError> {
        for entry in self.entries.iter_mut() {
            if let WorklistEntry::TVar(var_name, kind) = entry {
                if var_name == name {
                    match kind {
                        TyVarKind::Existential => {
                            *kind = TyVarKind::Solved(ty);
                            return Ok(());
                        }
                        TyVarKind::Solved(_) => {
                            // Variable already solved, that's OK
                            return Ok(());
                        }
                        _ => {
                            // Skip universal variables, markers, etc.
                            continue;
                        }
                    }
                }
            }
        }
        Err(ErrandTypeError::UnboundVariable(name.to_string()))
    }

    pub fn before(&self, a: &str, b: &str) -> bool {
        let mut pos_a = None;
        let mut pos_b = None;

        for (i, entry) in self.entries.iter().enumerate() {
            if let WorklistEntry::TVar(name, _) = entry {
                if name == a {
                    pos_a = Some(i);
                }
                if name == b {
                    pos_b = Some(i);
                }
            }
        }

        match (pos_a, pos_b) {
            (Some(pa), Some(pb)) => pa < pb,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ErrandTypeError {
    UnboundVariable(String),
    SubtypingError { left: ErrandType, right: ErrandType },
    NotAFunction(ErrandType),
    ArityMismatch { expected: usize, actual: usize },
    UnsupportedOperation(String),
    RegionTypingError(String),
}

pub type TypeResult<T> = Result<T, ErrandTypeError>;

/// Analysis result for an instruction (Zig-style caching)
#[derive(Debug, Clone)]
struct AnalysisResult {
    ty: ErrandType,
    analyzed: bool,
}

pub struct ErrandInference {
    worklist: Worklist,
    trace: Vec<String>,
    // Data constructors
    data_constructors: HashMap<String, ErrandType>,
    /// Variable typing context for locals and parameters (per-function)
    var_context: HashMap<String, ErrandType>,
    /// Module-level variables (visible to all functions)
    module_context: HashMap<String, ErrandType>,
    /// Analysis cache: instruction index -> analysis result (Zig-style)
    /// This caches types for instructions so we don't re-analyze them
    analysis_cache: HashMap<instr_index, AnalysisResult>,
    /// Reference to PreIR for looking up instructions by index
    pub preir: PreIR,
}

impl ErrandInference {
    pub fn new() -> Self {
        ErrandInference {
            worklist: Worklist::new(),
            trace: Vec::new(),
            data_constructors: HashMap::new(),
            var_context: HashMap::new(),
            module_context: HashMap::new(),
            analysis_cache: HashMap::new(),
            preir: PreIR::init(),
        }
    }

    /// Create inference engine with PreIR and Program
    /// 
    /// This performs the global pass to collect struct definitions and function placeholders,
    /// then sets up the inference engine ready for Zig-style demand-driven analysis.
    pub fn with_preir_and_program(
        preir: PreIR,
        _program: &crate::frontend::ast::Program,
    ) -> Self {
        // Collect global definitions (structs + function placeholders)
        let data_constructors = Self::collect_global_definitions(&preir);
        
        let mut inference = ErrandInference {
            worklist: Worklist::new(),
            trace: Vec::new(),
            data_constructors: HashMap::new(), // Temporary empty map
            var_context: HashMap::new(),
            module_context: HashMap::new(),
            analysis_cache: HashMap::new(),
            preir: PreIR::init(), // Temporary, will be set below
        };
        
        // Add existential type variables from data_constructors to worklist
        // This ensures they're tracked during inference
        for (_, ty) in &data_constructors {
            inference.add_etvars_to_worklist(ty);
        }
        
        // Set the data_constructors and preir
        inference.data_constructors = data_constructors;
        inference.preir = preir;
        
        inference
    }

    /// Set up var_context with function parameters before analyzing a function body.
    /// Clears var_context first so parameters from previous functions don't leak.
    /// Does not clear module_context, so module-level variables remain visible.
    pub fn setup_function_context(&mut self, parameters: &[Parameter]) {
        use crate::backend::errand_builtins::type_expr_to_errand_type;
        self.var_context.clear();
        for param in parameters {
            let param_ty = match &param.type_expr {
                Some(te) => type_expr_to_errand_type(te),
                None => ErrandType::ETVar(format!("param_{}", param.id.name)),
            };
            self.var_context.insert(param.id.name.clone(), param_ty);
        }
    }

    /// Populate module_context from var_context. Call after analyzing the main region
    /// so that module-level variables are visible to all functions.
    pub fn promote_var_context_to_module(&mut self) {
        self.module_context.extend(self.var_context.drain());
    }

    /// Analyze all instructions in a region (in order). Use for the main region so that
    /// VarDecls are processed before any instruction that references them.
    pub fn analyze_region(&mut self, region_data: &RegionData) -> TypeResult<ErrandType> {
        let mut last_ty = ErrandType::Con("Unit".to_string());
        for i in region_data.instr_start..region_data.instr_end {
            if let Some(instr) = self.preir.get_instruction(i) {
                match instr {
                    Instr::VarDecl(_) => {
                        let _ = self.analyze_instr_index(i)?;
                    }
                    Instr::FuncDecl(_) | Instr::StructDecl(_) => {}
                    _ => {
                        last_ty = self.analyze_instr_index(i)?;
                    }
                }
            }
        }
        self.solve()?;
        Ok(last_ty)
    }

    /// Analyze an instruction by index (Zig-style entry point)
    /// 
    /// This is the main entry point for type inference. Given an instruction index,
    /// it recursively analyzes all dependencies by following instruction indices.
    /// Results are cached to avoid re-analysis.
    /// 
    /// This implements demand-driven, lazy analysis:
    /// - Start from a single instruction index (e.g., function body root)
    /// - Recursively analyze dependencies by following indices
    /// - Cache results per instruction index
    /// - Only analyze what's reachable from the entry point
    pub fn analyze_instr_index(
        &mut self,
        idx: instr_index,
    ) -> TypeResult<ErrandType> {
        // Check cache first (Zig-style memoization)
        if let Some(result) = self.analysis_cache.get(&idx) {
            if result.analyzed {
                return Ok(result.ty.clone());
            }
        }

        // Get instruction from PreIR (stored in struct)
        // Clone the instruction to avoid borrow checker issues
        let instr = self.preir
            .get_instruction(idx)
            .ok_or_else(|| ErrandTypeError::UnsupportedOperation(format!(
                "Invalid instruction index: {}",
                idx
            )))?
            .clone();

        // Analyze the instruction (this may recursively call analyze_instr_index)
        let ty = self.analyze_instr_recursive(&instr, idx)?;

        // Cache the result
        self.analysis_cache.insert(
            idx,
            AnalysisResult {
                ty: ty.clone(),
                analyzed: true,
            },
        );

        Ok(ty)
    }

    /// Recursively analyze an instruction and its dependencies
    fn analyze_instr_recursive(
        &mut self,
        instr: &Instr,
        _idx: instr_index,
    ) -> TypeResult<ErrandType> {
        match instr {
            // Literals: infer type directly
            Instr::Literal(lit) => Ok(self.infer_literal_type(lit)),

            // Variable reference: look up in context (local first, then module-level)
            Instr::VarRef(var_data) => {
                if let Some(var_ty) = self.var_context.get(&var_data.name).cloned() {
                    Ok(var_ty)
                } else if let Some(var_ty) = self.module_context.get(&var_data.name).cloned() {
                    Ok(var_ty)
                } else if let Some(var_ty) = self.worklist.find_var(&var_data.name).cloned() {
                    Ok(var_ty)
                } else if let Some(var_ty) = self.data_constructors.get(&var_data.name).cloned() {
                    Ok(var_ty)
                } else {
                    Err(ErrandTypeError::UnboundVariable(var_data.name.clone()))
                }
            }

            // Binary operation: recursively analyze operands
            Instr::BinOp(binop_data) => {
                let left_ty = self.analyze_instr_index(binop_data.left)?;
                let right_ty = self.analyze_instr_index(binop_data.right)?;
                let (_left_ty, _right_ty, result_ty) = self.infer_binop_types(&binop_data.op);
                
                // Check operand types match expected
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: left_ty,
                    right: _left_ty.clone(),
                }));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: right_ty,
                    right: _right_ty.clone(),
                }));
                
                Ok(result_ty)
            }

            // Unary operation: recursively analyze operand
            Instr::UnOp(unop_data) => {
                let operand_ty = self.analyze_instr_index(unop_data.operand)?;
                let (_operand_ty, result_ty) = self.infer_unop_types(&unop_data.op);
                
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: operand_ty,
                    right: _operand_ty.clone(),
                }));
                
                Ok(result_ty)
            }

            // Function call: look up function type, recursively analyze arguments,
            // return the result type when arguments are provided
            Instr::FnCall(call_data) => {
                let func_ty = self
                    .data_constructors
                    .get(&call_data.name)
                    .ok_or_else(|| ErrandTypeError::UnboundVariable(call_data.name.clone()))?
                    .clone();

                // Analyze all arguments recursively
                let mut arg_tys = Vec::new();
                for &arg_idx in &call_data.arguments {
                    arg_tys.push(self.analyze_instr_index(arg_idx)?);
                }

                // Add subtyping constraints: each argument must match the expected parameter type.
                // When func_ty is ETVar (placeholder for user-defined functions), build the
                // expected arrow type and push a constraint so the solver can instantiate it.
                let mut remaining_ty = func_ty.clone();
                for arg_ty in &arg_tys {
                    match &remaining_ty {
                        ErrandType::Arrow(param_ty, result_ty) => {
                            self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                                left: arg_ty.clone(),
                                right: *param_ty.clone(),
                            }));
                            remaining_ty = *result_ty.clone();
                        }
                        ErrandType::ETVar(evar_name) => {
                            // Placeholder for user-defined function: ensure TVar is in worklist
                            // (it comes from collect_global_definitions, not our worklist)
                            self.worklist.push(WorklistEntry::TVar(
                                evar_name.clone(),
                                TyVarKind::Existential,
                            ));
                            // Build expected arrow type and push constraint so solver can instantiate
                            let result_evar = self.worklist.fresh_evar();
                            self.worklist.push(WorklistEntry::TVar(
                                result_evar.clone(),
                                TyVarKind::Existential,
                            ));
                            let expected_arrow = ErrandType::Arrow(
                                Box::new(arg_ty.clone()),
                                Box::new(ErrandType::ETVar(result_evar.clone())),
                            );
                            self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                                left: remaining_ty.clone(),
                                right: expected_arrow,
                            }));
                            remaining_ty = ErrandType::ETVar(result_evar);
                        }
                        _ => return Err(ErrandTypeError::NotAFunction(remaining_ty)),
                    }
                }

                // Return the result type (remaining type after consuming all arguments)
                Ok(remaining_ty)
            }

            // If statement: recursively analyze condition and branches
            Instr::IfStatement(if_data) => {
                let cond_ty = self.analyze_instr_index(if_data.condition)?;
                
                // Condition must be Bool
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: cond_ty,
                    right: ErrandType::Con("Bool".to_string()),
                }));

                let then_ty = self.analyze_instr_index(if_data.then_branch)?;
                
                if let Some(else_idx) = if_data.else_branch {
                    let else_ty = self.analyze_instr_index(else_idx)?;
                    
                    // Both branches must have compatible types
                    // For now, just use then_ty (should find common supertype)
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: else_ty,
                        right: then_ty.clone(),
                    }));
                }
                
                Ok(then_ty)
            }

            // While loop: recursively analyze condition and body
            Instr::WhileLoop(while_data) => {
                let cond_ty = self.analyze_instr_index(while_data.condition)?;
                
                // Condition must be Bool
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: cond_ty,
                    right: ErrandType::Con("Bool".to_string()),
                }));

                // Body is analyzed but result is ignored (loops return Unit)
                let _body_ty = self.analyze_instr_index(while_data.body)?;
                
                Ok(ErrandType::Con("Unit".to_string()))
            }

            // Return: recursively analyze value if present
            Instr::Return(return_data) => {
                if let Some(value_idx) = return_data.value {
                    self.analyze_instr_index(value_idx)
                } else {
                    Ok(ErrandType::Con("Unit".to_string()))
                }
            }

            // Region: recursively analyze all instructions in the region
            Instr::Region(region_data) => {
                let mut last_ty = ErrandType::Con("Unit".to_string());
                
                // Analyze all instructions in the region range
                for i in region_data.instr_start..region_data.instr_end {
                    if let Some(instr) = self.preir.get_instruction(i) {
                        match instr {
                            Instr::VarDecl(_) => {
                                // Analyze VarDecl to populate var_context for later VarRef lookups
                                let _ = self.analyze_instr_index(i)?;
                            }
                            Instr::FuncDecl(_) | Instr::StructDecl(_) => {
                                // Skip - these are handled separately
                            }
                            _ => {
                                last_ty = self.analyze_instr_index(i)?;
                            }
                        }
                    }
                }
                
                self.solve()?;
                Ok(last_ty)
            }

            // Variable declaration: recursively analyze value and add to var_context
            Instr::VarDecl(var_decl) => {
                let value_ty = self.analyze_instr_index(var_decl.value)?;
                self.var_context.insert(var_decl.name.clone(), value_ty);
                Ok(ErrandType::Con("Unit".to_string()))
            }

            // Function declaration: recursively analyze body
            Instr::FuncDecl(func_data) => {
                let body_ty = self.analyze_instr_index(func_data.body_index)?;
                
                // Build function type from parameters and body
                let func_type = self.build_function_type_from_params(&func_data.parameters, body_ty);
                
                // Unify with placeholder type in data_constructors
                if let Some(placeholder_ty) = self.data_constructors.get(&func_data.name) {
                    // Ensure TVar for placeholder evar is in worklist (from collect_global_definitions)
                    if let ErrandType::ETVar(evar_name) = placeholder_ty {
                        self.worklist.push(WorklistEntry::TVar(
                            evar_name.clone(),
                            TyVarKind::Existential,
                        ));
                    }
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: func_type.clone(),
                        right: placeholder_ty.clone(),
                    }));
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: placeholder_ty.clone(),
                        right: func_type.clone(),
                    }));
                    self.solve()?;
                }
                
                Ok(ErrandType::Con("Unit".to_string())) // Function declarations return Unit
            }

            // Struct declaration: just return Unit
            Instr::StructDecl(_) => Ok(ErrandType::Con("Unit".to_string())),

            // For loop: not supported
            Instr::ForLoop(_) => Err(ErrandTypeError::UnsupportedOperation(
                "ForLoop".to_string(),
            )),
        }
    }

    /// Build a function type from parameters and return type
    fn build_function_type_from_params(
        &self,
        parameters: &[Parameter],
        return_type: ErrandType,
    ) -> ErrandType {
        use crate::backend::errand_builtins::type_expr_to_errand_type;
        
        // Build function type by folding parameter types: T1 -> T2 -> ... -> ReturnType
        parameters.iter().rev().fold(return_type, |acc, param| {
            let param_type = match &param.type_expr {
                Some(type_expr) => type_expr_to_errand_type(type_expr),
                None => ErrandType::ETVar(format!("param_{}", param.id.name)),
            };
            ErrandType::Arrow(Box::new(param_type), Box::new(acc))
        })
    }

    /// Collect global definitions (structs and functions) from PreIR
    /// 
    /// This performs a single pass to:
    /// 1. Collect struct definitions and add them to data_constructors
    /// 2. Collect function names and add placeholder types:
    ///    - If function has complete signature (all params + return type), use explicit type
    ///    - Otherwise, create existential type variable for mutual recursion support
    /// 
    /// Returns the populated data_constructors map (including builtins)
    pub fn collect_global_definitions(preir: &PreIR) -> HashMap<String, ErrandType> {
        let mut data_constructors = add_builtin_data_constructors();
        
        // Add builtin functions
        for (name, ty) in add_builtin_functions() {
            data_constructors.insert(name, ty);
        }

        // Collect function definitions with placeholder types first
        let mut worklist = Worklist::new();
        for instr in &preir.instructions {
            if let Instr::FuncDecl(FuncData { name, .. }) = instr {
                // Don't overwrite builtins (printf, malloc, free, as_ptr, getfield)
                if data_constructors.contains_key(name) {
                    continue;
                }
                // Create existential type variable as placeholder for mutual recursion
                // This allows functions to reference each other before inference
                let evar = worklist.fresh_evar();
                let func_type = ErrandType::ETVar(evar);
                
                data_constructors.insert(name.clone(), func_type);
            }
        }

        // Collect struct definitions: add constructor type using exact name from definition.
        // Process after FuncDecl so struct constructor types overwrite placeholders.
        for instr in &preir.instructions {
            if let Instr::StructDecl(StructData { name, fields }) = instr {
                // Build constructor type: field1 -> field2 -> ... -> struct_name
                let constructor_ty = fields.iter().rev().fold(
                    ErrandType::Con(name.clone()),
                    |acc, field| ErrandType::Arrow(
                        Box::new(type_expr_to_errand_type(&field.field_type)),
                        Box::new(acc),
                    ),
                );
                data_constructors.insert(name.clone(), constructor_ty);
            }
        }

        data_constructors
    }


    pub fn check_instr(&mut self, instr: &Instr, expected_ty: &ErrandType) -> TypeResult<()> {
        self.worklist.push(WorklistEntry::Judgment(Judgment::Chk {
            instr: instr.clone(),
            ty: expected_ty.clone(),
        }));

        self.solve()
    }

    pub fn infer_instr(&mut self, instr: &Instr) -> TypeResult<ErrandType> {
        let result_ty = ErrandType::ETVar(self.worklist.fresh_evar());
        if let ErrandType::ETVar(var_name) = &result_ty {
            self.worklist.push(WorklistEntry::TVar(
                var_name.clone(),
                TyVarKind::Existential,
            ));
        }

        self.worklist.push(WorklistEntry::Judgment(Judgment::Inf {
            instr: instr.clone(),
            ty: result_ty.clone(),
        }));

        self.solve()?;
        Ok(result_ty)
    }

    fn solve(&mut self) -> TypeResult<()> {
        while let Some(entry) = self.worklist.pop() {
            match entry {
                WorklistEntry::TVar(_, _) => {
                    // Skip variable bindings during processing
                    continue;
                }
                WorklistEntry::Var(_, _) => {
                    // Skip term variable bindings during processing
                    continue;
                }
                WorklistEntry::Judgment(judgment) => {
                    self.solve_judgment(judgment)?;
                }
            }
        }
        Ok(())
    }

    fn solve_judgment(&mut self, judgment: Judgment) -> TypeResult<()> {
        match judgment {
            Judgment::Sub { left, right } => self.solve_subtype(left, right),
            Judgment::Inf { instr, ty } => self.solve_inference(instr, ty),
            Judgment::Chk { instr, ty } => self.solve_checking(instr, ty),
            Judgment::InstrSeq { instrs: _, ty: _ } => {
                // TODO: Implement instruction sequence typing
                Ok(())
            }
        }
    }

    fn solve_subtype(&mut self, left: ErrandType, right: ErrandType) -> TypeResult<()> {
        self.trace.push(format!("Sub {} <: {}", self.type_to_string(&left), self.type_to_string(&right)));

        if left == right {
            return Ok(());
        }

        match (&left, &right) {
            // Primitive type reflexivity
            // (ErrandType::Int, ErrandType::Int) => Ok(()),
            // (ErrandType::Float, ErrandType::Float) => Ok(()),
            // (ErrandType::Bool, ErrandType::Bool) => Ok(()),
            // (ErrandType::String, ErrandType::String) => Ok(()),
            // (ErrandType::Unit, ErrandType::Unit) => Ok(()),
            (ErrandType::Var(a), ErrandType::Var(b)) if a == b => Ok(()),
            (ErrandType::ETVar(a), ErrandType::ETVar(b)) if a == b => Ok(()),

            // Concrete type is subtype of type variable (for polymorphic params like getfield's struct arg)
            (ErrandType::Con(_), ErrandType::Var(_)) => Ok(()),

            // TODO? Struct subtyping

            // Function subtyping (contravariant in argument, covariant in result)
            (ErrandType::Arrow(a1, a2), ErrandType::Arrow(b1, b2)) => {
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: *b1.clone(),
                    right: *a1.clone(),
                }));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: *a2.clone(),
                    right: *b2.clone(),
                }));
                Ok(())
            }

            // Existential variable instantiation
            (ErrandType::ETVar(a), _) if !self.occurs_check(a, &right) => {
                self.instantiate_left(a, &right)
            }
            (_, ErrandType::ETVar(a)) if !self.occurs_check(a, &left) => {
                self.instantiate_right(&left, a)
            }

            _ => Err(ErrandTypeError::SubtypingError {
                left,
                right,
            }),
        }
    }

    fn solve_inference(&mut self, instr: Instr, ty: ErrandType) -> TypeResult<()> {
        self.trace.push(format!("Inf {} ⊢ {}", self.instr_to_string(&instr), self.type_to_string(&ty)));

        log::info!("Solving Inference: {:?}", instr);

        match instr {
            Instr::Literal(literal) => {
                let literal_ty = self.infer_literal_type(&literal);
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: literal_ty,
                    right: ty,
                }));
                Ok(())
            }

            Instr::VarRef(var_data) => {
                if let Some(var_ty) = self.var_context.get(&var_data.name).cloned() {
                    // Have we already typed this variable?
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: var_ty,
                        right: ty,
                    }));
                    Ok(())
                } else if let Some(var_ty) = self.worklist.find_var(&var_data.name).cloned() {
                    // Have we seen, but not yet typed this variable?
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: var_ty,
                        right: ty,
                    }));
                    Ok(())
                } else if let Some(var_ty) = self.data_constructors.get(&var_data.name).cloned() {
                    // Check data constructors for constructor variables
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: var_ty,
                        right: ty,
                    }));
                    Ok(())
                } else {
                    log::error!("Failed to find variable: {} Not in var_context, worklist, or data_constructors", var_data.name);
                    Err(ErrandTypeError::UnboundVariable(var_data.name))
                }
            }

            Instr::BinOp(binop_data) => {
                let (_left_ty, _right_ty, result_ty) = self.infer_binop_types(&binop_data.op);

                // Check that the result type matches the expected type
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: result_ty,
                    right: ty,
                }));

                // TODO: Add constraints for left and right operands using their indices
                // This would require access to the instruction sequence

                Ok(())
            }

            Instr::UnOp(unop_data) => {
                let (_operand_ty, result_ty) = self.infer_unop_types(&unop_data.op);

                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: result_ty,
                    right: ty,
                }));

                // TODO: Add constraint for operand using its index

                Ok(())
            }

            Instr::FnCall(call_data) => {
                if let Some(func_ty) = self.data_constructors.get(&call_data.name).cloned() {
                    // For now, assume function returns the expected type
                    // TODO: Implement proper function application typing
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: func_ty,
                        right: ty,
                    }));
                    Ok(())
                } else {
                    log::error!("Failed to find function: {} Not in data_constructors", call_data.name);
                    Err(ErrandTypeError::UnboundVariable(call_data.name))
                }
            }

            Instr::IfStatement(_if_data) => {
                // Both branches must have the same type as the expected result
                // The condition must be Bool
                // TODO: Add constraints for condition and branches using their indices

                // For now, just check that the result type is compatible
                Ok(())
            }

            Instr::WhileLoop(_while_data) => {
                // While loops return Unit
                // The condition must be Bool
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: ErrandType::Con("Unit".to_string()),
                    right: ty,
                }));

                // TODO: Add constraint that condition is Bool using its index

                Ok(())
            }

            Instr::Return(return_data) => {
                if let Some(_value_idx) = return_data.value {
                    // TODO: Add constraint for return value using its index
                    Ok(())
                } else {
                    // Return with no value returns Unit
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: ErrandType::Con("Unit".to_string()),
                        right: ty,
                    }));
                    Ok(())
                }
            }

            Instr::Region(_region_data) => {
                // Regions return the type of their final expression
                // TODO: Implement proper region typing
                Ok(())
            }

            Instr::VarDecl(_var_decl) => {
                // Variable declarations return Unit
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: ErrandType::Con("Unit".to_string()),
                    right: ty,
                }));
                Ok(())
            }

            Instr::FuncDecl(_func_decl) => {
                // Function declarations return Unit
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: ErrandType::Con("Unit".to_string()),
                    right: ty,
                }));
                Ok(())
            }

            Instr::StructDecl(_struct_decl) => {
                // Struct declarations return Unit
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: ErrandType::Con("Unit".to_string()),
                    right: ty,
                }));
                Ok(())
            }

            Instr::ForLoop(_) => {
                // Skip for loops as requested
                Err(ErrandTypeError::UnsupportedOperation("ForLoop".to_string()))
            }
        }
    }

    fn solve_checking(&mut self, instr: Instr, ty: ErrandType) -> TypeResult<()> {
        self.trace.push(format!("Chk {} ⇐ {}", self.instr_to_string(&instr), self.type_to_string(&ty)));

        // For most instructions, we can use the inference mode and check subtyping
        let inferred_ty = ErrandType::ETVar(self.worklist.fresh_evar());
        if let ErrandType::ETVar(var_name) = &inferred_ty {
            self.worklist.push(WorklistEntry::TVar(
                var_name.clone(),
                TyVarKind::Existential,
            ));
        }

        self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
            left: inferred_ty.clone(),
            right: ty,
        }));

        self.worklist.push(WorklistEntry::Judgment(Judgment::Inf {
            instr,
            ty: inferred_ty,
        }));

        Ok(())
    }

    fn infer_literal_type(&self, literal: &LiteralPl) -> ErrandType {
        match literal {
            LiteralPl::Int(_) => ErrandType::Con("Int".to_string()),
            LiteralPl::Float(_) => ErrandType::Con("Float".to_string()),
            LiteralPl::Boolean(_) => ErrandType::Con("Bool".to_string()),
            LiteralPl::String(_) => ErrandType::Con("String".to_string()),
            LiteralPl::Symbol(_) => ErrandType::Con("String".to_string()), // Treat symbols as strings for now
            LiteralPl::Unit => ErrandType::Con("Unit".to_string()),
        }
    }

    // TODO: Remove operators, and just use functions
    fn infer_binop_types(&self, op: &BinaryOperator) -> (ErrandType, ErrandType, ErrandType) {
        match op {
            // Arithmetic operations: Int -> Int -> Int (or Float -> Float -> Float)
            BinaryOperator::Add | BinaryOperator::Subtract | BinaryOperator::Multiply | 
            BinaryOperator::Divide | BinaryOperator::Modulo | BinaryOperator::Power => {
                // For simplicity, assume Int operations for now
                (ErrandType::Con("Int".to_string()), ErrandType::Con("Int".to_string()), ErrandType::Con("Int".to_string()))
            }
            // Comparison operations: Int -> Int -> Bool (or Float -> Float -> Bool)
            BinaryOperator::LessThan | BinaryOperator::LessThanEqual | 
            BinaryOperator::GreaterThan | BinaryOperator::GreaterThanEqual |
            BinaryOperator::Equal | BinaryOperator::NotEqual => {
                (ErrandType::Con("Int".to_string()), ErrandType::Con("Int".to_string()), ErrandType::Con("Bool".to_string()))
            }
            // Logical operations: Bool -> Bool -> Bool
            BinaryOperator::And | BinaryOperator::Or => {
                (ErrandType::Con("Bool".to_string()), ErrandType::Con("Bool".to_string()), ErrandType::Con("Bool".to_string()))
            }
            // Bitwise operations: Int -> Int -> Int
            BinaryOperator::Ampersand | BinaryOperator::Pipe => {
                (ErrandType::Con("Int".to_string()), ErrandType::Con("Int".to_string()), ErrandType::Con("Int".to_string()))
            }
            // Special operations (for now treat as polymorphic)
            BinaryOperator::Dot => {
                // Field access - left operand is struct, result depends on field
                (ErrandType::Con("Struct".to_string()), ErrandType::Con("String".to_string()), ErrandType::Con("Int".to_string())) // simplified
            }
            BinaryOperator::Assignment => {
                // Assignment - both operands should have same type, returns Unit
                (ErrandType::Con("Int".to_string()), ErrandType::Con("Int".to_string()), ErrandType::Con("Unit".to_string())) // simplified
            }
        }
    }

    fn add_etvars_to_worklist(&mut self, ty: &ErrandType) {
        match ty {
            ErrandType::ETVar(var_name) => {
                self.worklist.push(WorklistEntry::TVar(
                    var_name.clone(),
                    TyVarKind::Existential,
                ));
            }
            ErrandType::Arrow(t1, t2) => { //| ErrandType::App(t1, t2) 
                self.add_etvars_to_worklist(&*t1);
                self.add_etvars_to_worklist(&*t2);
            }
            ErrandType::Forall(_, t) => {
                self.add_etvars_to_worklist(&*t);
            }
            ErrandType::Product(types) | ErrandType::Sum(types) => {
                for t in types {
                    self.add_etvars_to_worklist(&*t);
                }
            }
            ErrandType::Var(_) | ErrandType::Con(_) => {
                // No existential variables to add
            }
        }
    }

    fn infer_unop_types(&self, op: &UnaryOperator) -> (ErrandType, ErrandType) {
        match op {
            UnaryOperator::Negate => (ErrandType::Var("Int".to_string()), ErrandType::Var("Int".to_string())),
            UnaryOperator::Not => (ErrandType::Var("Bool".to_string()), ErrandType::Var("Bool".to_string())),
        }
    }

    fn instantiate_left(&mut self, var: &str, ty: &ErrandType) -> TypeResult<()> {
        match ty {
            ErrandType::ETVar(b) if self.worklist.before(var, b) => {
                self.worklist.solve_evar(b, ErrandType::ETVar(var.to_string()))?;
                Ok(())
            }
            ErrandType::Arrow(t1, t2) => {
                let a1 = self.worklist.fresh_evar();
                let a2 = self.worklist.fresh_evar();
                let arrow_ty = ErrandType::Arrow(
                    Box::new(ErrandType::ETVar(a1.clone())),
                    Box::new(ErrandType::ETVar(a2.clone())),
                );
                self.worklist.solve_evar(var, arrow_ty)?;

                self.worklist.push(WorklistEntry::TVar(a1.clone(), TyVarKind::Existential));
                self.worklist.push(WorklistEntry::TVar(a2.clone(), TyVarKind::Existential));

                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: *t1.clone(),
                    right: ErrandType::ETVar(a1),
                }));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: ErrandType::ETVar(a2),
                    right: *t2.clone(),
                }));

                Ok(())
            }
            _ if self.is_monotype(ty) => {
                self.worklist.solve_evar(var, ty.clone())?;
                Ok(())
            }
            _ => Err(ErrandTypeError::UnsupportedOperation(format!("instantiate_left with {}", self.type_to_string(ty)))),
        }
    }

    fn instantiate_right(&mut self, ty: &ErrandType, var: &str) -> TypeResult<()> {
        match ty {
            ErrandType::ETVar(a) if self.worklist.before(var, a) => {
                self.worklist.solve_evar(a, ErrandType::ETVar(var.to_string()))?;
                Ok(())
            }
            ErrandType::Arrow(t1, t2) => {
                let a1 = self.worklist.fresh_evar();
                let a2 = self.worklist.fresh_evar();
                let arrow_ty = ErrandType::Arrow(
                    Box::new(ErrandType::ETVar(a1.clone())),
                    Box::new(ErrandType::ETVar(a2.clone())),
                );
                self.worklist.solve_evar(var, arrow_ty)?;

                self.worklist.push(WorklistEntry::TVar(a1.clone(), TyVarKind::Existential));
                self.worklist.push(WorklistEntry::TVar(a2.clone(), TyVarKind::Existential));

                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: ErrandType::ETVar(a1),
                    right: *t1.clone(),
                }));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: *t2.clone(),
                    right: ErrandType::ETVar(a2),
                }));

                Ok(())
            }
            _ if self.is_monotype(ty) => {
                self.worklist.solve_evar(var, ty.clone())?;
                Ok(())
            }
            _ => Err(ErrandTypeError::UnsupportedOperation(format!("instantiate_right with {}", self.type_to_string(ty)))),
        }
    }

    fn occurs_check(&self, var: &str, ty: &ErrandType) -> bool {
        match ty {
            ErrandType::ETVar(name) | ErrandType::Var(name) | ErrandType::Con(name) => name == var,
            ErrandType::Arrow(t1, t2) => {
                self.occurs_check(var, t1) || self.occurs_check(var, t2)
            }
            ErrandType::Forall(_, t) => self.occurs_check(var, t),
            ErrandType::Product(types) | ErrandType::Sum(types) => types.iter().any(|t| self.occurs_check(var, t)),
        }
    }

    fn is_monotype(&self, ty: &ErrandType) -> bool {
        match ty {
            ErrandType::Var(_) | ErrandType::ETVar(_) | ErrandType::Con(_) => true,
            ErrandType::Arrow(t1, t2) => self.is_monotype(t1) && self.is_monotype(t2),
            ErrandType::Product(types) | ErrandType::Sum(types) => types.iter().all(|t| self.is_monotype(t)),
            ErrandType::Forall(_, _) => false,
        }
    }

    fn type_to_string(&self, ty: &ErrandType) -> String {
        match ty {
            ErrandType::Var(name) | ErrandType::Con(name) => name.clone(),
            ErrandType::ETVar(name) => name.clone(),
            ErrandType::Arrow(t1, t2) => format!("{} -> {}", self.type_to_string(t1), self.type_to_string(t2)),
            ErrandType::Forall(var, t) => format!("∀{}. {}", var, self.type_to_string(t)),
            ErrandType::Product(types) => format!("{}", types.iter().map(|t| self.type_to_string(t)).collect::<Vec<String>>().join(" × ")),
            ErrandType::Sum(types) => format!("{}", types.iter().map(|t| self.type_to_string(t)).collect::<Vec<String>>().join(" + ")),
        }
    }

    fn instr_to_string(&self, instr: &Instr) -> String {
        match instr {
            Instr::Literal(lit) => format!("{}", lit),
            Instr::VarRef(var) => var.name.clone(),
            Instr::BinOp(binop) => format!("binop {:?}", binop.op),
            Instr::UnOp(unop) => format!("unop {:?}", unop.op),
            Instr::FnCall(call) => format!("call {}", call.name),
            Instr::IfStatement(_) => "if".to_string(),
            Instr::WhileLoop(_) => "while".to_string(),
            Instr::ForLoop(_) => "for".to_string(),
            Instr::Return(_) => "return".to_string(),
            Instr::Region(_) => "region".to_string(),
            Instr::VarDecl(var) => format!("var {}", var.name),
            Instr::FuncDecl(func) => format!("func {}", func.name),
            Instr::StructDecl(struct_data) => format!("struct {}", struct_data.name),
        }
    }

    pub fn get_trace(&self) -> &[String] {
        &self.trace
    }
}