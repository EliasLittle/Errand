#!/usr/bin/env rust-script

//! ```cargo
//! [dependencies]
//! Errand = { path = "/Users/eliaslittle/Errand" }
//! ```

use Errand::frontend::ast::{Program, Expression, BinaryOperator, Id};
use Errand::backend::preir_gen::compile_preir;

fn main() {
    let program = Program {
        expressions: vec![
            // x = 1
            Expression::BinaryOp {
                operator: BinaryOperator::Assignment,
                left: Box::new(Expression::Identifier {
                    id: Id { name: "x".to_string() },
                    type_expr: None,
                }),
                right: Box::new(Expression::Int(1)),
            },
            // x = x + 1
            Expression::BinaryOp {
                operator: BinaryOperator::Assignment,
                left: Box::new(Expression::Identifier {
                    id: Id { name: "x".to_string() },
                    type_expr: None,
                }),
                right: Box::new(Expression::BinaryOp {
                    operator: BinaryOperator::Add,
                    left: Box::new(Expression::Identifier {
                        id: Id { name: "x".to_string() },
                        type_expr: None,
                    }),
                    right: Box::new(Expression::Int(1)),
                }),
            },
        ],
    };

    match compile_preir(&program) {
        Ok(preir) => {
            println!("=== PreIR Formatting Test ===");
            println!("{}", preir.format_all());
            println!("=== End PreIR Formatting Test ===");
        }
        Err(e) => {
            println!("Error compiling PreIR: {}", e);
        }
    }
}
