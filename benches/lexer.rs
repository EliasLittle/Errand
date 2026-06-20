//! Lexer benchmarks: wall time via Criterion, allocation deltas via stats_alloc.
//!
//! Criterion measures only elapsed time, not heap. This bench installs
//! `stats_alloc` as the global allocator and prints one `Region` delta per
//! scenario (a single `Lexer::new` + `lex("")` pass) so you can compare
//! allocation counts and byte totals across lexer changes.

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use stats_alloc::{Region, Stats, StatsAlloc, INSTRUMENTED_SYSTEM};
use std::alloc::System;
use Errand::frontend::lexer::Lexer;

#[global_allocator]
static GLOBAL: &StatsAlloc<System> = &INSTRUMENTED_SYSTEM;

/// Allocations from `source.to_string()`, `Lexer::new`, and `lex("")` for one pass.
fn alloc_stats_one_pass(source: &str) -> Stats {
    let region = Region::new(&GLOBAL);
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.lex("").expect("lex");
    let change = region.change();
    black_box(tokens.len());
    change
}

fn eprint_alloc_stats(label: &str, s: &Stats) {
    eprintln!(
        "[lexer/memory] {label}: alloc_ops={} dealloc_ops={} realloc_ops={} \
         bytes_requested={} bytes_freed={} bytes_realloc_delta={}",
        s.allocations,
        s.deallocations,
        s.reallocations,
        s.bytes_allocated,
        s.bytes_deallocated,
        s.bytes_reallocated
    );
}

fn basic_fixture() -> String {
    include_str!("../tests/basic/basic.err").to_string()
}

fn bench_lex_basic(c: &mut Criterion) {
    eprint_alloc_stats(
        "lexer_basic_err (one new+lex)",
        &alloc_stats_one_pass(include_str!("../tests/basic/basic.err")),
    );
    let mut lexer = Lexer::new(basic_fixture());
    c.bench_function("lexer_basic_err", |b| {
        b.iter(|| {
            let tokens = lexer.lex(black_box("")).expect("lex");
            black_box(tokens.len())
        });
    });
}

fn medium_fixture() -> String {
    let mut s = String::with_capacity(120_000);
    for i in 0..5000 {
        use std::fmt::Write;
        writeln!(&mut s, "ident_{i} = {i}").unwrap();
    }
    s
}

fn bench_lex_medium(c: &mut Criterion) {
    let medium = medium_fixture();
    eprint_alloc_stats(
        "lexer_5k_assignments (one new+lex)",
        &alloc_stats_one_pass(&medium),
    );
    let mut lexer = Lexer::new(medium);
    c.bench_function("lexer_5k_assignments", |b| {
        b.iter(|| {
            let tokens = lexer.lex(black_box("")).expect("lex");
            black_box(tokens.len())
        });
    });
}

criterion_group!(benches, bench_lex_basic, bench_lex_medium);
criterion_main!(benches);
