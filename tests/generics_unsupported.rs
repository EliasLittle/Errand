//! Negative tests for generics features the language does **not** support.
//!
//! Most generic patterns — including nested same-type containers (`Box<Box<Int>>`),
//! transitive generic->generic calls, and multi-layer container types — are
//! supported (see `tests/compile_run_golden.rs`). One case remains unsupported
//! and must be *rejected cleanly* (non-zero exit) rather than miscompiled:
//!
//!   Polymorphic recursion (a generic function calling itself at an ever-growing
//!   type), which has no finite monomorphization.
//!
//! When a case is implemented, it will start compiling and its assertion will
//! flip; that is the signal to move the fixture into the golden run suite.
//!
//! Note: the compiler reports errors through its tracing layer, not stderr, so
//! the diagnostic is read back from the `--trace-json` file.

use std::path::Path;
use std::process::Command;

struct UnsupportedCase {
    name: &'static str,
    /// Path relative to the repo root.
    source_rel: &'static str,
    /// Substring the compiler's diagnostic must contain, so the test verifies
    /// the program is rejected for the *expected* reason.
    expect_error: &'static str,
}

const CASES: &[UnsupportedCase] = &[
    UnsupportedCase {
        name: "polymorphic_recursion",
        source_rel: "tests/generics_unsupported/polymorphic_recursion.err",
        expect_error: "did not converge",
    },
];

fn errand_binary() -> &'static str {
    option_env!("CARGO_BIN_EXE_Errand").expect(
        "CARGO_BIN_EXE_Errand must be set when running integration tests with `cargo test`",
    )
}

#[test]
fn unsupported_generics_fail_to_compile() {
    let errand = errand_binary();
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let work = manifest.join("target").join("generics_unsupported");
    std::fs::create_dir_all(&work).expect("create work dir");

    for case in CASES {
        let src = manifest.join(case.source_rel);
        let exe = work.join(format!("{}_exe", case.name));
        let trace = work.join(format!("{}.trace.jsonl", case.name));

        let out = Command::new(errand)
            .current_dir(&work)
            .arg(&src)
            .arg("-o")
            .arg(&exe)
            .arg("--trace-json")
            .arg(&trace)
            .output()
            .unwrap_or_else(|e| panic!("{}: spawn Errand: {e}", case.name));

        assert!(
            !out.status.success(),
            "{}: expected compilation to FAIL (this generic feature is unsupported), \
             but the compiler succeeded. If support was added, move this fixture into \
             tests/compile_run_golden.rs.",
            case.name
        );

        let diagnostics = std::fs::read_to_string(&trace).unwrap_or_default();
        assert!(
            diagnostics.contains(case.expect_error),
            "{}: compilation failed, but not for the expected reason.\n\
             expected diagnostic to contain: {:?}\n--- trace ---\n{diagnostics}",
            case.name,
            case.expect_error,
        );
    }
}
