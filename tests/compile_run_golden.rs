//! Compile selected `.err` fixtures with the `Errand` binary, link with `gcc`, run the
//! executable, and compare stdout + exit code to golden expectations under
//! `tests/compile_run_expected/`.
//!
//! Fixtures are copied into `target/compile_run_golden/` so the compiler does not write
//! `.o` files next to tracked sources.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

struct GoldenCase {
    name: &'static str,
    /// Path relative to the repo root (e.g. `tests/fibonacci/fibonacci.err`).
    source_rel: &'static str,
    expected: &'static str,
}

const CASES: &[GoldenCase] = &[
    GoldenCase {
        name: "fibonacci",
        source_rel: "tests/fibonacci/fibonacci.err",
        expected: include_str!("compile_run_expected/fibonacci.expected"),
    },
    GoldenCase {
        name: "add_one",
        source_rel: "tests/addOne/addOne.err",
        expected: include_str!("compile_run_expected/add_one.expected"),
    },
    GoldenCase {
        name: "add_one_no_return",
        source_rel: "tests/addOne/addOneNoReturn.err",
        expected: include_str!("compile_run_expected/add_one_no_return.expected"),
    },
    GoldenCase {
        name: "simple_math",
        source_rel: "tests/test_printf/simple_math.err",
        expected: include_str!("compile_run_expected/simple_math.expected"),
    },
    GoldenCase {
        name: "sieve",
        source_rel: "tests/sieve/sieve.err",
        expected: include_str!("compile_run_expected/sieve.expected"),
    },
    GoldenCase {
        name: "typeinfer",
        source_rel: "tests/typeinfer/typeinfer.err",
        expected: include_str!("compile_run_expected/typeinfer.expected"),
    },
    GoldenCase {
        name: "generics_target",
        source_rel: "tests/generics_target/generics_target.err",
        expected: include_str!("compile_run_expected/generics_target.expected"),
    },
    GoldenCase {
        name: "context_target",
        source_rel: "tests/context_target/context_target.err",
        expected: include_str!("compile_run_expected/context_target.expected"),
    },
    // Generic struct with a *struct* type argument threaded through a function
    // parameter: exercises monomorphized layout registration (`Box__Apple`).
    GoldenCase {
        name: "generic_struct_field_target",
        source_rel: "tests/generic_struct_field_target/generic_struct_field_target.err",
        expected: include_str!("compile_run_expected/generic_struct_field_target.expected"),
    },
    // Generic function monomorphized per concrete type so a call inside its body
    // dispatches the right overload for each instantiation.
    GoldenCase {
        name: "generic_dispatch_target",
        source_rel: "tests/generic_dispatch_target/generic_dispatch_target.err",
        expected: include_str!("compile_run_expected/generic_dispatch_target.expected"),
    },
];

fn parse_expected(content: &str) -> (i32, String) {
    let content = content.strip_prefix('\u{feff}').unwrap_or(content);
    let first_line_end = match content.find('\n') {
        Some(i) => i,
        None => {
            return if let Some(n) = content
                .strip_prefix("# exit:")
                .map(str::trim)
                .and_then(|s| s.parse().ok())
            {
                (n, String::new())
            } else {
                (0, content.to_string())
            };
        }
    };
    let first = &content[..first_line_end];
    if let Some(rest) = first.strip_prefix("# exit:") {
        let code: i32 = rest
            .trim()
            .parse()
            .unwrap_or_else(|e| panic!("bad exit line {first:?}: {e}"));
        let stdout = content[first_line_end + 1..].to_string();
        return (code, stdout);
    }
    (0, content.to_string())
}

fn copy_fixture(manifest: &Path, rel: &Path, work: &Path, stem: &str) -> PathBuf {
    let dest = work.join(format!("{stem}.err"));
    let src = manifest.join(rel);
    fs::copy(&src, &dest)
        .unwrap_or_else(|e| panic!("copy {} -> {}: {e}", src.display(), dest.display()));
    dest
}

fn errand_binary() -> &'static str {
    option_env!("CARGO_BIN_EXE_Errand").expect(
        "CARGO_BIN_EXE_Errand must be set when running integration tests with `cargo test`",
    )
}

/// Compile `case` with the `Errand` binary, run the resulting executable, and assert its
/// stdout + exit code match the golden expectation. Panics (failing the test) on any mismatch.
fn run_golden_case(case: &GoldenCase, errand: &str, manifest: &Path, work: &Path, trace: &Path) {
    let (want_exit, want_stdout) = parse_expected(case.expected);

    let src = copy_fixture(manifest, Path::new(case.source_rel), work, case.name);
    let exe = work.join(format!("{}_exe", case.name));

    let compile = Command::new(errand)
        .current_dir(work)
        .arg(&src)
        .arg("-o")
        .arg(&exe)
        .arg("--trace-json")
        .arg(trace)
        .status()
        .unwrap_or_else(|e| panic!("{}: spawn Errand: {e}", case.name));

    assert!(
        compile.success(),
        "{}: compiler exited with {} (see {})",
        case.name,
        compile,
        trace.display()
    );

    let run = Command::new(&exe)
        .current_dir(work)
        .output()
        .unwrap_or_else(|e| panic!("{}: run {}: {e}", case.name, exe.display()));

    let got_stdout = String::from_utf8_lossy(&run.stdout).into_owned();
    let got_stderr = String::from_utf8_lossy(&run.stderr).into_owned();
    let got_code = run.status.code();

    assert_eq!(
        got_code,
        Some(want_exit),
        "{}: exit code mismatch (stderr={got_stderr:?})",
        case.name
    );
    assert_eq!(
        got_stdout, want_stdout,
        "{}: stdout mismatch\n--- expected ({} bytes) ---\n{want_stdout:?}\n--- got ({} bytes) ---\n{got_stdout:?}",
        case.name,
        want_stdout.len(),
        got_stdout.len(),
    );
}

#[test]
fn compile_run_golden_fixtures() {
    let errand = errand_binary();
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let work = manifest.join("target").join("compile_run_golden");
    fs::create_dir_all(&work).expect("create work dir");

    let trace = work.join("trace.jsonl");

    for case in CASES {
        run_golden_case(case, errand, manifest, &work, &trace);
    }
}
