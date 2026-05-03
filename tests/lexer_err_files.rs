//! Lex every `*.err` file under `tests/` recursively. Lexing must succeed for each
//! fixture (parser/type errors are not exercised here).
//!
//! Some fixtures use syntax the lexer does not implement yet (`#` comments,
//! `[` `]` literals, etc.); those paths are listed in `SKIP_LEX`.

use std::fs;
use std::path::{Path, PathBuf};
use Errand::frontend::lexer::Lexer;

/// Paths under `tests/` that must not be lexed with the current lexer (relative
/// to the `tests` directory, using `/` separators).
const SKIP_LEX: &[&str] = &[
    "ffi_test/ffi_complex_test.err", // `#` line comments
    "pipes.err",                     // `[` `]` array literals
];

fn collect_err_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_err_files(&path, out);
        } else if path.extension().and_then(|e| e.to_str()) == Some("err") {
            out.push(path);
        }
    }
}

#[test]
fn lex_all_err_files_under_tests() {
    let tests_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests");
    let mut paths = Vec::new();
    collect_err_files(&tests_dir, &mut paths);
    paths.sort();
    assert!(
        !paths.is_empty(),
        "expected at least one .err file under {}",
        tests_dir.display()
    );

    for path in paths {
        let rel = path
            .strip_prefix(&tests_dir)
            .expect("path under tests")
            .to_string_lossy()
            .replace('\\', "/");
        if SKIP_LEX.iter().any(|s| *s == rel.as_str()) {
            continue;
        }
        let source =
            fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {}: {e}", path.display()));
        let mut lexer = Lexer::new(source);
        lexer.lex("").unwrap_or_else(|e| {
            panic!("lex {} failed: {e}", path.display());
        });
    }
}
