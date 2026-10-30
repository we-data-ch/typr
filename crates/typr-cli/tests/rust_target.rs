//! Integration tests for the `rust` target (rfcs/0000-compilation-targets.md, Step 3):
//! `typr run --target rust` type-checks, lowers to the typed IR, writes a Cargo project under
//! `target/rust/` and runs the binary. Each test drives the real `typr` binary on a scratch
//! project, like `tests/r_name_lint.rs`. They need `cargo` on the PATH (always true under
//! `cargo test`).

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

fn scaffold(name: &str, main_ty: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("typr_rust_{}_{}", name, std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(dir.join("TypR")).unwrap();
    fs::write(dir.join("typr.toml"), "[project]\ntargets = [\"rust\"]\n").unwrap();
    fs::write(dir.join("DESCRIPTION"), "Package: rusttest\nVersion: 0.1.0\n").unwrap();
    fs::write(dir.join("TypR/main.ty"), main_ty).unwrap();
    dir
}

fn typr(project: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_typr"))
        .args(args)
        .current_dir(project)
        .output()
        .expect("failed to run typr binary")
}

fn stdout(o: &Output) -> String {
    String::from_utf8_lossy(&o.stdout).into_owned()
}

fn stderr(o: &Output) -> String {
    String::from_utf8_lossy(&o.stderr).into_owned()
}

#[test]
fn portable_program_runs_as_a_native_binary() {
    let dir = scaffold(
        "portable",
        r#"
type Shape <- .Circle(num) | .Square(num);
let area <- fn(s: Shape): num {
    match s { .Circle(r) => 3.14159 * r * r, .Square(c) => c * c }
};
let safe_div <- fn(a: int, b: int): int | na { if (b == 0) { NA } else { a / b } };
let fact <- fn(n: int): int { if (n <= 1) { 1 } else { n * fact(n - 1) } };
print(area(.Circle(2.0)));
print(area(.Square(3.0)));
print(safe_div(6, 3));
print(safe_div(6, 0));
print(fact(5));
"#,
    );
    let out = typr(&dir, &["run"]);
    assert!(out.status.success(), "{}", stderr(&out));
    assert_eq!(
        stdout(&out).lines().filter(|l| l.starts_with('[')).collect::<Vec<_>>(),
        ["[1] 12.56636", "[1] 9", "[1] 2", "[1] NA", "[1] 120"]
    );
    assert!(dir.join("target/rust/Cargo.toml").exists());
    let _ = fs::remove_dir_all(&dir);
}

#[test]
fn integer_overflow_is_an_error_on_rust_too() {
    let dir = scaffold(
        "overflow",
        "let fact <- fn(n: int): int { if (n <= 1) { 1 } else { n * fact(n - 1) } };\nprint(fact(20));\n",
    );
    let out = typr(&dir, &["run"]);
    assert!(!out.status.success(), "overflow must stop the program");
    assert!(stderr(&out).contains("Error: integer overflow"), "{}", stderr(&out));
    let _ = fs::remove_dir_all(&dir);
}

#[test]
fn modules_and_records_compile() {
    let dir = scaffold(
        "modules",
        r#"
module geo {
  @pub let double <- fn(x: int): int { x + x };
};
type Pt <- list { x: int, y: char };
let p <- Pt:{ x = 4, y = "a" };
print(geo$double(21));
print(p$y);
"#,
    );
    let out = typr(&dir, &["run"]);
    assert!(out.status.success(), "{}", stderr(&out));
    assert!(
        stdout(&out).contains("[1] 42") && stdout(&out).contains("[1] \"a\""),
        "{}",
        stdout(&out)
    );
    let _ = fs::remove_dir_all(&dir);
}

#[test]
fn r_only_code_is_refused_before_any_rust_is_written() {
    let dir = scaffold("r_only", "let f <- fn(): Any { R { Sys.time() } };\nprint(f());\n");
    let out = typr(&dir, &["build"]);
    assert!(!out.status.success());
    assert!(stderr(&out).contains("is not available on target"), "{}", stderr(&out));
    assert!(!dir.join("target/rust/src/main.rs").exists());
    let _ = fs::remove_dir_all(&dir);
}

#[test]
fn vectors_and_state_run_natively() {
    let dir = scaffold(
        "vectors",
        r#"
let v <- [1, 2, 3];
print(v + [10, 20, 30]);
print(v * 2);
print([1.5, 2.0, 3.25]);
print(v[1:2]);
let total <- fn(xs: [#N, int]): int { let acc <- 0; for (x in xs) { acc = acc + x; }; acc };
print(sum(v));
let s <- state(0);
set(s, 5);
print(get(s));
"#,
    );
    let out = typr(&dir, &["run"]);
    assert!(out.status.success(), "{}", stderr(&out));
    assert_eq!(
        stdout(&out).lines().collect::<Vec<_>>(),
        [
            "[1] 11 22 33",
            "[1] 2 4 6",
            "[1] 1.50 2.00 3.25",
            "[1] 1 2",
            "[1] 6",
            "[1] 5"
        ]
    );
    let _ = fs::remove_dir_all(&dir);
}

#[test]
fn an_index_out_of_range_stops_the_program() {
    let dir = scaffold("oob", "let v <- [1, 2, 3];\nprint(v[2]);\nprint(v[7]);\nprint(1);\n");
    let out = typr(&dir, &["run"]);
    assert!(!out.status.success());
    assert_eq!(stdout(&out).trim(), "[1] 2");
    assert!(
        stderr(&out).contains("Error: subscript out of bounds"),
        "{}",
        stderr(&out)
    );
    let _ = fs::remove_dir_all(&dir);
}

#[test]
fn generics_are_monomorphised_per_call_site() {
    let dir = scaffold(
        "generics",
        r#"
type Box<T> <- list { v: T };
type Opt<T> <- .Some(T) | .Nothing(char);
let id <- fn(x: T): T { x };
let first <- fn(xs: [#N, T]): T { xs[1] };
let unbox <- fn(b: Box<T>): T { b$v };
let get_or <- fn(o: Opt<T>, d: T): T { match o { .Some(x) => x, .Nothing(why) => d } };
let apply <- fn(f: (T) -> U, x: T): U { f(x) };
let adder <- fn(n: int): (int) -> int { fn(x: int): int { x + n } };
module m { @pub let pick <- fn(a: T, b: T): T { a }; };
let a: Opt<int> <- .Some(4);
let b: Opt<char> <- .Nothing("none");
print(id(1));
print(id("s"));
print(first([1.5, 2.5]));
print(unbox(Box:{ v = "boxed" }));
print(get_or(a, 0));
print(get_or(b, "dflt"));
print(apply(adder(1), 10));
print(m$pick(true, false));
"#,
    );
    let out = typr(&dir, &["run"]);
    assert!(out.status.success(), "{}", stderr(&out));
    assert_eq!(
        stdout(&out).lines().collect::<Vec<_>>(),
        [
            "[1] 1",
            "[1] \"s\"",
            "[1] 1.5",
            "[1] \"boxed\"",
            "[1] 4",
            "[1] \"dflt\"",
            "[1] 11",
            "[1] TRUE"
        ]
    );
    let _ = fs::remove_dir_all(&dir);
}

#[test]
fn overloads_and_interface_parameters_resolve_statically() {
    let dir = scaffold(
        "overloads",
        r#"
type Pt <- list { x: int, y: int };
type Ci <- list { r: int };
type Sized <- interface { size: (Self) -> int };
let size <- fn(p: Pt): int { p$x + p$y };
let size <- fn(c: Ci): int { c$r };
let total <- fn(s: Sized): int { size(s) + 1 };
print(total(Pt:{ x = 1, y = 2 }));
print(total(Ci:{ r = 10 }));
print(size(Ci:{ r = 3 }));
"#,
    );
    let out = typr(&dir, &["run"]);
    assert!(out.status.success(), "{}", stderr(&out));
    assert_eq!(stdout(&out).lines().collect::<Vec<_>>(), ["[1] 4", "[1] 11", "[1] 3"]);
    let _ = fs::remove_dir_all(&dir);
}

#[test]
fn any_is_a_type_parameter_until_one_binding_holds_two_types() {
    let dir = scaffold(
        "any",
        r#"
type Box <- list { v: Any };
let show <- fn(x: Any): char { "ok" };
let pick <- fn(c: bool, a: Any, b: Any): Any { if (c) { a } else { b } };
let flag <- true;
let x: Any <- if (flag) { 1 } else { "a" };
print(show(1));
print(show("a"));
print(x);
print(pick(false, 1, "two"));
let bs <- [Box:{ v = 1 }, Box:{ v = "a" }];
print(bs[2]$v);
let n <- x as! int;
print(n);
"#,
    );
    let out = typr(&dir, &["run"]);
    assert!(out.status.success(), "{}", stderr(&out));
    assert_eq!(
        stdout(&out).lines().collect::<Vec<_>>(),
        ["[1] \"ok\"", "[1] \"ok\"", "[1] 1", "[1] \"two\"", "[1] \"a\"", "[1] 1"]
    );
    let _ = fs::remove_dir_all(&dir);
}

#[test]
fn a_failed_narrowing_of_any_stops_the_program() {
    let dir = scaffold("downcast", "let x: Any <- 1;\nlet s <- x as! char;\nprint(s);\n");
    let out = typr(&dir, &["run"]);
    assert!(!out.status.success());
    assert!(stderr(&out).contains("Validation failed for type"), "{}", stderr(&out));
    let _ = fs::remove_dir_all(&dir);
}

const API: &str = "@pub let area <- fn(r: num): num { 3.0 * r };\n@pub let inc <- fn(n: int): int { n + 1 };\n";

#[test]
fn a_library_platform_builds_without_an_entry_point() {
    let dir = scaffold("lib", API);
    let out = typr(&dir, &["build", "--platform", "lib"]);
    assert!(out.status.success(), "{}", stderr(&out));
    assert!(dir.join("target/rust/src/lib.rs").exists());
    assert!(!dir.join("target/rust/src/main.rs").exists());
    // `run` needs an executable.
    let out = typr(&dir, &["run", "--platform", "lib"]);
    assert!(
        !out.status.success() && stderr(&out).contains("needs the `bin` platform"),
        "{}",
        stderr(&out)
    );
    let _ = fs::remove_dir_all(&dir);
}

#[test]
fn platform_comes_from_typr_toml_and_is_validated() {
    let dir = scaffold("platform_cfg", API);
    fs::write(
        dir.join("typr.toml"),
        "[project]\ntargets = [\"rust\"]\n[rust]\nplatform = \"lib\"\n",
    )
    .unwrap();
    assert!(typr(&dir, &["build"]).status.success());
    assert!(dir.join("target/rust/src/lib.rs").exists());
    let out = typr(&dir, &["build", "--platform", "jvm"]);
    assert!(
        !out.status.success() && stderr(&out).contains("unknown platform `jvm`"),
        "{}",
        stderr(&out)
    );
    let _ = fs::remove_dir_all(&dir);
}

#[test]
fn wasm_platform_exports_scalar_functions() {
    // Needs the wasm32 target; without it the build, not the compiler, is what fails.
    let have_target = Command::new("rustc")
        .args(["--print", "target-libdir", "--target", "wasm32-unknown-unknown"])
        .output()
        .map(|o| o.status.success() && Path::new(String::from_utf8_lossy(&o.stdout).trim()).exists())
        .unwrap_or(false);
    if !have_target {
        return;
    }
    let dir = scaffold("wasm", API);
    let out = typr(&dir, &["build", "--platform", "wasm"]);
    assert!(out.status.success(), "{}", stderr(&out));
    let wasm = dir.join("target/rust/target/wasm32-unknown-unknown/debug");
    assert!(fs::read_dir(&wasm)
        .unwrap()
        .any(|e| e.unwrap().path().extension().is_some_and(|x| x == "wasm")));
    let _ = fs::remove_dir_all(&dir);
}

#[test]
fn extendr_refuses_an_export_that_cannot_cross_into_r() {
    let dir = scaffold(
        "extendr_t054",
        "type Sh <- .Left(num) | .Right(num);\n@pub let f <- fn(s: Sh): num { 1.0 };\n",
    );
    let out = typr(&dir, &["build", "--platform", "extendr"]);
    assert!(!out.status.success());
    let err = stderr(&out);
    assert!(
        err.contains("cannot be exported through extendr") && err.contains("`Sh` is a union"),
        "{err}"
    );
    assert!(
        !dir.join("target/rust/src/lib.rs").exists(),
        "nothing is written when the boundary check fails"
    );
    let _ = fs::remove_dir_all(&dir);
}
