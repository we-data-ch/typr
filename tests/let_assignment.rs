use typr_core::components::context::config::Environment;
use typr_core::components::error_message::type_error::TypeError;

#[test]
fn test_let_assignment_type_error() {
    let code = "let a: char <- 5;";

    let res =
        typr_cli::engine::compile_string_with_errors(code, "test.ty", Environment::StandAlone);

    let type_errors = res.type_errors();

    assert!(
        type_errors
            .iter()
            .any(|err| matches!(err, TypeError::Let(_, _))),
        "Expected a TypeError::Let for mismatched let assignment, got: {:?}",
        type_errors
    );
}
