#[test]
fn debug_typing_module() {
    use typr_core::processes::type_checking::{typing, Context, Lang};
    use typr_core::processes::parsing::parse2;
    use typr_core::utils::builder;
    use nom_locate::LocatedSpan;
    
    type Span<'a> = LocatedSpan<&'a str>;
    
    let input = "module M { @pub let x <- 42; }";
    let span: Span = LocatedSpan::new(input);
    let parsed = parse2(span).unwrap();
    eprintln!("DEBUG parsed: {:?}", parsed);
    
    let ctx = Context::default();
    let result = typing(&ctx, &parsed);
    eprintln!("DEBUG typing result context has M: {:?}", result.context.get_processed_module("M"));
    eprintln!("DEBUG processed_modules keys: {:?}", result.context.processed_modules.keys().collect::<Vec<_>>());
}
