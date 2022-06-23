use crate::{
    parse::{
        lex::{lex, Location},
        parse, Ast,
        AstValue::*,
        TypeContext,
    },
    typecheck::Type::*,
};

#[test]
fn typecheck_small() { // this function tests nothing‽
    let source = "() : Nil";
    let mut context = TypeContext::new();
    let parsed = parse(lex(source), &mut context);
    assert_eq!(
        super::typecheck(parsed, context).unwrap(),
        Ast(
            // note to self - make sure the equality check checks type equality too
            Nil,
            Expr(Box::new(Ast(
                Identifier("Nil".to_string()),
                TypeKind,
                Location(0)
            ))),
            Location(0)
        )
    );
}

#[test]
fn typecheck_small_top_level() {
    let source = "(a . 1)";
    let mut context = TypeContext::new();
    let parsed = parse(lex(source), &mut context);
    assert_eq!(

    );
}

// #[test]
// fn typecheck_fully_annotated() {
// let source = "()";
// let mut context = TypeContext::new();
// let parsed = super::parse(lex(source), &mut context);
// assert_eq!(super::typecheck(parsed, context));
// }
