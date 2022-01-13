use super::{AstValue::*, Location, Token::*, TypeContext, TypeVar};

#[test]
fn lex_works() {
    let lexed = super::lex("(    asdasd asdasd)assdasd))((");
    let golden = vec![
        LeftBracket(Location(0)),
        IdentifierToken("asdasd".to_string(), Location(0)),
        IdentifierToken("asdasd".to_string(), Location(0)),
        RightBracket(Location(0)),
        IdentifierToken("assdasd".to_string(), Location(0)),
        RightBracket(Location(0)),
        RightBracket(Location(0)),
        LeftBracket(Location(0)),
        LeftBracket(Location(0)),
    ];

    assert_eq!(lexed, golden);
}

#[test]
fn lex_works_with_symbols() {
    assert_eq!(
        super::lex("a b-c 12345 ..."),
        vec![
            IdentifierToken("a".to_string(), Location(0)),
            IdentifierToken("b-c".to_string(), Location(0)),
            NumberToken("12345".to_string(), Location(0)),
            Dot(Location(0)),
            Dot(Location(0)),
            Dot(Location(0))
        ]
    );
}

#[test]
fn lex_ids_starting_with_numbers() {
    assert_eq!(
        super::lex("111aaa"),
        vec![
            NumberToken("111".to_string(), Location(0)),
            IdentifierToken("aaa".to_string(), Location(0)),
        ]
    );
}

#[test]
fn lex_punctuation() {
    assert_eq!(
        super::lex(".():"),
        vec![
            Dot(Location(0)),
            LeftBracket(Location(0)),
            RightBracket(Location(0)),
            Colon(Location(0))
        ]
    );
}

#[test]
fn parse_identifier() {
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("hello");
    assert_eq!(
        super::parse(tokens, parse_context),
        Identifier("hello".to_string())
            .into_ast(TypeVar::new(golden_context), Location(0))
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                TypeVar::new(golden_context),
                Location(0)
            )
    );
}

#[test]
fn parse_list() {
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("(hello)");
    assert_eq!(
        super::parse(tokens, parse_context),
        Identifier("hello".to_string())
            .into_ast(TypeVar::new(golden_context), Location(0))
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                TypeVar::new(golden_context),
                Location(0)
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                TypeVar::new(golden_context),
                Location(0)
            )
    );
}

#[test]
fn parse_simple_order_in_list() {
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("(hello world!)");
    assert_eq!(
        super::parse(tokens, parse_context).pretty(),
        Identifier("hello".to_string())
            .into_ast(TypeVar::new(golden_context), Location(0))
            .cons_to(
                Identifier("world!".to_string())
                    .into_ast(TypeVar::new(golden_context), Location(0))
                    .cons_to(
                        Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                        TypeVar::new(golden_context),
                        Location(0)
                    ),
                TypeVar::new(golden_context),
                Location(0)
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                TypeVar::new(golden_context),
                Location(0)
            )
            .pretty()
    );
}

#[test]
fn parse_listed_order_in_list() {
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("((hello) (world!))");
    assert_eq!(
        super::parse(tokens, parse_context).pretty(),
        Identifier("hello".to_string())
            .into_ast(TypeVar::new(golden_context), Location(0))
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                TypeVar::new(golden_context),
                Location(0)
            )
            .cons_to(
                Identifier("world!".to_string())
                    .into_ast(TypeVar::new(golden_context), Location(0))
                    .cons_to(
                        Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                        TypeVar::new(golden_context),
                        Location(0)
                    )
                    .cons_to(
                        Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                        TypeVar::new(golden_context),
                        Location(0)
                    ),
                TypeVar::new(golden_context),
                Location(0)
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                TypeVar::new(golden_context),
                Location(0)
            )
            .pretty()
    );
}

#[test]
fn parse_big_list() {
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("(hello there i have words)");
    assert_eq!(
        super::parse(tokens, parse_context).pretty(),
        ["hello", "there", "i", "have", "words"]
            .iter()
            .rev()
            .fold(
                Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                |acc, &name| {
                    Identifier(name.to_owned())
                        .into_ast(TypeVar::new(golden_context), Location(0))
                        .cons_to(acc, TypeVar::new(golden_context), Location(0))
                }
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                TypeVar::new(golden_context),
                Location(0)
            )
            .pretty()
    );
}

#[test]
fn parse_two_lists() {
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("() ()");
    assert_eq!(
        super::parse(tokens, parse_context),
        Nil.into_ast(TypeVar::new(golden_context), Location(0))
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context), Location(0))
                    .cons_to(
                        Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                        TypeVar::new(golden_context),
                        Location(0)
                    ),
                TypeVar::new(golden_context),
                Location(0)
            )
    );
}

#[test]
fn parse_nested_list() {
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("((hello))");
    assert_eq!(
        super::parse(tokens, parse_context),
        Identifier("hello".to_string())
            .into_ast(TypeVar::new(golden_context), Location(0))
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                TypeVar::new(golden_context),
                Location(0)
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                TypeVar::new(golden_context),
                Location(0)
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                TypeVar::new(golden_context),
                Location(0)
            )
    )
}

#[test]
fn parse_smaller_list_structure() {
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("() (())");
    assert_eq!(
        super::parse(tokens, parse_context).pretty(),
        Nil.into_ast(TypeVar::new(golden_context), Location(0))
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context), Location(0))
                    .cons_to(
                        Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                        TypeVar::new(golden_context),
                        Location(0)
                    )
                    .cons_to(
                        Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                        TypeVar::new(golden_context),
                        Location(0)
                    ),
                TypeVar::new(golden_context),
                Location(0)
            )
            .pretty(),
    );
}

#[test]
fn parse_medium_list() {
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("(id (id2))");
    assert_eq!(
        super::parse(tokens, parse_context).pretty(),
        Identifier("id".to_string())
            .into_ast(TypeVar::new(golden_context), Location(0))
            .cons_to(
                Identifier("id2".to_string())
                    .into_ast(TypeVar::new(golden_context), Location(0))
                    .cons_to(
                        Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                        TypeVar::new(golden_context),
                        Location(0)
                    )
                    .cons_to(
                        Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                        TypeVar::new(golden_context),
                        Location(0)
                    ),
                TypeVar::new(golden_context),
                Location(0)
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                TypeVar::new(golden_context),
                Location(0)
            )
            .pretty()
    );
}

#[test]
fn parse_list_structure() {
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("() (() (()))");
    assert_eq!(
        super::parse(tokens, parse_context).pretty(),
        Nil.into_ast(TypeVar::new(golden_context), Location(0))
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context), Location(0))
                    .cons_to(
                        Nil.into_ast(TypeVar::new(golden_context), Location(0))
                            .cons_to(
                                Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                                TypeVar::new(golden_context),
                                Location(0)
                            )
                            .cons_to(
                                Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                                TypeVar::new(golden_context),
                                Location(0)
                            ),
                        TypeVar::new(golden_context),
                        Location(0)
                    )
                    .cons_to(
                        Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                        TypeVar::new(golden_context),
                        Location(0)
                    ),
                TypeVar::new(golden_context),
                Location(0)
            )
            .pretty()
    );
}

#[test]
fn parse_infix_dot() {
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("a . b");
    assert_eq!(
        super::parse(tokens, parse_context).pretty(),
        Identifier("a".to_string())
            .into_ast(TypeVar::new(golden_context), Location(0))
            .cons_to(
                Identifier("b".to_string()).into_ast(TypeVar::new(golden_context), Location(0)),
                TypeVar::new(golden_context),
                Location(0)
            )
            .pretty()
    );
}

#[test]
fn parse_infix_dot_in_list() {
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("(a . b)");
    assert_eq!(
        super::parse(tokens, parse_context).pretty(),
        Identifier("a".to_string())
            .into_ast(TypeVar::new(golden_context), Location(0))
            .cons_to(
                Identifier("b".to_string()).into_ast(TypeVar::new(golden_context), Location(0)),
                TypeVar::new(golden_context),
                Location(0)
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                TypeVar::new(golden_context),
                Location(0)
            )
            .pretty()
    );
}

#[test]
fn parse_colon() {
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("a : b");
    assert_eq!(
        super::parse(tokens, parse_context).pretty(),
        Identifier("a".to_owned())
            .into_ast(TypeVar::new(golden_context), Location(0))
            .ascribe(
                Identifier("b".to_owned()).into_ast(TypeVar::new(golden_context), Location(0)),
                TypeVar::new(golden_context),
                Location(0)
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                TypeVar::new(golden_context),
                Location(0)
            )
            .pretty()
    );
}

#[test]
fn parse_adjacent_colons() {
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("(a : b c : d)");
    assert_eq!(
        super::parse(tokens, parse_context).pretty(),
        Identifier("a".to_owned())
            .into_ast(TypeVar::new(golden_context), Location(0))
            .ascribe(
                Identifier("b".to_owned()).into_ast(TypeVar::new(golden_context), Location(0)),
                TypeVar::new(golden_context),
                Location(0)
            )
            .cons_to(
                Identifier("c".to_owned())
                    .into_ast(TypeVar::new(golden_context), Location(0))
                    .ascribe(
                        Identifier("d".to_owned())
                            .into_ast(TypeVar::new(golden_context), Location(0)),
                        TypeVar::new(golden_context),
                        Location(0)
                    )
                    .cons_to(
                        Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                        TypeVar::new(golden_context),
                        Location(0)
                    ),
                TypeVar::new(golden_context),
                Location(0)
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                TypeVar::new(golden_context),
                Location(0)
            )
            .pretty()
    );
}

#[test]
fn parse_nested_colons() {
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("(a : b : d)");
    assert_eq!(
        super::parse(tokens, parse_context).pretty(),
        Identifier("a".to_owned())
            .into_ast(TypeVar::new(golden_context), Location(0))
            .ascribe(
                Identifier("b".to_owned()).into_ast(TypeVar::new(golden_context), Location(0)),
                TypeVar::new(golden_context),
                Location(0)
            )
            .ascribe(
                Identifier("d".to_owned()).into_ast(TypeVar::new(golden_context), Location(0)),
                TypeVar::new(golden_context),
                Location(0)
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                TypeVar::new(golden_context),
                Location(0)
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context), Location(0)),
                TypeVar::new(golden_context),
                Location(0)
            )
            .pretty()
    );
}
