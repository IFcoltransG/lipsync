use std::collections::HashMap;

use crate::parse::{Ast, AstNode, AstValue::*, TypeContext, TypeVar};

fn typecheck(ast: Ast<TypeVar>, context: TypeContext) -> Option<Ast<!>> {
    let mut current_state = Substitution::default();
    dbg!(ast.clone());
    let substitutions = current_state.from_top_level(ast, <_>::default(), context);
    // above should create a substitution representing trying to type the ast with a
    // given type? (Or perhaps just recurse through the tree and look for
    // constraints?)
    dbg!(current_state);
    todo!()
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate) enum Type<V> {
    Unknown(V),
    Expr(AstNode<V>),
    Special(String),
    TypeKind,
}

#[derive(Debug, Clone)]
struct Substitution {
    subs: HashMap<TypeVar, Type<TypeVar>>,
    constraints: Vec<Constraint>,
    scope: HashMap<String, Type<TypeVar>>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum ProjectionType {
    LeftOf,
    RightOf,
    StartOf,
    EndOf,
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum Constraint {
    Identification(Type<TypeVar>, Type<TypeVar>),
    Subtyping(Type<TypeVar>, Type<TypeVar>), // left is subtype, right is supertype
    Projection(Type<TypeVar>, Type<TypeVar>, ProjectionType), /* left is projected type, right
                                              * is type from
                                              * which is projected */
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum TypeOfTypeNamingIsHard {
    Declaration,
}

impl Default for TypeOfTypeNamingIsHard {
    fn default() -> Self {
        Self::Declaration
    }
}

impl Default for Substitution {
    fn default() -> Self {
        Self {
            subs: Default::default(),
            constraints: Default::default(),
            scope: Default::default(),
        }
    }
}

impl Substitution {
    fn from_ast(
        // takes a substitution context, as well as a piece of ast. Then should type its sub-exprs
        // by recursion (TODO)
        &mut self,
        Ast(ast, inner_type, location): Ast<TypeVar>,
        context: TypeContext,
    ) {
        match ast {
            Identifier(name) => {
                let variable_type = self
                    .scope
                    .get(&name)
                    .expect("An undefined name really shouldn't crash but it does now"); // FIXME
                self.constraints.push(Constraint::Identification(
                    inner_type,
                    variable_type.clone(),
                ));
            }
            Number(_) => {
                self.constraints.push(Constraint::Subtyping(
                    inner_type,
                    Type::Special("Number thing".into()),
                ));
            }
            Cons(left_box, right_box) => {
                let (Ast(_, left_type, _), Ast(_, right_type, _)) =
                    (*left_box.clone(), *right_box.clone());
                self.constraints.push(Constraint::Projection(
                    left_type,
                    inner_type.clone(),
                    ProjectionType::LeftOf,
                ));
                self.constraints.push(Constraint::Projection(
                    right_type,
                    inner_type.clone(),
                    ProjectionType::RightOf,
                ));
                self.constraints.push(Constraint::Subtyping(
                    inner_type,
                    Type::Special("cons of l and r".into()),
                ));
                self.from_ast(*left_box, context);
                self.from_ast(*right_box, context);
            }
            Ascription(boxed_value, ascribed_type) => {
                let Ast(_, expr_in_box_type, _) = *boxed_value.clone();
                self.constraints
                    .push(Constraint::Subtyping(inner_type.clone(), ascribed_type));
                self.constraints
                    .push(Constraint::Identification(inner_type, expr_in_box_type));
                self.from_ast(*boxed_value, context)
            }
            Nil => {
                self.constraints.push(Constraint::Subtyping(
                    inner_type,
                    Type::Special("Nil".into()),
                ));
            }
        }
    }

    fn from_top_level(
        &mut self,
        Ast(ast, inner_type, location): Ast<TypeVar>,
        extra_parameter: TypeOfTypeNamingIsHard, // why do I need this?
        context: TypeContext,
    ) {
        self.from_ast(Ast(ast.clone(), inner_type, location), context);
        match ast {
            Identifier(_) => panic!(
                "Could not typecheck identifier as declaration {:?} ",
                location
            ),
            Number(_) => panic!("Could not typecheck number as declaration {:?} ", location),
            Cons(head_box, tail_box) => { // expects a cons of (name . value)
                self.from_name(*head_box, <_>::default(), context);
            }
            Ascription(boxed_value, boxed_type) => {
                let Ast(value, _, ascription_location) = *boxed_value;
            }
            Nil => panic!(
                "Could not typecheck empty list as declaration {:?} ",
                location
            ),
        }
    }

    fn from_name(
        &mut self,
        Ast(ast, inner_type, location): Ast<TypeVar>,
        extra_parameter: TypeOfTypeNamingIsHard,
        context: TypeContext,
    ) -> String {
        match ast {
            Identifier(name) => return name,
            Number(_) => panic!(
                "Non-identifier used as name for a declaration at {:?}",
                location
            ),
            Cons(..) => panic!(
                "Non-identifier used as name for a declaration at {:?}",
                location
            ),
            Ascription(..) => panic!(
                "Non-identifier used as name for a declaration at {:?}",
                location
            ),
            Nil => panic!(
                "Non-identifier used as name for a declaration at {:?}",
                location
            ),
        }
    }
}

#[cfg(test)]
mod tests;
