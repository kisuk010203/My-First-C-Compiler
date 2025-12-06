use crate::{grammar::TokenType, t};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Void,
}

impl Type {
    pub fn from_token_type(token_type: &TokenType<'_>) -> Option<Self> {
        match token_type {
            t!("int") => Some(Type::Int),
            t!("void") => Some(Type::Void),
            _ => None,
        }
    }
}
