/// macro for tokenizing symbols & keywords
#[macro_export]
macro_rules! t {
    (";") => {
        $crate::lexer_base::token::Token::Symbol($crate::lexer_base::token::SymbolType::Semicolon)
    };
    ("{") => {
        $crate::lexer_base::token::Token::Symbol($crate::lexer_base::token::SymbolType::LBrace)
    };
    ("}") => {
        $crate::lexer_base::token::Token::Symbol($crate::lexer_base::token::SymbolType::RBrace)
    };
    ("(") => {
        $crate::lexer_base::token::Token::Symbol($crate::lexer_base::token::SymbolType::LParen)
    };
    (")") => {
        $crate::lexer_base::token::Token::Symbol($crate::lexer_base::token::SymbolType::RParen)
    };
    ("int") => {
        $crate::lexer_base::token::Token::Keyword($crate::lexer_base::token::KeywordType::Int)
    };
    ("void") => {
        $crate::lexer_base::token::Token::Keyword($crate::lexer_base::token::KeywordType::Void)
    };
    ("return") => {
        $crate::lexer_base::token::Token::Keyword($crate::lexer_base::token::KeywordType::Return)
    };
}
