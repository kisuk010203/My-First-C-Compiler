/// macro for tokenizing symbols & keywords
#[macro_export]
macro_rules! t {
    ("+") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::Plus)
    };
    ("-") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::Minus)
    };
    ("*") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::Star)
    };
    ("/") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::Slash)
    };
    ("!") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::Not)
    };
    (";") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::Semicolon)
    };
    ("{") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::LBrace)
    };
    ("}") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::RBrace)
    };
    ("(") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::LParen)
    };
    (")") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::RParen)
    };
    ("int") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::Int)
    };
    ("void") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::Void)
    };
    ("return") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::Return)
    };
    ("<") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::LessThan)
    };
    (">") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::GreaterThan)
    };
    ("<=") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::LessThanOrEqual)
    };
    (">=") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::GreaterThanOrEqual)
    };
    ("==") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::EqualEqual)
    };
    ("!=") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::NotEqual)
    };
    ("if") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::If)
    };
    ("else") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::Else)
    };
    ("while") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::While)
    };
    ("do") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::Do)
    };
    ("break") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::Break)
    };
    ("continue") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::Continue)
    };
    ("for") => {
        $crate::grammar::TokenType::Static($crate::grammar::StaticToken::For)
    };
}
