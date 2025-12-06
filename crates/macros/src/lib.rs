//! Procedural macros for the compiler project.
//!
//! Currently provides:
//!
//! - `statement_enum! { ... }` A function-like macro that:
//!     * re-emits the given statement structs,
//!     * generates a fixed enum `Statement<...>` whose variants wrap those
//!       structs, and
//!     * generates `From<Struct> for Statement` implementations.
//!
//! Usage example:
//!
//! ```ignore
//! use compiler_macros::statement_enum;
//!
//! statement_enum! {
//!     #[derive(Debug)]
//!     pub struct BlockStmt<'a> {
//!         pub statements: Vec<Statement<'a>>,
//!     }
//!
//!     #[derive(Debug)]
//!     pub struct IfStmt<'a> {
//!         pub cond: Expression<'a>,
//!         pub then_block: Box<Statement<'a>>,
//!         pub else_block: Option<Box<Statement<'a>>>,
//!     }
//! }
//! ```
//!
//! This expands roughly to:
//!
//! ```ignore
//! #[derive(Debug)]
//! pub struct BlockStmt<'a> { /* ... */ }
//!
//! #[derive(Debug)]
//! pub struct IfStmt<'a> { /* ... */ }
//!
//! #[derive(Debug, Clone)]
//! pub enum Statement<'a> {
//!     Block(BlockStmt<'a>),
//!     If(IfStmt<'a>),
//! }
//!
//! impl<'a> From<BlockStmt<'a>> for Statement<'a> {
//!     fn from(value: BlockStmt<'a>) -> Self {
//!         Statement::Block(value)
//!     }
//! }
//!
//! impl<'a> From<IfStmt<'a>> for Statement<'a> {
//!     fn from(value: IfStmt<'a>) -> Self {
//!         Statement::If(value)
//!     }
//! }
//! ```
//!
//! Conventions:
//! - The enum name is fixed to `Statement`.
//! - Struct names must end with `Stmt`. The enum variant name is obtained by
//!   stripping the `Stmt` suffix, e.g. `BlockStmt` -> `Block`.

extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Generics, Ident, ItemStruct, Result, Visibility,
    parse::{Parse, ParseStream},
    parse_macro_input,
};

/// Input to the `statement_enum!` macro.
///
/// The expected syntax is:
///
/// ```ignore
/// statement_enum! {
///     #[derive(Debug)]
///     pub struct BlockStmt<'a> { /* fields */ }
///
///     #[derive(Debug)]
///     pub struct IfStmt<'a> { /* fields */ }
/// }
/// ```
///
/// The macro will:
/// - Re-emit all provided structs as-is.
/// - Generate a `Statement` enum whose generics and visibility are taken from
///   the first struct's declaration.
/// - Generate enum variants whose names are derived by stripping the `Stmt`
///   suffix from each struct name, e.g. `BlockStmt` -> `Block`.
/// - Generate `From<Struct> for Statement` implementations for each struct.
struct StatementEnumInput {
    structs: Vec<ItemStruct>,
}

impl Parse for StatementEnumInput {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let mut structs = Vec::new();

        while !input.is_empty() {
            let item: ItemStruct = input.parse()?;
            structs.push(item);
        }

        Ok(Self { structs })
    }
}

/// Function-like macro that defines a set of statement structs, an enum
/// `Statement`, and the corresponding `From` implementations, all in one go.
///
/// Conventions:
/// - The enum name is fixed: `Statement`.
/// - The enum visibility and generics are derived from the first struct. For
///   example, if the first struct is:
///
///   ```ignore
///   pub struct BlockStmt<'a> { .. }
///   ```
///
///   then the generated enum will be:
///
///   ```ignore
///   pub enum Statement<'a> { .. }
///   ```
///
/// - Each struct name must end with `Stmt`. The enum variant name is derived by
///   stripping the `Stmt` suffix:
///     * `BlockStmt`   -> `Statement::Block(BlockStmt<'a>)`
///     * `DoWhileStmt` -> `Statement::DoWhile(DoWhileStmt<'a>)`
#[proc_macro]
pub fn statement_enum(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as StatementEnumInput);

    if input.structs.is_empty() {
        return syn::Error::new(
            proc_macro2::Span::call_site(),
            "statement_enum! expects at least one `struct` item",
        )
        .to_compile_error()
        .into();
    }

    let structs = &input.structs;

    // Use the first struct to derive enum visibility and generics.
    let first = &structs[0];

    let enum_ident = Ident::new("Statement", first.ident.span());
    let enum_vis: &Visibility = &first.vis;
    let enum_generics: &Generics = &first.generics;
    let (_, enum_ty_generics, enum_where_clause) = enum_generics.split_for_impl();

    // Build enum variants and From impls.
    let mut variants = Vec::new();
    let mut from_impls = Vec::new();

    for s in structs {
        let struct_ident: &Ident = &s.ident;
        let struct_name_str = struct_ident.to_string();

        // Struct name must end with `Stmt`.
        let base_name = match struct_name_str.strip_suffix("Stmt") {
            Some(base) if !base.is_empty() => base,
            _ => {
                return syn::Error::new_spanned(
                    struct_ident,
                    "statement_enum!: struct name must end with `Stmt`, \
                     e.g. `BlockStmt` -> enum variant `Block`",
                )
                .to_compile_error()
                .into();
            }
        };

        let variant_ident = Ident::new(base_name, struct_ident.span());

        // Variant: Block(BlockStmt<'a>),
        variants.push(quote! {
            #variant_ident(#struct_ident #enum_ty_generics),
        });

        // Use the struct's own generics for the From impl.
        let s_generics: &Generics = &s.generics;
        let (impl_generics, ty_generics, where_clause) = s_generics.split_for_impl();

        from_impls.push(quote! {
            impl #impl_generics From<#struct_ident #ty_generics>
                for #enum_ident #enum_ty_generics
                #where_clause
            {
                fn from(value: #struct_ident #ty_generics) -> Self {
                    #enum_ident::#variant_ident(value)
                }
            }
        });
    }

    // Re-emit original structs, then generate the enum and From impls.
    let expanded = quote! {
        // Original statement structs
        #(#structs)*

        // Automatically generated statement enum
        #[derive(Debug, Clone)]
        #enum_vis enum #enum_ident #enum_generics #enum_where_clause {
            #(#variants)*
        }

        // From<Struct> for Statement implementations
        #(#from_impls)*
    };

    TokenStream::from(expanded)
}
