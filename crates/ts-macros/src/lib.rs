use std::collections::HashSet;

use proc_macro2::{Ident, TokenStream};
use proc_macro2_diagnostics::SpanDiagnosticExt;
use quote::quote_spanned;
use syn::{parse::Parse, punctuated::Punctuated, *};

/// Usage:
/// ```rust,noplayground
/// ts_macros::query! {
/// 	MyQuery(FOO, _, BAR);
/// 	r#"
/// (function_definition
///   (parameters . (string) @FOO)
///   (block
/// 	(expression_statement
/// 	  (call
/// 		(_) @callee
/// 		(parameters . (string) @BAR)))))"#
/// };
/// ```
///
/// Generates:
/// ```rust,noplayground
/// pub enum MyQuery {}
/// impl MyQuery {
/// 	pub const FOO: u32 = 0;
/// 	pub const BAR: u32 = 2;
/// 	pub fn query() -> &'static Query;
/// }
/// ```
#[proc_macro]
pub fn query(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let def = syn::parse_macro_input!(tokens as QueryDefinition);
	def.into_tokens().into()
}

struct QueryDefinition {
	name: syn::Ident,
	captures: Punctuated<Capture, Token![,]>,
	query: syn::LitStr,
}

enum Capture {
	Ignored,
	Name(Ident),
}

impl Parse for QueryDefinition {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		let name = input.parse()?;
		let contents;
		parenthesized!(contents in input);
		let captures = Punctuated::parse_terminated(&contents)?;
		_ = input.parse::<Token![;]>();
		let template = input.parse()?;
		Ok(Self {
			name,
			captures,
			query: template,
		})
	}
}

impl Parse for Capture {
	fn parse(input: parse::ParseStream) -> Result<Self> {
		let lh = input.lookahead1();
		if lh.peek(Ident) {
			let ident: Ident = input.parse()?;
			if ident.to_string().starts_with('_') {
				Ok(Capture::Ignored)
			} else {
				Ok(Capture::Name(ident))
			}
		} else if lh.peek(Token![_]) {
			input.parse::<Token![_]>().map(|_| Capture::Ignored)
		} else {
			Err(lh.error())
		}
	}
}

impl QueryDefinition {
	fn into_tokens(self) -> TokenStream {
		let query = self.query.value();
		let mut query = query.as_str();
		let mut captures = HashSet::new();
		let mut const_stack = self.captures.iter();
		let mut diagnostics = Vec::new();
		while let Some(mut start) = query.find('@') {
			start += 1;
			if start >= query.len() {
				diagnostics.push(self.query.span().error("Unexpected end of query"));
				break;
			}
			let end = query[start..]
				.find(|c: char| !c.is_ascii_alphanumeric() && c != '_')
				.unwrap_or(query.len() - start);
			let capture = &query[start..start + end];
			if captures.insert(capture) {
				let Some(const_) = const_stack.next() else {
					diagnostics.push(self.query.span().error(format!("Not enough captures: @{capture}")));
					break;
				};
				if let Capture::Name(name) = const_ {
					if capture != name.to_string() {
						diagnostics.push(name.span().error(format!("Expected '{capture}', got '{name}'")));
					}
				}
			}
			query = &query[start + end..];
		}
		let consts = self.captures.iter().enumerate().filter_map(|(idx, capture)| {
			let idx = idx as u32;
			if let Capture::Name(name) = capture {
				Some(quote_spanned!(name.span()=> pub const #name: u32 = #idx;))
			} else {
				None
			}
		});
		let name = self.name;
		let query = self.query;
		let diagnostics = diagnostics.into_iter().map(|diag| diag.emit_as_item_tokens());
		quote_spanned!(name.span()=>
			pub enum #name {}
			impl #name {
				#(#consts)*
				pub fn query() -> &'static ::tree_sitter::Query {
					use ::std::sync::OnceLock as _OnceLock;
					static QUERY: _OnceLock<::tree_sitter::Query> = _OnceLock::new();
					QUERY.get_or_init(|| {
						::tree_sitter::Query::new(::tree_sitter_python::language(), #query).unwrap()
					})
				}
			}
			#(#diagnostics)*
		)
	}
}
