use std::collections::HashMap;

use proc_macro2::{Ident, TokenStream};
use proc_macro2_diagnostics::SpanDiagnosticExt;
use quote::quote_spanned;
use syn::{parse::Parse, punctuated::Punctuated, *};

/// Usage:
/// ```rust,noplayground
/// ts_macros::query! {
/// 	MyQuery(FOO, BAR);
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
	captures: Punctuated<Ident, Token![,]>,
	query: syn::LitStr,
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

impl QueryDefinition {
	fn into_tokens(self) -> TokenStream {
		let query = self.query.value();
		let mut query = query.as_str();
		let mut captures = HashMap::new();
		let mut diagnostics = Vec::new();
		let mut index = 0;
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
			if captures.insert(capture, index).is_none() {
				index += 1;
			}
			query = &query[start + end..];
		}
		let mut consts = vec![];
		for capture in self.captures.iter() {
			if let Some(index) = captures.get(&capture.to_string().as_ref()) {
				let index = *index as u32;
				consts.push(quote_spanned!(capture.span()=> pub const #capture: u32 = #index;))
			} else {
				diagnostics.push(capture.span().error(format!("No capture '{capture}' found in query")));
			}
		}
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
