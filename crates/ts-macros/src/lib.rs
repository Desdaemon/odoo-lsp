use std::{
	borrow::Cow,
	collections::{hash_map::Entry, HashMap},
};

use heck::ToUpperCamelCase;
use proc_macro2::{Ident, Literal, TokenStream};
use proc_macro2_diagnostics::SpanDiagnosticExt;
use quote::{quote, quote_spanned, ToTokens};
use syn::{parse::Parse, punctuated::Punctuated, *};

/// Define a [tree-sitter query], optionally extracting its named captures into an enum.
/// 
/// *Usage:*
/// ```rust,noplayground
/// ts_macros::query! {
///     MyQuery(Foo, Bar);
///     r#"
/// (function_definition
///   (parameters . (string) @FOO)
///   (block
///     (expression_statement
///       (call
///         (_) @callee
///         (parameters . (string) @BAR)))))"#
/// };
/// ```
///
/// Generates:
/// ```rust,noplayground
/// pub enum MyQuery {
/// 	Foo = 0,
/// 	Bar = 2,
/// }
/// impl MyQuery {
///     pub fn query() -> &'static Query;
/// 	pub fn from(raw: u32) -> Option<Self>;
/// }
/// ```
/// [tree-sitter query]: https://tree-sitter.github.io/tree-sitter/using-parsers#query-syntax
#[proc_macro]
pub fn query(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let def = syn::parse_macro_input!(tokens as QueryDefinition);
	def.into_tokens(TsLang::Python).into()
}

/// See [query!] for usage info
#[proc_macro]
pub fn query_js(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let def = syn::parse_macro_input!(tokens as QueryDefinition);
	def.into_tokens(TsLang::Javascript).into()
}

struct QueryDefinition {
	meta: Vec<TokenStream>,
	name: syn::Ident,
	captures: Punctuated<Ident, Token![,]>,
	query: syn::LitStr,
}

impl Parse for QueryDefinition {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		let mut meta = vec![];
		while input.peek(Token![#]) {
			input.parse::<Token![#]>()?;
			let content;
			bracketed!(content in input);
			meta.push(content.parse()?);
		}
		let name = input.parse()?;
		let contents;
		parenthesized!(contents in input);
		let captures = Punctuated::parse_terminated(&contents)?;
		input.parse::<Token![;]>()?;
		let template = input.parse()?;
		Ok(Self {
			name,
			meta,
			captures,
			query: template,
		})
	}
}

enum TsLang {
	Python,
	Javascript,
}

impl ToTokens for TsLang {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		match self {
			Self::Python => tokens.extend(quote!(::tree_sitter_python::language())),
			Self::Javascript => tokens.extend(quote!(::tree_sitter_javascript::language())),
		}
	}
}

impl QueryDefinition {
	fn into_tokens(self, language: TsLang) -> TokenStream {
		let query = self.query.value();
		let mut query = query.as_str();
		let mut captures = HashMap::new();
		let mut diagnostics = Vec::new();
		let mut index = 0u32;
		// TODO: Skip symbols inside quotes
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
			let key = if capture.starts_with('_') {
				Cow::from(capture)
			} else {
				Cow::from(capture.to_upper_camel_case())
			};
			if let Entry::Vacant(entry) = captures.entry(key.into_owned()) {
				entry.insert(index);
				index += 1;
			}
			query = &query[start + end..];
		}
		let mut cases = vec![];
		let mut variants = vec![];
		for capture in self.captures.iter() {
			if let Some(index) = captures.get(capture.to_string().as_str()) {
				let index = Literal::usize_unsuffixed(*index as _);
				cases.push(quote_spanned!(capture.span() => #index => Some(Self::#capture),));
				variants.push(quote_spanned!(capture.span()=> #capture = #index,));
			} else {
				diagnostics.push(capture.span().error(format!("No capture '{capture}' found in query")));
			}
		}
		let name = self.name;
		let query = self.query;
		let meta = self.meta;
		let diagnostics = diagnostics.into_iter().map(|diag| diag.emit_as_item_tokens());
		quote_spanned!(name.span()=>
			#(#[#meta])*
			pub enum #name {
				#(#variants)*
			}

			#[allow(dead_code)]
			impl #name {
				#[inline]
				pub fn from(raw: u32) -> Option<Self> {
					match raw {
						#(#cases)*
						_ => None,
					}
				}
				pub fn query() -> &'static ::tree_sitter::Query {
					use ::std::sync::OnceLock as _OnceLock;
					static QUERY: _OnceLock<::tree_sitter::Query> = _OnceLock::new();
					QUERY.get_or_init(|| {
						::tree_sitter::Query::new(#language, #query).unwrap()
					})
				}
			}
			#(#diagnostics)*
		)
	}
}
