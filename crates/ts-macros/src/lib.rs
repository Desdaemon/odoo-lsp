use std::collections::{hash_map::Entry, HashMap};

use heck::ToUpperCamelCase;
use proc_macro2::{Delimiter, Literal, TokenStream, TokenTree};
use proc_macro2_diagnostics::SpanDiagnosticExt;
use quote::{quote, quote_spanned, ToTokens};
use syn::{parse::Parse, punctuated::Punctuated, *};

/// Define a [tree-sitter query], optionally extracting its named captures into an enum.
///
/// *Usage:*
/// ```rust,noplayground
/// ts_macros::query! {
///     MyQuery(Foo, Bar);
/// (function_definition
///  (parameters . (string) @FOO)
///  (block
///    (expression_statement
///      (call
///        (_) @callee
///        (parameters . (string) @BAR)))))
/// };
/// ```
///
/// Generates:
/// ```rust,noplayground
/// pub enum MyQuery {
///     Foo = 0,
///     Bar = 2,
/// }
/// impl MyQuery {
///     pub fn query() -> &'static Query;
///     pub fn from(raw: u32) -> Option<Self>;
/// }
/// ```
/// [tree-sitter query]: https://tree-sitter.github.io/tree-sitter/using-parsers#query-syntax
#[proc_macro]
pub fn query(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let def = syn::parse_macro_input!(tokens as QueryDefinition);
	def.into_tokens(TsLang::Python).into()
}

#[proc_macro]
#[deprecated = "Use `query` with #[lang = \"..\"] instead"]
pub fn query_js(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let def = syn::parse_macro_input!(tokens as QueryDefinition);
	def.into_tokens(TsLang::Javascript).into()
}

struct QueryDefinition {
	meta: Vec<TokenStream>,
	lang: Option<syn::LitStr>,
	name: syn::Ident,
	captures: Punctuated<Ident, Token![,]>,
	query: TokenStream,
}

mod kw {
	syn::custom_keyword!(lang);
}

impl Parse for QueryDefinition {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		let mut meta = vec![];
		let mut lang = None;
		while input.peek(Token![#]) {
			input.parse::<Token![#]>()?;
			let content;
			bracketed!(content in input);
			if content.peek(kw::lang) {
				content.parse::<kw::lang>()?;
				content.parse::<Token![=]>()?;
				lang = Some(content.parse()?);
				continue;
			}
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
			lang,
		})
	}
}

enum TsLang {
	Python,
	Javascript,
	Custom(syn::LitStr),
}

impl ToTokens for TsLang {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		match self {
			Self::Python => tokens.extend(quote!(::tree_sitter_python)),
			Self::Javascript => tokens.extend(quote!(::tree_sitter_javascript)),
			Self::Custom(lang) => match syn::parse_str::<syn::Path>(&lang.value()) {
				Ok(lang) => tokens.extend(quote!(#lang)),
				Err(err) => {
					let report = err.to_compile_error();
					tokens.extend(quote_spanned!(lang.span() => #report))
				}
			},
		}
	}
}

fn tokens_to_string(tokens: TokenStream, output: &mut String) {
	let mut tokens = tokens.into_iter().peekable();
	while let Some(token) = tokens.next() {
		match token {
			TokenTree::Group(group) => {
				let (lhs, rhs) = match group.delimiter() {
					Delimiter::Parenthesis => ("(", ")"),
					Delimiter::Brace => ("{", "}"),
					Delimiter::Bracket => ("[", "]"),
					Delimiter::None => (" ", " "),
				};
				output.push_str(lhs);
				tokens_to_string(group.stream(), output);
				output.push_str(rhs);
			}
			// if an identifier follows any of these punctuations, we must not add a space.
			TokenTree::Punct(punct) if matches!(punct.as_char(), '@' | '#') => {
				output.push(' ');
				output.push(punct.as_char());
				let Some(TokenTree::Ident(ident)) = tokens.peek() else {
					continue;
				};
				output.push_str(&ident.to_string());
				tokens.next();
				let mut ident_allowed = false;
				loop {
					// A dash is part of a valid Scheme identifier, so it allows at most one more identifier.
					// Any other punctuation (usually ! or ?) marks the end of the identifier.
					match tokens.peek() {
						Some(TokenTree::Punct(punct)) => {
							let punct = punct.as_char();
							output.push(punct);
							tokens.next();
							if punct != '-' {
								break;
							}
							ident_allowed = true;
						}
						Some(TokenTree::Ident(ident)) if ident_allowed => {
							output.push_str(&ident.to_string());
							tokens.next();
							ident_allowed = false;
						}
						_ => break,
					}
				}
			}
			_ => {
				output.push(' ');
				output.push_str(&token.to_string());
			}
		}
	}
}

impl QueryDefinition {
	fn into_tokens(self, language: TsLang) -> TokenStream {
		let language = self.lang.map(TsLang::Custom).unwrap_or(language);
		let mut captures = HashMap::new();
		let mut diagnostics = Vec::new();
		let mut index = 0u32;
		let mut tokens = self.query.clone().into_iter();
		let mut expect_capture = false;
		while let Some(token) = tokens.next() {
			match token {
				TokenTree::Punct(punct) if punct.as_char() == '@' => {
					expect_capture = true;
				}
				TokenTree::Ident(capture) if expect_capture => {
					expect_capture = false;
					let capture = quote!(#capture).to_string();
					let key = if capture.starts_with('_') {
						capture
					} else {
						capture.to_upper_camel_case()
					};
					if let Entry::Vacant(entry) = captures.entry(key) {
						entry.insert(index);
						index += 1;
					}
				}
				TokenTree::Group(group) => {
					tokens = group
						.stream()
						.into_iter()
						.chain(tokens)
						.collect::<TokenStream>()
						.into_iter();
				}
				_ => {}
			}
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
		let mut query = String::new();
		tokens_to_string(self.query, &mut query);
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
						::tree_sitter::Query::new(&#language::LANGUAGE.into(), #query).unwrap()
					})
				}
			}
			#(#diagnostics)*
		)
	}
}
