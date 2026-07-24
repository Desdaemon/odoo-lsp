//! Schema-based (RELAX NG) XML assistance, editor-agnostic.
//!
//! Historically the RNG-backed experience (element/attribute completion and
//! documentation) was only available in VSCode via the RedHat XML extension,
//! which was pointed at the bundled [`odoo.rng`] by the client. This module
//! brings the same schema knowledge into the language server itself so every
//! editor benefits, without touching parsing, indexing or diagnostics.
//!
//! The [`odoo.rng`] grammar is embedded at compile time and lazily compiled
//! into a flat `element name -> {children, attributes}` lookup. Completions and
//! hovers are offered purely as a *fallback*: the semantic analyzer in
//! [`super`] always gets the first shot, and this only runs where the cursor is
//! on an element/attribute *name* (which the analyzer, being value-oriented,
//! never handles).
//!
//! [`odoo.rng`]: ../../odoo.rng

use std::collections::{BTreeMap, BTreeSet};
use std::ops::Range;
use std::sync::LazyLock;

use xmlparser::{ElementEnd, Token, Tokenizer};

/// The bundled Odoo RELAX NG grammar, embedded at compile time.
const ODOO_RNG: &str = include_str!("../../odoo.rng");

/// A single completion candidate derived from the schema.
pub struct SchemaItem {
	pub label: String,
	pub kind: SchemaKind,
	/// Cleaned-up `<a:documentation>` text, if any.
	pub doc: Option<String>,
	/// Text to insert, possibly an LSP snippet (see [`SchemaItem::snippet`]).
	pub insert: String,
	pub snippet: bool,
	/// Byte range within the analyzed slice that the insertion replaces.
	pub range: Range<usize>,
}

pub enum SchemaKind {
	Element,
	Attribute,
	Value,
}

#[derive(Default)]
struct AttrInfo {
	doc: Option<String>,
	/// Enumerated `<value>` options seen for this attribute.
	values: Vec<String>,
	/// Whether the grammar always requires this attribute — i.e. every path to
	/// it avoids `<optional>`, `<zeroOrMore>` and `<choice>`. Conservative: if
	/// any occurrence is reachable without being mandatory, this stays `false`.
	required: bool,
	/// Whether the value space is open-ended (some occurrence admits free-form
	/// `<text>`/`<data>`, or declares no `<value>` at all). Only a *closed*
	/// enumeration — `!open` with a non-empty [`AttrInfo::values`] — can be
	/// validated, to avoid false positives.
	open: bool,
}

#[derive(Default)]
struct ElemInfo {
	doc: Option<String>,
	children: BTreeSet<&'static str>,
	attrs: BTreeMap<&'static str, AttrInfo>,
	/// Whether the content model admits arbitrary child elements (an
	/// `<element>` with an `<anyName>` wildcard). When set, the [`children`]
	/// list is not exhaustive and disallowed-child checks are suppressed.
	///
	/// [`children`]: ElemInfo::children
	open_children: bool,
}

struct Schema {
	/// Element name -> its allowed children and attributes.
	elements: BTreeMap<&'static str, ElemInfo>,
	/// Element names allowed at the document root (`<start>`).
	roots: BTreeSet<&'static str>,
}

/// A minimal DOM node built from the RNG source. Text runs are flattened into
/// [`Node::text`]; the exact interleaving with children is irrelevant for the
/// documentation/value extraction we need.
struct Node {
	tag: &'static str,
	attrs: Vec<(&'static str, &'static str)>,
	children: Vec<usize>,
	text: String,
}

fn attr(node: &Node, name: &str) -> Option<&'static str> {
	node.attrs.iter().find(|(k, _)| *k == name).map(|(_, v)| *v)
}

/// Parse the (well-formed, trusted) RNG into a flat arena of [`Node`]s.
fn parse_dom(src: &'static str) -> Vec<Node> {
	let mut nodes: Vec<Node> = Vec::new();
	let mut stack: Vec<usize> = Vec::new();
	for token in Tokenizer::from(src) {
		let Ok(token) = token else { break };
		match token {
			Token::ElementStart { local, .. } => {
				let idx = nodes.len();
				nodes.push(Node {
					tag: local.as_str(),
					attrs: Vec::new(),
					children: Vec::new(),
					text: String::new(),
				});
				if let Some(&parent) = stack.last() {
					nodes[parent].children.push(idx);
				}
				stack.push(idx);
			}
			Token::Attribute { local, value, .. } => {
				if let Some(&idx) = stack.last() {
					nodes[idx].attrs.push((local.as_str(), value.as_str()));
				}
			}
			Token::ElementEnd { end, .. } => match end {
				// `<foo>`: the element stays open, children follow.
				ElementEnd::Open => {}
				// `</foo>` or `<foo/>`: the current element is done.
				ElementEnd::Close(..) | ElementEnd::Empty => {
					stack.pop();
				}
			},
			Token::Text { text } | Token::Cdata { text, .. } => {
				if let Some(&idx) = stack.last() {
					nodes[idx].text.push_str(text.as_str());
				}
			}
			_ => {}
		}
	}
	nodes
}

/// Collect all descendant text of a node, mapping `<br/>` to newlines. Used to
/// pull `<a:documentation>` prose out of the DOM.
fn collect_text(nodes: &[Node], idx: usize, out: &mut String) {
	out.push_str(&nodes[idx].text);
	for &child in &nodes[idx].children {
		if nodes[child].tag == "br" {
			out.push('\n');
		}
		collect_text(nodes, child, out);
	}
}

/// Normalize doc prose: unescape the handful of entities used in the grammar,
/// trim each line and drop blank padding.
fn clean_doc(raw: &str) -> Option<String> {
	let unescaped = raw
		.replace("&lt;", "<")
		.replace("&gt;", ">")
		.replace("&quot;", "\"")
		.replace("&apos;", "'")
		.replace("&amp;", "&");
	let mut lines: Vec<&str> = unescaped.lines().map(str::trim).collect();
	while lines.first().is_some_and(|l| l.is_empty()) {
		lines.remove(0);
	}
	while lines.last().is_some_and(|l| l.is_empty()) {
		lines.pop();
	}
	let joined = lines.join("\n");
	(!joined.is_empty()).then_some(joined)
}

/// The `<a:documentation>` directly under `node`, if present.
fn direct_doc(nodes: &[Node], idx: usize) -> Option<String> {
	let child = nodes[idx]
		.children
		.iter()
		.copied()
		.find(|&c| nodes[c].tag == "documentation")?;
	let mut raw = String::new();
	collect_text(nodes, child, &mut raw);
	clean_doc(&raw)
}

/// Collect enumerated `<value>` options anywhere under an `<attribute>`, and set
/// `free` if the value space also admits free-form input (`<text>`/`<data>`, or
/// an `<anyName>` wildcard) — i.e. it is *not* a closed enumeration.
fn scan_attr_values(nodes: &[Node], idx: usize, out: &mut Vec<String>, free: &mut bool) {
	for &child in &nodes[idx].children {
		match nodes[child].tag {
			"value" => {
				let v = nodes[child].text.trim();
				if !v.is_empty() {
					out.push(v.to_string());
				}
			}
			// Anything that lets an arbitrary string through opens the enum.
			"text" | "data" | "anyName" => *free = true,
			// Recurse through pattern combinators, but never into a nested
			// element (attributes cannot contain elements anyway).
			"choice" | "group" | "optional" | "oneOrMore" | "zeroOrMore" | "interleave" | "mixed" => {
				scan_attr_values(nodes, child, out, free)
			}
			_ => {}
		}
	}
}

/// Walk an element's content model, expanding `<ref>`s, accumulating the
/// allowed child element names and attributes into `info`. Nested `<element>`s
/// contribute only their *name* — they are compiled independently by the
/// top-level pass over every element node.
fn scan_content(
	nodes: &[Node],
	idx: usize,
	info: &mut ElemInfo,
	defines: &BTreeMap<&'static str, usize>,
	visited: &mut BTreeSet<&'static str>,
	// Whether attributes reached here are mandatory. Cleared once we descend
	// through anything that makes its contents conditional.
	required: bool,
) {
	for &child in &nodes[idx].children {
		match nodes[child].tag {
			"element" => {
				if let Some(name) = attr(&nodes[child], "name") {
					info.children.insert(name);
				} else {
					// `<element><anyName/>` — the content model admits arbitrary
					// children, so its child list can't be treated as exhaustive.
					info.open_children = true;
				}
			}
			"attribute" => {
				if let Some(name) = attr(&nodes[child], "name") {
					let existed = info.attrs.contains_key(name);
					let mut values = Vec::new();
					let mut free = false;
					scan_attr_values(nodes, child, &mut values, &mut free);
					// An occurrence with no enumerated values places no closed
					// restriction on the attribute either.
					let occurrence_open = free || values.is_empty();
					let entry = info.attrs.entry(name).or_default();
					if entry.doc.is_none() {
						entry.doc = direct_doc(nodes, child);
					}
					// The same attribute is often reached through several refs (and
					// several element definitions merge into one entry), so dedup
					// while preserving first-seen order.
					for value in values {
						if !entry.values.contains(&value) {
							entry.values.push(value);
						}
					}
					entry.open = entry.open || occurrence_open;
					// Required only if it is mandatory on *every* path that
					// reaches it.
					entry.required = if existed { entry.required && required } else { required };
				}
			}
			"ref" => {
				if let Some(name) = attr(&nodes[child], "name")
					&& let Some(&target) = defines.get(name)
					&& visited.insert(name)
				{
					scan_content(nodes, target, info, defines, visited, required);
				}
			}
			// `<optional>`, `<zeroOrMore>` and `<choice>` make their contents
			// conditional, so attributes below them are never mandatory.
			"optional" | "zeroOrMore" | "choice" => scan_content(nodes, child, info, defines, visited, false),
			"group" | "oneOrMore" | "interleave" | "mixed" => {
				scan_content(nodes, child, info, defines, visited, required)
			}
			_ => {}
		}
	}
}

fn build_schema() -> Schema {
	let nodes = parse_dom(ODOO_RNG);

	// Index all named `<define>`s so `<ref>`s can be expanded.
	let mut defines: BTreeMap<&'static str, usize> = BTreeMap::new();
	for (idx, node) in nodes.iter().enumerate() {
		if node.tag == "define"
			&& let Some(name) = attr(node, "name")
		{
			defines.insert(name, idx);
		}
	}

	// Compile every `<element name=...>` occurrence into a flat table. When a
	// name appears in multiple contexts we take the union, which errs towards
	// offering more completions — the right bias for a non-intrusive fallback.
	let mut elements: BTreeMap<&'static str, ElemInfo> = BTreeMap::new();
	for (idx, node) in nodes.iter().enumerate() {
		if node.tag != "element" {
			continue;
		}
		let Some(name) = attr(node, "name") else { continue };
		let mut info = std::mem::take(elements.entry(name).or_default());
		if info.doc.is_none() {
			info.doc = direct_doc(&nodes, idx);
		}
		let mut visited = BTreeSet::new();
		scan_content(&nodes, idx, &mut info, &defines, &mut visited, true);
		elements.insert(name, info);
	}

	// Root elements come from `<start>`.
	let mut roots = BTreeSet::new();
	if let Some(start) = nodes.iter().position(|n| n.tag == "start") {
		let mut info = ElemInfo::default();
		let mut visited = BTreeSet::new();
		scan_content(&nodes, start, &mut info, &defines, &mut visited, false);
		// `<start>` references top-level elements directly, so their names land
		// in `children`.
		roots = info.children;
	}

	Schema { elements, roots }
}

static SCHEMA: LazyLock<Schema> = LazyLock::new(build_schema);

/// The attribute names the grammar always requires on `element`. Empty when
/// the element is unknown or has no mandatory attributes.
pub fn required_attributes(element: &str) -> impl Iterator<Item = &'static str> {
	SCHEMA
		.elements
		.get(&element)
		.into_iter()
		.flat_map(|info| info.attrs.iter().filter_map(|(name, ai)| ai.required.then_some(*name)))
}

/// Whether `name` is not a defined element anywhere in the grammar. Used to
/// keep disallowed-child diagnostics conservative: only elements the schema has
/// never heard of (typically typos) are flagged, never a known element that is
/// merely in an unexpected place.
pub fn is_unknown_element(name: &str) -> bool {
	!SCHEMA.elements.contains_key(name)
}

/// Whether `child` is a permitted child element of `parent` per the grammar.
/// Returns `None` when this can't be judged — the parent is unknown, or its
/// content model admits arbitrary children — so callers should not flag it.
pub fn is_allowed_child(parent: &str, child: &str) -> Option<bool> {
	let info = SCHEMA.elements.get(parent)?;
	if info.open_children {
		return None;
	}
	Some(info.children.contains(child))
}

/// The closed set of values `attr` may take on `element`, if any. Returns `None`
/// unless the grammar restricts the attribute to a fixed enumeration (no
/// free-form `<text>`/`<data>` escape hatch), so open-ended attributes are never
/// flagged.
pub fn enum_values(element: &str, attr: &str) -> Option<&'static [String]> {
	let ai = SCHEMA.elements.get(element)?.attrs.get(attr)?;
	(!ai.open && !ai.values.is_empty()).then_some(ai.values.as_slice())
}

/// Where the cursor sits, expressed in schema terms.
enum Ctx<'a> {
	/// Typing an element name after `<`; `parent` is the enclosing element.
	Child {
		parent: Option<&'a str>,
		range: Range<usize>,
	},
	/// Typing an element name inside a closing tag `</...`; `open` is the
	/// innermost still-open element that this tag would close.
	Close { open: Option<&'a str>, range: Range<usize> },
	/// Typing an attribute name inside `<elem ...>`.
	Attr { elem: &'a str, range: Range<usize> },
	/// Typing (or about to type) an attribute value.
	Value {
		elem: &'a str,
		attr: &'a str,
		range: Range<usize>,
	},
}

/// Scan the tag `<...` starting at `tag_start` and determine the in-tag context
/// at `cursor`. Returns `None` for positions we don't complete (e.g. inside a
/// closing tag).
fn analyze_in_tag(text: &str, tag_start: usize, cursor: usize) -> Option<Ctx<'_>> {
	let bytes = text.as_bytes();
	// Declarations, processing instructions and comments are never completed.
	if bytes.get(tag_start + 1).is_some_and(|&c| c == b'!' || c == b'?') {
		return None;
	}
	// A closing tag `</name`: offer the element it closes. The actual name is
	// resolved by the caller from the open-element stack.
	if bytes.get(tag_start + 1) == Some(&b'/') {
		let name_start = tag_start + 2;
		let mut name_end = name_start;
		while name_end < text.len() && !matches!(bytes[name_end], b' ' | b'\t' | b'\r' | b'\n' | b'/' | b'>') {
			name_end += 1;
		}
		return Some(Ctx::Close {
			open: None,
			range: name_start..name_end,
		});
	}
	let name_start = tag_start + 1;
	// Element name runs until whitespace or a tag terminator.
	let name_end = {
		let mut i = name_start;
		while i < text.len() && !matches!(bytes[i], b' ' | b'\t' | b'\r' | b'\n' | b'/' | b'>') {
			i += 1;
		}
		i
	};
	let elem = &text[name_start..name_end];

	// Cursor still within the element-name token → element-name completion.
	if cursor <= name_end {
		return Some(Ctx::Child {
			parent: None, // resolved by caller from the tag stack
			range: name_start..name_end,
		});
	}

	// Otherwise walk the attribute region up to the cursor.
	#[derive(Clone, Copy)]
	enum St<'a> {
		Ws,
		Name(usize),
		AfterName(&'a str),
		AfterEq(&'a str),
		InVal(&'a str, u8, usize),
	}
	let mut st = St::Ws;
	let mut i = name_end;
	while i < cursor {
		let c = bytes[i];
		let ws = matches!(c, b' ' | b'\t' | b'\r' | b'\n');
		st = match st {
			St::Ws => {
				if ws || c == b'/' || c == b'>' {
					St::Ws
				} else {
					St::Name(i)
				}
			}
			St::Name(s) => {
				if c == b'=' {
					St::AfterEq(&text[s..i])
				} else if ws {
					St::AfterName(&text[s..i])
				} else {
					St::Name(s)
				}
			}
			St::AfterName(nm) => {
				if c == b'=' {
					St::AfterEq(nm)
				} else if ws {
					St::AfterName(nm)
				} else {
					St::Name(i)
				}
			}
			St::AfterEq(nm) => {
				if c == b'"' || c == b'\'' {
					St::InVal(nm, c, i + 1)
				} else {
					St::AfterEq(nm)
				}
			}
			St::InVal(nm, q, vs) => {
				if c == q {
					St::Ws
				} else {
					St::InVal(nm, q, vs)
				}
			}
		};
		i += 1;
	}

	match st {
		St::Name(s) => Some(Ctx::Attr { elem, range: s..cursor }),
		St::Ws | St::AfterName(_) => Some(Ctx::Attr {
			elem,
			range: cursor..cursor,
		}),
		St::AfterEq(attr) => Some(Ctx::Value {
			elem,
			attr,
			range: cursor..cursor,
		}),
		St::InVal(attr, quote, vs) => {
			// Value ends at the closing quote (if any before the tag closes).
			let mut end = cursor;
			while end < text.len() && bytes[end] != quote && bytes[end] != b'>' {
				end += 1;
			}
			if bytes.get(end) != Some(&quote) {
				end = cursor;
			}
			Some(Ctx::Value {
				elem,
				attr,
				range: vs..end,
			})
		}
	}
}

/// Determine the schema context at `cursor` by scanning from the start of the
/// slice, tracking the stack of open elements.
fn analyze(text: &str, cursor: usize) -> Option<Ctx<'_>> {
	let cursor = cursor.min(text.len());
	let bytes = text.as_bytes();
	let mut stack: Vec<&str> = Vec::new();
	let mut i = 0;
	while i < cursor {
		if bytes[i] != b'<' {
			i += 1;
			continue;
		}
		// Comments and CDATA.
		if text[i..].starts_with("<!--") {
			i = text[i..].find("-->").map(|p| i + p + 3).unwrap_or(cursor);
			continue;
		}
		if text[i..].starts_with("<![CDATA[") {
			i = text[i..].find("]]>").map(|p| p + i + 3).unwrap_or(cursor);
			continue;
		}
		let next = bytes.get(i + 1).copied();
		// Declarations / processing instructions / doctype: skip to `>`.
		if next == Some(b'!') || next == Some(b'?') {
			i = text[i..].find('>').map(|p| i + p + 1).unwrap_or(cursor);
			continue;
		}
		// Locate this tag's terminating `>` while respecting quoted values.
		let mut j = i + 1;
		let mut quote = 0u8;
		while j < text.len() {
			let c = bytes[j];
			if quote != 0 {
				if c == quote {
					quote = 0;
				}
			} else if c == b'"' || c == b'\'' {
				quote = c;
			} else if c == b'>' {
				break;
			}
			j += 1;
		}
		let tag_close = j; // index of `>` or text.len()

		if cursor <= tag_close {
			// Cursor is inside this (possibly unterminated) tag.
			let ctx = analyze_in_tag(text, i, cursor)?;
			return Some(match ctx {
				Ctx::Child { range, .. } => Ctx::Child {
					parent: stack.last().copied(),
					range,
				},
				Ctx::Close { range, .. } => Ctx::Close {
					open: stack.last().copied(),
					range,
				},
				other => other,
			});
		}

		// Tag fully precedes the cursor: update the element stack.
		if next == Some(b'/') {
			stack.pop();
		} else if bytes.get(tag_close.wrapping_sub(1)) != Some(&b'/') {
			// Not self-closing: push the element name.
			let name_start = i + 1;
			let mut e = name_start;
			while e < tag_close && !matches!(bytes[e], b' ' | b'\t' | b'\r' | b'\n' | b'/' | b'>') {
				e += 1;
			}
			stack.push(&text[name_start..e]);
		}
		i = tag_close + 1;
	}
	// Cursor reached in text content (not immediately after `<`): nothing to
	// offer — let the caller's snippet fallback handle it.
	None
}

/// Compute schema-based completions for `cursor` within `text`. Returns an
/// empty vec when the cursor isn't on a name the grammar knows about.
pub fn completions(text: &str, cursor: usize) -> Vec<SchemaItem> {
	let schema = &*SCHEMA;
	let Some(ctx) = analyze(text, cursor) else {
		return Vec::new();
	};
	let mut out = Vec::new();
	match ctx {
		Ctx::Child { parent, range } => {
			let prefix = &text[range.start..cursor.min(range.end).max(range.start)];
			let names: Box<dyn Iterator<Item = &&str>> = match parent {
				Some(p) => match schema.elements.get(p) {
					Some(info) => Box::new(info.children.iter()),
					None => return out,
				},
				None => Box::new(schema.roots.iter()),
			};
			for &name in names {
				if !name.starts_with(prefix) {
					continue;
				}
				out.push(SchemaItem {
					label: name.to_string(),
					kind: SchemaKind::Element,
					doc: schema.elements.get(name).and_then(|e| e.doc.clone()),
					insert: name.to_string(),
					snippet: false,
					range: range.clone(),
				});
			}
		}
		Ctx::Close { open, range } => {
			let Some(name) = open else { return out };
			let prefix = &text[range.start..cursor.min(range.end).max(range.start)];
			if name.starts_with(prefix) {
				out.push(SchemaItem {
					label: name.to_string(),
					kind: SchemaKind::Element,
					// doc: schema.elements.get(name).and_then(|e| e.doc.clone()),
					doc: None,
					insert: name.to_string(),
					snippet: false,
					range,
				});
			}
		}
		Ctx::Attr { elem, range } => {
			let Some(info) = schema.elements.get(elem) else {
				return out;
			};
			let prefix = &text[range.start..cursor.min(range.end).max(range.start)];
			for (name, ai) in &info.attrs {
				if !name.starts_with(prefix) {
					continue;
				}
				out.push(SchemaItem {
					label: name.to_string(),
					kind: SchemaKind::Attribute,
					doc: ai.doc.clone(),
					insert: format!("{name}=\"$1\""),
					snippet: true,
					range: range.clone(),
				});
			}
		}
		Ctx::Value { elem, attr, range } => {
			let Some(info) = schema.elements.get(elem) else {
				return out;
			};
			let Some(ai) = info.attrs.get(attr) else { return out };
			let prefix = &text[range.start..cursor.min(range.end).max(range.start)];
			for value in &ai.values {
				if !value.starts_with(prefix) {
					continue;
				}
				out.push(SchemaItem {
					label: value.clone(),
					kind: SchemaKind::Value,
					doc: None,
					insert: value.clone(),
					snippet: false,
					range: range.clone(),
				});
			}
		}
	}
	out
}

/// Documentation for the element/attribute name under `cursor`, with the byte
/// range of the hovered token.
pub fn hover(text: &str, cursor: usize) -> Option<(String, Range<usize>)> {
	let cursor = cursor.min(text.len());
	let bytes = text.as_bytes();
	// Find the `<` that opens the tag containing the cursor (no intervening `>`).
	let mut tag_start = None;
	let mut k = cursor.min(text.len().saturating_sub(1));
	loop {
		match bytes.get(k) {
			Some(b'>') if k < cursor => break,
			Some(b'<') => {
				tag_start = Some(k);
				break;
			}
			_ => {}
		}
		if k == 0 {
			break;
		}
		k -= 1;
	}
	let tag_start = tag_start?;
	let is_end = bytes.get(tag_start + 1) == Some(&b'/');
	if bytes.get(tag_start + 1).is_some_and(|&c| c == b'!' || c == b'?') {
		return None;
	}
	let name_start = if is_end { tag_start + 2 } else { tag_start + 1 };
	let name_end = {
		let mut i = name_start;
		while i < text.len() && !matches!(bytes[i], b' ' | b'\t' | b'\r' | b'\n' | b'/' | b'>') {
			i += 1;
		}
		i
	};
	let elem = text.get(name_start..name_end)?;
	let schema = &*SCHEMA;
	let info = schema.elements.get(elem)?;

	// Hovering the element name itself.
	if cursor >= name_start && cursor <= name_end {
		return info.doc.clone().map(|doc| (doc, name_start..name_end));
	}
	if is_end {
		return None;
	}

	// Otherwise find the attribute-name token under the cursor.
	let mut i = name_end;
	while i < text.len() {
		let c = bytes[i];
		if c == b'>' {
			break;
		}
		if matches!(c, b' ' | b'\t' | b'\r' | b'\n' | b'/' | b'=') {
			i += 1;
			continue;
		}
		if c == b'"' || c == b'\'' {
			// Skip a quoted value.
			i += 1;
			while i < text.len() && bytes[i] != c {
				i += 1;
			}
			i += 1;
			continue;
		}
		// Start of an attribute name token.
		let start = i;
		while i < text.len() && !matches!(bytes[i], b' ' | b'\t' | b'\r' | b'\n' | b'/' | b'>' | b'=') {
			i += 1;
		}
		if cursor >= start && cursor <= i {
			let name = &text[start..i];
			return info
				.attrs
				.get(name)
				.and_then(|ai| ai.doc.clone())
				.map(|doc| (doc, start..i));
		}
	}
	None
}

#[cfg(test)]
mod tests;
