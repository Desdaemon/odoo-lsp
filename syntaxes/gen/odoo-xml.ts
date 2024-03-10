#!/usr/bin/env -S deno run
import type { Grammar } from "./schema";

type XmlStringVariantProducer<T> = (
	quote: '"' | "'",
	entityName: "string.quoted.double.xml" | "string.quoted.single.xml",
) => T;

export function makeXmlStringVariants<const T>(func: XmlStringVariantProducer<T>): [T, T] {
	return [func('"', "string.quoted.double.xml"), func("'", "string.quoted.single.xml")];
}

const r = String.raw;

/** Either at the beginning of the line, or any number of whitespace. */
export const leadingWs = r`(?:^|\s+)`;

export const pythonExtrapolationDelimiter = "constant.character.format.placeholder.other.python";

export function defineGrammar<T extends Grammar<R>, R extends string>(grammar: T & Grammar<R>): T {
	return grammar;
}

/** @see https://github.com/microsoft/vscode/blob/main/extensions/xml/syntaxes/xml.tmLanguage.json */
export default function grammar() {
	return defineGrammar({
		scopeName: "text.xml.odoo",
		fileTypes: ["xml"],
		injectionSelector: "L:meta.tag.no-content.xml, L:meta.tag.xml",
		patterns: [
			// FIXME: Until microsoft/TypeScript#57537 lands, simple patterns have to go before generated patterns.
			// https://github.com/microsoft/TypeScript/pull/57537

			// string interpolated values
			...makeXmlStringVariants((quote, name) => ({
				begin: r`${leadingWs}(t-(?:call|valuef|attf-\S+|slot))\s*=(${quote})`,
				end: `(${quote})`,
				contentName: name,
				beginCaptures: {
					1: {
						name: "entity.other.attribute-name.localname.xml",
					},
					2: { name },
				},
				endCaptures: {
					1: { name },
				},
				patterns: [
					{ include: "#jinjaEscape" },
					{ include: "#rubyEscape" },
					{ include: "constant.character.entity.xml" },
				],
			})),
			// opt out of injection
			...makeXmlStringVariants((quote, name) => ({
				begin: r`${leadingWs}(t-(?:name|inherit(?:-mode)?|nocache\S*|set-slot|ref|translation))\s*=(${quote})`,
				end: `(${quote})`,
				contentName: name,
				beginCaptures: {
					1: {
						name: "entity.other.attribute-name.localname.xml",
					},
					2: { name },
				},
				endCaptures: {
					1: { name },
				},
			})),
			// Owl-specific attributes
			...makeXmlStringVariants((quote, name) => ({
				begin: r`${leadingWs}(t-on-\S+)\s*=(${quote})`,
				end: r`\s*(${quote})`,
				contentName: "meta.embedded.inline.javascript",
				beginCaptures: {
					1: {
						name: "entity.other.attribute-name.localname.xml",
					},
					2: { name },
				},
				endCaptures: {
					1: { name },
				},
				patterns: [{ include: "source.js" }],
			})),
			...makeXmlStringVariants((quote, name) => ({
				begin: r`${leadingWs}(required|include|readonly|(?:column_)?invisible|(?:filter_)?domain|context|eval|options|attrs|(?:t|decoration)-\S+)\s*=(${quote})`,
				end: r`\s*(${quote})`,
				contentName: "meta.embedded.inline.python",
				beginCaptures: {
					1: {
						name: "entity.other.attribute-name.localname.xml",
					},
					2: { name },
				},
				endCaptures: {
					1: { name },
				},
				patterns: [{ include: "source.python" }],
			})),
			...makeXmlStringVariants((quote, name) => ({
				begin: r`${leadingWs}(expr)\s*=(${quote})`,
				end: r`\s*(${quote})`,
				contentName: "meta.embedded.inline.xpath",
				beginCaptures: {
					1: {
						name: "entity.other.attribute-name.localname.xml",
					},
					2: { name },
				},
				endCaptures: {
					1: { name },
				},
			})),
		],
		repository: {
			jinjaEscape: {
				begin: "({{)",
				beginCaptures: {
					1: { name: pythonExtrapolationDelimiter },
				},
				end: "(}})",
				endCaptures: {
					1: { name: pythonExtrapolationDelimiter },
				},
				contentName: "meta.embedded.inline.python",
				patterns: [{ include: "source.python" }],
			},
			rubyEscape: {
				begin: "(#{)",
				beginCaptures: {
					1: { name: pythonExtrapolationDelimiter },
				},
				end: "(})",
				endCaptures: {
					1: { name: pythonExtrapolationDelimiter },
				},
				contentName: "meta.embedded.inline.python",
				patterns: [{ include: "source.python" }],
			},
		},
	});
}

// biome-ignore lint/suspicious/noExplicitAny: Deno-specific
if ((import.meta as any).main) {
	console.log(JSON.stringify(grammar(), null, "\t"));
}
