import * as vscode from "vscode";
import type { State } from "./extension";

interface XMLExtensionApi {
	// addXMLCatalogs(_: string[]): void;
	addXMLFileAssociations(_: { systemId: string; pattern: string }[]): void;
}

export async function registerXmlFileAssociations(
	context: vscode.ExtensionContext,
	traceOutputChannel: vscode.OutputChannel,
	extensionState: State,
) {
	try {
		// see https://github.com/redhat-developer/vscode-xml/blob/main/src/api/xmlExtensionApi.ts
		const xmlApi = await vscode.extensions.getExtension<XMLExtensionApi>("redhat.vscode-xml")?.activate();
		if (xmlApi) {
			xmlApi.addXMLFileAssociations([
				// HACK: systemId must be unique
				{
					systemId: `${context.extensionUri}/odoo.rng`,
					pattern: "views/*.xml",
				},
				{
					systemId: `${context.extensionUri}/./odoo.rng`,
					pattern: "data/*.xml",
				},
				{
					systemId: `${context.extensionUri}/././odoo.rng`,
					pattern: "security/*.xml",
				},
				{
					systemId: `${context.extensionUri}/./././odoo.rng`,
					pattern: "report/*.xml",
				},
				{
					systemId: `${context.extensionUri}/././././odoo.rng`,
					pattern: "wizard/*.xml",
				},
			]);
		} else if (!extensionState.noXmlReminders) {
			// Recommend that the user install the XML extension
			vscode.window
				.showInformationMessage(
					"Install the 'XML' extension for a better XML editing experience.",
					"Install",
					"Remind me later",
					"Never show again",
				)
				.then((choice) => {
					if (choice === "Never show again") extensionState.noXmlReminders = true;
					if (choice !== "Install") return;
					vscode.commands.executeCommand("workbench.extensions.search", "@id:redhat.vscode-xml");
				});
		}
	} catch (err) {
		traceOutputChannel.appendLine(`Failed to register XML file associations: ${err}`);
	}
}

export async function registerXPathSemanticTokensProvider(
	context: vscode.ExtensionContext,
	traceOutputChannel: vscode.OutputChannel,
	extensionState: State,
) {
	const extension = vscode.extensions.all.find((e) => e.id === "deltaxml.xslt-xpath");
	if (!extension) {
		if (extensionState.noXPathReminders) return;
		vscode.window
			.showInformationMessage(
				"Install the 'XSLT/XPath' extension for XPath semantic highlighting.",
				"Install",
				"Remind me later",
				"Never show again",
			)
			.then((choice) => {
				if (choice === "Never show again") extensionState.noXPathReminders = true;
				if (choice !== "Install") return;
				vscode.commands.executeCommand("workbench.extensions.search", "@id:deltaxml.xslt-xpath");
			});
		return;
	}
	await extension.activate();
	let module: typeof XPathExtension;
	try {
		module = await import(`${extension.extensionPath}/${extension.packageJSON.main}`);
	} catch (err) {
		traceOutputChannel.appendLine(`Failed to import XPath extension module: ${err}`);
		return;
	}

	const odooXmlConfiguration: XPathExtension.LanguageConfiguration = {
		variableElementNames: ["xpath"],
		expressionAtts: ["expr"],
		nativePrefix: "",
		tvtAttributes: [],
		nonNativeAvts: false,
		docType: DocumentTypes.Other,
	};

	const Provider = OdooXpathSemanticTokensProviderMixin(module.XsltSemanticTokensProvider);
	const provider = new Provider(odooXmlConfiguration);

	context.subscriptions.push(
		vscode.languages.registerDocumentSemanticTokensProvider({ language: "xml" }, provider, provider.legend),
	);
}

enum DocumentTypes {
	XSLT = 0,
	XSLT40 = 1,
	XPath = 2,
	DCP = 3,
	SCH = 4,
	Other = 5,
}

/** @see https://github.com/DeltaXML/vscode-xslt-tokenizer/blob/master/src/extension.ts */
declare namespace XPathExtension {
	class XsltSemanticTokensProvider implements vscode.DocumentSemanticTokensProvider {
		/*private*/ xslLexer: __private__.XslLexer;
		public constructor(languageConfig: LanguageConfiguration);
		provideDocumentSemanticTokens(
			document: vscode.TextDocument,
			token: vscode.CancellationToken,
		): vscode.ProviderResult<vscode.SemanticTokens>;
	}

	/** not exported by the actual extension, do not use directly. */
	namespace __private__ {
		class XslLexer {
			// biome-ignore lint/complexity/useLiteralKeys: otherwise not allowed by TS
			get ["constructor"](): typeof XslLexer;
			public provideCharLevelState: boolean;
			public analyse(xsl: string, keepNameTests?: boolean): BaseToken[];
			public static getTextmateTypeLegend(): string[];
		}
		enum ErrorType {}
	}

	interface LanguageConfiguration {
		expressionAtts: string[];
		variableElementNames: string[];
		linkElementAttrNames?: [string, string];
		avtAtts?: string[];
		nativePrefix: string;
		tvtAttributes: string[];
		nonNativeAvts: boolean;
		schemaData?: /*SchemaData*/ unknown;
		docType: DocumentTypes;
		resourceNames?: string[];
		featureNames?: string[];
		propertyNames?: string[];
		rootElementSnippets?: /*Snippet*/ unknown[];
		elementSnippets?: /*Snippet*/ unknown[];
		isVersion4?: boolean;
	}
	export interface BaseToken {
		line: number;
		startCharacter: number;
		length: number;
		value: string;
		charType?: number;
		tokenType: number;
		context?: BaseToken | null;
		error?: __private__.ErrorType;
		nesting?: number;
		referenced?: boolean;
		tagElementId?: number;
	}
}

export const OdooXpathSemanticTokensProviderMixin = (clazz: typeof XPathExtension.XsltSemanticTokensProvider) =>
	class XmlSemanticTokensProviderMixin extends clazz {
		private static legend: vscode.SemanticTokensLegend;
		private static attributeNameToken: number;
		private static attributeValueToken: number;

		constructor(config: XPathExtension.LanguageConfiguration) {
			super(config);
			this.xslLexer.provideCharLevelState = false;
		}
		async provideDocumentSemanticTokens(document: vscode.TextDocument, cancellationToken: vscode.CancellationToken) {
			const tokens = this.xslLexer.analyse(document.getText());
			const builder = new vscode.SemanticTokensBuilder();
			const attributeNameToken = XmlSemanticTokensProviderMixin.attributeNameToken;
			const attributeValueToken = XmlSemanticTokensProviderMixin.attributeValueToken;

			let skipTillStart = 0;
			let inExpr = false;

			for (const token of tokens) {
				if (cancellationToken.isCancellationRequested) break;
				if (
					!inExpr &&
					attributeNameToken === token.tokenType &&
					"expr" ===
						document.getText(
							new vscode.Range(token.line, token.startCharacter, token.line, token.startCharacter + token.length),
						)
				) {
					inExpr = true;
					skipTillStart = 3; // skips `expr = "`
				}
				if (inExpr && skipTillStart-- > 0) continue;
				if (inExpr) builder.push(token.line, token.startCharacter, token.length, token.tokenType, 0);

				if (attributeValueToken === token.tokenType) inExpr = false;
			}
			return builder.build();
		}

		get legend() {
			if (XmlSemanticTokensProviderMixin.legend) return XmlSemanticTokensProviderMixin.legend;
			const tokenTypesLegend = this.xslLexer.constructor.getTextmateTypeLegend();
			const tokenModifiersLegend = [
				"declaration",
				"documentation",
				"member",
				"static",
				"abstract",
				"deprecated",
				"modification",
				"async",
			];
			const tokenModifiers = new Map<string, number>();
			tokenModifiersLegend.forEach((tokenModifier, index) => tokenModifiers.set(tokenModifier, index));
			XmlSemanticTokensProviderMixin.attributeNameToken = tokenTypesLegend.indexOf("attributeName");
			XmlSemanticTokensProviderMixin.attributeValueToken = tokenTypesLegend.indexOf("attributeValue");
			XmlSemanticTokensProviderMixin.legend = new vscode.SemanticTokensLegend(tokenTypesLegend, tokenModifiersLegend);
			return XmlSemanticTokensProviderMixin.legend;
		}
	};
