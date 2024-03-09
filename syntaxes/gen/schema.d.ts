/** translated from https://raw.githubusercontent.com/martinring/tmlanguage/master/tmlanguage.json */
export type Grammar<R extends string> = {
	patterns: readonly Pattern<NoInfer<R>>[];
	/**
	 * a dictionary (i.e. key/value pairs) of rules which can be included from other places in
	 * the grammar. The key is the name of the rule and the value is the actual rule. Further
	 * explanation (and example) follow with the description of the include rule key.
	 */
	repository?: Record<R, Pattern<R>>;
	/**
	 * this is an array of file type extensions that the grammar should (by default) be used
	 * with. This is referenced when TextMate does not know what grammar to use for a file the
	 * user opens. If however the user selects a grammar from the language pop-up in the status
	 * bar, TextMate will remember that choice.
	 */
	fileTypes?: string[];
	firstLineMatch?: string;
	/**
	 * regular expressions that lines (in the document) are matched against. If a line matches
	 * one of the patterns (but not both), it becomes a folding marker (see the foldings section
	 * for more info).
	 */
	foldingStartMarker?: string;
	/**
	 * regular expressions that lines (in the document) are matched against. If a line matches
	 * one of the patterns (but not both), it becomes a folding marker (see the foldings section
	 * for more info).
	 */
	foldingStopMarker?: string;
	name?: string;
	/**
	 * this should be a unique name for the grammar, following the convention of being a
	 * dot-separated name where each new (left-most) part specializes the name. Normally it
	 * would be a two-part name where the first is either text or source and the second is the
	 * name of the language or document type. But if you are specializing an existing type, you
	 * probably want to derive the name from the type you are specializing. For example Markdown
	 * is text.html.markdown and Ruby on Rails (rhtml files) is text.html.rails. The advantage
	 * of deriving it from (in this case) text.html is that everything which works in the
	 * text.html scope will also work in the text.html.«something» scope (but with a lower
	 * precedence than something specifically targeting text.html.«something»).
	 */
	scopeName: string;
	uuid?: string;
	[property: string]: unknown;
};

export type Pattern<R extends string = string> = {
	applyEndPatternLast?: number;
	/**
	 * these keys allow matches which span several lines and must both be mutually exclusive
	 * with the match key. Each is a regular expression pattern. begin is the pattern that
	 * starts the block and end is the pattern which ends the block. Captures from the begin
	 * pattern can be referenced in the end pattern by using normal regular expression
	 * back-references. This is often used with here-docs. A begin/end rule can have nested
	 * patterns using the patterns key.
	 */
	begin?: string;
	/**
	 * allows you to assign attributes to the captures of the begin pattern. Using the captures
	 * key for a begin/end rule is short-hand for giving both beginCaptures and endCaptures with
	 * same values.
	 */
	beginCaptures?: Captures<R>;
	/**
	 * allows you to assign attributes to the captures of the match pattern. Using the captures
	 * key for a begin/end rule is short-hand for giving both beginCaptures and endCaptures with
	 * same values.
	 */
	captures?: Captures<R>;
	comment?: string;
	/**
	 * this key is similar to the name key but only assigns the name to the text between what is
	 * matched by the begin/end patterns.
	 */
	contentName?: Name;
	/**
	 * set this property to 1 to disable the current pattern
	 */
	disabled?: number;
	/**
	 * these keys allow matches which span several lines and must both be mutually exclusive
	 * with the match key. Each is a regular expression pattern. begin is the pattern that
	 * starts the block and end is the pattern which ends the block. Captures from the begin
	 * pattern can be referenced in the end pattern by using normal regular expression
	 * back-references. This is often used with here-docs. A begin/end rule can have nested
	 * patterns using the patterns key.
	 */
	end?: string;
	/**
	 * allows you to assign attributes to the captures of the end pattern. Using the captures
	 * key for a begin/end rule is short-hand for giving both beginCaptures and endCaptures with
	 * same values.
	 */
	endCaptures?: Captures<R>;
	/**
	 * this allows you to reference a different language, recursively reference the grammar
	 * itself or a rule declared in this file’s repository.
	 */
	include?: Ref<R>;
	/**
	 * a regular expression which is used to identify the portion of text to which the name
	 * should be assigned. Example: '\b(true|false)\b'.
	 */
	match?: string;
	/**
	 * the name which gets assigned to the portion matched. This is used for styling and
	 * scope-specific settings and actions, which means it should generally be derived from one
	 * of the standard names.
	 */
	name?: Name;
	/**
	 * applies to the region between the begin and end matches
	 */
	patterns?: readonly Pattern<R>[];
	/**
	 * these keys allow matches which span several lines and must both be mutually exclusive
	 * with the match key. Each is a regular expression pattern. begin is the pattern that
	 * starts the block and while continues it.
	 */
	while?: string;
	/**
	 * allows you to assign attributes to the captures of the while pattern. Using the captures
	 * key for a begin/while rule is short-hand for giving both beginCaptures and whileCaptures
	 * with same values.
	 */
	whileCaptures?: Captures<R>;
	[property: string]: unknown;
};

type Split<T extends string> =
	T extends `${infer F}${infer R}` ? F | Split<R> : T extends "" ? never : T;
type Ascii = Split<"abcdefghijklmnopqrstuvwxyz">;
type Ref<R extends string> = `#${R}` | `${Ascii}${string}`

/**
 * allows you to assign attributes to the captures of the begin pattern. Using the captures
 * key for a begin/end rule is short-hand for giving both beginCaptures and endCaptures with
 * same values.
 */
export type Captures<R extends string> = Record<
	`${number}`,
	{
		name: string;
		patterns?: Pattern<R>[];
	}
>;

/**
 * this key is similar to the name key but only assigns the name to the text between what is
 * matched by the begin/end patterns.
 *
 * the name which gets assigned to the portion matched. This is used for styling and
 * scope-specific settings and actions, which means it should generally be derived from one
 * of the standard names.
 */
export type Name =
	| "comment"
	| "comment.block"
	| "comment.block.documentation"
	| "comment.line"
	| "comment.line.double-dash"
	| "comment.line.double-slash"
	| "comment.line.number-sign"
	| "comment.line.percentage"
	| "constant"
	| "constant.character"
	| "constant.character.escape"
	| "constant.language"
	| "constant.numeric"
	| "constant.other"
	| "constant.regexp"
	| "constant.rgb-value"
	| "constant.sha.git-rebase"
	| "emphasis"
	| "entity"
	| "entity.name"
	| "entity.name.class"
	| "entity.name.function"
	| "entity.name.method"
	| "entity.name.section"
	| "entity.name.selector"
	| "entity.name.tag"
	| "entity.name.type"
	| "entity.other"
	| "entity.other.attribute-name"
	| "entity.other.inherited-class"
	| "header"
	| "invalid"
	| "invalid.deprecated"
	| "invalid.illegal"
	| "keyword"
	| "keyword.control"
	| "keyword.control.less"
	| "keyword.operator"
	| "keyword.operator.new"
	| "keyword.other"
	| "keyword.other.unit"
	| "markup"
	| "markup.bold"
	| "markup.changed"
	| "markup.deleted"
	| "markup.heading"
	| "markup.inline.raw"
	| "markup.inserted"
	| "markup.italic"
	| "markup.list"
	| "markup.list.numbered"
	| "markup.list.unnumbered"
	| "markup.other"
	| "markup.punctuation.list.beginning"
	| "markup.punctuation.quote.beginning"
	| "markup.quote"
	| "markup.raw"
	| "markup.underline"
	| "markup.underline.link"
	| "meta"
	| "meta.cast"
	| "meta.parameter.type.variable"
	| "meta.preprocessor"
	| "meta.preprocessor.numeric"
	| "meta.preprocessor.string"
	| "meta.return-type"
	| "meta.selector"
	| "meta.structure.dictionary.key.python"
	| "meta.tag"
	| "meta.type.annotation"
	| "meta.type.name"
	| "metatag.php"
	| "storage"
	| "storage.modifier"
	| "storage.modifier.import.java"
	| "storage.modifier.package.java"
	| "storage.type"
	| "storage.type.cs"
	| "storage.type.java"
	| "string"
	| "string.html"
	| "string.interpolated"
	| "string.jade"
	| "string.other"
	| "string.quoted"
	| "string.quoted.double"
	| "string.quoted.other"
	| "string.quoted.single"
	| "string.quoted.triple"
	| "string.regexp"
	| "string.unquoted"
	| "string.xml"
	| "string.yaml"
	| "strong"
	| "support"
	| "support.class"
	| "support.constant"
	| "support.function"
	| "support.function.git-rebase"
	| "support.other"
	| "support.property-value"
	| "support.type"
	| "support.type.property-name"
	| "support.type.property-name.css"
	| "support.type.property-name.less"
	| "support.type.property-name.scss"
	| "support.variable"
	| "variable"
	| "variable.language"
	| "variable.name"
	| "variable.other"
	| "variable.parameter"
	| (string & {});
