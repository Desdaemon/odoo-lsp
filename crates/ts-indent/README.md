# ts-indent

Basic formatter for tree-sitter queries written in Scheme.

- [x] Indentation
- [ ] Auto-break long lines

## Usage

Install using `cargo install -p ts-indent`. The program operates on standard input and output.

Example usage in Helix:

```toml
# in .helix/config.toml
[keys.normal]
g.z.f = ":pipe ts-indent"
```

Select an S-expression with <kbd>Alt+O</kbd>, then use <kbd>gzf</kbd> to format it.
