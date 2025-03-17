# Release Instructions

- Bump versions in `Cargo.toml`s and `package.json`
- Run:

```sh
git cliff --tag $NEW_VERSION -o CHANGELOG.md
git tag $NEW_VERSION
```

- Download .vsix file after CI is done, upload to marketplaces
