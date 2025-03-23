# Changelog

## [0.6.0](https://github.com/Desdaemon/odoo-lsp/releases/tag/v0.6.0) - 2025-03-17

Compare: [`v0.5.0...v0.6.0`](https://github.com/Desdaemon/odoo-lsp/compare/v0.5.0...v0.6.0)

### Added

- Add widget and action tag complete/gotodefs [`92e0c16`](https://github.com/Desdaemon/odoo-lsp/commit/92e0c16a7b0ea4a4b5c56365a41d3405ea42c279)
- Niche field completions [`b81b64a`](https://github.com/Desdaemon/odoo-lsp/commit/b81b64ac956b3b60dfe9d62b4322b64448864ffc), Closes #43, #36
- Zed extension [`3e07b08`](https://github.com/Desdaemon/odoo-lsp/commit/3e07b082a340bd450560736cb030038c79386850)
- Method completions [`7d9244e`](https://github.com/Desdaemon/odoo-lsp/commit/7d9244e1728ccec4a650bc38ca27d71373e45a5e)
- Method return type analysis [`5809dbe`](https://github.com/Desdaemon/odoo-lsp/commit/5809dbe9d9baacb2479beddba35a085293e6ec80)
- Super type analysis [`439d090`](https://github.com/Desdaemon/odoo-lsp/commit/439d090fc7b17fd547015df45fd6add1670667a5)
- Assistance for compute functions [`e85e0c5`](https://github.com/Desdaemon/odoo-lsp/commit/e85e0c5692b153c4e69db7ab974d720e123f6a7a)
- Method docstrings [`94f4a72`](https://github.com/Desdaemon/odoo-lsp/commit/94f4a72ea8d4d12f9f9a974a8c6135ab58e28454)
- **xml:** Add `column_invisible` documentation [`5d61b99`](https://github.com/Desdaemon/odoo-lsp/commit/5d61b99c0009bfee47f447b8f83bbae4e85573a7)
- Allow specifying thread count [`1b434ee`](https://github.com/Desdaemon/odoo-lsp/commit/1b434eed71316efc91ac9aad5a6d9de9f4547eb3)
- Offer to show duplicate base [`cf3c84f`](https://github.com/Desdaemon/odoo-lsp/commit/cf3c84f9760fe48fe0085bfcacf2d7b9d2f135ff), Closes #50

### Fixed

- On change handler should block again [`ce643c7`](https://github.com/Desdaemon/odoo-lsp/commit/ce643c73e868e3c39df00a4bb2f78d5b3fb0ea43)
- Make did-open command wait longer [`c12dfd5`](https://github.com/Desdaemon/odoo-lsp/commit/c12dfd5ceb0eaff552894ab02aa407cb93187ad5)
- Do not block on non-essential requests [`b4b9763`](https://github.com/Desdaemon/odoo-lsp/commit/b4b97638d2729dea7678cf7e4bba996b37576745)
- Change to single-thread mode again [`f209001`](https://github.com/Desdaemon/odoo-lsp/commit/f209001422eb9187070bf1831748bc5b848f2a19)
- **internal:** Use handrolled async lock hashmap [`23ef89e`](https://github.com/Desdaemon/odoo-lsp/commit/23ef89e346c27354bb07834888969fc76c42b35d)
- Revert single-threaded mode [`67e22a1`](https://github.com/Desdaemon/odoo-lsp/commit/67e22a1a602bb35f88c4480bfec9a68e2d9df651)
- Update shell calling and readme [`1d88bce`](https://github.com/Desdaemon/odoo-lsp/commit/1d88bce13d6d99ce66e265e52c2cdcd58f4b0133), Closes #40
- **analyze:** Handle singular array inherit [`ffd9ebb`](https://github.com/Desdaemon/odoo-lsp/commit/ffd9ebbadbd2eb3cf953fa8111a5941d7a9e8bf2), Closes #31, #39
- **xml:** Stricter python expr analysis [`b2f4f99`](https://github.com/Desdaemon/odoo-lsp/commit/b2f4f991bbb223f83558fbc3fd38249eb9afe0a1), Closes #37
- Duplicated method references when saving [`c30dc5a`](https://github.com/Desdaemon/odoo-lsp/commit/c30dc5a7a37411be825c329acfe64ba2f851a445)
- Update method overrides in same file [`d5df84c`](https://github.com/Desdaemon/odoo-lsp/commit/d5df84c6dc582e8402d387d74743ab2c2c59e119)
- **ci:** Workaround for git bug [`957f0aa`](https://github.com/Desdaemon/odoo-lsp/commit/957f0aabaa8bc7dfb061e42b869688133ae4e4cb)
- **windows:** Do not use unix-only extension [`335c8dc`](https://github.com/Desdaemon/odoo-lsp/commit/335c8dc88d3de2eae0dcfd01faaf199176afdf38)
- **ci:** Use previous Ubuntu LTS [`540fabb`](https://github.com/Desdaemon/odoo-lsp/commit/540fabb00684344a1003a5ec6c23806b90295366)
- Analysis for \_for_xml_id() and copy() [`09718ec`](https://github.com/Desdaemon/odoo-lsp/commit/09718ecec1ccdcbbdaf72995442c6102681c9605)
- **xml:** Syntax error in schema [`39c6b43`](https://github.com/Desdaemon/odoo-lsp/commit/39c6b43f697b0dae95269a5808dec759ac0b9c47)
- Super analysis for decorated_definition [`5c611bc`](https://github.com/Desdaemon/odoo-lsp/commit/5c611bc98f545a961bd306ec213462a0673d5756), Closes #49
- **windows:** Path handling [`1140e28`](https://github.com/Desdaemon/odoo-lsp/commit/1140e286a2287419a74fe2973d1e93d40013cf3e)
- Progress reports should not block [`64e8cc4`](https://github.com/Desdaemon/odoo-lsp/commit/64e8cc41666ede4ce1724f4bd62e1a86f89ff04e)

### Refactor

- **python:** Unordered field descriptors [`cb32677`](https://github.com/Desdaemon/odoo-lsp/commit/cb3267708ec67ddc09527bf0b9ff9d4cbd5940bc)

### Testing

- Fixture testing in Rust [`43f00c2`](https://github.com/Desdaemon/odoo-lsp/commit/43f00c2de925525681d03bcd012cc12444802a57)
- **infra:** Switch to nextest [`0bde1e7`](https://github.com/Desdaemon/odoo-lsp/commit/0bde1e773872b1ee6a6903bfb792d71c6e3495c1)
- **ci:** Use codecov coverage format [`cc7adb4`](https://github.com/Desdaemon/odoo-lsp/commit/cc7adb408b2514470c9c3cc91e76e744a885f2e5)
- Switch cache impl [`6e935bc`](https://github.com/Desdaemon/odoo-lsp/commit/6e935bcf016e515836d9069fcee4b0eb713a6b86)
- **infra:** Remove pytest [`2a53f7f`](https://github.com/Desdaemon/odoo-lsp/commit/2a53f7f6ca781d4af00a0326bb0d8d5e2c5469e2)

## [0.5.0](https://github.com/Desdaemon/odoo-lsp/releases/tag/v0.5.0) - 2024-08-28

Compare: [`v0.4.1...v0.5.0`](https://github.com/Desdaemon/odoo-lsp/compare/v0.4.1...v0.5.0)

### Added

- Try to embed nightly version in binary [`fdeb2e0`](https://github.com/Desdaemon/odoo-lsp/commit/fdeb2e00e127617cf0516df097e552c5e7c0a5d9)
- Allow configuring completion limits [`8895160`](https://github.com/Desdaemon/odoo-lsp/commit/889516086fda3082a76f90bb8cc4476380ad1064)
- Complete report_name references [`7008fbc`](https://github.com/Desdaemon/odoo-lsp/commit/7008fbc9fa5dcf097342e2805ba7f5142c5314dd)
- Default to 1-thread execution [`6ce8d76`](https://github.com/Desdaemon/odoo-lsp/commit/6ce8d766b9afa2b25c553dbbfa02d79ccc838e44)
- Go-to Owl components and templates [`d5cf8f6`](https://github.com/Desdaemon/odoo-lsp/commit/d5cf8f60e5e958eeeba0207247ce32ce343f60e6)
- Hover for component props [`ae5d491`](https://github.com/Desdaemon/odoo-lsp/commit/ae5d491c276053e36009f48ed11fc9041da42bd4)
- **xml:** Add schema for tree views [`9e30871`](https://github.com/Desdaemon/odoo-lsp/commit/9e30871d88963ddfa7c0a55c403670869700e1e9)
- **xml:** Add form view schema [`e52b7fc`](https://github.com/Desdaemon/odoo-lsp/commit/e52b7fc479aeedfc6424891e6d2fefd5ae6f8fb6)
- **xml:** Add more form elements and attributes [`ad62bda`](https://github.com/Desdaemon/odoo-lsp/commit/ad62bda954c2732915b48db37941de50646b58f0)
- Allow grouped() mapped access [`3961eec`](https://github.com/Desdaemon/odoo-lsp/commit/3961eec1370a472f784d09e3644370bb947ab82f)

### Fixed

- **xml:** Don't access xpath fields [`531e69c`](https://github.com/Desdaemon/odoo-lsp/commit/531e69c4b8f2769039994bc230c8cde899d475e3)
- Reduce memory usage [`ecfbda5`](https://github.com/Desdaemon/odoo-lsp/commit/ecfbda571b0c8b2c4c08e493b297e3660b72d473)
- Allow domain='..' in fields [`1a9e394`](https://github.com/Desdaemon/odoo-lsp/commit/1a9e3942ce9b3b01a33a288bb3e1961116b2d267)
- Make immutable strings fitter on 32-bit [`2a99b89`](https://github.com/Desdaemon/odoo-lsp/commit/2a99b89340bef52c84f29a29be9e441c14a5b4fa)
- Do not skip local config file [`2126748`](https://github.com/Desdaemon/odoo-lsp/commit/21267489b1c74a02f6dc85ebc1ee3ca69047632f)
- Default to primary inheritance for templates [`f5fc32c`](https://github.com/Desdaemon/odoo-lsp/commit/f5fc32cb3cd4458a2744aaadbb49446305c8cc04)
- Diagnostics for attributes [`f661806`](https://github.com/Desdaemon/odoo-lsp/commit/f66180690f0fb0c1437848f08a5d60aa6e3da53f)
- Consistent use of paths for Windows [`d34fbf9`](https://github.com/Desdaemon/odoo-lsp/commit/d34fbf960c8cee965ae1c4f5353ed3867125c75b)
- Support incomplete attributes [`70af361`](https://github.com/Desdaemon/odoo-lsp/commit/70af36188209c0538fcda95dff1d3694bb0d9ba7)
- Allow more builtin fields [`20a554a`](https://github.com/Desdaemon/odoo-lsp/commit/20a554a02950d230877f016414a20c63a2719e9a)
- Do not clobber record names [`785c388`](https://github.com/Desdaemon/odoo-lsp/commit/785c388f5427c3c3d9509c124529195698b1f458)
- Use version for self update [`ebb6df2`](https://github.com/Desdaemon/odoo-lsp/commit/ebb6df249f96d0d9127591d2d9f0c498e4245a59)
- Broken root finder, prevent panics [`a16fda9`](https://github.com/Desdaemon/odoo-lsp/commit/a16fda9391f064cf0df9f21b24e99f6323ac0eba)
- Properly wait for setup on all handlers [`7c6cdc8`](https://github.com/Desdaemon/odoo-lsp/commit/7c6cdc8bd65258e68edf7e018e34c43db6f6d93d)
- Shift 1 position when completing on dot [`e2a0fcc`](https://github.com/Desdaemon/odoo-lsp/commit/e2a0fccf1c96f4fb325fcb7c606a58398952c39f)
- Add ancestor fields to field set [`54f57bb`](https://github.com/Desdaemon/odoo-lsp/commit/54f57bb0c5688e6e034dfb446de75c763e015a0e)

### Refactor

- Python queries [`3272e43`](https://github.com/Desdaemon/odoo-lsp/commit/3272e43e2f3b74f6961ae84b3600a61a0e0f9ace)

## [0.4.1](https://github.com/Desdaemon/odoo-lsp/releases/tag/v0.4.1) - 2024-05-01

Compare: [`v0.4.0...v0.4.1`](https://github.com/Desdaemon/odoo-lsp/compare/v0.4.0...v0.4.1)

### Added

- Complete domain=.. for relational fields [`dcf8fc8`](https://github.com/Desdaemon/odoo-lsp/commit/dcf8fc8b39fc0429d1a54234c238884edb0148d9)
- Syntax highlighting for domain='..' [`34ee224`](https://github.com/Desdaemon/odoo-lsp/commit/34ee2243b8f62f52da1b41aa2a212901ea1b3a82)
- Allow named groupby and aggregate parameters [`d961032`](https://github.com/Desdaemon/odoo-lsp/commit/d9610325b444d5f27c4a76b9b3a5e143a849b494)
- Nested XML fields [`2b629e0`](https://github.com/Desdaemon/odoo-lsp/commit/2b629e02798f6fdf437aa205455b0214d4ecffc1)
- Allow $.template = `..` [`7f53e84`](https://github.com/Desdaemon/odoo-lsp/commit/7f53e843e4a9f4c45b0876d180f3e36af7b37752)
- Completion at attribute commas [`acec172`](https://github.com/Desdaemon/odoo-lsp/commit/acec172f52bdbeeaf4e961e18f5162c919106710)
- Template annotations and completions [`74b74eb`](https://github.com/Desdaemon/odoo-lsp/commit/74b74ebaac92a66d3e7f2c37e2100aa6e202a9c1)
- **python:** Reveal types of binary expressions [`85a3d83`](https://github.com/Desdaemon/odoo-lsp/commit/85a3d835bfdc3e99122738c0ed6dd643d4a6e0ed)
- **xml:** Allow res_model as model string [`1394c9c`](https://github.com/Desdaemon/odoo-lsp/commit/1394c9c45943c901bcd7ba859991621cef1e2512)
- **python:** Analyze comprehensions in create() [`fe151b5`](https://github.com/Desdaemon/odoo-lsp/commit/fe151b50549b8d5ada820a98c350faa6c394c916)

### Fixed

- **extension:** Nightly installation flow [`0068984`](https://github.com/Desdaemon/odoo-lsp/commit/0068984883c348e0309a304bdf57dfc843ec166e)
- **extension:** Wrong comparison with birthtime [`7db6521`](https://github.com/Desdaemon/odoo-lsp/commit/7db6521fc4b41007ed40beaa558eba4226e11ca5)
- Edge case with model fields [`342239e`](https://github.com/Desdaemon/odoo-lsp/commit/342239e264a80c165668e53f89603615646dc606)
- Revert to `#match?` [`eda9111`](https://github.com/Desdaemon/odoo-lsp/commit/eda9111542cfdc9fa944b1b53cd92458e3afaaeb)
- Populate XML fields on demand [`973a6ee`](https://github.com/Desdaemon/odoo-lsp/commit/973a6ee492da7fbf007cae1e61cade4cd62e3d03)
- Detect more inline Python in XML [`168edb7`](https://github.com/Desdaemon/odoo-lsp/commit/168edb73ad04fa88e0b61485d4aff617157d28fb)
- Prevent completion OOB [`fdb3f4b`](https://github.com/Desdaemon/odoo-lsp/commit/fdb3f4bef41b668d4e314438787f958acfdcf6c8)

## [0.4.0](https://github.com/Desdaemon/odoo-lsp/releases/tag/v0.4.0) - 2024-04-20

Compare: [`v0.3.1...v0.4.0`](https://github.com/Desdaemon/odoo-lsp/compare/v0.3.1...v0.4.0)

### Added

- Warn when duplicate base modules are found [`66df773`](https://github.com/Desdaemon/odoo-lsp/commit/66df77372762fde5ea12fbc19347e652770d2bce)
- Limit redundant indexing [`135fcfd`](https://github.com/Desdaemon/odoo-lsp/commit/135fcfd66bd6934e70794c666bb1380bcc856d37)
- Add Python/JS injections for VSCode [`8cc6c0f`](https://github.com/Desdaemon/odoo-lsp/commit/8cc6c0fe95bcce4864942ec08fb5c4c1a4ae0b64)
- Xpath semantic tokens [`b665779`](https://github.com/Desdaemon/odoo-lsp/commit/b665779d7ba482bae2b7bb970ab9572455811c80)
- Offer to install nightlies of extension [`9a6e597`](https://github.com/Desdaemon/odoo-lsp/commit/9a6e597e8ef8a0b90a89494400ed6c357d602f0a)
- **internal:** Split path keys to improve mem use [`24193c8`](https://github.com/Desdaemon/odoo-lsp/commit/24193c88aab1999b6d6daf9323f63729f680d01f)
- Migrate grammar to yaml [`af8d013`](https://github.com/Desdaemon/odoo-lsp/commit/af8d013d359d04e9b09d2b8457289f2bbc0a8e62)
- Change hover message for component/template [`5543870`](https://github.com/Desdaemon/odoo-lsp/commit/5543870c88dd20f630ba08318562d22d90665cb8)
- Update owl grammar [`929d15f`](https://github.com/Desdaemon/odoo-lsp/commit/929d15f04f9ad5e6a926f153d54a279034102061)
- Optimize memory usage [`f529b42`](https://github.com/Desdaemon/odoo-lsp/commit/f529b42f8239654bee82fea405752acdd8599a11)
- Preserve types of subscript expressions [`4144579`](https://github.com/Desdaemon/odoo-lsp/commit/41445792b9ba5d0a8960d459a44adb774803964b)
- Completion inside arch [`2f24674`](https://github.com/Desdaemon/odoo-lsp/commit/2f24674ec54ae6e26ec1cbb933eee6610192dfcc)
- Handle model= in XML fields [`ff6e325`](https://github.com/Desdaemon/odoo-lsp/commit/ff6e325c00c483715caa2b62e96dd2b8316b29c6)
- **extension:** Use curl for more compatibility [`37e8d5f`](https://github.com/Desdaemon/odoo-lsp/commit/37e8d5fca6b6a6a1e5a761628a293f25b95c83c6)
- Support XML groups [`fe2a880`](https://github.com/Desdaemon/odoo-lsp/commit/fe2a880e6ce28e7fbc67a3b45c04c431b2c58b0a)
- Hover for qweb templates [`c6f07e0`](https://github.com/Desdaemon/odoo-lsp/commit/c6f07e02e33eebc962fd70f8c668171e52d877b5)
- **extension:** Use LogOutputChannel [`989fe49`](https://github.com/Desdaemon/odoo-lsp/commit/989fe494358a16eb54aaf429d506b85f91013457)

### Documentation

- Update README to include syntax demo [`287c590`](https://github.com/Desdaemon/odoo-lsp/commit/287c590e36efddd2b8e837c9da78aa44bfb7378c)
- Update code documentation [`1a0c32d`](https://github.com/Desdaemon/odoo-lsp/commit/1a0c32d35e81b0ef9ad586d1d6dc4dc8e2720b75)

### Fixed

- Store inherit_id as full XML ID [`c09a54b`](https://github.com/Desdaemon/odoo-lsp/commit/c09a54bc0b50ec4f14803f8e0f2f08371230d695)
- Wrong import path for xpath extension [`d663e6e`](https://github.com/Desdaemon/odoo-lsp/commit/d663e6e5c6c4c060c7207091e21691dfe71bc22a)
- **extension:** Compare dates for nightly updates [`876f9eb`](https://github.com/Desdaemon/odoo-lsp/commit/876f9eb88faf249bd5360102779e04614d2f2546)
- Do not update records on change [`ccc9559`](https://github.com/Desdaemon/odoo-lsp/commit/ccc955966998e27b456d21dc4a72082243a496d5)
- Inherit_ids not resolving [`625db08`](https://github.com/Desdaemon/odoo-lsp/commit/625db08aca9d54cbe4dae4d0cf48233a43871a5e)
- Missing menuitem groups completions [`4f43c62`](https://github.com/Desdaemon/odoo-lsp/commit/4f43c622016797b7dd0795eeb19e36c2e8dbf279)
- Correct conditions for new nightlies [`498bb81`](https://github.com/Desdaemon/odoo-lsp/commit/498bb8176faf4a3fc03ff789c65931c6430cdb99)
- Greedy parsing of xml attribute name [`94b02f4`](https://github.com/Desdaemon/odoo-lsp/commit/94b02f47594bb0c69efd45c8b8a44ea2db5deb8c)
- Attempt to remove duplicate diags [`f5cd4a2`](https://github.com/Desdaemon/odoo-lsp/commit/f5cd4a20a501bb326177706606c34b2418d7267d)
- **python:** Correctly resolve mapped relations [`115aea5`](https://github.com/Desdaemon/odoo-lsp/commit/115aea5eab626274f67d226c71eb5b8fb166dd43)
- Make logs more visible [`daf05f5`](https://github.com/Desdaemon/odoo-lsp/commit/daf05f5b91b4b9d4e555ce71843c13cd851c979f)

## [0.3.1](https://github.com/Desdaemon/odoo-lsp/releases/tag/v0.3.1) - 2024-03-06

Compare: [`v0.3.0...v0.3.1`](https://github.com/Desdaemon/odoo-lsp/compare/v0.3.0...v0.3.1)

### Added

- Diagnose wrong usage of dotted access [`8424d3c`](https://github.com/Desdaemon/odoo-lsp/commit/8424d3c96fd824bd20a919a7c598a8093b6781b0)
- Xml hovers [`2fc9a22`](https://github.com/Desdaemon/odoo-lsp/commit/2fc9a2280b90bdcb6432f9df5b83f3fe8dd45f56)
- **analysis:** Resolve types of .mapped('..') [`bc54a28`](https://github.com/Desdaemon/odoo-lsp/commit/bc54a28a2baa14d69784f5cfa40208b8a2fadce8)
- Support for Model.\_for_xml_id [`93efcf9`](https://github.com/Desdaemon/odoo-lsp/commit/93efcf9b7377aabcb450266f47dc51b29a315332)
- New command to restart LSP server [`7ff9235`](https://github.com/Desdaemon/odoo-lsp/commit/7ff92354dbf6464de0db1d69209cc9f5841162c7)
- Support for read and read_group fields [`bae66d8`](https://github.com/Desdaemon/odoo-lsp/commit/bae66d8f0c3ec048c82078d2420171feaa27801f)
- Debug output for templates [`8a28582`](https://github.com/Desdaemon/odoo-lsp/commit/8a28582ceb06b379726a636015c3b930f5cf606a)
- Completions for component props [`510843d`](https://github.com/Desdaemon/odoo-lsp/commit/510843defcf950404371ca111fa6c9b7a99a237c)
- Self-update [`ebeb4e8`](https://github.com/Desdaemon/odoo-lsp/commit/ebeb4e8ee91a8ae63899a8ff74efb1c9d56b19b8)
- Jump-to-def for component props [`922cc68`](https://github.com/Desdaemon/odoo-lsp/commit/922cc6842d4bba05daf400f5c6d7f2556c155ae2)
- **internal:** Copy-on-write for fields [`54319f9`](https://github.com/Desdaemon/odoo-lsp/commit/54319f98e86ae79e2290b3c31e3d96da46249c2e)
- Include XML schema with extension [`1e3c533`](https://github.com/Desdaemon/odoo-lsp/commit/1e3c53384db36e5b71985fe23b9114c37ca772e8)

### Fixed

- Wrong paths for tsconfig [`239802e`](https://github.com/Desdaemon/odoo-lsp/commit/239802e7fa128967671bab0b05752c19f77d1df1)
- Skip attribute diagnostics if not modified [`f2a7adb`](https://github.com/Desdaemon/odoo-lsp/commit/f2a7adb5afc4e4437f24bafb0579678a86515621)
- Limit diagnostics range based on edits [`06f6dd2`](https://github.com/Desdaemon/odoo-lsp/commit/06f6dd2ff351c110d81b2833bb55c932366ad47e)
- Wait for initialization before first requests [`813c2a8`](https://github.com/Desdaemon/odoo-lsp/commit/813c2a8a58a0a053f38d1f65d0b00e4fc2db4fac)
- Prevent panic when clearing oob'd diags [`7bf2b03`](https://github.com/Desdaemon/odoo-lsp/commit/7bf2b0391cbcd4322458bd25456ecabca059cb29)
- **tsconfig:** Correct path for recursive modules [`ad253f7`](https://github.com/Desdaemon/odoo-lsp/commit/ad253f775ae10120cde1d87a8e53a24afb107a7b)
- **ci:** Failed to cross-compile Linux targets [`ba41685`](https://github.com/Desdaemon/odoo-lsp/commit/ba41685c42889c781c6b3727b81c144cb7bb8dfa)
- Issue with wrong download link for nightlies [`9235ef9`](https://github.com/Desdaemon/odoo-lsp/commit/9235ef9d744a610262a91cf92648f0a867671d75)

## [0.3.0](https://github.com/Desdaemon/odoo-lsp/releases/tag/v0.3.0) - 2023-12-18

Compare: [`v0.2.1...v0.3.0`](https://github.com/Desdaemon/odoo-lsp/compare/v0.2.1...v0.3.0)

### Added

- Basic support for fields in domains [`a9fcbb5`](https://github.com/Desdaemon/odoo-lsp/commit/a9fcbb5a1a0fdc77c3e8613eb2474cb8016e5fbc)
- Index owl components [`e849d7a`](https://github.com/Desdaemon/odoo-lsp/commit/e849d7af83831ec16a275c43d8c34ed6c2901a57)
- Component references for t-name, t-inherit [`1584003`](https://github.com/Desdaemon/odoo-lsp/commit/1584003ca520e0f50c12171c0db00c494af85f74)
- Tsconfig command [`1445da7`](https://github.com/Desdaemon/odoo-lsp/commit/1445da75a5b0ecb83d57d02d531c276bf9c5b580)
- Generate tsconfig for legacy modules [`30dc1a1`](https://github.com/Desdaemon/odoo-lsp/commit/30dc1a115b4964417963657e10efa38dea81ff67)
- Complete mixins' fields [`49f721e`](https://github.com/Desdaemon/odoo-lsp/commit/49f721e61d36d66252af7cb7dd04243cb9e2b6c6)
- Optimize fields completion [`aeec586`](https://github.com/Desdaemon/odoo-lsp/commit/aeec586b0ce5c9f99a8ea50ea5933acfca277b70)
- Completion for fields in write(), create() [`dd40f0b`](https://github.com/Desdaemon/odoo-lsp/commit/dd40f0b426ff86d745b53a0751c979df5ebaeada)
- Field hover for mapped access [`25ef8fe`](https://github.com/Desdaemon/odoo-lsp/commit/25ef8fe69491e801b8c5f346a5277b893725c4dd)
- Init command [`3871f1f`](https://github.com/Desdaemon/odoo-lsp/commit/3871f1f7616b5fd82987c57bf95f12020f6b6d68)
- Goto-definitions for mapped access [`8c6ef47`](https://github.com/Desdaemon/odoo-lsp/commit/8c6ef478d93e9f323d0089bf60e9ff242320d17b)
- Template refs, goto-defs for components [`ccb2346`](https://github.com/Desdaemon/odoo-lsp/commit/ccb23463b7e96013c6742f4ca6dc8f554242393b)
- Memory usage report [`0fe1a04`](https://github.com/Desdaemon/odoo-lsp/commit/0fe1a045ae0b7c3fc624030f9f25266526951959)
- **analyze:** Reveal types in forms [`8107c68`](https://github.com/Desdaemon/odoo-lsp/commit/8107c6806c40fe80eacacb9dcfa91b21093625b5)
- Basic python diagnostics [`b74bd11`](https://github.com/Desdaemon/odoo-lsp/commit/b74bd11f3f1ca17fba4af7ce6c3c2e4a8c0eb522)
- Pull-based diagnostics [`abe6e99`](https://github.com/Desdaemon/odoo-lsp/commit/abe6e9939d3e26cd5ac4d289fafd0bf38ad80a4c)
- **index:** Process \_inherits inheritance [`2acc76b`](https://github.com/Desdaemon/odoo-lsp/commit/2acc76bcfe7ad01b207fbe652b4448808b2ba49b)

### Fixed

- Deadlock when completing fields for self [`5cbf44c`](https://github.com/Desdaemon/odoo-lsp/commit/5cbf44cd39e59e387cdff98b5275f37a2fc9156c)
- Module detection from path [`265fa2f`](https://github.com/Desdaemon/odoo-lsp/commit/265fa2fccf56479d76c000d8df48f8332fa8ff30)
- Properly parse module aliases [`82f25c1`](https://github.com/Desdaemon/odoo-lsp/commit/82f25c1e85f05b5b87ea4c95a1bd40aa64c3f6e9)
- Deadlock when completing related= [`729ae55`](https://github.com/Desdaemon/odoo-lsp/commit/729ae551ccf53ace79eaaab920f2744163a41626)
- Revert root scanning mechanism [`1bee611`](https://github.com/Desdaemon/odoo-lsp/commit/1bee611b44bf3315e2cd6b41fa985a8109d64a63)
- Edge case with base class fields not resolved [`9d0429d`](https://github.com/Desdaemon/odoo-lsp/commit/9d0429d6a749f491f3b49b2796062ea8026d7a8a)
- Allow duplicate module declarations [`f542bc1`](https://github.com/Desdaemon/odoo-lsp/commit/f542bc1b4baa946c5fb80fc11f3129d76ae3b40b)
- Don't include definition in template refs [`4e20ba3`](https://github.com/Desdaemon/odoo-lsp/commit/4e20ba37172f1bcd4927e40c319facfbc2044508)
- Relax \_name requirement for completion [`65d2990`](https://github.com/Desdaemon/odoo-lsp/commit/65d29909814b66cc0c8dc409f3e605989577fd2b)
- Fallback to clients not supporting pull-diags [`33dc4e6`](https://github.com/Desdaemon/odoo-lsp/commit/33dc4e6db2f56011778103d28fe96785966c6d02)
- Confusion between character and byte offsets [`2daa160`](https://github.com/Desdaemon/odoo-lsp/commit/2daa16029f7b3ca5002811e6d0d21096f5d7b7af)
- Allow more model builtins [`3b0e4b3`](https://github.com/Desdaemon/odoo-lsp/commit/3b0e4b334003fbfc7b45da6a68533985c17ccbd5)
- Include identifier name in hover [`b2d45cd`](https://github.com/Desdaemon/odoo-lsp/commit/b2d45cd8a8a39f61c29a78d8b546c7f96d35f76a)
- Preserve fields after saving [`c77d0d0`](https://github.com/Desdaemon/odoo-lsp/commit/c77d0d0e97de29d9b0b221020dc6e44ca95e3767)
- Clear diagnostics on close [`235f3bd`](https://github.com/Desdaemon/odoo-lsp/commit/235f3bd5e4048e8397c3ecc0284cbb963fda3d0c)

### Refactor

- Analyzer [`4fa3c4f`](https://github.com/Desdaemon/odoo-lsp/commit/4fa3c4f841cfc193ccb36582c47c079b3e6760e3)
- Limit usage of CharRange [`4ed16ba`](https://github.com/Desdaemon/odoo-lsp/commit/4ed16bae8a2b88fd5606f3d0c831d239c8b7349c)

## [0.2.1](https://github.com/Desdaemon/odoo-lsp/releases/tag/v0.2.1) - 2023-11-07

Compare: [`v0.2.0...v0.2.1`](https://github.com/Desdaemon/odoo-lsp/compare/v0.2.0...v0.2.1)

### Added

- Menuitem [`2d51e8b`](https://github.com/Desdaemon/odoo-lsp/commit/2d51e8bbe669ccec8c85dfbb41a9a06f6067b5aa)
- Qweb templates [`71dc1f0`](https://github.com/Desdaemon/odoo-lsp/commit/71dc1f05d7794061e94fe2992bd3e95b4d526b4b)
- Mapped access (api.depends etc.) [`5c20257`](https://github.com/Desdaemon/odoo-lsp/commit/5c2025742ffa83f2acb4a8e612f26a584ae1dae5)
- Mapper functions (mapped, filtered etc.) [`d35008d`](https://github.com/Desdaemon/odoo-lsp/commit/d35008d5dbcd10a11dd112dc02394b242db6265f)

### Fixed

- **index:** Out-of-order \_name and \_inherit [`54d7623`](https://github.com/Desdaemon/odoo-lsp/commit/54d762384ff8deeaca5682eff38511fe6ee27950)
- Allow matching dangling decorators [`ce34fc1`](https://github.com/Desdaemon/odoo-lsp/commit/ce34fc1dcd49965aa12ec66b0a43b02a85724980)
- Actually limit num. of completion items [`4a0fafa`](https://github.com/Desdaemon/odoo-lsp/commit/4a0fafa6880ff2b52017a276b261623f396f13b6)
- **analyze:** Out-of-order meta fields [`326ace2`](https://github.com/Desdaemon/odoo-lsp/commit/326ace25b828e818e82861edd654703e54497322)
- Allow \_inherit = ['..'] as primary model name [`ff1878b`](https://github.com/Desdaemon/odoo-lsp/commit/ff1878b670f5f2d7954beb0c14d958dc9de98b05)
- **analyze:** Api.constrains don't complete dotted [`0b55f35`](https://github.com/Desdaemon/odoo-lsp/commit/0b55f352711537aad186fcf5c2430b541a0ffa63)

## [0.2.0](https://github.com/Desdaemon/odoo-lsp/releases/tag/v0.2.0) - 2023-10-04

Compare: [`v0.1.2...v0.2.0`](https://github.com/Desdaemon/odoo-lsp/compare/v0.1.2...v0.2.0)

### Added

- Resolve types of `super()` [`ef98f6b`](https://github.com/Desdaemon/odoo-lsp/commit/ef98f6bb932c94c5427e69c883075d7e44a694ab)
- Hover arbitrary expressions [`e39e649`](https://github.com/Desdaemon/odoo-lsp/commit/e39e649b5d268d057d208a575060059b8a7aebbf)
- Update models on save [`30f1687`](https://github.com/Desdaemon/odoo-lsp/commit/30f16873d5bbf1f5f1d44192890733bad00d4555)

### Documentation

- Add nvim (lsp-zero) setup [`1e36d89`](https://github.com/Desdaemon/odoo-lsp/commit/1e36d89895bf0035ab23d8243765fe31ef6d6049)

### Fixed

- **ci:** Run unit tests [`cbebeca`](https://github.com/Desdaemon/odoo-lsp/commit/cbebecaeac881eef6224775fd25c411ac5403d2a)
- Broken queries [`ea4fca8`](https://github.com/Desdaemon/odoo-lsp/commit/ea4fca88d33fdfe40a4491464d67d9e1f69549a8)
- Model indexing [`5b02082`](https://github.com/Desdaemon/odoo-lsp/commit/5b02082079e6879d4c17cbe6a6a438ba4177684e)
- **ci:** Make nightly releases linear [`44828b5`](https://github.com/Desdaemon/odoo-lsp/commit/44828b5399eabd99e25d0c1af569f921c69918d8)
- Edge case with completion query [`f3a4c60`](https://github.com/Desdaemon/odoo-lsp/commit/f3a4c60827bc8e1beadfe2c2adf8b651b2bcafa1)
- Remove block_on and make all futures Send [`118664d`](https://github.com/Desdaemon/odoo-lsp/commit/118664d0c6e8bb41f800f028c065a6cbcbf59f30)
- Open/close capabilities [`6b259f6`](https://github.com/Desdaemon/odoo-lsp/commit/6b259f65afad502c3e9f1c4c1d817db6bf802451)
- Wrong path splitting [`8444188`](https://github.com/Desdaemon/odoo-lsp/commit/84441880a690da8a15fc14696da086c2d022764f)

### Refactor

- Unify queries [`0aa67ba`](https://github.com/Desdaemon/odoo-lsp/commit/0aa67ba715f59380a36a5d27f0b2aeb8421ed390)
- Backend [`4a697bd`](https://github.com/Desdaemon/odoo-lsp/commit/4a697bd9b8479cfb18338551b06acadec597c6bc)

## [0.1.2](https://github.com/Desdaemon/odoo-lsp/releases/tag/v0.1.2) - 2023-09-07

Compare: [`v0.1.1...v0.1.2`](https://github.com/Desdaemon/odoo-lsp/compare/v0.1.1...v0.1.2)

### Added

- Change completion markup [`0c187a3`](https://github.com/Desdaemon/odoo-lsp/commit/0c187a355c3108fedaf1c8a83c95e9ddea1e87b3)
- Python field completions [`3dd6a23`](https://github.com/Desdaemon/odoo-lsp/commit/3dd6a23fb25b7b987fbda865bbe47d0858e55c84)
- Goto definitions for python fields [`a565bca`](https://github.com/Desdaemon/odoo-lsp/commit/a565bca75786fa5e68198453217e1713276f0d35)
- Completions for python models/fields [`db656e4`](https://github.com/Desdaemon/odoo-lsp/commit/db656e4bbc5748e790bfec828d54ad39fc57aa8c)

### Fixed

- **ci:** Prevent nightly build gap [`cdd5a9c`](https://github.com/Desdaemon/odoo-lsp/commit/cdd5a9c798c71852388b1edc2018efc4ae1eee8b)
- More robust field completions [`dcddf54`](https://github.com/Desdaemon/odoo-lsp/commit/dcddf54b4b774517b01cacec2349ac02950af4a8)
- **manifest:** Do not include non-Rust files [`2dc129f`](https://github.com/Desdaemon/odoo-lsp/commit/2dc129f42ff67cef61dac4da06f32988ab06c7a1)
- **ci:** Attempt to fix prerelease flag [`a0ab8c9`](https://github.com/Desdaemon/odoo-lsp/commit/a0ab8c9ef5f6a3e6dd146effbd646272fe8e0272)
- Skip error msg if not 404 [`4f9fedf`](https://github.com/Desdaemon/odoo-lsp/commit/4f9fedf6b4bd721a3f6581a6ff65f9317b52a4b0)
- Syntax error in query [`d9c13d8`](https://github.com/Desdaemon/odoo-lsp/commit/d9c13d8b962b66bf4740a651ab0f1ecfade265fe)
- Wrong binding for list comprehension [`75c9ca5`](https://github.com/Desdaemon/odoo-lsp/commit/75c9ca572c3f6dd0d850b8a22bb6bcda7a824fd7)
- Wrong capture indices [`c4e12d4`](https://github.com/Desdaemon/odoo-lsp/commit/c4e12d445e142cf2c06503517f273fde412571ee)

### Refactor

- Rename fields [`93d607a`](https://github.com/Desdaemon/odoo-lsp/commit/93d607a1a77cd22ace836f98bc10e7a8eb0117b4)
- More interning [`8f14448`](https://github.com/Desdaemon/odoo-lsp/commit/8f14448e5127d3d541a114bf18b0ff808e29da48)
- Change hover message [`8584164`](https://github.com/Desdaemon/odoo-lsp/commit/8584164202ba5ef89ca2c971cc39bf9a9563f355)
- Static capture indices [`016720b`](https://github.com/Desdaemon/odoo-lsp/commit/016720b0594be769c4588e0d9ea3b7379c7a3918)
- Lsp range [`1322076`](https://github.com/Desdaemon/odoo-lsp/commit/1322076823146a521d4c3b9f570a5d507424fcf0)
- Inline remaining queries [`b6aca67`](https://github.com/Desdaemon/odoo-lsp/commit/b6aca677bb1aaeaa459b9a0d0649e459f6a3d1ac)

## [0.1.1](https://github.com/Desdaemon/odoo-lsp/releases/tag/v0.1.1) - 2023-08-31

Compare: [`v0.1.0-dev.3...v0.1.1`](https://github.com/Desdaemon/odoo-lsp/compare/v0.1.0-dev.3...v0.1.1)

### Added

- Multi-inheritance [`004d33c`](https://github.com/Desdaemon/odoo-lsp/commit/004d33ceb0d67650c19762db40c0968e0d9532db)
- Field completions [`dbc5abb`](https://github.com/Desdaemon/odoo-lsp/commit/dbc5abb0a92411e7429396081918d963d6bfed71)
- Field references [`b0691e6`](https://github.com/Desdaemon/odoo-lsp/commit/b0691e6be481885ecde3298ed60e6fa5459465d1)
- Resolve details for field names [`f6cd07c`](https://github.com/Desdaemon/odoo-lsp/commit/f6cd07cdbbc71b7339af17450fcc32c3884b7d7b)
- Request.render [`b51ac1d`](https://github.com/Desdaemon/odoo-lsp/commit/b51ac1dd4dcd870868ca801dcb45ab718f28cd3b)
- Prefer nightly and version override [`3e8c5c6`](https://github.com/Desdaemon/odoo-lsp/commit/3e8c5c6f26799750c5d430b81359a9fe86958c10)

### Documentation

- Fields demo [`b137603`](https://github.com/Desdaemon/odoo-lsp/commit/b137603b7779e3f11ffd81beae744aa20fc6d00f)

### Fixed

- Propery parse base classes [`43af733`](https://github.com/Desdaemon/odoo-lsp/commit/43af733525dcbcf4b397633f45380a0fc1d499b3)
- Allow abstract models [`d39afbb`](https://github.com/Desdaemon/odoo-lsp/commit/d39afbb092dd37f09168e97c7a31eba2d9067cb7)
- Inheritance with mixins [`1f911f0`](https://github.com/Desdaemon/odoo-lsp/commit/1f911f02adc04b75c303e421f93e93ece01ad1b0)

### Refactor

- Replace FastStr with ImStr [`f6213a0`](https://github.com/Desdaemon/odoo-lsp/commit/f6213a010a4fa2e60a084041906cca2fd22f6518)
- Manage memory usage [`2994b6b`](https://github.com/Desdaemon/odoo-lsp/commit/2994b6b7b631e66a1c0a4245acd5d05f952dbc53)
- Model_fields [`edffd24`](https://github.com/Desdaemon/odoo-lsp/commit/edffd2460bbe4f2dbad1df0f0f6cbe87ec211f1e)
- Intern most strings [`c75f77d`](https://github.com/Desdaemon/odoo-lsp/commit/c75f77d1d70738d01658d67fd05acddf7a9e1d3d)
- More interning, fix model fields parsing [`991a7ae`](https://github.com/Desdaemon/odoo-lsp/commit/991a7aed04237dce441a06b7b164021da74d724b)

## [0.1.0-dev.3](https://github.com/Desdaemon/odoo-lsp/releases/tag/v0.1.0-dev.3) - 2023-08-27

Compare: [`v0.1.0.pre-2...v0.1.0-dev.3`](https://github.com/Desdaemon/odoo-lsp/compare/v0.1.0.pre-2...v0.1.0-dev.3)

### Added

- Xml-id references [`2722a4d`](https://github.com/Desdaemon/odoo-lsp/commit/2722a4d4af2b4c9c75fcbcfa478dbf783595c041)
- Create fast indices [`51de4d1`](https://github.com/Desdaemon/odoo-lsp/commit/51de4d1c114400642fa0fa3d950ea48c19267da4)

### Fixed

- Wrong cwd for odoo-lsp [`97dd932`](https://github.com/Desdaemon/odoo-lsp/commit/97dd9328c534dda3eda5888cc871e760b4534b3b)

### Refactor

- Common gather loop for xml [`368b79a`](https://github.com/Desdaemon/odoo-lsp/commit/368b79a19b5cd153dc13b71a1ffcb5882beab08d)

### Testing

- Isolate tests with miri [`a73b048`](https://github.com/Desdaemon/odoo-lsp/commit/a73b0485bb99fdf643a980bade21ed5e476318aa)

## [0.1.0.pre-2](https://github.com/Desdaemon/odoo-lsp/releases/tag/v0.1.0.pre-2) - 2023-08-25

Compare: [`v0.1.0.pre-1...v0.1.0.pre-2`](https://github.com/Desdaemon/odoo-lsp/compare/v0.1.0.pre-1...v0.1.0.pre-2)

### Added

- Completions for `ref` (todo) [`91bd2a6`](https://github.com/Desdaemon/odoo-lsp/commit/91bd2a6614b9f07c733e3441459b38fe0e406722)
- Prefix foreign modules in `ref` completions [`eba6fa0`](https://github.com/Desdaemon/odoo-lsp/commit/eba6fa06aad7b0041dcacd533effaa9a70c1c046)
- Only index ir.ui.view records [`3688fc8`](https://github.com/Desdaemon/odoo-lsp/commit/3688fc8c8aae455e49b0f82801032934c3ce4f72)
- Completions for template inherit_id [`2f091dc`](https://github.com/Desdaemon/odoo-lsp/commit/2f091dc64fb2a0bc8aa208973516c546d2e9ff1c)
- Goto definition for template inherit_id [`58f86cf`](https://github.com/Desdaemon/odoo-lsp/commit/58f86cfe51bcf2b50aae78b9855eb178af2a66f8)
- Catch panics (todo) [`8eb1d9a`](https://github.com/Desdaemon/odoo-lsp/commit/8eb1d9a81cb6c025fea7e705113b2813460ca60d)
- Parse some configuration [`9f299b8`](https://github.com/Desdaemon/odoo-lsp/commit/9f299b8b802e716278390afee4ba00e116411c9f)
- Allow local config via .odoo_lsp [`2735805`](https://github.com/Desdaemon/odoo-lsp/commit/2735805bb33d211055ff16a2e591f5ff855643b6)
- Completions for \*.env.ref() [`2cdf5f6`](https://github.com/Desdaemon/odoo-lsp/commit/2cdf5f69de33ce0d1f53d50f0c5f66c63e9c4b57)
- Completions for env.ref() [`266b291`](https://github.com/Desdaemon/odoo-lsp/commit/266b291ec47173e0fe9262f0c1e5680f4cde5b2a)
- Goto-definitions for env.ref [`a8c8744`](https://github.com/Desdaemon/odoo-lsp/commit/a8c8744cd3c21a5506da112f1646f1a3d6041856)
- Catch panics [`8ffc7ba`](https://github.com/Desdaemon/odoo-lsp/commit/8ffc7bab5eea04fb28cc66f4956603505287e0bf)
- Index models [`b12cd5e`](https://github.com/Desdaemon/odoo-lsp/commit/b12cd5e3d00afb93512f8d8fd4f613afa4618ed9)
- Complete model names [`056d248`](https://github.com/Desdaemon/odoo-lsp/commit/056d248a8126257e84e7863d0b39d7bfe831147d)
- Model name references [`fff627d`](https://github.com/Desdaemon/odoo-lsp/commit/fff627d2e76fed9dc91ace078e71811135fa9431)
- Relational fields [`458f570`](https://github.com/Desdaemon/odoo-lsp/commit/458f57053f02e3166b7f409f4b4eb01ae1ca26b0)
- Workspace symbols [`76bfeab`](https://github.com/Desdaemon/odoo-lsp/commit/76bfeab4b833f871e16b2433de7fd4809ad34338)
- Extension downloads prebuilt binaries [`ce50fdf`](https://github.com/Desdaemon/odoo-lsp/commit/ce50fdf9347a36b59ebc70bf3b8cdd5b04703c1d)
- Model completion/references in xmls [`428f17d`](https://github.com/Desdaemon/odoo-lsp/commit/428f17dcd6efa44b9eaaebb26cb18622d0ecd50d)

### Documentation

- Add demo [`fe738ab`](https://github.com/Desdaemon/odoo-lsp/commit/fe738ab5a4d8144795c321987eeba12d7133e669)
- Add template screencast [`8ac427e`](https://github.com/Desdaemon/odoo-lsp/commit/8ac427e9410aec598b9f525631ca7e92bbaa460a)
- Update install instructions [`44f0479`](https://github.com/Desdaemon/odoo-lsp/commit/44f0479462478e25f64cde040754a141c8c9f034)
- Add env.ref demo [`e42ab55`](https://github.com/Desdaemon/odoo-lsp/commit/e42ab5576268114de3ca4ecb17298dd8ccb553fa)
- Update readme [`1241b18`](https://github.com/Desdaemon/odoo-lsp/commit/1241b1883a24ca1880fba24cdac2112a8c53a4f9)

### Fixed

- Record_ranges desync w/ module_index [`1f48b36`](https://github.com/Desdaemon/odoo-lsp/commit/1f48b3637539c1f42d453a59fafee54ee98b5806)
- Add roots on init + out-of-root modules [`1c1103a`](https://github.com/Desdaemon/odoo-lsp/commit/1c1103ab36d22c87c455a41ad4d33088a6ec4d17)
- Broken badge on readme [`b588818`](https://github.com/Desdaemon/odoo-lsp/commit/b588818995dee47d4e93d58af19431baf795f8da)
- **ci:** Empty SHA [`b7eb228`](https://github.com/Desdaemon/odoo-lsp/commit/b7eb228fe321ff95c09fec8c1212d08d2a7cd73a)
- Ignore nested records [`5bd3553`](https://github.com/Desdaemon/odoo-lsp/commit/5bd35535bd04bec62b5194f470434d81ff0e50d4)
- Parse more incomplete xml [`8082a53`](https://github.com/Desdaemon/odoo-lsp/commit/8082a53a4828e463356cd8d87878eb0d886b4001)
- No completions when deleting text [`3ca6266`](https://github.com/Desdaemon/odoo-lsp/commit/3ca6266b4324fae2ff2c9973a9e68716cbe96af2)

### Refactor

- Pass most Ropes by value [`c4104ab`](https://github.com/Desdaemon/odoo-lsp/commit/c4104ab3818b8c693322e2ff8b4cc5e132b5d42e)
- Compare by str [`f8f76d0`](https://github.com/Desdaemon/odoo-lsp/commit/f8f76d06cafa0b1bd3c40adc4b40006f100e74d2)
- Move query to separate file [`5e3511e`](https://github.com/Desdaemon/odoo-lsp/commit/5e3511e115d605b8ab0ed54618dc7b194a0ad6a7)
- Separate functions by language [`9719d50`](https://github.com/Desdaemon/odoo-lsp/commit/9719d50a1326dea1aae5101b3cd4c59458d6e382)

<!-- generated by git-cliff -->
