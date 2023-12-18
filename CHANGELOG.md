# [0.3.0](https://github.com/Desdaemon/odoo-lsp/compare/v0.2.1...v0.3.0) (2023-12-18)


### Bug Fixes

* allow duplicate module declarations ([f542bc1](https://github.com/Desdaemon/odoo-lsp/commit/f542bc1b4baa946c5fb80fc11f3129d76ae3b40b))
* allow more model builtins ([3b0e4b3](https://github.com/Desdaemon/odoo-lsp/commit/3b0e4b334003fbfc7b45da6a68533985c17ccbd5))
* clear diagnostics on close ([235f3bd](https://github.com/Desdaemon/odoo-lsp/commit/235f3bd5e4048e8397c3ecc0284cbb963fda3d0c))
* confusion between character and byte offsets ([2daa160](https://github.com/Desdaemon/odoo-lsp/commit/2daa16029f7b3ca5002811e6d0d21096f5d7b7af))
* deadlock when completing fields for self ([5cbf44c](https://github.com/Desdaemon/odoo-lsp/commit/5cbf44cd39e59e387cdff98b5275f37a2fc9156c))
* deadlock when completing related= ([729ae55](https://github.com/Desdaemon/odoo-lsp/commit/729ae551ccf53ace79eaaab920f2744163a41626))
* don't include definition in template refs ([4e20ba3](https://github.com/Desdaemon/odoo-lsp/commit/4e20ba37172f1bcd4927e40c319facfbc2044508))
* edge case with base class fields not resolved ([9d0429d](https://github.com/Desdaemon/odoo-lsp/commit/9d0429d6a749f491f3b49b2796062ea8026d7a8a))
* fallback to clients not supporting pull-diags ([33dc4e6](https://github.com/Desdaemon/odoo-lsp/commit/33dc4e6db2f56011778103d28fe96785966c6d02))
* include identifier name in hover ([b2d45cd](https://github.com/Desdaemon/odoo-lsp/commit/b2d45cd8a8a39f61c29a78d8b546c7f96d35f76a))
* module detection from path ([265fa2f](https://github.com/Desdaemon/odoo-lsp/commit/265fa2fccf56479d76c000d8df48f8332fa8ff30))
* preserve fields after saving ([c77d0d0](https://github.com/Desdaemon/odoo-lsp/commit/c77d0d0e97de29d9b0b221020dc6e44ca95e3767))
* properly parse module aliases ([82f25c1](https://github.com/Desdaemon/odoo-lsp/commit/82f25c1e85f05b5b87ea4c95a1bd40aa64c3f6e9))
* relax _name requirement for completion ([65d2990](https://github.com/Desdaemon/odoo-lsp/commit/65d29909814b66cc0c8dc409f3e605989577fd2b))
* revert root scanning mechanism ([1bee611](https://github.com/Desdaemon/odoo-lsp/commit/1bee611b44bf3315e2cd6b41fa985a8109d64a63))


### Features

* **analyze:** reveal types in forms ([8107c68](https://github.com/Desdaemon/odoo-lsp/commit/8107c6806c40fe80eacacb9dcfa91b21093625b5))
* basic python diagnostics ([b74bd11](https://github.com/Desdaemon/odoo-lsp/commit/b74bd11f3f1ca17fba4af7ce6c3c2e4a8c0eb522))
* basic support for fields in domains ([a9fcbb5](https://github.com/Desdaemon/odoo-lsp/commit/a9fcbb5a1a0fdc77c3e8613eb2474cb8016e5fbc))
* complete mixins' fields ([49f721e](https://github.com/Desdaemon/odoo-lsp/commit/49f721e61d36d66252af7cb7dd04243cb9e2b6c6))
* completion for fields in write(), create() ([dd40f0b](https://github.com/Desdaemon/odoo-lsp/commit/dd40f0b426ff86d745b53a0751c979df5ebaeada))
* component references for t-name, t-inherit ([1584003](https://github.com/Desdaemon/odoo-lsp/commit/1584003ca520e0f50c12171c0db00c494af85f74))
* field hover for mapped access ([25ef8fe](https://github.com/Desdaemon/odoo-lsp/commit/25ef8fe69491e801b8c5f346a5277b893725c4dd))
* generate tsconfig for legacy modules ([30dc1a1](https://github.com/Desdaemon/odoo-lsp/commit/30dc1a115b4964417963657e10efa38dea81ff67))
* goto-definitions for mapped access ([8c6ef47](https://github.com/Desdaemon/odoo-lsp/commit/8c6ef478d93e9f323d0089bf60e9ff242320d17b))
* index owl components ([e849d7a](https://github.com/Desdaemon/odoo-lsp/commit/e849d7af83831ec16a275c43d8c34ed6c2901a57))
* **index:** process _inherits inheritance ([2acc76b](https://github.com/Desdaemon/odoo-lsp/commit/2acc76bcfe7ad01b207fbe652b4448808b2ba49b))
* init command ([3871f1f](https://github.com/Desdaemon/odoo-lsp/commit/3871f1f7616b5fd82987c57bf95f12020f6b6d68))
* memory usage report ([0fe1a04](https://github.com/Desdaemon/odoo-lsp/commit/0fe1a045ae0b7c3fc624030f9f25266526951959))
* optimize fields completion ([aeec586](https://github.com/Desdaemon/odoo-lsp/commit/aeec586b0ce5c9f99a8ea50ea5933acfca277b70))
* pull-based diagnostics ([abe6e99](https://github.com/Desdaemon/odoo-lsp/commit/abe6e9939d3e26cd5ac4d289fafd0bf38ad80a4c))
* template refs, goto-defs for components ([ccb2346](https://github.com/Desdaemon/odoo-lsp/commit/ccb23463b7e96013c6742f4ca6dc8f554242393b))
* tsconfig command ([1445da7](https://github.com/Desdaemon/odoo-lsp/commit/1445da75a5b0ecb83d57d02d531c276bf9c5b580))



## [0.2.2](https://github.com/Desdaemon/odoo-lsp/compare/v0.2.1...v0.2.2) (2023-12-02)


### Bug Fixes

* deadlock when completing fields for self ([5cbf44c](https://github.com/Desdaemon/odoo-lsp/commit/5cbf44cd39e59e387cdff98b5275f37a2fc9156c))
* deadlock when completing related= ([729ae55](https://github.com/Desdaemon/odoo-lsp/commit/729ae551ccf53ace79eaaab920f2744163a41626))
* module detection from path ([265fa2f](https://github.com/Desdaemon/odoo-lsp/commit/265fa2fccf56479d76c000d8df48f8332fa8ff30))
* properly parse module aliases ([82f25c1](https://github.com/Desdaemon/odoo-lsp/commit/82f25c1e85f05b5b87ea4c95a1bd40aa64c3f6e9))
* revert root scanning mechanism ([1bee611](https://github.com/Desdaemon/odoo-lsp/commit/1bee611b44bf3315e2cd6b41fa985a8109d64a63))


### Features

* basic support for fields in domains ([a9fcbb5](https://github.com/Desdaemon/odoo-lsp/commit/a9fcbb5a1a0fdc77c3e8613eb2474cb8016e5fbc))
* complete mixins' fields ([49f721e](https://github.com/Desdaemon/odoo-lsp/commit/49f721e61d36d66252af7cb7dd04243cb9e2b6c6))
* completion for fields in write(), create() ([dd40f0b](https://github.com/Desdaemon/odoo-lsp/commit/dd40f0b426ff86d745b53a0751c979df5ebaeada))
* component references for t-name, t-inherit ([1584003](https://github.com/Desdaemon/odoo-lsp/commit/1584003ca520e0f50c12171c0db00c494af85f74))
* field hover for mapped access ([25ef8fe](https://github.com/Desdaemon/odoo-lsp/commit/25ef8fe69491e801b8c5f346a5277b893725c4dd))
* generate tsconfig for legacy modules ([30dc1a1](https://github.com/Desdaemon/odoo-lsp/commit/30dc1a115b4964417963657e10efa38dea81ff67))
* index owl components ([e849d7a](https://github.com/Desdaemon/odoo-lsp/commit/e849d7af83831ec16a275c43d8c34ed6c2901a57))
* init command ([3871f1f](https://github.com/Desdaemon/odoo-lsp/commit/3871f1f7616b5fd82987c57bf95f12020f6b6d68))
* optimize fields completion ([aeec586](https://github.com/Desdaemon/odoo-lsp/commit/aeec586b0ce5c9f99a8ea50ea5933acfca277b70))
* tsconfig command ([1445da7](https://github.com/Desdaemon/odoo-lsp/commit/1445da75a5b0ecb83d57d02d531c276bf9c5b580))



## [0.2.1](https://github.com/Desdaemon/odoo-lsp/compare/v0.2.0...v0.2.1) (2023-11-07)


### Bug Fixes

* actually limit num. of completion items ([4a0fafa](https://github.com/Desdaemon/odoo-lsp/commit/4a0fafa6880ff2b52017a276b261623f396f13b6))
* allow _inherit = ['..'] as primary model name ([ff1878b](https://github.com/Desdaemon/odoo-lsp/commit/ff1878b670f5f2d7954beb0c14d958dc9de98b05))
* allow matching dangling decorators ([ce34fc1](https://github.com/Desdaemon/odoo-lsp/commit/ce34fc1dcd49965aa12ec66b0a43b02a85724980))
* **analyze:** api.constrains don't complete dotted ([0b55f35](https://github.com/Desdaemon/odoo-lsp/commit/0b55f352711537aad186fcf5c2430b541a0ffa63))
* **analyze:** out-of-order meta fields ([326ace2](https://github.com/Desdaemon/odoo-lsp/commit/326ace25b828e818e82861edd654703e54497322))
* **index:** out-of-order _name and _inherit ([54d7623](https://github.com/Desdaemon/odoo-lsp/commit/54d762384ff8deeaca5682eff38511fe6ee27950))


### Features

* mapped access (api.depends etc.) ([5c20257](https://github.com/Desdaemon/odoo-lsp/commit/5c2025742ffa83f2acb4a8e612f26a584ae1dae5))
* Mapper functions (mapped, filtered etc.) ([d35008d](https://github.com/Desdaemon/odoo-lsp/commit/d35008d5dbcd10a11dd112dc02394b242db6265f))
* menuitem ([2d51e8b](https://github.com/Desdaemon/odoo-lsp/commit/2d51e8bbe669ccec8c85dfbb41a9a06f6067b5aa))
* qweb templates ([71dc1f0](https://github.com/Desdaemon/odoo-lsp/commit/71dc1f05d7794061e94fe2992bd3e95b4d526b4b))



# [0.2.0](https://github.com/Desdaemon/odoo-lsp/compare/v0.1.2...v0.2.0) (2023-10-04)

### Bug Fixes

- broken queries ([ea4fca8](https://github.com/Desdaemon/odoo-lsp/commit/ea4fca88d33fdfe40a4491464d67d9e1f69549a8))
- **ci:** make nightly releases linear ([44828b5](https://github.com/Desdaemon/odoo-lsp/commit/44828b5399eabd99e25d0c1af569f921c69918d8))
- **ci:** run unit tests ([cbebeca](https://github.com/Desdaemon/odoo-lsp/commit/cbebecaeac881eef6224775fd25c411ac5403d2a))
- edge case with completion query ([f3a4c60](https://github.com/Desdaemon/odoo-lsp/commit/f3a4c60827bc8e1beadfe2c2adf8b651b2bcafa1))
- model indexing ([5b02082](https://github.com/Desdaemon/odoo-lsp/commit/5b02082079e6879d4c17cbe6a6a438ba4177684e))
- open/close capabilities ([6b259f6](https://github.com/Desdaemon/odoo-lsp/commit/6b259f65afad502c3e9f1c4c1d817db6bf802451))
- remove block_on and make all futures Send ([118664d](https://github.com/Desdaemon/odoo-lsp/commit/118664d0c6e8bb41f800f028c065a6cbcbf59f30))
- wrong path splitting ([8444188](https://github.com/Desdaemon/odoo-lsp/commit/84441880a690da8a15fc14696da086c2d022764f))

### Features

- hover arbitrary expressions ([e39e649](https://github.com/Desdaemon/odoo-lsp/commit/e39e649b5d268d057d208a575060059b8a7aebbf))
- resolve types of `super()` ([ef98f6b](https://github.com/Desdaemon/odoo-lsp/commit/ef98f6bb932c94c5427e69c883075d7e44a694ab))
- update models on save ([30f1687](https://github.com/Desdaemon/odoo-lsp/commit/30f16873d5bbf1f5f1d44192890733bad00d4555))

## [0.1.2](https://github.com/Desdaemon/odoo-lsp/compare/v0.1.1...v0.1.2) (2023-09-07)

### Features

- change completion markup ([0c187a3](https://github.com/Desdaemon/odoo-lsp/commit/0c187a355c3108fedaf1c8a83c95e9ddea1e87b3))
- completions for python models/fields ([db656e4](https://github.com/Desdaemon/odoo-lsp/commit/db656e4bbc5748e790bfec828d54ad39fc57aa8c))
- goto definitions for python fields ([a565bca](https://github.com/Desdaemon/odoo-lsp/commit/a565bca75786fa5e68198453217e1713276f0d35))
- python field completions ([3dd6a23](https://github.com/Desdaemon/odoo-lsp/commit/3dd6a23fb25b7b987fbda865bbe47d0858e55c84))

### Bug Fixes

- **ci:** attempt to fix prerelease flag ([a0ab8c9](https://github.com/Desdaemon/odoo-lsp/commit/a0ab8c9ef5f6a3e6dd146effbd646272fe8e0272))
- **ci:** prevent nightly build gap ([cdd5a9c](https://github.com/Desdaemon/odoo-lsp/commit/cdd5a9c798c71852388b1edc2018efc4ae1eee8b))
- **manifest:** do not include non-Rust files ([2dc129f](https://github.com/Desdaemon/odoo-lsp/commit/2dc129f42ff67cef61dac4da06f32988ab06c7a1))
- more robust field completions ([dcddf54](https://github.com/Desdaemon/odoo-lsp/commit/dcddf54b4b774517b01cacec2349ac02950af4a8))
- skip error msg if not 404 ([4f9fedf](https://github.com/Desdaemon/odoo-lsp/commit/4f9fedf6b4bd721a3f6581a6ff65f9317b52a4b0))
- syntax error in query ([d9c13d8](https://github.com/Desdaemon/odoo-lsp/commit/d9c13d8b962b66bf4740a651ab0f1ecfade265fe))
- wrong binding for list comprehension ([75c9ca5](https://github.com/Desdaemon/odoo-lsp/commit/75c9ca572c3f6dd0d850b8a22bb6bcda7a824fd7))
- wrong capture indices ([c4e12d4](https://github.com/Desdaemon/odoo-lsp/commit/c4e12d445e142cf2c06503517f273fde412571ee))

## [0.1.1](https://github.com/Desdaemon/odoo-lsp/compare/004d33ceb0d67650c19762db40c0968e0d9532db...v0.1.1) (2023-08-31)

### Features

- field completions ([dbc5abb](https://github.com/Desdaemon/odoo-lsp/commit/dbc5abb0a92411e7429396081918d963d6bfed71))
- field references ([b0691e6](https://github.com/Desdaemon/odoo-lsp/commit/b0691e6be481885ecde3298ed60e6fa5459465d1))
- multi-inheritance ([004d33c](https://github.com/Desdaemon/odoo-lsp/commit/004d33ceb0d67650c19762db40c0968e0d9532db))
- prefer nightly and version override ([3e8c5c6](https://github.com/Desdaemon/odoo-lsp/commit/3e8c5c6f26799750c5d430b81359a9fe86958c10))
- request.render ([b51ac1d](https://github.com/Desdaemon/odoo-lsp/commit/b51ac1dd4dcd870868ca801dcb45ab718f28cd3b))
- resolve details for field names ([f6cd07c](https://github.com/Desdaemon/odoo-lsp/commit/f6cd07cdbbc71b7339af17450fcc32c3884b7d7b))

### Bug Fixes

- allow abstract models ([d39afbb](https://github.com/Desdaemon/odoo-lsp/commit/d39afbb092dd37f09168e97c7a31eba2d9067cb7))
- inheritance with mixins ([1f911f0](https://github.com/Desdaemon/odoo-lsp/commit/1f911f02adc04b75c303e421f93e93ece01ad1b0))
- propery parse base classes ([43af733](https://github.com/Desdaemon/odoo-lsp/commit/43af733525dcbcf4b397633f45380a0fc1d499b3))

## 0.1.0-dev.3 (2023-08-27)

### Features

- allow local config via .odoo_lsp ([2735805](https://github.com/Desdaemon/odoo-lsp/commit/2735805bb33d211055ff16a2e591f5ff855643b6))
- catch panics ([8ffc7ba](https://github.com/Desdaemon/odoo-lsp/commit/8ffc7bab5eea04fb28cc66f4956603505287e0bf))
- catch panics (todo) ([8eb1d9a](https://github.com/Desdaemon/odoo-lsp/commit/8eb1d9a81cb6c025fea7e705113b2813460ca60d))
- complete model names ([056d248](https://github.com/Desdaemon/odoo-lsp/commit/056d248a8126257e84e7863d0b39d7bfe831147d))
- completions for \*.env.ref() ([2cdf5f6](https://github.com/Desdaemon/odoo-lsp/commit/2cdf5f69de33ce0d1f53d50f0c5f66c63e9c4b57))
- completions for `ref` (todo) ([91bd2a6](https://github.com/Desdaemon/odoo-lsp/commit/91bd2a6614b9f07c733e3441459b38fe0e406722))
- completions for env.ref() ([266b291](https://github.com/Desdaemon/odoo-lsp/commit/266b291ec47173e0fe9262f0c1e5680f4cde5b2a))
- completions for template inherit_id ([2f091dc](https://github.com/Desdaemon/odoo-lsp/commit/2f091dc64fb2a0bc8aa208973516c546d2e9ff1c))
- create fast indices ([51de4d1](https://github.com/Desdaemon/odoo-lsp/commit/51de4d1c114400642fa0fa3d950ea48c19267da4))
- extension downloads prebuilt binaries ([ce50fdf](https://github.com/Desdaemon/odoo-lsp/commit/ce50fdf9347a36b59ebc70bf3b8cdd5b04703c1d))
- goto definition for template inherit_id ([58f86cf](https://github.com/Desdaemon/odoo-lsp/commit/58f86cfe51bcf2b50aae78b9855eb178af2a66f8))
- goto-definitions for env.ref ([a8c8744](https://github.com/Desdaemon/odoo-lsp/commit/a8c8744cd3c21a5506da112f1646f1a3d6041856))
- index models ([b12cd5e](https://github.com/Desdaemon/odoo-lsp/commit/b12cd5e3d00afb93512f8d8fd4f613afa4618ed9))
- model completion/references in xmls ([428f17d](https://github.com/Desdaemon/odoo-lsp/commit/428f17dcd6efa44b9eaaebb26cb18622d0ecd50d))
- model name references ([fff627d](https://github.com/Desdaemon/odoo-lsp/commit/fff627d2e76fed9dc91ace078e71811135fa9431))
- only index ir.ui.view records ([3688fc8](https://github.com/Desdaemon/odoo-lsp/commit/3688fc8c8aae455e49b0f82801032934c3ce4f72))
- parse some configuration ([9f299b8](https://github.com/Desdaemon/odoo-lsp/commit/9f299b8b802e716278390afee4ba00e116411c9f))
- prefix foreign modules in `ref` completions ([eba6fa0](https://github.com/Desdaemon/odoo-lsp/commit/eba6fa06aad7b0041dcacd533effaa9a70c1c046))
- relational fields ([458f570](https://github.com/Desdaemon/odoo-lsp/commit/458f57053f02e3166b7f409f4b4eb01ae1ca26b0))
- workspace symbols ([76bfeab](https://github.com/Desdaemon/odoo-lsp/commit/76bfeab4b833f871e16b2433de7fd4809ad34338))
- xml-id references ([2722a4d](https://github.com/Desdaemon/odoo-lsp/commit/2722a4d4af2b4c9c75fcbcfa478dbf783595c041))

### Bug Fixes

- add roots on init + out-of-root modules ([1c1103a](https://github.com/Desdaemon/odoo-lsp/commit/1c1103ab36d22c87c455a41ad4d33088a6ec4d17))
- broken badge on readme ([b588818](https://github.com/Desdaemon/odoo-lsp/commit/b588818995dee47d4e93d58af19431baf795f8da))
- **ci:** empty SHA ([b7eb228](https://github.com/Desdaemon/odoo-lsp/commit/b7eb228fe321ff95c09fec8c1212d08d2a7cd73a))
- ignore nested records ([5bd3553](https://github.com/Desdaemon/odoo-lsp/commit/5bd35535bd04bec62b5194f470434d81ff0e50d4))
- no completions when deleting text ([3ca6266](https://github.com/Desdaemon/odoo-lsp/commit/3ca6266b4324fae2ff2c9973a9e68716cbe96af2))
- parse more incomplete xml ([8082a53](https://github.com/Desdaemon/odoo-lsp/commit/8082a53a4828e463356cd8d87878eb0d886b4001))
- record_ranges desync w/ module_index ([1f48b36](https://github.com/Desdaemon/odoo-lsp/commit/1f48b3637539c1f42d453a59fafee54ee98b5806))
- wrong cwd for odoo-lsp ([97dd932](https://github.com/Desdaemon/odoo-lsp/commit/97dd9328c534dda3eda5888cc871e760b4534b3b))
