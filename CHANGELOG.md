# Changelog

## [0.3.5](https://github.com/elixir-tools/spitfire/compare/v0.3.4...v0.3.5) (2026-02-10)


### Bug Fixes

* operator precedence ([#78](https://github.com/elixir-tools/spitfire/issues/78)) ([0c91f43](https://github.com/elixir-tools/spitfire/commit/0c91f43f4af7fc06a222ab6258d8f37536278db8))

## [0.3.4](https://github.com/elixir-tools/spitfire/compare/v0.3.3...v0.3.4) (2026-02-09)


### Bug Fixes

* align ellipsis operator precedence with elixir ([#96](https://github.com/elixir-tools/spitfire/issues/96)) ([b4f94c9](https://github.com/elixir-tools/spitfire/commit/b4f94c97676dba359c7f6f573005980b13fac4c8))

## [0.3.3](https://github.com/elixir-tools/spitfire/compare/v0.3.2...v0.3.3) (2026-02-08)


### Bug Fixes

* add `:eof` to the terminator list in `parse_stab_expression/1` ([#92](https://github.com/elixir-tools/spitfire/issues/92)) ([8f976ef](https://github.com/elixir-tools/spitfire/commit/8f976ef1432fcb0de5dab08a04aca06b1a969a99))
* better align parsing with elixir's matched/unmatched/no-parens ([#93](https://github.com/elixir-tools/spitfire/issues/93)) ([808783e](https://github.com/elixir-tools/spitfire/commit/808783e6bf838e8821a9fd05c00b072cfa99834b))

## [0.3.2](https://github.com/elixir-tools/spitfire/compare/v0.3.1...v0.3.2) (2026-02-01)


### Bug Fixes

* associativity in bracketless kw list ([#89](https://github.com/elixir-tools/spitfire/issues/89)) ([91b5d5b](https://github.com/elixir-tools/spitfire/commit/91b5d5b38a75408543abb4a908a632622512366d))
* extract newlines only from newline carrying tokens ([#86](https://github.com/elixir-tools/spitfire/issues/86)) ([edba800](https://github.com/elixir-tools/spitfire/commit/edba800af197ebae5d5e9cbb13c26c5f0532a25a))
* improve handling of matched/unmatched expressions ([#88](https://github.com/elixir-tools/spitfire/issues/88)) ([dd45cb1](https://github.com/elixir-tools/spitfire/commit/dd45cb1de39929975ce6e260f0739db8fd5bb5d9))
* keyword list associativity ([#85](https://github.com/elixir-tools/spitfire/issues/85)) ([a7d2a54](https://github.com/elixir-tools/spitfire/commit/a7d2a5487ae0790d0b9c7815e143f89093317275))
* parse ellipsis_op as unary operator ([#83](https://github.com/elixir-tools/spitfire/issues/83)) ([e86f1f4](https://github.com/elixir-tools/spitfire/commit/e86f1f423094739bfa5b54bdd72a97e5c6d1e8c5))
* rearrange `!(left) in right` ([#91](https://github.com/elixir-tools/spitfire/issues/91)) ([fae2579](https://github.com/elixir-tools/spitfire/commit/fae2579b29212a4c7551022897f95a7b4d040461))

## [0.3.1](https://github.com/elixir-tools/spitfire/compare/v0.3.0...v0.3.1) (2026-01-20)


### Bug Fixes

* handle unexpected tokens when parsing stabs ([#76](https://github.com/elixir-tools/spitfire/issues/76)) ([365007b](https://github.com/elixir-tools/spitfire/commit/365007be455bafa0b559579430e440e95a357724))

## [0.3.0](https://github.com/elixir-tools/spitfire/compare/v0.2.1...v0.3.0) (2026-01-19)


### Features

* Handle atom_safe and kw_identifier_safe tokens ([#72](https://github.com/elixir-tools/spitfire/issues/72)) ([c4fd161](https://github.com/elixir-tools/spitfire/commit/c4fd16102f8fb341be9aa77119f480382368af73))


### Bug Fixes

* handle eof inside list-esque constructs ([#67](https://github.com/elixir-tools/spitfire/issues/67)) ([14a2bf3](https://github.com/elixir-tools/spitfire/commit/14a2bf31da53f5b6e4c9d1db539ac2b068b73f7e))
* handle no_fuel_remaining in parse_with_comments ([#73](https://github.com/elixir-tools/spitfire/issues/73)) ([f777bb4](https://github.com/elixir-tools/spitfire/commit/f777bb4badc7c8f89b90b398aba2e3e0cec71c90))
* improve recovery from unexpected semicolons ([#70](https://github.com/elixir-tools/spitfire/issues/70)) ([792e9c9](https://github.com/elixir-tools/spitfire/commit/792e9c9e1c7996767775b50ff09712513d25a755))
* infinite loop parsing incomplete struct ([#66](https://github.com/elixir-tools/spitfire/issues/66)) ([bebd845](https://github.com/elixir-tools/spitfire/commit/bebd84510edf4d6530e673462ef3847d04ca657e))
* parsing crashes on code with errors ([#65](https://github.com/elixir-tools/spitfire/issues/65)) ([b6bbdb4](https://github.com/elixir-tools/spitfire/commit/b6bbdb48859722a35fc1238ef049554fcd23a308))
* recover from incomplete keyword lists ([#71](https://github.com/elixir-tools/spitfire/issues/71)) ([0513a5d](https://github.com/elixir-tools/spitfire/commit/0513a5d98a2760cba189ba7b78b39a1707ca237c))

## [0.2.1](https://github.com/elixir-tools/spitfire/compare/v0.2.0...v0.2.1) (2025-06-03)


### Bug Fixes

* make spitfire compile on elixir 1.13 ([#62](https://github.com/elixir-tools/spitfire/issues/62)) ([5eda84e](https://github.com/elixir-tools/spitfire/commit/5eda84ee8cae1b82c6a0717a54677585f12adfa8))

## [0.2.0](https://github.com/elixir-tools/spitfire/compare/v0.1.5...v0.2.0) (2025-03-07)


### Features

* support 1.18+ metadata ([#58](https://github.com/elixir-tools/spitfire/issues/58)) ([30047e1](https://github.com/elixir-tools/spitfire/commit/30047e1252a210305b197fdd786827efae008f1e)), closes [#49](https://github.com/elixir-tools/spitfire/issues/49)


### Bug Fixes

* correctly match on successful tokenize ([f937a7a](https://github.com/elixir-tools/spitfire/commit/f937a7ab7baa275d3b5b66b25f09ce604a1d10f9))
* parse do blocks after paren_identifiers ([2320bd4](https://github.com/elixir-tools/spitfire/commit/2320bd49b3ec1f8caaa7daa2de078776ba2da5a1))
* reset nesting inside parameterless right stab expressions ([1bf80bc](https://github.com/elixir-tools/spitfire/commit/1bf80bc71c3ed8fadcf833e6ebc04c9c4f1e3614))

## [0.1.5](https://github.com/elixir-tools/spitfire/compare/v0.1.4...v0.1.5) (2025-02-07)


### Bug Fixes

* crash on tokenizer error ([#52](https://github.com/elixir-tools/spitfire/issues/52)) ([2d1bb63](https://github.com/elixir-tools/spitfire/commit/2d1bb63c064f2b2831dfb7d85643909b8d499b4a))

## [0.1.4](https://github.com/elixir-tools/spitfire/compare/v0.1.3...v0.1.4) (2024-12-20)


### Bug Fixes

* address warnings found by elixir 1.18 ([#50](https://github.com/elixir-tools/spitfire/issues/50)) ([434521e](https://github.com/elixir-tools/spitfire/commit/434521e5d4c7721d992c3ca67d8e9a877439cf1c))

## [0.1.3](https://github.com/elixir-tools/spitfire/compare/v0.1.2...v0.1.3) (2024-06-19)


### Bug Fixes

* match error case from `Macro.Env.expand_import/5` ([#45](https://github.com/elixir-tools/spitfire/issues/45)) ([ebd5ba6](https://github.com/elixir-tools/spitfire/commit/ebd5ba6cb05b2df8df97e596e738f3f2b89e035d))

## [0.1.2](https://github.com/elixir-tools/spitfire/compare/v0.1.1...v0.1.2) (2024-06-17)


### Bug Fixes

* typespec for Spitfire.Env ([#43](https://github.com/elixir-tools/spitfire/issues/43)) ([781c108](https://github.com/elixir-tools/spitfire/commit/781c108d2142a3cf0df31e9d6f46c05c54f461d0))

## [0.1.1](https://github.com/elixir-tools/spitfire/compare/v0.1.0...v0.1.1) (2024-06-14)


### Bug Fixes

* include src directory ([63b48a4](https://github.com/elixir-tools/spitfire/commit/63b48a410f6f0ee21847ec91ed4773a319ac6158))

## [0.1.0](https://github.com/elixir-tools/spitfire/compare/spitfire-v0.1.0...spitfire-v0.1.0) (2024-06-14)


### Features

* accept column and line options ([12a1827](https://github.com/elixir-tools/spitfire/commit/12a1827821265170a58e40b5ffd2bb785f789d91))
* accept options ([d2bd1d5](https://github.com/elixir-tools/spitfire/commit/d2bd1d5dfbb6a8e6a7feb76536795f3daa5539e0))
* access syntax ([0372af3](https://github.com/elixir-tools/spitfire/commit/0372af32d276829553ad927874a18455fa335454))
* adds &parse_with_comments/2 function ([#19](https://github.com/elixir-tools/spitfire/issues/19)) ([e90e686](https://github.com/elixir-tools/spitfire/commit/e90e686fd5263290131fd1ba0d952396e9175ee4))
* anon functions ([0548457](https://github.com/elixir-tools/spitfire/commit/0548457b29e94cbf4b5472fafee6630feeb946c0))
* anonymous function capture syntax ([3d553a0](https://github.com/elixir-tools/spitfire/commit/3d553a0abcb48c1f922636ced4b11708f7b3f3bf))
* at expressions ([3a6e179](https://github.com/elixir-tools/spitfire/commit/3a6e179caf29a0af5d3cc321a0611b158491cdfa))
* call anon function ([f47b1e7](https://github.com/elixir-tools/spitfire/commit/f47b1e7daf354205662504fd9aef017f894a8232))
* charlists ([5f17497](https://github.com/elixir-tools/spitfire/commit/5f1749703c06de322870842e9a2ea3629b1b9b49))
* chars, more literal encodings, eoe meta ([80d26f2](https://github.com/elixir-tools/spitfire/commit/80d26f2123d4cbe25d3e4dc87c9aa73e1d3b4998))
* container_cursor_to_quoted/2 ([#33](https://github.com/elixir-tools/spitfire/issues/33)) ([e47385f](https://github.com/elixir-tools/spitfire/commit/e47385f64db19f65b8efdd57d003272376446a4e))
* do blocks, the right way ([c3117a9](https://github.com/elixir-tools/spitfire/commit/c3117a98e300d47e9c26b5db48aca31338dae238))
* do/end token metadata ([bcd839b](https://github.com/elixir-tools/spitfire/commit/bcd839b661c4af2f2864ae58ed27cccf50391828))
* else block in if exprs ([40f815b](https://github.com/elixir-tools/spitfire/commit/40f815b8237adcefb17238ad9213ffe1b39a4807))
* end_of_expression and closing metadata ([5424109](https://github.com/elixir-tools/spitfire/commit/542410981755ceb350ccf9d6e2781a9ff170b291))
* environment querying ([#35](https://github.com/elixir-tools/spitfire/issues/35)) ([e5111c5](https://github.com/elixir-tools/spitfire/commit/e5111c5f862338e742ad643021b07e0b07412257))
* even more error handling ([6359193](https://github.com/elixir-tools/spitfire/commit/635919333a16674a6c304b4c2d9cfca2e03c23af))
* even more more error handling ([e5e7f50](https://github.com/elixir-tools/spitfire/commit/e5e7f50a1ab7524b0dc489815d7fba7c16601c15))
* fixup anon functions ([34af2db](https://github.com/elixir-tools/spitfire/commit/34af2dbf4236177120eb415e7fee3d5ef846223b))
* fuel ([#23](https://github.com/elixir-tools/spitfire/issues/23)) ([af84760](https://github.com/elixir-tools/spitfire/commit/af84760913b8908648eeb7f50ed90bd30ccaa9ed))
* function calls ([b6f3137](https://github.com/elixir-tools/spitfire/commit/b6f31372a325ed277b137c84970f5d2a27a68931))
* function capture ([43f7b71](https://github.com/elixir-tools/spitfire/commit/43f7b713c565c1b6759d019aef4d6907e425592e))
* grouped expressions ([44c87d3](https://github.com/elixir-tools/spitfire/commit/44c87d3e9a98dd4aee089b10bf8ae595f359a0d2))
* handle access syntax on multiple lines ([#10](https://github.com/elixir-tools/spitfire/issues/10)) ([83d739a](https://github.com/elixir-tools/spitfire/commit/83d739ab4c3cb9638a7b2a4c290a47528aedc02a))
* heredoc ([d8c9711](https://github.com/elixir-tools/spitfire/commit/d8c9711112f0298e71ad23863444bf0f1348ff48))
* line and column meta ([6807fd7](https://github.com/elixir-tools/spitfire/commit/6807fd74b5c4af9c07fbda47a5212143a350ba66))
* literal encoder ([72b72fd](https://github.com/elixir-tools/spitfire/commit/72b72fd151f3111070b49196d96528785f6c79ad))
* more access syntax ([e91752a](https://github.com/elixir-tools/spitfire/commit/e91752a5bda6a4481672a04d9128c020f64c1478))
* more error handling ([5e1f96b](https://github.com/elixir-tools/spitfire/commit/5e1f96b41ccc3d8495b761f2fdafa1ab6ae26a54))
* more operators ([96ebe05](https://github.com/elixir-tools/spitfire/commit/96ebe05ebe1c4ff6a8847cc3c9ca0b7ce98864f1))
* multi aliases ([b4d40f0](https://github.com/elixir-tools/spitfire/commit/b4d40f09b7b02f52994e6c4f546bdc6d7a6e0250)), closes [#3](https://github.com/elixir-tools/spitfire/issues/3)
* multi line grouped expressions ([50f6c7c](https://github.com/elixir-tools/spitfire/commit/50f6c7c4946efe0b8de60004cef321fb64549185))
* parse a lot of stuff ([e79f446](https://github.com/elixir-tools/spitfire/commit/e79f446cf3a02c6ec9becb93fbf370524da0613b))
* parse booleans ([dba4466](https://github.com/elixir-tools/spitfire/commit/dba4466007a81e9f90540695276a1a6562be7ed5))
* parse call experssions to allow proper unquote ([6c28852](https://github.com/elixir-tools/spitfire/commit/6c28852dd83287b7038ade1108083f6e864732d3))
* parse floats ([a76cdb0](https://github.com/elixir-tools/spitfire/commit/a76cdb0d3739b8b4d69433f45aded0e6d6299676))
* pipe operator in lists ([226e339](https://github.com/elixir-tools/spitfire/commit/226e33940d6e3cf929ba2cbfa9846c05e4961989))
* power operator ([ad57a2b](https://github.com/elixir-tools/spitfire/commit/ad57a2b37dccdfc10b029d57dd26c0a49f833193))
* quoted atom and match op ([5a80e4b](https://github.com/elixir-tools/spitfire/commit/5a80e4b908652aac05b34c0d6c6910a923bdb7cd))
* rewrite what i had ([4bbacdc](https://github.com/elixir-tools/spitfire/commit/4bbacdcf3473e44e9a942c2085442accae2ec88c))
* right stab expressions ([1723064](https://github.com/elixir-tools/spitfire/commit/17230645277e914ab8d1b5d39d76dc884f4dc65b))
* sigils ([24909d0](https://github.com/elixir-tools/spitfire/commit/24909d02e98fdccbdcb420a3ea583429d9630900))
* some error handling ([a3e8534](https://github.com/elixir-tools/spitfire/commit/a3e8534b524ea239689a05d224e5a8135c696d62))
* string interpolation ([7326458](https://github.com/elixir-tools/spitfire/commit/7326458ba9399d0f0378fa8434922693519b67af))
* structs and special identifiers ([05fe4ed](https://github.com/elixir-tools/spitfire/commit/05fe4ed906d6a44c7fda13dbb48b42f9f6e211c9))
* tuples and left stab expressions ([c036033](https://github.com/elixir-tools/spitfire/commit/c0360339edf0be922da5fe38067947dddf1eecbc))
* unsafe atoms ([4cd820c](https://github.com/elixir-tools/spitfire/commit/4cd820c6d030283d0c05f9781d61b9576501abd8))
* when ([77c0eda](https://github.com/elixir-tools/spitfire/commit/77c0eda2db8e723ead802d7433e07f77bfa5901d))


### Bug Fixes

* 0-arity anon function type spec ([586c087](https://github.com/elixir-tools/spitfire/commit/586c0872457112d35ad923f3ff4a964d3461a520))
* access syntax on dot call ([d2bfb45](https://github.com/elixir-tools/spitfire/commit/d2bfb45d025b86340221f182d08dd4d84e9d6f4b))
* add eoe metadata to 0-arity anon functions ([444b9a2](https://github.com/elixir-tools/spitfire/commit/444b9a2a8a963370eb03f11b205e59c83830f514))
* aliases can start with non alias tokens ([7c557cc](https://github.com/elixir-tools/spitfire/commit/7c557cca3fc183a4693df9e0c3021eb53e2d9ce3)), closes [#5](https://github.com/elixir-tools/spitfire/issues/5)
* ambiguous_op meta ([b3e8ea4](https://github.com/elixir-tools/spitfire/commit/b3e8ea4f7c8e173ecccc32a697b3f190dbeff1a0))
* **bin:** create tmp dir ([#1](https://github.com/elixir-tools/spitfire/issues/1)) ([39cc50b](https://github.com/elixir-tools/spitfire/commit/39cc50b5e6f0ba802b2351ef25667b765a665516))
* block identifiers that take right stabs ([1dbf037](https://github.com/elixir-tools/spitfire/commit/1dbf03789dec778e7d50d68f2fad63de513b7c8c))
* blocks inside paren function calls ([7e61ec2](https://github.com/elixir-tools/spitfire/commit/7e61ec282e9f618266e136cff5679c1fb402e71a)), closes [#11](https://github.com/elixir-tools/spitfire/issues/11)
* bracketless kw list with new line between key and value ([6089f71](https://github.com/elixir-tools/spitfire/commit/6089f71809b039e1e9f169e8ba7ab82c827c9691))
* consume fuel in infix loop ([#24](https://github.com/elixir-tools/spitfire/issues/24)) ([555b977](https://github.com/elixir-tools/spitfire/commit/555b977b75b1058f137d21d727dc21875627a753))
* do blocks inside grouped expressions ([e219362](https://github.com/elixir-tools/spitfire/commit/e219362d803b0df6eb3956b4ba1a25db02b42706))
* don't encode errors directly into AST ([#27](https://github.com/elixir-tools/spitfire/issues/27)) ([df7732f](https://github.com/elixir-tools/spitfire/commit/df7732f00c5a38cb7b52c98a0698e68928dd4581))
* don't include closing meta when paren identifier has error ([0580d2c](https://github.com/elixir-tools/spitfire/commit/0580d2c8b7ebe3fdb3cdc16171f66817152842fc))
* don't include closing meta when paren identifier has error ([c3e1b1d](https://github.com/elixir-tools/spitfire/commit/c3e1b1d113bafc7617f1a6bbdcdea0a643e2dd73))
* elixir-makeup/makeup ([#17](https://github.com/elixir-tools/spitfire/issues/17)) ([db80ec4](https://github.com/elixir-tools/spitfire/commit/db80ec411b0644a6e646861ef0306c2f405df168))
* ellipsis op ([4577b8c](https://github.com/elixir-tools/spitfire/commit/4577b8c37d9c9b386c561a30e1ba9bc660894dd3))
* empty block_identifiers ([#20](https://github.com/elixir-tools/spitfire/issues/20)) ([5953bed](https://github.com/elixir-tools/spitfire/commit/5953bed93e6903c075a589091a3f94097d73a085))
* **env:** reset pdict after expansion ([#36](https://github.com/elixir-tools/spitfire/issues/36)) ([d1db691](https://github.com/elixir-tools/spitfire/commit/d1db691883f0d983a4e6a090e24fe2f0741b403a))
* error handling ([26e68d4](https://github.com/elixir-tools/spitfire/commit/26e68d4b184caf32ccee840ef0e862c911ff51ef))
* fix binding power of when_op ([77db668](https://github.com/elixir-tools/spitfire/commit/77db6686e0b1471caed600c3d7f827bcdb21057b))
* function calls with multi line args ([8888a71](https://github.com/elixir-tools/spitfire/commit/8888a7131691f389b0ba51ae5b5b4c556f4c92a0))
* handle code that begins with comments ([f549fb4](https://github.com/elixir-tools/spitfire/commit/f549fb46a1f264e6af3a6319f7bab5d2b9cebc8f))
* handle op_identifier ([f5b9ba0](https://github.com/elixir-tools/spitfire/commit/f5b9ba0cfbad5367f9c806c0d75402a79aa07497))
* handle prefix operator with newline ([#22](https://github.com/elixir-tools/spitfire/issues/22)) ([4a2fcb6](https://github.com/elixir-tools/spitfire/commit/4a2fcb6914d3dbc50067110a5ec4aea867dc2ed0))
* in match op precedence ([c8ebe2a](https://github.com/elixir-tools/spitfire/commit/c8ebe2aa4ddd12196c8d222d33b171bf0ff0ac85))
* initialize error field in interpolation parser ([8f991b8](https://github.com/elixir-tools/spitfire/commit/8f991b87b0543b7bd6801855a1f1be4e3b4bebb4))
* list as only expr in stab ([5712150](https://github.com/elixir-tools/spitfire/commit/5712150e3b7bd065c00af43cdfe5827d5615cb49))
* literal encoding for kw_identifier and capture ints ([c6b206f](https://github.com/elixir-tools/spitfire/commit/c6b206f3de4d43df612cd9cbddbe0b03327549e5))
* lone do blocks inside other expressions ([8b47e3a](https://github.com/elixir-tools/spitfire/commit/8b47e3a1dc952ed99332e96c3db131b6403ff33d))
* lone range op ([9d631ff](https://github.com/elixir-tools/spitfire/commit/9d631ff63e66bf3261cd21ae6eea6c5d498d8e74))
* more access syntax ([d3ed437](https://github.com/elixir-tools/spitfire/commit/d3ed437b535b94e4fac7c4e147d73ffe423501be))
* more robust multi aliases ([fb1e213](https://github.com/elixir-tools/spitfire/commit/fb1e213d6ce88f94e7687795e115b92d05780686))
* multiline lists ([b36cbf5](https://github.com/elixir-tools/spitfire/commit/b36cbf5223e4128d4cfb23d658bd144453a376e2))
* nested access syntax ([#15](https://github.com/elixir-tools/spitfire/issues/15)) ([7af1625](https://github.com/elixir-tools/spitfire/commit/7af162576a45f555f6c83edc398e94a56cc693ca)), closes [#14](https://github.com/elixir-tools/spitfire/issues/14)
* new line after fn keyword ([bbcf66a](https://github.com/elixir-tools/spitfire/commit/bbcf66acf1905f6f0fee1ab706a3e90457f32b8a))
* newlines in bitstring ([a585cb2](https://github.com/elixir-tools/spitfire/commit/a585cb2835857b595636798bd04726f3798d651f))
* newlines in call expression ([52ec090](https://github.com/elixir-tools/spitfire/commit/52ec090afb2b704b34282ae3c1566ada7eb73f2c))
* newlines inside interpolation ([99eb9ab](https://github.com/elixir-tools/spitfire/commit/99eb9ab5a260d016ece80f2d430ef336479bb040))
* newlines on map dot syntax ([3a6b8fe](https://github.com/elixir-tools/spitfire/commit/3a6b8fe8f381609a9dc26133df6af6fec6a6f6ff))
* newlines on tuples ([774f5c5](https://github.com/elixir-tools/spitfire/commit/774f5c5a8e22257e66a9a5b21aab0626da9269ba))
* parse list-like syntax error with a single item ([#21](https://github.com/elixir-tools/spitfire/issues/21)) ([d39f91d](https://github.com/elixir-tools/spitfire/commit/d39f91d6e66f70cb3093bad4947d7e46bba40b43))
* parsing right stabs ([8587642](https://github.com/elixir-tools/spitfire/commit/85876421cf7344e6d3c73e3f3fbe2ba0997b277d))
* pin operator in struct type ([8e53932](https://github.com/elixir-tools/spitfire/commit/8e53932b9e6934b8e322bcb95f1a44120311b97a))
* pipe operator precedence ([07fab38](https://github.com/elixir-tools/spitfire/commit/07fab386b24861dc422512dfba30a4c44893d2bc))
* precedence for prefix style dual_op ([620ada5](https://github.com/elixir-tools/spitfire/commit/620ada5397207d57f7c8adce3d29bc644f560457))
* precedence of access syntax ([8111a21](https://github.com/elixir-tools/spitfire/commit/8111a21a6c1d67687ff7331ea39714cf38e9d312))
* remaining differences between string_to_quoted ([c67d644](https://github.com/elixir-tools/spitfire/commit/c67d6448afac31f0e9e13c93631347e0a06d7937))
* remove dbg and some comments ([3ab807e](https://github.com/elixir-tools/spitfire/commit/3ab807e489249f41d2b2061b56e0d9d39d9f22b7))
* rest nestings in compound data structures ([957da86](https://github.com/elixir-tools/spitfire/commit/957da86dcef4abbff22753ab970489455d27efad))
* semicolons ([#28](https://github.com/elixir-tools/spitfire/issues/28)) ([f913c60](https://github.com/elixir-tools/spitfire/commit/f913c6025875c9d69b4d35f94cae3e70c7f6320e))
* single element lists ([e655d88](https://github.com/elixir-tools/spitfire/commit/e655d88751b9132bd0b8d646ded4a807178daa19))
* special case capture_int ([bd03ac6](https://github.com/elixir-tools/spitfire/commit/bd03ac6d09b1037ba290d82f0c12ae7c0483f58c))
* special case pipe op for maps ([c05af09](https://github.com/elixir-tools/spitfire/commit/c05af099e6992c7beb84a5f1674e0ac256c703e8))
* stab collection, also more syntax ([e98ed66](https://github.com/elixir-tools/spitfire/commit/e98ed66a01d4ad5422d5a9d5027b55d5cf762cd0))
* stabs within grouped expressions ([b94706f](https://github.com/elixir-tools/spitfire/commit/b94706f0b2290874b3e9b5315023c2bf6dfab71a))
* struct literal with module attr as type ([1a0a6af](https://github.com/elixir-tools/spitfire/commit/1a0a6af3184296cfb8e2d7365e7c8debec17fb1b))
* struct types ([#16](https://github.com/elixir-tools/spitfire/issues/16)) ([1000574](https://github.com/elixir-tools/spitfire/commit/100057499a1629a88af8c38a8d6f9e324cbe3980)), closes [#13](https://github.com/elixir-tools/spitfire/issues/13)
* trailing commas ([f260f97](https://github.com/elixir-tools/spitfire/commit/f260f97b33e4b4e449c7ab6ee63a5ea96edf487c)), closes [#7](https://github.com/elixir-tools/spitfire/issues/7)
* trailing commas in lists ([149c774](https://github.com/elixir-tools/spitfire/commit/149c77496beca60da53795b7612c20e35ed6173d))
* turn grouped !/not expressions into blocks ([6a37c61](https://github.com/elixir-tools/spitfire/commit/6a37c61e884179d1cd1f9acc31aa11108b19e813))
* type/assoc precedence ([62aa23e](https://github.com/elixir-tools/spitfire/commit/62aa23e78299dd111c89d90ece8802e3af1b2f6c))
* unary operator ([9faf653](https://github.com/elixir-tools/spitfire/commit/9faf653b717980257bd128a4dd27af4a1bd7399b))
* unary operator for real ([0be0f08](https://github.com/elixir-tools/spitfire/commit/0be0f0849416b61a1d8032371f4e2fc4b539f1a5))
* unquote in struct type ([ea6dc48](https://github.com/elixir-tools/spitfire/commit/ea6dc485f1efc543350ede3c0ea29cebf700893c))
* unquote_splicing quirks ([a9bce8a](https://github.com/elixir-tools/spitfire/commit/a9bce8a520f31b87be76259fd16a045aed1e157e))
* unsafe keywords ([da37dfd](https://github.com/elixir-tools/spitfire/commit/da37dfd62baf3096d031b06d648ba319e034d362))
* upstream changes to the tokenizer ([#34](https://github.com/elixir-tools/spitfire/issues/34)) ([178b00b](https://github.com/elixir-tools/spitfire/commit/178b00becd55b33e080f23c9ed0d1126d57574be))
* various changes from testing elixir codebase ([6312312](https://github.com/elixir-tools/spitfire/commit/63123122fa2f7be07df418584a5ffff72926ee3f))
* when operator in normal usage ([bd7f244](https://github.com/elixir-tools/spitfire/commit/bd7f244e792efe5a6d0053ee0911b25405feb77b))


### Performance Improvements

* don't use kw for opts ([dc20c07](https://github.com/elixir-tools/spitfire/commit/dc20c07c265923ba861087e2736e4b3bc28447bd))


### Miscellaneous Chores

* release 0.1.0 ([4501ab4](https://github.com/elixir-tools/spitfire/commit/4501ab47e278468b7aa3b130375bc711aaaec073))
