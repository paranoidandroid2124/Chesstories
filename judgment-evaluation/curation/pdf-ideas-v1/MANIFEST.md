# PDF ideas v1 manifest

This manifest inventories the 15 book artifacts in this corpus. Byte sizes and SHA-256 values are computed from the stored artifact bytes. This manifest is intentionally not self-hashed.

Source identity authority: [source-index.json](../../references/source-index.json). The document_id values below are the only source identities used by this corpus.

## Document, primary, reserve, and stratum counts

| Artifact directory | document_id | Complete source scores | Known-result source slice | Position-line fragments | Primary total | Reserve total |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| ai-revolution | ref-dc427a9593ab48248d19627b245fafaa | 4 | 1 | 0 | 5 | 3 |
| modern-benoni | ref-35c04c3495434a6290c20ee4712660a6 | 5 | 0 | 0 | 5 | 3 |
| positional-sacrifices | ref-bc7b3f1ff52f4c7a9940dad53cd6b975 | 5 | 0 | 0 | 5 | 3 |
| modernized-benko | ref-edf868ccf0a3425488f55ef6b4b08acf | 0 | 0 | 5 | 5 | 3 |
| basman-williams | ref-009749ddb7ef4fe0a033b33e9712fc74 | 5 | 0 | 0 | 5 | 3 |
| Total | 5 document_ids | 19 | 1 | 5 | 25 | 15 |

The one known-result source slice is Lc0 - Stockfish in ai-revolution. The five Modernized Benko entries are fragments and are only eligible for local move/idea-continuation semantic review, never game-end/conversion evidence or complete-game counts.

## Independent PGN parse/replay summary

An independent one-time python-chess pass loaded every primary.pgn record, checked parser errors, and replayed every mainline move for legality from the PGN start position. The Lc0 - Stockfish slice replayed from its stored SetUp/FEN position; all other records replayed from the normal initial position.

| Artifact directory | Records | Mainline plies | Parser errors | Legal replay |
| --- | ---: | ---: | ---: | --- |
| ai-revolution | 5 | 601 | 0 | pass |
| modern-benoni | 5 | 423 | 0 | pass |
| positional-sacrifices | 5 | 349 | 0 | pass |
| modernized-benko | 5 | 175 | 0 | pass |
| basman-williams | 5 | 344 | 0 | pass |
| Total | 25 | 1,892 | 0 | 25/25 pass |

The PGNs contain 6 Fragment true records: the AI Revolution known-result source slice and the five Modernized Benko position-line fragments. Every one of the 25 records has exactly one SourceDocument tag and one SourcePDFPages tag.

## Artifact byte sizes and SHA-256

| Artifact | Bytes | SHA-256 |
| --- | ---: | --- |
| ai-revolution/candidates.md | 10,763 | 91d844b8ede3f0a403718231204bee9b7111560d3233b6be5d86bb8323326e94 |
| ai-revolution/primary.pgn | 4,734 | eae003ebeccba0d48cc9b80767b01f24acbf8b77e2cb6192c87ff646b5f07dee |
| ai-revolution/validation.md | 2,909 | 4a9fbd33bc4fa8a9d9127690ad435fe1ad60f188c5e4eba88db07099ea9f7bd0 |
| basman-williams/candidates.md | 10,744 | 772d6789d9270fc2ec66763dda460fd0b49b2444c9faea2cb8ea60da15e8c81f |
| basman-williams/primary.pgn | 3,136 | 47d3dd5530f2714494e37b61993ee02b49d68e04021420cb0c8e3512d84af561 |
| basman-williams/validation.md | 3,320 | a52120ba9e55aa5200d0e3c580e4f59e4ca6d7eb9855bba6efd283b52407ec85 |
| modern-benoni/candidates.md | 9,883 | 29d96968ab134ea6948c4e5eda8573922fa8fedd74ced1dc418cae078282f6fe |
| modern-benoni/primary.pgn | 3,455 | d8b87fb05b5ead7bf36a10e8bee07baf1686cca2ed25dd6220dab255ea50328d |
| modern-benoni/validation.md | 3,234 | 3a4c072f8592158a32294df640fd55513a84e65ebfbc8a772ac5023eb90c3be6 |
| modernized-benko/candidates.md | 12,033 | 0c8fba76551eb22f1564382c589c27960f54bf684ff8e82efcd521e17622f861 |
| modernized-benko/primary.pgn | 2,099 | 532044a7fe839108600a7245282f4f7986a15d77e50094dcb66ff99188a6481e |
| modernized-benko/validation.md | 4,450 | b3b3b7bf9c55bdedb4cf090ce7cd6d2da5a267582b44bd09935aedc0a91ddd02 |
| positional-sacrifices/candidates.md | 10,875 | 87bef8d531e4d23aaa0f019048849ea93d4e7068fcf91b4f9c936a4b197f464d |
| positional-sacrifices/primary.pgn | 3,222 | c37c519c782f627a3db02fda296d1507c3db0dd314ed5a9716acdf25d06519df |
| positional-sacrifices/validation.md | 3,132 | 5e528548697f204a72be45240f6d50895cf4337647b2aad59138ac2b14d51cfc |
