# 포지션 검토 카드 pilot v1 — one-time development validation

## 범위와 방법

이 문서는 자동 테스트가 아니라 pilot을 작성할 때 한 번 수행한 reviewer-only development record다. `python-chess 1.11.2`로 각 source-book `primary.pgn`을 읽고, 선택한 mainline node까지 재생했다. 각 카드에서 다음을 확인했다.

1. card의 Standard FEN이 해당 PGN에서 reviewed move **직전**의 board FEN과 일치한다.
2. 그 위치에서 reviewed SAN/UCI가 legal move다.
3. 그 move는 별도로 만든 변형이 아니라 원 `primary.pgn`의 다음 mainline move와 일치한다.

이 FEN-before/SAN/UCI/next-mainline 확인은 reviewer-only derivation이다. `next mainline move`와 어떤 short context도 system-facing `fronts.md`에 기록하거나 검증 대상으로 주장하지 않는다. 엔진, Tablebase/Syzygy, 웹 자료는 사용하지 않았다. source corpus와 `source-index.json`은 읽기 전용으로 유지했다.

## 카드별 legality 및 provenance

| Card | Source book directory / primary.pgn locator | Source locator (document_id; 1-based PDF pages) | FEN before move | SAN/UCI legal and next mainline move | Result |
| --- | --- | --- | --- | --- | --- |
| CARD-001 | `ai-revolution`, record 1, ply 27 / 14.White | `ref-dc427a9593ab48248d19627b245fafaa`; 203-209 | matched | `e5` / `e4e5` matched | pass |
| CARD-002 | `ai-revolution`, record 5, ply 64 / 32.Black | `ref-dc427a9593ab48248d19627b245fafaa`; 503-508 | matched | `Re8` / `f8e8` matched | pass |
| CARD-003 | `basman-williams`, record 2, ply 18 / 9.Black | `ref-009749ddb7ef4fe0a033b33e9712fc74`; 10-12 | matched | `g5` / `g6g5` matched | pass |
| CARD-004 | `basman-williams`, record 5, ply 31 / 16.White | `ref-009749ddb7ef4fe0a033b33e9712fc74`; 28-29 | matched | `Rh4` / `h1h4` matched | pass |
| CARD-005 | `modern-benoni`, record 2, ply 34 / 17.Black | `ref-35c04c3495434a6290c20ee4712660a6`; 99-105 | matched | `Qd7` / `c7d7` matched | pass |
| CARD-006 | `modern-benoni`, record 4, ply 48 / 24.Black | `ref-35c04c3495434a6290c20ee4712660a6`; 241-250 | matched | `g4` / `g5g4` matched | pass |
| CARD-007 | `modernized-benko`, record 1, ply 17 / 9.White | `ref-edf868ccf0a3425488f55ef6b4b08acf`; 59-62 | matched | `Nf3` / `g1f3` matched | pass |
| CARD-008 | `modernized-benko`, record 4, ply 22 / 11.Black | `ref-edf868ccf0a3425488f55ef6b4b08acf`; 130-137 | matched | `Nxe4` / `f6e4` matched | pass |
| CARD-009 | `positional-sacrifices`, record 3, ply 53 / 27.White | `ref-bc7b3f1ff52f4c7a9940dad53cd6b975`; 315-320 | matched | `Rd5` / `d1d5` matched | pass |
| CARD-010 | `positional-sacrifices`, record 5, ply 33 / 17.White | `ref-bc7b3f1ff52f4c7a9940dad53cd6b975`; 452-456 | matched | `Qf4` / `d4f4` matched | pass |

Summary: reviewer-only **10/10** FEN-before, legality, and next-mainline provenance derivations passed.

## System-facing front payload audit

`fronts.md`는 각 카드에 opaque `CARD-001`–`CARD-010` heading과 정확히 세 본문 줄만 둔다: Standard FEN, side to move, reviewed SAN/UCI. 이 audit은 10개 section 모두가 그 shape에 맞음을 확인했다.

| Leakage check in `fronts.md` | Result |
| --- | --- |
| Source directory, document locator, game/fragment identity, result, branch | 0 occurrences; reviewer-only back/validation에만 보존 |
| `primary.pgn`, record, ply, source/retrieval metadata | 0 occurrences |
| Pre-move context and post-reviewed-move mainline continuation | 0 plies; context field 없음 |
| Extra per-card body fields beyond FEN / side / SAN-UCI | 0 |
| Card headings and permitted payload fields | 10 cards, 10/10 shape pass |

## Coverage and separation checks

| Check | Recorded result |
| --- | --- |
| `fronts.md` cards | CARD-001 through CARD-010; 10 |
| System-facing front body fields | 3 per card (FEN, side, SAN/UCI); 10/10 |
| `backs.md` cards | CARD-001 through CARD-010; 10 |
| Front/back card-ID correspondence | 10:10, one-to-one |
| Card-to-source locator correspondence | 10:10, one-to-one (table above) |
| Cards per book directory | 2 each for all five directories |
| Production/runtime/test callers | 0; this packet is static reviewer documentation only |

## Fragment handling

CARD-007 and CARD-008 come from `modernized-benko` records marked `[Fragment "true"]` with PGN result `*`. Both replay from the standard initial position; neither requires a reconstructed FEN. Their cards intentionally concern only the stored local line. They do not claim a complete historical score, game result, or endgame conversion, and they are ineligible as a runtime/test oracle.

## Unresolved ambiguity record

- Selected ten move sequences: no unresolved move-level ambiguity.
- CARD-007 and CARD-008 have intentionally uncredited book-analysis identities and no historical result. Those absent facts are retained as unknown rather than reconstructed.
- The source corpus records one broader, excluded issue: Pantelic — Nestorovic (Modernized Benko PDF pages 202-203) has internally inconsistent notation. It is not in `primary.pgn`, not on a card, and does not affect any 10/10 result above.
- Some complete-score sources omit exact dates/rounds or a practical termination mechanism. This packet does not infer them; supplied identifying detail remains reviewer-only provenance.
