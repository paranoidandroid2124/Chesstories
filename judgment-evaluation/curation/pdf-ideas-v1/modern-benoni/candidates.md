# Modern Benoni PDF curation

## Corpus locator

Source identity authority: [source-index.json](../../../references/source-index.json). This artifact uses `document_id: ref-35c04c3495434a6290c20ee4712660a6`; every locator below is this document ID plus 1-based PDF page(s). No local filesystem path or visual source asset is retained.

## Scope and boundary

This curation uses only the designated John Doknjas PDF. PDF page numbers below are 1-based file pages, not the printed book folios. The explanations are short paraphrases of the book's analytical ideas; they are not engine conclusions or expected-answer labels.

`primary.pgn` contains complete main-game move scores for development curation and semantic review only. No answer strings, label IDs, one-to-one oracle, engine evaluation, schema, or test fixture is created. All five primary scores begin from the normal initial position, are complete rather than fragments, and therefore use no SetUp/FEN header.

## Primary shortlist

| Game identity | PDF pages / core page | Principal curation role |
| --- | --- | --- |
| N.Grandelius - E.Hedman; Swedish Championship, Falun 2012; 0-1 | 47-52 / 48 | Defensive resource under an exchange imbalance |
| Sta.Kovac - N.González Rabago; Correspondence 2015; 1/2-1/2 | 99-105 / 103-104 | Static constraint: pinned knight plus two targets |
| M.Warmerdam - D.Navara; Greek League 2019; 0-1 | 167-175 / 172 | Structural break against a restrained Fianchetto set-up |
| J.Refalo - J.Guevara Pijoan; Correspondence 2018; 1/2-1/2 | 241-250 / 249 | Prepared kingside pawn lever becoming forcing |
| S.Grishchenko - R.Murtazin; Voronezh 2017; 1/2-1/2 | 252-259 / 258 | Flank-space advance and a long manoeuvring transition |

### N.Grandelius - E.Hedman

- Identity: Swedish Championship, Falun 2012; result 0-1.
- PDF coverage: pages 47-52; core page 48.
- Main score: complete game in primary.pgn, under the matching White/Black/Event headers; no fragment and no SetUp/FEN.
- Idea anchor and continuous moves: 11...Qxc8 12.Nf3 Rd8 13.fxe5 Nxe5 14.Bg5+ Kf8 15.Bxd8 Qxd8.
- Paraphrased evidence: The book presents 12...Rd8 as a flexible defensive placement: it pressures the d5-pawn, can support a d6 blockade, and keeps options for f-file defence. The later king move accepts the exchange loss in return for time, activity, and a position in which White's development remains difficult.
- Alternative or counter-explanation: 12...Re8 is the more routine alternative, but the book treats White's f-pawn push and f-file pressure as a reason to prefer the game move. At move 14, 14...f6 is also discussed as a more material-conservative defence.
- Neutral diversity metadata: Mikenas Attack with 8.e5; early-to-middle-game defence; White's f4/e5 chain against Black's d6 structure; temporary exchange imbalance; Black defending first and then converting over a long horizon.
- Replay and ambiguity: PASS, 100 legal plies. No move-level ambiguity; the PDF score gives 0-1 but does not encode the practical ending mechanism (for example, resignation or time).

### Sta.Kovac - N.González Rabago

- Identity: Correspondence 2015; result 1/2-1/2.
- PDF coverage: pages 99-105; core pages 103-104.
- Main score: complete game in primary.pgn, under the matching White/Black/Event headers; no fragment and no SetUp/FEN.
- Idea anchor and continuous moves: 15.Bg5 Qc7 16.Qa4 Nf6 17.Nb5 Qd7 18.Re1 Nxd5 19.Qb3 Bf8.
- Paraphrased evidence: The book's central point is that ...Qd7 pins the b5-knight, so White must manage both that knight and the d5-pawn. Developing with ...Bb7 then attacks d5 while releasing the a-pawn, which makes the constraint a positional one rather than a single tactic.
- Alternative or counter-explanation: The book gives 19...axb5 as a calmer route to a simpler result, whereas 19...Bf8 retains more imbalance. White can instead retreat the knight with 18.Nc3, changing the position into a more equal ending rather than testing the pin.
- Neutral diversity metadata: Modern set-up after 10.Nxb5; middlegame; b5-knight and d5-pawn as coupled weaknesses; Black has an extra-pawn/initiative tension rather than a fixed material advantage; restrictive, medium-horizon play.
- Replay and ambiguity: PASS, 69 legal plies. No move-level ambiguity; the recorded draw is not a board-terminal result, so its practical cause is not specified by the score.

### M.Warmerdam - D.Navara

- Identity: Greek League 2019; result 0-1.
- PDF coverage: pages 167-175; core page 172.
- Main score: complete game in primary.pgn, under the matching White/Black/Event headers; no fragment and no SetUp/FEN.
- Idea anchor and continuous moves: 12.h3 b6 13.f4 c4 14.Nxc4 Qc7 15.Nd2 Nc5 16.e4 Bd7.
- Paraphrased evidence: The book explains ...b6 as preserving queenside tactical resources while limiting White's a-pawn cramp, and ...c4 as immediately turning e4 into a target. The proposed compensation is active piece placement, c-file access, and future queenside play while White still has development work.
- Alternative or counter-explanation: The text explicitly contrasts 13...c4 with 13...Rb8, after which White can gain a small pull. Later it also identifies 21...Qa7 as a stronger practical choice than the game continuation, so this is a source-backed example with a meaningful alternative story.
- Neutral diversity metadata: Fianchetto structure; strategic restraint turning into a central/queenside break; e4/d5 versus c4 and b6; material is initially level and later changes through liquidation; Black counterplay against a positional squeeze.
- Replay and ambiguity: PASS, 100 legal plies. No move-level ambiguity; the final 0-1 is source-reported without a separately stated ending mechanism.

### J.Refalo - J.Guevara Pijoan

- Identity: Correspondence 2018; result 1/2-1/2.
- PDF coverage: pages 241-250; core page 249.
- Main score: complete game in primary.pgn, under the matching White/Black/Event headers; no fragment and no SetUp/FEN.
- Idea anchor and continuous moves: 21.Qd1 Qe5 22.Bc4 Rab8 23.Nd3 Qe7 24.Na4 g4 25.Nb6 gxf3 26.Qxf3 Nf6.
- Paraphrased evidence: The book frames ...Qe5 as inviting the knight to d3 so the e4-pawn loses support; this is the preparation for ...g4. The pawn lever changes a strategically prepared kingside position into a concrete sequence in which both sides must account for the e5-square and exposed files.
- Alternative or counter-explanation: 14...Nh5 is supplied as a viable alternative to the game's 14...g5, and White's 15.h4 is treated as a direct test of the expansion. After the anchor, the quieter 27.Nxd7 is presented as less testing than White's central advance.
- Neutral diversity metadata: Sämisch with Qd2 and the Nd1/Nec3 plan; middle game moving quickly from preparation to tactics; g-pawn lever against a white e4/d5 centre; material remains fluid rather than fixed; Black initiative versus White central and queenside ambitions.
- Replay and ambiguity: PASS, 69 legal plies. No move-level ambiguity; the draw's practical basis is not encoded in the final board position.

### S.Grishchenko - R.Murtazin

- Identity: Voronezh 2017; result 1/2-1/2.
- PDF coverage: pages 252-259; core page 258.
- Main score: complete game in primary.pgn, under the matching White/Black/Event headers; no fragment and no SetUp/FEN.
- Idea anchor and continuous moves: 14.e3 Nf6 15.Nc4 h5 16.f3 Qe7 17.a4 b6 18.Kf2 Bb7.
- Paraphrased evidence: The book treats ...h5 as a space-gaining, preventive move that makes a White kingside attack harder before it starts. The ensuing positions are characterized as a long manoeuvring contest for dark versus light squares, not an immediate forced win.
- Alternative or counter-explanation: The text prefers 15...h5 to 15...Ng4 because the latter lets White use f3 and g4 to clamp useful squares. It also discusses 14...Ne5 as another route, making the game move a choice of plan rather than the only continuation.
- Neutral diversity metadata: 6.Nf3 with 7.Bg5 and the bishop chase; middlegame to manoeuvring phase; Black h6/g5/h5/h4 space pattern versus White e3/e4 and f-pawn intentions; no forced material imbalance at the anchor; proactive containment on the kingside before later central/queenside play.
- Replay and ambiguity: PASS, 85 legal plies. No move-level ambiguity; the recorded draw is not checkmate or stalemate, so its practical termination remains unspecified.

## Alternates, capped at three

| Game identity | PDF pages / core page | Why it remains an alternate |
| --- | --- | --- |
| C.Lovrinovic - D.Sutkovic; Travnik 2017; 0-1 | 27-35 / 29 | Clear queenside pawn break with ...b5 and a later passed-pawn ending, but it overlaps the shortlist's counterplay/transition coverage. |
| J.Aijala - T.Luukkonen; Finnish League 2010; 0-1 | 52-58 / 58 | Four Pawns structure with a source-highlighted ...b5 response to an early central advance; valuable if a more direct structural-break case is needed. |
| D.Ivanovic - A.Indjic; Serbian League 2012; 0-1 | 215-222 / 215 | Distinct 7.Nge2 structure where ...b6 and ...Ba6 exchange the problematic bishop and improve long-term coordination; useful as a quieter minor-piece-transformational reserve. |

## Diversity summary

The five primary games deliberately start from five different Benoni branches: Mikenas, the Modern 10.Nxb5 system, Fianchetto, Sämisch, and 6.Nf3 with 7.Bg5. Their selected anchors separate defensive resource, static restriction, structural break, tactical pawn lever, and flank-space advance; material features range from an accepted exchange imbalance through equal-material manoeuvring to changing material after liquidation.

They also spread the time horizon: immediate defence, accumulating pressure, a break against a squeeze, prepared tactical conversion, and a protracted square-complex contest. This is a curation of source-supported ideas, not a claim that any book assessment is engine truth.
