# 포지션 검토 카드 pilot v1 — backs (reviewer-only)

이 문서는 reviewer-only다. `fronts.md`와 함께 system input으로 주지 않는다. 아래는 source의 간결한 paraphrase와 보드 확인 지점을 제공할 뿐, 정답 문장·필수 키워드·단일 Cause·expected label·exact acceptance rule을 만들지 않는다.

모든 카드에서 보드와 근거에 맞는 복수의 체스적 설명을 허용한다. source의 관찰과 보드의 사실만으로 충분히 뒷받침되지 않으면 abstention을 선택하고 그 한계를 적는다. 각 reviewer는 **verdict confidence**와 **Cause confidence**를 서로 독립적으로 기록한다.

각 카드의 첫 provenance 항목은 reviewer-only 식별자다. source directory, game/fragment identity, result/branch, `primary.pgn` record/ply는 front 또는 system input에 제공하지 않는다.

## CARD-001

- Reviewer-only provenance: source book directory `ai-revolution`; game identity S. Karjakin — J. Duda, Lindores Abbey Preliminaries 2020 (rapid), result `1-0`; `primary.pgn` record 1, ply 27 (14.White).
- Source locator: `document_id: ref-dc427a9593ab48248d19627b245fafaa`; 1-based PDF pages 203-209 (central discussion: 205).
- Paraphrased source evidence: 책은 이 중앙 폰 전진을 강제 계산이 알려진 계통에서의 전략적 선택으로 놓고, 즉시 끝나는 수순보다 기물 활동과 Black bishop들의 합류 난점을 보상 요소로 설명한다.
- Board checks: `e4`의 White pawn이 `e5`로 간다. 저장된 mainline에서는 `14...dxe5 15.Bxf6 Bxf6 16.Bh5+`가 이어지므로, 폰 구조·교환·check의 순서를 재생하여 확인할 수 있다.
- Alternative / counter-reading: source는 `20...a5`를 `Qa5`를 막고 light-squared bishop 전개를 돕는 방어 선택으로 제시한다. 따라서 활동성의 설명은 수비 측 선택을 지우지 않아야 한다.
- Horizon: medium-to-long; 첫 중앙 변화 뒤의 기물 배치와 수비 전개가 중요하다.
- Reviewer prompts: 보드에서 확인한 변화와 장기 배치 사이를 하나 이상 설명해 보라. `...a5`가 만드는 반대 서사를 함께 검토하라. 충분한 근거가 없으면 abstain하고, verdict confidence와 Cause confidence를 별도로 기록하라.

## CARD-002

- Reviewer-only provenance: source book directory `ai-revolution`; game identity J. Duda — S. Vidit, FIDE World Cup 2021, result `1-0`; `primary.pgn` record 5, ply 64 (32.Black).
- Source locator: `document_id: ref-dc427a9593ab48248d19627b245fafaa`; 1-based PDF pages 503-508 (defensive comparison: 507-508).
- Paraphrased source evidence: 책은 played rook move를 더 활동적인 `32...Rf4` 구상과 나란히 두고, 뒤이어 king placement와 queenside pawn race가 어떻게 국면을 바꾸는지 다룬다.
- Board checks: rook이 `f8`에서 `e8`로 간 뒤 `33.Nd3 g5`가 mainline에 있다. 이후의 `...Re2+`, `Kh1`, queenside pawn moves가 실제로 가능한지 기보를 따라 확인할 수 있다.
- Alternative / counter-reading: source가 제시한 비교 수순은 `32...Rf4 33.Ra1 Rc4 34.Nd3 c5`다. 이 자원은 card의 수를 설명할 때 다른 수비·반격 가능성을 검토하게 한다.
- Horizon: endgame-focused, with a short local rook decision feeding a longer pawn-race transition.
- Reviewer prompts: local rook placement, checking routes, king safety, pawn mobility 중 무엇을 보드에서 확인할 수 있는지 서술하라. source의 대안이 다른 설명을 지지하는지도 검토하라. 확신의 두 종류를 분리해 적고, 근거가 모자라면 abstain할 수 있다.

## CARD-003

- Reviewer-only provenance: source book directory `basman-williams`; game identity S. Williams — P. Poobalasingam, Hastings 2008, result `1-0`; `primary.pgn` record 2, ply 18 (9.Black).
- Source locator: `document_id: ref-009749ddb7ef4fe0a033b33e9712fc74`; 1-based PDF pages 10-12 (core pages 11-12).
- Paraphrased source evidence: 책은 이 flank pawn advance 뒤의 폐쇄와 light-square 변화, 그리고 뒤늦은 queenside/b-file 활용을 연결해 설명한다. 즉시 king attack만으로 읽지 않는 사례로 제시된다.
- Board checks: Black pawn이 `g6`에서 `g5`로 가고 White pawn은 이미 `h5`에 있다. 이어지는 `10.Be3 b6`과 나중의 기물·폰 통로를 replay하여 어떤 wing이 닫히고 어떤 square가 남는지 확인할 수 있다.
- Alternative / counter-reading: source는 `9...Nxh5 10.Bxh5 gxh5 11.Be3 Rg8`을 별도 진행으로 든다. 이 선택은 동일한 폰 배치에서 가능한 해석이 하나뿐이 아님을 보여 준다.
- Horizon: long; early wing geometry is connected to a later queenside conversion.
- Reviewer prompts: 변경된 pawn contacts와 mobility를 보드에서 구체적으로 확인하라. 대안 line이 어느 설명을 약화하거나 보완하는지 자유롭게 논하라. abstention의 이유도 적을 수 있으며, verdict confidence와 Cause confidence는 분리한다.

## CARD-004

- Reviewer-only provenance: source book directory `basman-williams`; game identity R. Rapport — P. Svidler, Grand Chess Tour Rapid, Paris 2021, result `1-0`; `primary.pgn` record 5, ply 31 (16.White).
- Source locator: `document_id: ref-009749ddb7ef4fe0a033b33e9712fc74`; 1-based PDF pages 28-29 (core page 29).
- Paraphrased source evidence: 책은 `h6` 뒤의 rook moves를 연속된 threat 관계와 연결하고, 짧은 기보 안에서 bishop과 rook의 상호 작용이 바뀌는 점을 다룬다.
- Board checks: rook이 `h1`에서 `h4`로 이동하고 mainline은 `16...Bxf3 17.Rf4 Qd8 18.Bxd7`로 계속된다. 각 capture와 attacked square가 실제 보드에서 성립하는지 확인할 수 있다.
- Alternative / counter-reading: source는 `14...Nc6 15.hxg6 hxg6 16.Ra3 Rf5 17.Bd2`를 다른 Black setup으로 제시한다. 이 비교는 저장된 짧은 수순을 자동적인 결과로 보지 않게 한다.
- Horizon: short, concentrated move relationship.
- Reviewer prompts: 연속 수순에서 무엇이 즉시 바뀌는지 하나 이상의 언어로 설명하라. 대안 setup이 남기는 반증 가능성도 고려하라. 근거 부족에는 abstention이 가능하며, verdict confidence와 Cause confidence를 별도로 남긴다.

## CARD-005

- Reviewer-only provenance: source book directory `modern-benoni`; game identity Sta. Kovac — N. González Rabago, Correspondence 2015, result `1/2-1/2`; `primary.pgn` record 2, ply 34 (17.Black).
- Source locator: `document_id: ref-35c04c3495434a6290c20ee4712660a6`; 1-based PDF pages 99-105 (core pages 103-104).
- Paraphrased source evidence: 책은 queen placement를 `b5` knight와 `d5` pawn이 함께 관리되어야 하는 상태와 연결하고, 뒤의 piece development가 그 관계를 이어 가는 방식으로 설명한다.
- Board checks: queen이 `c7`에서 `d7`로 가며 White knight은 `b5`, White pawn은 `d5`에 있다. `18.Re1 Nxd5 19.Qb3 Bf8`라는 stored continuation을 재생해 압력과 방어 가능한 square를 확인할 수 있다.
- Alternative / counter-reading: source는 White의 `18.Nc3` retreat와, 뒤 수순에서 `...axb5`를 포함한 더 단순한 방향을 언급한다. 따라서 pin/pressure라는 표현을 쓰더라도 한 가지 전개만 필연이라고 두지 않는다.
- Horizon: medium; a static relation can persist across several developing moves.
- Reviewer prompts: knight·pawn·queen의 관계를 보드 좌표로 확인한 뒤 가능한 설명을 제시하라. 대안 진행이 의미하는 차이도 검토하라. 필요하면 abstain하고, verdict confidence와 Cause confidence를 독립적으로 기록하라.

## CARD-006

- Reviewer-only provenance: source book directory `modern-benoni`; game identity J. Refalo — J. Guevara Pijoan, Correspondence 2018, result `1/2-1/2`; `primary.pgn` record 4, ply 48 (24.Black).
- Source locator: `document_id: ref-35c04c3495434a6290c20ee4712660a6`; 1-based PDF pages 241-250 (core page 249).
- Paraphrased source evidence: 책은 앞선 queen moves가 knight route와 `e4` pawn의 support를 바꾸는 준비가 된 뒤 이 pawn lever가 나온다고 설명한다. 그 뒤 kingside tension이 구체적인 수순으로 바뀐다는 점을 다룬다.
- Board checks: Black pawn이 `g5`에서 `g4`로 가고 White knight은 `a4`에 있다. mainline의 `25.Nb6 gxf3 26.Qxf3 Nf6`를 통해 pawn capture, file opening, piece locations를 직접 확인할 수 있다.
- Alternative / counter-reading: source는 earlier `14...Nh5`와 White의 `15.h4`를 다른 시험으로 두며, anchor 뒤 `27.Nxd7`도 별도 선택으로 언급한다. 이는 준비와 후속 수순의 해석을 열어 둔다.
- Horizon: short-to-medium; preparation becomes a concrete local sequence.
- Reviewer prompts: `e4`, `f3`, 열린 line, knight route 중 실제로 확인되는 사항을 구분해 설명하라. source alternatives가 다른 narrative를 허용하는지 살펴보라. abstention과 두 confidence의 분리 기록이 가능하다.

## CARD-007

- Reviewer-only provenance: source book directory `modernized-benko`; fragment identity Book analysis, 5.e3 branch, uncredited, `[Fragment "true"]`, result `*`; `primary.pgn` record 1, ply 17 (9.White).
- Source locator: `document_id: ref-edf868ccf0a3425488f55ef6b4b08acf`; 1-based PDF pages 59-62 (core page 61).
- Paraphrased source evidence: 책은 이 branch에서 extra pawn을 오래 지키기보다 반환하고 positional control을 유지하는 선택을 설명한다. 이는 분석 fragment의 local continuation에 관한 관찰이지 완전 게임 결말의 주장 아니다.
- Board checks: White knight이 `g1`에서 `f3`로 가고, stored line은 `9...Nxd5 10.Nxd5 Bxd5`로 이어진다. 각 central capture와 material count를 fragment 안에서 직접 확인할 수 있다.
- Alternative / counter-reading: source는 `9.Qb3 e6 10.e4 Nxe4` 및 `9.Bc4 e6 10.e4 Nxe4`를 더 sharp한 분기로 든다. 따라서 pawn return/control의 설명에도 여러 진행이 가능하다.
- Horizon: short-to-medium; only the stored fragment’s local position is in scope.
- Reviewer prompts: fragment 안에서 확인되는 반환·기물 위치·center의 변화를 서술하라. 완전 game conversion을 추론하지 말고, source와 보드가 충분하지 않으면 abstain하라. verdict confidence와 Cause confidence를 별도 기록하라.

## CARD-008

- Reviewer-only provenance: source book directory `modernized-benko`; fragment identity Book analysis, 5.Nc3 branch, uncredited, `[Fragment "true"]`, result `*`; `primary.pgn` record 4, ply 22 (11.Black).
- Source locator: `document_id: ref-edf868ccf0a3425488f55ef6b4b08acf`; 1-based PDF pages 130-137 (core page 135; later material discussion page 137).
- Paraphrased source evidence: 책은 knight capture를 awkward `b5` knight에 대한 답으로 소개하고, 후속 line에서 초기 material change가 rook-and-pawn versus two-knights 형태 및 central/file activity로 이어질 수 있음을 설명한다.
- Board checks: Black knight이 `f6`에서 `e4` pawn을 잡고, stored line은 `12.Bxe4 Bg7 13.Bd2 Ba6 14.Bd3 Bxa1`로 계속된다. capture들의 순서와 material transformation은 fragment 범위에서 replay할 수 있다.
- Alternative / counter-reading: source는 `11...Bg7`과 White의 `12.Bb2`를 다른 선택으로 제시한다. 따라서 장기 보상이나 piece activity를 설명할 때 선택 의존성을 남겨야 한다.
- Horizon: long within a fragment; do not turn it into a claim about the historical game result.
- Reviewer prompts: fragment에서 확정 가능한 material·file·piece-location 사실과 더 긴 설명을 구분하라. 대안이 제시하는 반증을 함께 다루고, 보드/근거가 모자라면 abstain하라. verdict confidence와 Cause confidence는 별도다.

## CARD-009

- Reviewer-only provenance: source book directory `positional-sacrifices`; game identity Shakhriyar Mamedyarov — Viswanathan Anand, Zagreb 2019, round 9, result `1-0`; `primary.pgn` record 3, ply 53 (27.White).
- Source locator: `document_id: ref-bc7b3f1ff52f4c7a9940dad53cd6b975`; 1-based PDF pages 315-320 (core page 318).
- Paraphrased source evidence: 책은 rook move 뒤의 exchange sequence를 passed `d`-pawn, bishop pair, space, and patient piece play와 연결한다. 즉각적인 finish가 아닌 장기 보상·수비 자원을 함께 다루는 예로 쓴다.
- Board checks: White rook이 `d1`에서 `d5`로 가고 `27...Bxd5 28.exd5`가 stored mainline이다. ensuing pawn, bishop pair, open/closed routes를 기보와 보드에서 확인할 수 있다.
- Alternative / counter-reading: source는 `27.Bc1`, Black의 `...b5` resource, 그리고 later `Rxf7/...Rd6`, `37.Qf1`, `38...Qf2` 같은 방어·개선 가능성을 기록한다. 따라서 하나의 장기 설명을 고정하지 않는다.
- Horizon: long; the local exchange is followed by extended piece and pawn play.
- Reviewer prompts: 즉시 exchange sequence와 더 긴 pawn/piece play를 구분해 설명하라. source가 남긴 defence와 improvement를 포함해 다른 합리적 해석을 허용하라. abstention 가능성을 보존하고 verdict confidence와 Cause confidence를 따로 남긴다.

## CARD-010

- Reviewer-only provenance: source book directory `positional-sacrifices`; game identity David Bronstein — Ljubomir Ljubojevic, Petropolis 1973, round 11, result `1-0`; `primary.pgn` record 5, ply 33 (17.White).
- Source locator: `document_id: ref-bc7b3f1ff52f4c7a9940dad53cd6b975`; 1-based PDF pages 452-456 (core page 454).
- Paraphrased source evidence: 책은 queen move 뒤의 `...Bxg1`과 `d6` sequence를 center와 king access의 관계로 설명하며, source 자체가 Black의 저항 계획과 White의 다른 continuation도 제시한다.
- Board checks: White queen이 `d4`에서 `f4`로 가고, stored line은 `17...Bxg1 18.d6`이다. `g1` rook capture, `d5` pawn advance, king lines를 직접 확인할 수 있다.
- Alternative / counter-reading: source는 `18...Qc5 19.Ne4 Qd4 20.Rd1 Qxb2`라는 defensive plan과 `19.O-O-O`를 다른 White continuation으로 든다. 저장된 short line만으로 보편적 결론을 만들지 않는다.
- Horizon: short-to-medium; concrete continuation with source-recorded counterplay.
- Reviewer prompts: immediate captures, central pawn, king routes 중 실제 board evidence를 우선 설명하라. source alternatives가 남기는 다른 판단을 논하라. source/board가 부족하면 abstain하고 verdict confidence와 Cause confidence를 분리해 기록하라.
