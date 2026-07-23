# 체스 판단 경계

Move Review의 백엔드 책임은 해설 문장을 직접 작성하는 것이 아니라, LLM이 정확한 해설을 작성할 수 있을 만큼 충분한 체스 판단을 구성하는 것이다.

입력 사실은 가능한 한 오래 typed evidence로 유지한다. board, legal state, PV, eval, engine depth, legal replay, candidate line은 prose나 source label이 아니라 provenance를 가진 fact/node/edge로 흘러야 한다.

단일 포지션 판단과 수의 상대평가는 다른 층이다. king exposure, open file, weak square, hanging piece, outpost는 한 position node의 속성이다. blunder, mistake, good move, only move는 played result와 best or reference result를 비교하는 multi-node assessment다.

전술 판단은 관계와 강제 라인으로 증명한다. pin, fork, overload, deflection, defender removal, forced reply, mate branch, eval swing이 같은 claim 안에서 연결되어야 한다.

전략 판단은 구조 feature와 엔진 안정성으로 증명한다. pawn structure, outpost, space, file control, counterplay restraint, target fixation, stable PV가 같은 claim 안에서 연결되어야 한다.

아이디어와 판정은 독립된 층이다. 한 수가 local idea를 만들 수 있고 동시에 best line 대비 큰 손실을 낼 수 있다. 판단 구조는 이 둘을 서로 덮지 않고 함께 보존해야 한다.

LLM에 넘기는 산출물은 player-facing sentence가 아니라 evidence-backed judgment packet이다. 포함할 것은 subject, node/edge references, engine comparison, tactical or strategic claim, counterfactual, verdict, scope, confidence다. 제외할 것은 prose fallback, guardrail sentence, renderer text, sourceKind 문자열 판정, 빈 slot이나 false flag로 숨긴 과거 경로다.
