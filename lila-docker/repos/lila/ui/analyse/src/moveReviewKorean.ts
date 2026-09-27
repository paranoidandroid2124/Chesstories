/// <reference lib="es2022.intl" />
const pieces: Record<string, string> = { pawn: '폰', knight: '나이트', bishop: '비숍', rook: '룩', queen: '퀸', king: '킹' };
const exact: Record<string, string> = {
  'These are observed continuations, not a forced outcome or proof of why the evaluations differ': '이는 제공된 수순에서 관측한 경과이며, 강제되는 결과나 평가 차이의 원인을 입증하는 것은 아닙니다',
  'These concrete attacks do not by themselves establish a forced gain': '이러한 구체적인 공격만으로 강제 이득이 성립한다고 볼 수는 없습니다',
  'The relative strength of these replies remains unverified': '이 응수들의 상대적 우열은 아직 검증되지 않았습니다',
  'Use of this support in a recapture has not yet been established in the current compared lines': '현재 비교 수순에서는 이 지원을 실제 되잡기에 사용하는지 아직 확인되지 않았습니다',
  'This distinguishes the defensive condition; it does not make the common reply illegal or explain the whole evaluation difference': '이는 방어 조건의 차이를 보여 줍니다. 공통 응수를 불가능하게 만들거나 평가 차이 전체를 설명하는 것은 아닙니다',
  'The later action is conditional on the named intervening replies': '뒤의 행동은 명시된 중간 응수가 이어진다는 조건에 달려 있습니다',
  'Each observed change belongs to its exact move': '각 변화는 관측된 바로 그 수에 해당합니다',
  'This is the observed sequence, not a forced line': '이는 관측된 진행 순서이며 강제 수순은 아닙니다',
  'These are observed exchanges, not a forced outcome or the sole reason for the evaluation': '이는 관측된 교환이며, 강제되는 결과나 평가의 유일한 이유를 뜻하지 않습니다',
  'No captures or promotions in the recorded continuation': '기록된 후속 수순에는 잡기나 승진이 없습니다',
  'that support is maintained': '그 지원이 유지됩니다',
  'neither side loses material or promotes': '양쪽 모두 기물 손실이나 승진이 없습니다',
  'The engine evaluations are close; the small differences alone do not rule out this idea': '엔진 평가가 비슷하므로, 작은 수치 차이만으로 이 아이디어를 배제할 수는 없습니다',
  'The engine evaluations are close; this difference alone does not establish a stronger choice': '엔진 평가가 비슷하므로, 이 차이만으로 더 강한 선택이라고 판단할 수는 없습니다',
  'Other replies and later plans remain outside this conclusion': '다른 응수와 이후의 계획은 이 결론의 범위에 포함되지 않습니다',
  'Exact branches and evidence are retained in the linked questions': '정확한 분기와 근거는 연결된 질문에서 확인할 수 있습니다',
  'This identifies an opposing option; its legality alone does not establish its tactical value': '이는 상대에게 가능한 선택을 보여 줍니다. 합법적인 수라는 사실만으로 전술적 가치가 입증되지는 않습니다',
  'Candidate idea: its practical value is not yet established': '검토할 아이디어이며 실전적 가치는 아직 확인되지 않았습니다',
  'Does avoiding that capture or changing the response invalidate the named continuation': '그 잡기를 피하거나 응수를 바꾸면 명시된 후속 수순이 성립하지 않게 되나요',
  'Observed returns and their adequacy are separate questions': '관측된 보상과 그 보상이 충분한지는 별개의 질문입니다',
};
const states: Record<string, string> = { present: '있음', absent: '없음', available: '가능', unavailable: '불가능', legal: '합법', illegal: '불법' };

function noun(text: string): string {
  return text.trim()
    .replace(/\b(?:the |that |a )/gi, '')
    .replace(/initially attacked /g, '처음 공격받던 ')
    .replace(/original attacker\b/gi, '원래 공격 기물')
    .replace(/checking /g, '체크를 건 ')
    .replace(/supported /g, '지원받는 ')
    .replace(/same /g, '같은 ')
    .replace(/original target/g, '원래 대상')
    .replace(/\btarget\b/g, '대상')
    .replace(/its capturer/g, '그 기물을 잡은 기물')
    .replace(/piece that took that target/g, '그 대상을 잡은 기물')
    .replace(/\b(white|black) (pawn|knight|bishop|rook|queen|king) originally on ([a-h][1-8])/gi,
      (_, side: string, role: string, square: string) => `원래 ${square}에 있던 ${side.toLowerCase() === 'white' ? '백' : '흑'} ${pieces[role.toLowerCase()]}`)
    .replace(/\b(pawn|knight|bishop|rook|queen|king) from ([a-h][1-8])/gi,
      (_, role: string, square: string) => `${square}에서 온 ${pieces[role.toLowerCase()]}`)
    .replace(/\b(White|Black)(?:'s)? (pawn|knight|bishop|rook|queen|king) on ([a-h][1-8])/gi,
      (_, side: string, role: string, square: string) => `${square}의 ${side.toLowerCase() === 'white' ? '백' : '흑'} ${pieces[role.toLowerCase()]}`)
    .replace(/\b(pawn|knight|bishop|rook|queen|king) on ([a-h][1-8])/gi,
      (_, role: string, square: string) => `${square}의 ${pieces[role.toLowerCase()]}`)
    .replace(/\b(one|two|three|four|[0-9]+) (pawns?|knights?|bishops?|rooks?|queens?|kings?)\b/gi,
      (_, amount: string, role: string) => `${pieces[role.toLowerCase().replace(/s$/, '')]} ${{ one: 1, two: 2, three: 3, four: 4 }[amount as 'one'] ?? amount}개`)
    .replace(/\b(white|black)(?:'s)?\b/gi, side => side.toLowerCase().startsWith('white') ? '백' : '흑')
    .replace(/\b(pawn|knight|bishop|rook|queen|king)s?\b/gi, (_, role: string) => pieces[role.toLowerCase()])
    .replace(/ on ([a-h][1-8])/g, ' (현재 $1)')
    .replace(/'s\b/g, '의')
    .replace(/,? and /g, ', ').replace(/nothing/g, '없음');
}

function withParticle(text: string, consonant: string, vowel: string): string {
  const last = text.charCodeAt(text.length - 1);
  return text + (last >= 0xac00 && last <= 0xd7a3 && (last - 0xac00) % 28 === 0 ? vowel : consonant);
}

function clause(input: string): string | undefined {
  const text = input.trim().replace(/[.!?]$/, '');
  if (!/[A-Za-z]{2,}/.test(text.replace(/⟦\d+⟧/g, ''))) return text;
  if (exact[text]) return exact[text];
  let m: RegExpMatchArray | null;
  // These are presentation rules for the runtime's finite English templates, not a general translator.
  // ponytail: unknown templates retain their marked English source; add an explicit rule when new runtime copy ships.
  if ((m = /^(?:In the (?:same )?provided|In the returned) (\S+) line[, :]\s*(.+)$/s.exec(text))) {
    const rest = clause(m[2]);
    return rest && `제공된 ${m[1]} 수순에서 ${rest}`;
  }
  if ((m = /^[Ii]n the provided (?:line|continuation) beginning (\S+), (.+)$/s.exec(text))) {
    const rest = clause(m[2]); return rest && `${m[1]}로 시작하는 제공된 수순에서 ${rest}`;
  }
  if ((m = /^In the returned continuation (.+?), (.+)$/s.exec(text))) {
    const rest = clause(m[2]); return rest && `제공된 ${m[1]} 후속 수순에서 ${rest}`;
  }
  if ((m = /^In the (?:same )?provided (?:line|continuation), (.+)$/s.exec(text))) {
    const rest = clause(m[1]); return rest && `제공된 수순에서 ${rest}`;
  }
  if ((m = /^In this line, (.+)$/s.exec(text))) {
    const rest = clause(m[1]); return rest && `이 수순의 기물 손실 — ${rest}`;
  }
  if ((m = /^(\S+): (.+)$/s.exec(text))) {
    const rest = clause(m[2]); return rest && `${m[1]}: ${rest}`;
  }
  if ((m = /^the opening exchange is: (.+)$/s.exec(text))) {
    const rest = clause(m[1]); return rest && `첫 교환의 진행: ${rest}`;
  }
  if ((m = /^At this line's end, these original targets remain: (.+)$/.exec(text)))
    return `이 수순이 끝날 때 남아 있는 원래 대상: ${m[1].split('; ').map(noun).join(', ')}`;
  if ((m = /^At this position, (\S+) evaluates (above|below) (\S+) for (white|black)$/.exec(text)))
    return `이 국면에서 ${noun(m[4])}의 ${m[1]} 평가는 ${m[3]}보다 ${m[2] === 'above' ? '높습니다' : '낮습니다'}`;
  if ((m = /^At this idea's starting position, (\S+) evaluates above (\S+), but the engine evaluations are close$/.exec(text)))
    return `이 아이디어의 시작 국면에서는 ${m[1]}의 평가가 ${m[2]}보다 높지만 엔진 평가 차이는 작습니다`;
  if ((m = /^(\S+) by (.+) creates these new attacks: (.+)$/.exec(text))) {
    const rest = clause(m[3]); return rest && `${noun(m[2])}의 ${m[1]}로 새로 생기는 공격: ${rest}`;
  }
  if ((m = /^The line opens after (.+) leaves (\S+) with (\S+)$/.exec(text)))
    return `${noun(m[1])}이 ${m[3]}로 ${m[2]}를 떠나면서 해당 선이 열립니다`;
  if ((m = /^The observed line after (\S+) includes (.+) moving to (\S+) after (.+)$/.exec(text)))
    return `${m[1]} 이후 관측된 수순에서는 ${m[4]}에 이어 ${noun(m[2])}이 ${m[3]}로 이동합니다`;
  if ((m = /^(\S+) changes (.+) control of (\S+): (present|absent) versus (present|absent)$/.exec(text)))
    return `${m[1]}에 따른 ${noun(m[2])}의 ${m[3]} 통제 비교: ${states[m[4]]} / ${states[m[5]]}`;
  if ((m = /^(\S+) changes (.+) location: (\S+) versus (\S+)$/.exec(text)))
    return `${m[1]}에 따른 ${noun(m[2])}의 위치 비교: ${m[3]} / ${m[4]}`;
  if ((m = /^Still needed: (\S+) versus (\S+) at the starting position \((\w+)\)$/.exec(text))) {
    const reason: Record<string, string> = { search_budget_exhausted: '탐색 예산 소진', investigation_limit: '조사 한도 도달', engine_unavailable: '엔진 사용 불가', time_budget_exhausted: '탐색 시간 소진' };
    return reason[m[3]] && `아직 필요한 비교: 시작 국면의 ${m[1]}와 ${m[2]} (${reason[m[3]]})`;
  }
  if ((m = /^(\S+) captures (.+) with check against (.+) before the later capture$/.exec(text)))
    return `뒤의 잡기가 일어나기 전에 ${m[1]}로 ${noun(m[2])}을 잡으며 ${noun(m[3])}을 체크합니다`;
  if ((m = /^(\S+) answers that check$/.exec(text))) return `${m[1]}로 그 체크에 대응합니다`;
  if ((m = /^The other continuation for the compared (.+): the original attacker is (.+)$/.exec(text)))
    return `비교 대상인 ${noun(m[1])}의 다른 후속 수순에서 원래 공격 기물은 ${noun(m[2])}입니다`;
  if ((m = /^capture by ([^;]+) of ([^;]+) is (available|unavailable)$/.exec(text)))
    return `${noun(m[1])}이 ${noun(m[2])}을 잡는 수는 ${m[3] === 'available' ? '가능합니다' : '불가능합니다'}`;
  if ((m = /^it is (available|unavailable)$/.exec(text))) return `해당 선택은 ${m[1] === 'available' ? '가능합니다' : '불가능합니다'}`;
  if ((m = /^[Aa]fter (\S+) it is (available|unavailable)$/.exec(text))) return `${m[1]} 이후 해당 선택은 ${m[2] === 'available' ? '가능합니다' : '불가능합니다'}`;
  if ((m = /^Against the common reply (\S+), support by (.+) for (.+) is (absent|present) after (\S+) and (absent|present) after (\S+)$/.exec(text)))
    return `공통 응수 ${m[1]}에 대해 ${noun(m[2])}의 ${noun(m[3])} 지원은 ${m[5]} 이후에는 ${states[m[4]]}, ${m[7]} 이후에는 ${states[m[6]]}입니다`;
  if ((m = /^What changes before (.+) acting on (\S+) after (\S+), which moves cause them, and is acting immediately available or preferable$/.exec(text)))
    return `${m[3]} 이후 ${noun(m[1])}이 ${m[2]}에서 행동하기까지 무엇이 바뀌나요? 어떤 수가 그 변화를 만들며, 즉시 행동하는 것이 가능하거나 더 나은가요`;
  if ((m = /^After (\S+), what compensates (white|black) for the observed material loss beginning with (\S+)$/.exec(text)))
    return `${m[1]} 이후 ${m[3]}로 시작하는 관측된 기물 손실에 대해 ${noun(m[2])}은 어떤 보상을 얻나요`;
  if ((m = /^There are (\d+) legal replies that do not take this target; only the named comparisons are tested$/.exec(text)))
    return `이 대상을 잡지 않는 합법적인 응수는 ${m[1]}개이며, 그중 명시된 비교만 검사했습니다`;
  if ((m = /^(White|Black) promotes (\d+) (\w+) to (?:a )?(\w+)$/.exec(text)))
    return `${noun(m[1])}: ${noun(m[3])} ${m[2]}개를 ${noun(m[4])}으로 승진`;
  if ((m = /^(?:After|At|after) (.+?), (.+)$/s.exec(text))) {
    const rest = clause(m[2]); return rest && `${m[1]} 이후 ${rest}`;
  }
  if (text.includes('; ')) {
    const parts = text.split('; ').map(clause);
    if (parts.every(part => part !== undefined)) return parts.join('; ');
  }
  if ((m = /^(.+), which is under attack from (.+)$/s.exec(text))) {
    const rest = clause(m[1]); return rest && `${rest}. 이 대상은 ${noun(m[2])}의 공격을 받고 있습니다`;
  }
  if ((m = /^(.+), giving (check|checkmate)$/s.exec(text))) {
    const rest = clause(m[1]); return rest && `${rest}. ${m[2] === 'check' ? '체크' : '체크메이트'}가 됩니다`;
  }
  if ((m = /^(.+?) (captures|recaptures) (.+) with (\S+)$/.exec(text)))
    return `${m[4]}에서 ${noun(m[1])}: ${noun(m[3])} ${m[2] === 'recaptures' ? '되잡기' : '잡기'}`;
  if ((m = /^The captured (.+) was the attacker$/.exec(text))) return `잡힌 ${noun(m[1])}이 공격하던 기물이었습니다`;
  if ((m = /^The initially attacked (.+) stays on (\S+)$/.exec(text))) return `처음 공격받던 ${noun(m[1])}은 ${m[2]}에 그대로 있습니다`;
  if ((m = /^The initially attacked (.+) moves to (\S+)$/.exec(text))) return `처음 공격받던 ${noun(m[1])}이 ${m[2]}로 이동합니다`;
  if ((m = /^This opens the ([a-h])-file$/.exec(text))) return `이로써 ${m[1]} 파일이 열립니다`;
  if ((m = /^(.+) remains on (\S+) at this line's end$/.exec(text))) return `이 수순이 끝날 때 ${noun(m[1])}은 ${m[2]}에 남습니다`;
  if ((m = /^(.+) is captured within this line$/.exec(text))) return `이 수순 안에서 ${noun(m[1])}이 잡힙니다`;
  if ((m = /^(.+) (attacks it again|still attacks it) there$/.exec(text))) return `그곳에서 ${noun(m[1])}이 그 대상을 ${m[2].startsWith('still') ? '계속' : '다시'} 공격합니다`;
  if ((m = /^(\S+) ends the support of (.+) by (.+)$/.exec(text))) return `${m[1]} 이후 ${noun(m[3])}의 ${noun(m[2])} 지원이 끝납니다`;
  if ((m = /^(.+) keeps covering it through that move$/.exec(text))) return `그 수까지 ${noun(m[1])}의 지원은 유지됩니다`;
  if ((m = /^(.+) moves to (\S+), ending its attack on (.+)$/.exec(text))) return `${noun(m[1])}이 ${m[2]}로 이동하면서 ${noun(m[3])}에 대한 공격이 끝납니다`;
  if ((m = /^(?:(\S+) )?[Bb]locks (.+)'s line to the initially attacked (.+), which stays on (\S+)$/.exec(text)))
    return `${m[1] ? `${m[1]}: ` : ''}${noun(m[2])}에서 처음 공격받던 ${noun(m[3])}으로 향하는 선을 막습니다. 그 대상은 ${m[4]}에 남습니다`;
  if ((m = /^(.+) attacks? that target after this move$/.exec(text))) return `이 수 이후 ${noun(m[1])}이 그 대상을 공격합니다`;
  if ((m = /^(.+) now controls (.+) next to the opposing king$/.exec(text))) return `${noun(m[1])}이 상대 킹 옆의 ${noun(m[2])}를 새로 통제합니다`;
  if (/^the king cannot enter (?:this square|these squares)$/.test(text)) return '상대 킹은 해당 칸으로 들어갈 수 없습니다';
  if ((m = /^[Uu]ses the (open|semi-open|closed) ([a-h])-file: (.+) controls (\S+), also supported by (.+)$/.exec(text)))
    return `${{ open: '열린', 'semi-open': '반쯤 열린', closed: '닫힌' }[m[1]]} ${m[2]} 파일을 사용합니다. ${noun(m[3])}이 ${m[4]}를 통제하고, ${noun(m[5])}도 그 칸을 지원합니다`;
  if ((m = /^(?:(\S+) )?[Gg]ives check to (.+) with (.+)$/.exec(text))) return `${m[1] ? `${m[1]}: ` : ''}${noun(m[3])}으로 ${noun(m[2])}을 체크합니다`;
  if ((m = /^(?:(\S+) )?[Mm]oves the king to (\S+)$/.exec(text))) return `${m[1] ? `${m[1]}: ` : ''}킹이 ${m[2]}로 이동합니다`;
  if ((m = /^(?:(\S+) )?[Bb]locks the check on (\S+)$/.exec(text))) return `${m[1] ? `${m[1]}: ` : ''}${m[2]}에서 체크를 막습니다`;
  if ((m = /^(.+) promotes the pawn to a (\w+)$/.exec(text))) return `${m[1]}에서 폰이 ${noun(m[2])}으로 승진합니다`;
  if ((m = /^the same (\w+) next moves with (\S+) to (\S+)$/i.exec(text)))
    return `같은 ${noun(m[1])}의 다음 이동은 ${m[2]}이며, 도착 칸은 ${m[3]}입니다`;
  if ((m = /^(\S+) keeps the ([a-h][1-8])-pawn on its starting square with the route to ([a-h][1-8]) clear$/.exec(text)))
    return `${m[1]}는 ${m[2]} 폰을 시작 칸에 남겨 두며, ${m[3]}까지 두 칸 전진할 경로가 열려 있습니다`;
  if ((m = /^(\S+) commits that same pawn to ([a-h][1-8]), from where a one-square advance to ([a-h][1-8]) also has a clear route$/.exec(text)))
    return `${m[1]}는 같은 폰을 ${m[2]}로 전진시키며, 그곳에서도 ${m[3]}까지 한 칸 전진할 경로가 열려 있습니다`;
  if ((m = /^(\S+) uses that two-square advance(?:, supporting the same pawn from ([a-h][1-8]), now on ([a-h][1-8])(?: after (.+))?)?$/.exec(text)))
    return `${m[1]}에서 그 두 칸 전진을 사용합니다${m[2] ? `. ${m[4] ? `${m[4]} 이후 ` : ''}원래 ${m[2]}에 있었고 현재 ${m[3]}에 있는 같은 폰을 지원합니다` : ''}`;
  if ((m = /^(.+?) (?:recaptures|actually recaptures) (.+)$/.exec(text))) return `${noun(m[1])}: ${noun(m[2])} 되잡기`;
  if ((m = /^(.+?) (later captures|also captures|captures) (.+)$/.exec(text)))
    return `${noun(m[1])}: ${m[2] === 'later captures' ? '나중에 ' : m[2] === 'also captures' ? '추가로 ' : ''}${noun(m[3])} 잡기`;
  if ((m = /^(Captures|Recaptures) (.+)$/.exec(text))) return `${noun(m[2])} ${m[1] === 'Recaptures' ? '되잡기' : '잡기'}`;
  if ((m = /^(\S+) (?:castles|Castles), bringing the rook from (\S+) to (\S+)$/.exec(text))) return `${m[1]}로 캐슬링하며 룩이 ${m[2]}에서 ${m[3]}로 이동합니다`;
  if ((m = /^Castles, bringing the rook from (\S+) to (\S+)$/.exec(text))) return `캐슬링하며 룩이 ${m[1]}에서 ${m[2]}로 이동합니다`;
  if ((m = /^(?:(\S+) )?[Dd]evelops the (\w+) from (\S+) to (\S+)(?:, controlling (.+))?$/.exec(text)))
    return `${m[1] ? `${m[1]}: ` : ''}${noun(m[2])}을 ${m[3]}에서 ${m[4]}로 전개합니다${m[5] ? `. 통제하는 칸: ${noun(m[5])}` : ''}`;
  if ((m = /^(\S+) (develops|brings) the (\w+) to (\S+)$/.exec(text))) return `${m[1]}: ${noun(m[3])}을 ${m[4]}로 ${m[2] === 'develops' ? '전개' : '이동'}합니다`;
  if ((m = /^(?:(\S+) )?[Mm]oves the (.+?) out of the attack from (.+)$/.exec(text)))
    return `${m[1] ? `${m[1]}: ` : ''}${noun(m[2])}이 ${noun(m[3])}의 공격에서 벗어납니다`;
  if ((m = /^(.+?) moves the initially attacked (.+?) out of (.+?)'s attack$/.exec(text)))
    return `${noun(m[1])}: 처음 공격받던 ${noun(m[2])}이 ${noun(m[3])}의 공격에서 벗어납니다`;
  if ((m = /^(.+?) (now targets|keeps targeting|attacks|attack|does not attack) (.+)$/.exec(text)))
    return `${withParticle(noun(m[1]), '이', '가')} ${withParticle(noun(m[3]), '을', '를')} ${m[2] === 'does not attack' ? '공격하지 않습니다' : m[2] === 'keeps targeting' ? '계속 공격합니다' : m[2] === 'now targets' ? '새로 공격합니다' : '공격합니다'}`;
  if ((m = /^(.+?) (adds? support to|keeps supporting|maintains support of|does not support|supports|support) (.+)$/.exec(text)))
    return `${withParticle(noun(m[1]), '이', '가')} ${withParticle(noun(m[3]), '을', '를')} ${m[2] === 'does not support' ? '지원하지 않습니다' : m[2].startsWith('add') ? '새로 지원합니다' : m[2] === 'keeps supporting' || m[2] === 'maintains support of' ? '계속 지원합니다' : '지원합니다'}`;
  if ((m = /^(.+?) ends its support of (.+)$/.exec(text))) return `${noun(m[1])}의 ${noun(m[2])} 지원이 끝납니다`;
  if ((m = /^(White|Black) loses (.+)$/.exec(text))) return `${noun(m[1])}: ${noun(m[2])}`;
  if ((m = /^(\S+) clears ([a-h][1-8])$/.exec(text))) return `${m[1]}로 ${m[2]}를 비웁니다`;
  if ((m = /^(?:later )?(\S+) (?:later )?(passes through|occupies) (that square|the vacated ([a-h][1-8]) square) in this line$/.exec(text)))
    return `이 수순의 뒤이은 ${m[1]}에서는 움직인 기물이 ${m[4] ? `비워진 ${m[4]}` : '그 칸'}을 ${m[2] === 'passes through' ? '지나갑니다' : '차지합니다'}`;
  if ((m = /^(?:later )?(\S+) uses the cleared line to target (.+) in this line$/.exec(text)))
    return `이 수순에서 나중에 ${m[1]}로 열린 선을 사용해 ${noun(m[2])}을 공격합니다`;
  if ((m = /^(?:(\S+) )?[Cc]onnects the rooks on (\S+) and (\S+)$/.exec(text))) return `${m[1] ? `${m[1]}: ` : ''}${m[2]}와 ${m[3]}의 룩을 연결합니다`;
  if ((m = /^(?:(\S+) )?[Aa]dvances the passed pawn to (\S+)$/.exec(text))) return `${m[1] ? `${m[1]}: ` : ''}통과폰을 ${m[2]}로 전진시킵니다`;
  if ((m = /^(?:(\S+) )?[Cc]reates a passed pawn on (\S+)$/.exec(text))) return `${m[1] ? `${m[1]}: ` : ''}${m[2]}에 통과폰이 생깁니다`;
  if ((m = /^(?:The|the) pawn on (\S+) is a passed pawn$/.exec(text))) return `${m[1]}의 폰은 통과폰입니다`;
  if ((m = /^(?:(\S+) )?[Ll]eaves doubled (white|black) pawns on (.+)$/.exec(text))) return `${m[1] ? `${m[1]}: ` : ''}${noun(m[3])}에 ${noun(m[2])}의 더블폰이 남습니다`;
  if ((m = /^(?:The|the) (white|black) pawn on (\S+) is no longer doubled$/.exec(text))) return `${m[2]}의 ${noun(m[1])} 폰은 더 이상 더블폰이 아닙니다`;
  if ((m = /^(?:(\S+) )?[Oo]ccupies (\S+), blocking (.+) from advancing to (\S+)$/.exec(text)))
    return `${m[1] ? `${m[1]}: ` : ''}${m[2]}를 차지해 ${noun(m[3])}의 ${m[4]} 전진을 막습니다`;
  if ((m = /^(?:(\S+) )?[Cc]lears (\S+), opening a line for (.+) toward (\S+)$/.exec(text)))
    return `${m[1] ? `${m[1]}: ` : ''}${m[2]}를 비워 ${noun(m[3])}의 ${m[4]} 방향 선을 엽니다`;
  if ((m = /^(?:[Bb]locks|[Oo]pens) the line from (.+) to (.+)$/.exec(text)))
    return `${noun(m[1])}에서 ${noun(m[2])}으로 향하는 선을 ${/^[Bb]/.test(text) ? '막습니다' : '엽니다'}`;
  if ((m = /^[Bb]locks the line from (.+) toward (\S+)$/.exec(text))) return `${noun(m[1])}에서 ${m[2]} 방향의 선을 막습니다`;
  if ((m = /^(.+) still (supports|support|attacks|attack) that target$/.exec(text))) return `${noun(m[1])}은 여전히 그 대상을 ${m[2].startsWith('support') ? '지원' : '공격'}합니다`;
  if ((m = /^(?:(\S+) )?[Pp]laces the rook on the (open|semi-open|closed) ([a-h])-file$/.exec(text)))
    return `${m[1] ? `${m[1]}: ` : ''}룩을 ${{ open: '열린', 'semi-open': '반쯤 열린', closed: '닫힌' }[m[2]]} ${m[3]} 파일에 놓습니다`;
  if ((m = /^The captured (\w+) played (.+)$/.exec(text))) return `잡힌 ${noun(m[1])}이 앞서 둔 수는 ${m[2]}입니다`;
  if ((m = /^This precedes that (\w+)'s removal with (.+)$/.exec(text))) return `이는 나중에 ${m[2]}로 그 ${noun(m[1])}이 잡히기 전의 일입니다`;
  if ((m = /^This follows the end of (.+)'s support$/.exec(text))) return `이는 ${noun(m[1])}의 지원이 끝난 뒤에 일어납니다`;
  if ((m = /^This uses the attack line opened when (.+) moved away$/.exec(text))) return `${noun(m[1])}이 이동하면서 열린 공격선을 사용합니다`;
  if ((m = /^(.+) promotes to a (\w+)$/.exec(text))) return `${noun(m[1])}이 ${noun(m[2])}으로 승진합니다`;
  return undefined;
}

/** Keep unknown prose explicitly English; never guess or discard a chess condition. */
export function koreanCommentary(text: string): { text: string; translated: boolean } {
  const parts = commentarySentences(text);
  const translated = parts.map(clause);
  // A broad action must never silently accept an untranslated qualifier in its subject or object.
  const hasEnglish = (part: string) => /[A-Za-z]{2}/.test(part.replace(/⟦\d+⟧|O-O(?:-O)?|[KQRBN]?[a-h]?[1-8]?x?[a-h][1-8](?:=[QRBN])?[+#]?/g, ''));
  if (translated.some(part => part === undefined || hasEnglish(part))) return { text, translated: false };
  return { text: translated.map((part, index) => part + (parts[index].match(/[.!?]$/)?.[0] ?? '')).join(' '), translated: true };
}

// Pawn SAN begins with a lowercase letter, which natural-language sentence segmenters can join to the preceding sentence.
export const commentarySentences = (text: string): string[] => text.trim().split(/(?<=[.!?])\s+/).filter(Boolean);
