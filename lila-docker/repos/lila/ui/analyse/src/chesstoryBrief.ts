type ChesstoryBriefSectionKey =
  | 'opening-idea'
  | 'middlegame-plan'
  | 'current-decision'
  | 'better-plan'
  | 'evidence';

export interface ChesstoryBriefSection {
  key: ChesstoryBriefSectionKey;
  title: string;
  body: string;
  pending: boolean;
}

export function chesstoryBriefSections(): ChesstoryBriefSection[] {
  return [
    {
      key: 'opening-idea',
      title: 'Opening idea',
      body: 'Which structure, tension, or tabiya did this game come from?',
      pending: true,
    },
    {
      key: 'middlegame-plan',
      title: 'Plan created by the opening',
      body: 'Which pawn break, piece route, or target should guide the middlegame?',
      pending: true,
    },
    {
      key: 'current-decision',
      title: 'Current decision',
      body: 'What did the selected move change in the plan or structure?',
      pending: true,
    },
    {
      key: 'better-plan',
      title: 'Better plan / alternative',
      body: 'Which candidate kept the plan clearer, safer, or more forcing?',
      pending: true,
    },
    {
      key: 'evidence',
      title: 'Evidence from the board',
      body: 'Which engine line, eval shift, or board cue makes the idea trustworthy?',
      pending: true,
    },
  ];
}
