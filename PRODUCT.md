# Chesstory product and interface context

## Purpose

Chesstory helps a chess player understand why a position changed, not merely which move an engine prefers. The product joins engine evidence, game context, and plain-language commentary in one review workspace.

## Primary users

- Players reviewing a finished game move by move.
- Coaches preparing an evidence-backed explanation.
- Curious players who want a trustworthy account of a position without reading raw engine output alone.

## Product register

This is a working analysis tool. Design must shorten the path from board position to evidence and explanation. Marketing flourishes, decorative motion, and ornamental containers do not belong in the analysis workspace.

## Voice

Calm, exact, and editorial. Prefer concrete chess language. State what a control does. Do not use hype, filler, emojis, or anthropomorphic AI copy.

## Product distinction

Chesstory is not a reskinned chess server analysis board. Its defining hierarchy is:

1. the position,
2. the evidence used to assess it,
3. the commentary that explains it,
4. the source game and review record.

Engine output is supporting evidence, not the product identity or the top-level navigation model.

## Interface system

The analysis workspace should feel like an annotated score: a stable board,
an evidence margin, and a readable narrative record. The board and evidence
panel are peers; commentary remains visually primary when commentary mode is
active.

- Use cool neutral paper/ink surfaces, terracotta for selected annotations,
  and desaturated blue for reference evidence. Reserve green and red for chess
  meaning and success/error state.
- Use Noto Sans for controls and dense data, a restrained editorial serif only
  for the wordmark and narrative headings, and tabular numerals for engine
  measurements.
- Prefer rules, alignment, and whitespace over nested cards. Workspace corners
  are square or lightly softened (4px maximum).
- Name the evidence area **Engine reference**. Show engine state with explicit
  text such as **Engine / On**, **Engine / Off**, **Running**, or **Paused**.
  Version and NNUE size are secondary metadata.
- Icon-only controls require accessible labels. Every action needs implemented,
  disabled, focus, and keyboard/touch states; touch targets are at least 44px.
- Default to no motion. When state change benefits from motion, use 90–160ms
  transform/opacity transitions and respect `prefers-reduced-motion`.
- Light and dark themes preserve hierarchy and contrast; theme switching must
  change the rendered theme.

## Anti-references

- Lichess's engine header structure and icon-only analysis chrome.
- Generic AI SaaS cards, tinted beige or olive palettes, purple gradients, glass panels, and decorative status dots.
- Controls that look clickable but do nothing.
- Repeated pills, excessive rounding, and motion without state meaning.
- Detached icon rails, gradients, glass panels, and
  anonymous toggle/name/gear strips.
