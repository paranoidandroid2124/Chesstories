# Chesstory interface system

## Design idea

The interface should feel like an annotated score on an editor's desk: a stable board, an evidence margin, and a readable narrative record. It is neither a game lobby nor a generic AI dashboard.

## Color

- **Ink:** cool blue-black surfaces and near-black text.
- **Paper:** cool neutral whites and grays, never an all-over yellow cast.
- **Red pencil:** terracotta for selected actions and annotations.
- **Reference blue:** desaturated blue for secondary evidence and board-adjacent tools.
- Green and red are reserved for chess meaning and success/error states.

Both light and dark themes must preserve the same hierarchy and contrast. Theme switching must actually change the theme.

## Type

- Use Noto Sans for controls, data, and dense product UI.
- Use a restrained editorial serif for the wordmark and narrative headings only.
- Keep labels in sentence case. Uppercase is reserved for compact metadata where scanning benefits.
- Engine measurements use tabular numerals.

## Layout

- The board and evidence panel are peers in a centered workspace.
- Do not use a detached Lichess-style vertical icon rail.
- Prefer rules, alignment, and whitespace over nested cards.
- Keep the primary board controls directly beneath the board.
- Engine identity, engine state, and settings must form a labelled evidence header rather than an anonymous toggle/name/gear strip.

## Components

- Corners are square or lightly softened, with a 4px maximum in the product workspace.
- Icon-only controls require an accessible label and are used only where space is constrained.
- Do not represent state with a decorative dot. Use explicit text such as On, Off, Running, or Paused.
- Do not use gradients, glassmorphism, floating ghost cards, or colored side stripes.
- Every button must have an implemented action, disabled state, focus state, and at least a 44px touch target.

## Motion

- Default to no motion.
- Use 90–160ms transitions only for hover, focus, disclosure, and real progress.
- Animate transform or opacity, not layout dimensions.
- Respect `prefers-reduced-motion` everywhere.

## Analysis workspace contract

- The engine area is named **Engine reference**.
- The engine control says **Engine / On** or **Engine / Off**; it is never an unexplained switch.
- Stockfish version and NNUE size are secondary metadata, not the title of the entire panel.
- Settings and threat actions are labelled on roomy layouts and may collapse to accessible icons only on narrow layouts.
- Commentary remains visually primary whenever commentary mode is active.
