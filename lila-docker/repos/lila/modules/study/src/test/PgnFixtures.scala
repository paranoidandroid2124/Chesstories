package lila.study

import chess.format.pgn.PgnStr

object PgnFixtures:

  val pgn1 = """
  { Root comment }
1. e4! $16 $40 $32 (1. d4?? d5 $146 { d5 is a good move }) (1. c4 { and }) (1. f4 { best }) 1... e6?! { e6 is a naughty move }
  """

  val pgn2 = """
  { Root comment }
1. e4! $16 $40 $32 (1. d4?? d5 $146 { d5 is a good move } 2. c4 (2. Nf3) (2. g3) 2... c5) (1. c4 { and }) (1. f4 { best }) 1... e6?! { e6 is a naughty move }
  """

  val pgn3 = "1. d4 d5 2. e4 e5"

  val pgn4 = """
[Event "nt9's Study: Chapter 2"]
[Site "https://lichess.org/study/Q41XcI0B/2LjSXwxW"]
[Result "*"]
[UTCDate "2023.04.22"]
[UTCTime "20:41:25"]
[Variant "Standard"]
[ECO "?"]
[Opening "?"]
[Annotator "https://lichess.org/@/nt9"]

{ this is a study without moves }

  """

  val pgn5 = """
[Event "nt9's Study: Chapter 3"]
[Site "https://lichess.org/study/Q41XcI0B/ypuKuiI4"]
[Result "*"]
[UTCDate "2023.04.30"]
[UTCTime "14:43:04"]
[Variant "Standard"]
[ECO "A40"]
[Opening "Queen's Pawn Game"]
[Annotator "https://lichess.org/@/nt9"]

{ shape glyphs }
1. d4 c6 (1... f6 2. c3?? $15 $138 $36 { jjjjjjjj } { [%csl Gd7,Re7,Bf6,Yh7,Yb7][%cal Gh4f4,Gf2e4] } (2. h4)) 2. f4 h5 3. h4 { [%csl Bd4,Gf4,Gf7][%cal Gc2c4,Gd2e4] }


  """

  val pgn6 = """
[Event "nt9's Study: Chapter 7"]
[Site "https://lichess.org/study/Q41XcI0B/JHnNE9Oi"]
[Result "*"]
[UTCDate "2023.05.23"]
[UTCTime "18:15:30"]
[Variant "Standard"]
[ECO "?"]
[Opening "?"]
[Annotator "https://lichess.org/@/nt9"]
[FEN "rnbqkbnr/pp1ppppp/2p5/8/3P1P2/8/PPP1P1PP/RNBQKBNR b KQkq - 0 2"]
[SetUp "1"]

{ custom position with Black to move }
  2... h5 3. b4


"""

  val pgn7 =
    """[Event "Norway Chess"]
[Site "-"]
[Date "2023.05.31"]
[Round "3"]
[WhiteTitle "GM"] [White "Carlsen, Magnus"]
[Black "So, Wesley"]
[Result "*"]
[Board "1"]
[WhiteClock "01:03:52"]
[WhiteElo "2853"]
[WhiteTitle "GM"]
[WhiteCountry "NOR"]
[WhiteFideId "1503014"]
[BlackClock "01:09:48"]
[BlackElo "2760"]
[BlackTitle "GM"]
[BlackCountry "USA"]
[BlackFideId "5202213"]

1. e4 { [%clk 1:59:59] } 1... e5 { [%clk 1:59:40] } 2. Nf3 { [%clk 1:59:51] } 2... Nc6 { [%clk 1:59:07] } 3. Bb5 { [%clk 1:59:44] } 3... Nf6 { [%clk 1:59:01] } 4. d3 { [%clk 1:59:36] } 4... Bc5 { [%clk 1:58:57] } 5. Bxc6 { [%clk 1:59:30] } 5... dxc6 { [%clk 1:58:53] } 6. O-O { [%clk 1:59:07] } 6... Nd7 { [%clk 1:57:22] } 7. h3 { [%clk 1:55:18] } 7... O-O { [%clk 1:55:11] } 8. Nc3 { [%clk 1:54:21] } 8... a5 { [%clk 1:52:37] } 9. a4 { [%clk 1:53:36] } 9... f6 { [%clk 1:49:06] } 10. Qe2 { [%clk 1:45:14] } 10... Re8 { [%clk 1:41:33] } 11. Be3 { [%clk 1:44:27] } 11... Bd6 { [%clk 1:36:47] } 12. Nd2 { [%clk 1:42:23] } 12... Nf8 { [%clk 1:36:24] } 13. f3 { [%clk 1:32:47] } 13... Ng6 { [%clk 1:17:22] } 14. Qf2 { [%clk 1:32:39] } 14... Be6 { [%clk 1:16:24] } 15. Ne2 { [%clk 1:31:54] } 15... Qd7 { [%clk 1:16:14] } 16. b3 { [%clk 1:27:21] } 16... Bb4 { [%clk 1:13:57] } 17. Rad1 { [%clk 1:24:50] } 17... b6 { [%clk 1:09:49] } 18. g4 { [%clk 1:03:52] }"""

  val pgn8 =
    """[Event "Rated Crazyhouse game"]
  [Site "https://lichess.org/tnA9XCCX"]
  [Date "2023.06.01"]
  [White "belowme"]
  [Black "Crazybugg"]
  [Result "1-0"]
  [UTCDate "2023.06.01"]
  [UTCTime "19:23:30"]
  [WhiteElo "2274"]
  [BlackElo "2184"]
  [WhiteRatingDiff "+4"]
  [BlackRatingDiff "-4"]
  [Variant "Crazyhouse"]
  [TimeControl "180+0"]
  [ECO "B01"]
  [Opening "Scandinavian Defense: Valencian Variation"]
  [Termination "Normal"]
  [Annotator "lichess.org"]

  1. e4 d5 2. exd5 Qxd5 3. Nc3 Qd8 { B01 Scandinavian Defense: Valencian Variation } 4. Bc4 e6 5. Qf3 Nf6 6. Nge2 Be7 7. d4 Bd7 8. Qxb7 Bc6 9. Bb5 O-O 10. Bxc6 Nxc6 11. Qxc6 P@b4 12. P@h6 bxc3 13. Qxc3 B@b4 14. hxg7 Bxc3+ 15. bxc3 Re8 16. B@e5 Ng4 17. B@f3 Nxe5 18. Bxa8 N@d3+ 19. cxd3 Nxd3+ 20. Kd2 B@h6+ 21. Kxd3 Qxa8 22. Bxh6 B@e4+ 23. Kd2 Q@b2+ 24. P@c2 Qxc2+ 25. Ke1 f6 26. R@h8+ Kf7 27. B@h5+ Bg6 28. N@g5+ fxg5 29. N@e5+ Kf6 30. N@g4+ Kf5 31. Ng3+ Kf4 32. P@e3# { White wins by checkmate. }"""

  val roundTrip = List(pgn1, pgn2, pgn3, pgn4, pgn5, pgn6, pgn7, pgn8).map(PgnStr(_))
