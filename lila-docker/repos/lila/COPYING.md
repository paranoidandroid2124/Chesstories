# Copying Chesstory

Any file in this project that does not state otherwise and is not listed as an
exception below is part of Chesstory and copyright (c) 2012-2026
the Chesstory and lila authors.

For a list of the upstream lila authors see the commit log or
https://github.com/lichess-org/lila/graphs/contributors.

Chesstory is free software; you can redistribute and/or modify it under the terms
of the GNU Affero General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version.

Chesstory is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
details.

See the LICENSE file for a copy of the _GNU Affero General Public License_.

## Exceptions (free)

<!-- prettier-ignore -->
Files | Author(s) | License
--- | --- | ---
public/font/lichess.{sfd,ttf,woff2} | [Dave Gandy](http://fontawesome.io/), [GitHub](https://github.com/primer/octicons), [Webalys](http://www.webalys.com/), [Zurb](http://zurb.com/playground/foundation-icon-fonts-3), [Daniel Bruce](http://www.entypo.com/), [Shapemade](http://steadysets.com/), [Sergey Shmidt](http://designmodo.com/linecons-free/) and the Lichess authors | [OFL](http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=OFL), [MIT](https://github.com/primer/octicons/blob/master/LICENSE), [CC BY 3.0](https://creativecommons.org/licenses/by/3.0/), AGPLv3+
public/font/lichess-chess.{sfd,woff2} | the [pgn4web](http://pgn4web.casaschi.net/home.html) authors | [GPLv2+](https://www.gnu.org/licenses/gpl-2.0.txt)
public/font/chesstory.woff2, public/font/chesstory-chess.woff2 | Chesstory fork (renamed copies of `lichess*.woff2`) | Same licenses as corresponding `lichess*.woff2` font assets above
Noto Sans in public/font | [Google](https://fonts.google.com/specimen/Noto+Sans) | [Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0)
Roboto in public/font | [Christian Robertson](https://fonts.google.com/specimen/Roboto) | [Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0)
Segment7 in public/font | [Cedders](https://www.fontspace.com/cedders) | [SIL Open Font License](https://www.fontspace.com/help#license-17)
public/flags | [Go Squared Ltd.](https://www.gosquared.com/resources/flag-icons/) | [MIT](https://github.com/gosquared/flags/blob/master/LICENSE.txt)
public/images/board | the lila authors and [pirouetti](https://lichess.org/@/pirouetti) | AGPLv3+
public/piece/mono | Thibault Duplessis and [Colin M.L. Burnett](https://en.wikipedia.org/wiki/User:Cburnett) | [GPLv2+](https://www.gnu.org/licenses/gpl-2.0.txt)
public/piece/cburnett | [Colin M.L. Burnett](https://en.wikipedia.org/wiki/User:Cburnett) | [GPLv2+](https://www.gnu.org/licenses/gpl-2.0.txt)
public/piece/chessnut | [Alexis Luengas](https://github.com/LexLuengas) | [Apache 2.0](https://github.com/LexLuengas/chessnut-pieces/blob/master/LICENSE.txt)
public/piece/letter | [usolando](https://lichess.org/@/usolando) | AGPLv3+
public/piece/pirouetti | [pirouetti](https://lichess.org/@/pirouetti) | AGPLv3+
public/piece/merida | Armando Hernandez Marroquin | [GPLv2+](https://www.gnu.org/licenses/gpl-2.0.txt)
public/piece/shapes | [flugsio](https://github.com/flugsio/chess_shapes) | [CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/)
public/piece/pixel | therealqtpi | AGPLv3+
public/piece/rhosgfx | [RhosGFX](https://rhosgfx.itch.io/) | [CC0 1.0](https://creativecommons.org/publicdomain/zero/1.0/)
public/piece/fantasy | [Maurizio Monge](https://github.com/maurimo/chess-art) | [MIT](https://github.com/maurimo/chess-art/blob/main/LICENSE)
public/piece/spatial | [Maurizio Monge](https://github.com/maurimo/chess-art) | [MIT](https://github.com/maurimo/chess-art/blob/main/LICENSE)
public/piece/celtic | [Maurizio Monge](https://github.com/maurimo/chess-art) | [MIT](https://github.com/maurimo/chess-art/blob/main/LICENSE)
public/piece/kiwen-suwi | [neverRare](https://github.com/neverRare) | [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)
public/piece/mpchess | [Maxime Chupin](https://github.com/chupinmaxime) | [GPL3v3+](https://www.gnu.org/licenses/quick-guide-gplv3.en.html)
public/piece/Firi | [James Faure](https://github.com/jfaure/Firi-pieceset) | [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)
## Exceptions (non-free)

<!-- prettier-ignore -->
Files | Author(s) | License
--- | --- | ---
public/logo/chesstory*.png, public/logo/chesstory.svg, public/favicon.ico, public/apple-touch-icon.png | Chesstory contributors | Chesstory trademark and brand assets. No trademark license is granted.

- The other sounds in public/sound
- The other artwork in public/images

## External services

Chesstory deployments may use these external services depending on the active
environment and feature configuration:

- Hosting, CDN, DNS, logging, and monitoring providers for operating the service
- Email delivery providers for account confirmation, password reset, and support
- [hCaptcha](https://hcaptcha.com) for signup and abuse-prevention challenges
- Public Lichess and Chess.com endpoints when a user requests public game imports
- AI or model providers when a user requests AI-assisted chess explanations
- Browser-side Stockfish engine assets and related WebAssembly packages listed in
  `ui/lib/package.json`
