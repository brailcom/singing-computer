% festival.ly --- Festival singing mode output
%
% Copyright (C) 2006 Brailcom, o.p.s.
%
% Author: Milan Zamazal <pdm@brailcom.org>
%
% COPYRIGHT NOTICE
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 2 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful, but
% WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
% or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.

\version "2.6.3"

#(ly:load "festival.scm")

% \festival #"filename" { \tempo N = X } { music }
festival =
#(def-music-function (parser location filename tempo music) (string? ly:music? ly:music?)
  (song:output-file music tempo filename)
  music)
