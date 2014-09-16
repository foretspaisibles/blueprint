%%% reveal.mp -- Revealing constructions

% Author: Michael Grünewald
% Date: Sun Sep 14 23:57:54 CEST 2014

% Blueprint (https://bitbucket.org/michipili/blueprint)
% This file is part of Blueprint
%
% Copyright © 2014 Michael Grünewald
%
% This file must be used under the terms of the CeCILL-B.
% This source file is licensed as described in the file COPYING, which
% you should have received as part of this distribution. The terms
% are also available at
% http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt

%%% Revealing constructions

pen reveal_pen;
color reveal_color;
boolean reveal_flag;
reveal_pen := pencircle scaled 5bp;
reveal_color := blue;
reveal_flag := false;

def reveal_drawoptions =
  drawoptions(withpen reveal_pen withcolor reveal_color);
enddef;

def withstyle_reveal =
  withpen reveal_pen withcolor reveal_color
enddef;

def maybe_reveal_origin =
  if reveal_flag:
    draw (0,0);
  fi;
enddef;

def maybe_reveal_currentpicture =
  if reveal_flag:
    draw bbox currentpicture withstyle_reveal;
  fi;
enddef;

def maybe_reveal_bbox =
  maybe_reveal_origin;
  maybe_reveal_currentpicture;
enddef;

pen reveal_pen;
color reveal_color;
boolean reveal_flag;
reveal_pen := pencircle scaled .5bp;
reveal_color := blue;
reveal_flag := false;

def reveal_drawoptions =
  save _op_;
  drawoptions(withpen reveal_pen withcolor reveal_color);
enddef;

vardef maybereveal(text _drawingcommands) =
  if reveal_flag:
    reveal_drawoptions;
    _drawingcommands;
  fi;
enddef;

def beginreveal = scantokens "maybereveal(" enddef;
def endreveal = scantokens ")" enddef;

vardef reveal text _drawingcommands =
  save reveal_flag;
  boolean reveal_flag;
  reveal_flag = true;
  _drawingcommands;
enddef;


vardef revealpoints@#(text _suffix_list) =
  maybereveal(
      forsuffixes $ = _suffix_list:
      label.top(str z@# $,  z@# $);
    endfor;
  );
enddef;

%%% End of file `reveal.mp'
