%%% pervasives.mp -- A basic library for Metapost

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

pervasives_version := 0.0;

%%% Ignore

def ignore(text ?) = enddef;

%%% Images

def interimpicture =
  save currentpicture;
  picture currentpicture;
  currentpicture := nullpicture;
enddef;

vardef picturewidth(expr _p) =
  length(lrcorner _p - llcorner _p)
enddef;

vardef pictureheight(expr _p) =
  length(ulcorner _p - llcorner _p)
enddef;


%%% Define a rotation by two vectors

vardef rotation(expr a, b) =
  save u, v, theta;
  pair u, v;
  transform theta;
  (0,0) transformed theta = (0,0);
  u = unitvector a;
  v = unitvector b;
  u transformed theta = v;
  (u rotated 90) transformed theta = v rotated 90;
  theta
enddef;


%%% Suffix list

% A suffix list can store a list of suffixes.  This list then can be
% accessed through the forsuffixlist command.

def suffixlist text LIST =
  numeric LIST._n;
  string LIST._contents;

  vardef LIST.init =
    #@._n := 0;
    #@._contents := "";
  enddef;

  vardef LIST.maybeinit =
    if not known #@._contents:
      #@.init;
    fi;
  enddef;

  vardef LIST.push(suffix $) =
    #@.maybeinit;
    if #@._n > 0:
      #@._contents := #@._contents & "," & (str $);
    else:
      #@._contents := str $;
    fi;
    gobble(incr #@._n);
  enddef;

enddef;

def forsuffixlist(suffix $, $$) =
  forsuffixes $ = scantokens $$._contents
enddef;


%%% Suffix-based dictionaries

def suffixalist(suffix _stem)(text _thingie) =
  _thingie _suffixalist_ _stem[];
  numeric _suffixalist_last _stem;
  _suffixalist_last _stem = 0;
  vardef _stem@# =
    suffixassoc(_stem, @#)
  enddef;
enddef;

vardef suffixassoc(suffix _stem, $) =
  save _stem;
  if not known _suffixalist_index _stem $:
    _suffixalist_index _stem $ = incr _suffixalist_last _stem;
  fi;
  _suffixalist_ _stem[_suffixalist_index _stem $]
enddef;


%%% String operations

vardef findsubstring(expr _text, _subtext) =
  save _i, _l, _m, _answer;
  numeric _i, _l, _m, _answer;
  _l = length(_text);
  _m = length(_subtext);

  for _i = 0 upto _l - _m:
    exitif known _answer;
    if _subtext = substring(_i, _i+_m) of _text: _answer = _i; fi;
  endfor;
  _answer
enddef;

vardef findprefix(expr _text, _subtext) =
  save _position, _prefix;
  numeric _position;
  string _prefix;

  _position = findsubstring(_text, _subtext);
  if known _position:
    _prefix = substring(0, _position) of _text;
  fi;
  _prefix
enddef;

vardef findsuffix(expr _text, _subtext) =
  save _position, _suffix;
  numeric _position;
  string _suffix;

  _position = findsubstring(_text, _subtext);
  if known _position:
    _suffix = substring(_position + length(_subtext), length(_text)) of _text;
  fi;
  _suffix
enddef;

%%% Number of arguments

vardef exprcount(text _argv) =
  save _i;
  numeric _i;
  _i := 0;
  for ? = _argv: gobble(incr _i); endfor;
  _i
enddef;

vardef suffixcount(text _argv) =
  save _i;
  numeric _i;
  _i := 0;
  forsuffixes ? = _argv: gobble(incr _i); endfor;
  _i
enddef;

%%% Enhanced mix and max

def _minmax_prologue primary u =
  if pair u:
    pair _u, _v;
  elseif string u:
    string _u, _v;
  else:
    numeric _u, _v;
  fi;
  _u = u;
enddef;

vardef _minf_internal@#(expr _first)(text _rest) =
  save _u, _v;
  _minmax_prologue @#(_first);
  for _item = _rest:
    _v := @#(_item);
    if _v < _u: _u := _v; fi;
  endfor;
  _u
enddef;

vardef _maxf_internal@#(expr _first)(text _rest) =
  save _u, _v;
  _minmax_prologue @#(_first);
  for _item = _rest:
    _v := @#(_item);
    if _v > _u: _u := _v; fi;
  endfor;
  _u
enddef;

vardef minf@#(text _argv) =
  if exprcount(_argv) > 1:
    _minf_internal@#(_argv)
  else:
    @#(_argv)
  fi
enddef;

vardef maxf@#(text _argv) =
  if exprcount(_argv) > 1:
    _maxf_internal@#(_argv)
  else:
    @#(_argv)
  fi
enddef;

vardef _mins_internal@#(suffix _first)(text _rest) =
  save _u, _v;
  _minmax_prologue @# _first;
  forsuffixes _item = _rest:
    _v := @# _item;
    if _v < _u: _u := _v; fi;
  endfor;
  _u
enddef;

vardef _maxs_internal@#(suffix _first)(text _rest) =
  save _u, _v;
  _minmax_prologue @# _first;
  forsuffixes _item = _rest:
    _v := @# _item;
    if _v > _u: _u := _v; fi;
  endfor;
  _u
enddef;

vardef mins@#(text _argv) =
  if suffixcount(_argv) > 1:
    _mins_internal@#(_argv)
  else:
    @# _argv
  fi
enddef;

vardef maxs@#(text _argv) =
  if suffixcount(_argv) > 1:
    _maxs_internal@#(_argv)
  else:
    @# _argv
  fi
enddef;

%%% Relations

vardef boxrelations@#(expr _height, _width) =
  z@#ul - z@#ll = z@#ur - z@#lr = _height*up;
  z@#ur - z@#ul = z@#lr - z@#ll = _width*right;
  z@#c = .5[z@#ll,z@#ur] = .5[z@#ul,z@#lr];
  z@#midupper = .5[z@#ul,z@#ur];
  z@#midleft = .5[z@#ll,z@#ul];
  z@#midright = .5[z@#lr,z@#ur];
  z@#midbottom = .5[z@#ll,z@#lr];
enddef;

vardef boxpath@# =
  z@#ul -- z@#ur -- z@#lr -- z@#ll -- cycle
enddef;


%%% Select font

string textfont, titlefont;
textfont = "\normalfont";
titlefont = "\normalfont";

vardef maketextfont(expr _font) =
  textfont & _font
enddef;

vardef maketitlefont(expr _font) =
  titlefont & _font
enddef;

%%% End of file `pervasives.mp'
