%%% component.mp -- Draw UML component diagrams

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

vardef spreadevenly(suffix _a, _b)(text _suffixes) =
  save _index;
  save _i, _n;
  string _index[];
  numeric _i, _n;
  _i := 0;
  forsuffixes $ = _a, _suffixes, _b:
    _index[incr _i] = str $;
  endfor;
  _n := _i;
  save _i;
  forsuffixes $ = _suffixes:
    $ - _a = whatever*(_b - _a);
  endfor;
  for _i = 3 upto _n:
    message (_index[_i] & " - " & _index[_i-1] &" = "
	  & _index[_i-1] & " - " & _index[_i-2]);
    (scantokens _index[_i]) - (scantokens _index[_i-1]) =
	(scantokens _index[_i-1]) - (scantokens _index[_i-2]);
  endfor;
enddef;


%%% Components

numeric componentvisualstereotypeheight;

componentvisualstereotypeheight = 12bp;


def _component_prologue =
  save _component_list;
  save componentbox;

  suffixlist _component_list;
  suffixalist(componentbox, picture);
enddef;



vardef component@#(expr _label) =
  save _height, _width;
  numeric _height, _width;
  _component_list.push(@#);
  componentbox@# = makecomponentbox(_label);
  _width =
    length(lrcorner componentbox@# - llcorner componentbox@#);
  _height =
    length(ulcorner componentbox@# - llcorner componentbox@#);
  z@#ul - z@#ll = z@#ur - z@#lr = _height*up;
  z@#ur - z@#ul = z@#lr - z@#ll = _width*right;
  z@#c = .5[z@#ll,z@#ur] = .5[z@#ul,z@#lr];
  z@#midupper = .5[z@#ul,z@#ur];
  z@#midleft = .5[z@#ll,z@#ul];
  z@#midright = .5[z@#lr,z@#ur];
  z@#midbottom = .5[z@#ll,z@#lr];
enddef;

vardef maybedrawcomponent@# =
  if known z@#c:
    drawcomponent@#;
  else:
    message("Component position is not known: " & str @#);
  fi;
enddef;

vardef drawcomponent@# =
  draw componentbox@# shifted z@#c;
enddef;

vardef drawcomponents(text _components) =
  forsuffixes $ = _components:
    maybedrawcomponent$;
  endfor;
enddef;

def drawcomponentvisualstereotype(expr _orig, _scale) =
  begingroup
    save a, b, c;
    save width, height, frac;
    save p, q, r;
    pair a, b, c;
    numeric width, height, frac;
    path p, q, r;
    c = _orig;
    height = _scale;
    width = height * sqrt(2) / 2;
    frac = height / 8;

    a = c + width*left + 2*frac*down;
    b = a + 4*frac*down;

    p = a + (width/4)*left + frac*down
      -- a + (width/4)*left + frac*up
      -- a + (width/4)*right + frac*up
      -- a + (width/4)*right + frac*down -- cycle;

    q = p shifted (b - a);

    r = a -- (c + width*left) -- c -- (c + height*down)
      -- (c + width*left + height*down) -- b;

    draw p;
    draw q;

    draw a -- b
      cutafter q
      cutbefore p;

    draw r
      cutafter q
      cutbefore p;

  endgroup;
enddef;

vardef makecomponentlabel(expr _text) =
  save _actual_text;
  save _label;

  save _style;
  save _position;

  string _actual_text;
  picture _label;

  string _style;
  string _position;

  _style = "\relax";
  _position = "c";

  _actual_text := "\begin{tabular}{>{"
    & _style
    & "}"
    & _position
    & "}"
    & _text
    & "\end{tabular}";

  _actual_text := "\begin{tabular}{c}"& _text &"\end{tabular}";

  _label := TEX(_actual_text);
  _label := _label shifted -center(_label);
  _label
enddef;


vardef makecomponentbox(expr _text) =
  interimpicture;
  save componentlabel;
  save stereoheight, extraspace, margin, minheight, p;

  picture componentlabel;
  numeric stereoheight, extraspace, margin, minheight;
  pair p[];

  stereoheight = componentvisualstereotypeheight;
  extraspace = stereoheight;
  margin = 2bp;
  minheight = 3*stereoheight;

  componentlabel = makecomponentlabel(_text);

  p0 = urcorner componentlabel + extraspace*right;

  p1 = p0 + margin*up + margin*right;
  p2 = p1 + 2*margin*down +
    (lrcorner componentlabel - urcorner componentlabel);
  p3 = p4 + 2*margin*down +
    (lrcorner componentlabel - urcorner componentlabel);
  p3 = p2 + 2*margin*left + 2*extraspace*left +
    (ulcorner componentlabel - urcorner componentlabel);

  drawcomponentvisualstereotype
    (urcorner componentlabel + extraspace*right, stereoheight);

  draw componentlabel;

  draw p1 -- p2 -- p3 -- p4 -- cycle;

  currentpicture
enddef;


%%% Connectors

numeric connectorlabeloff;
numeric connectorlabelscale;

connectorlabeloff = componentvisualstereotypeheight + 4bp;
connectorlabelscale = 0.95;


vardef makeconnectorstick(suffix a, b, c, d) =
  save p;
  path p;
  if known b and known c:
    p = a --- b -- c --- d;
  else:
    p = a -- d;
  fi;
  p
enddef;

vardef makeconnectorlabel(expr _label) =
  save p;
  picture p;
  if picture _label:
    p := _label;
  else:
    p := TEX("\parbox{12ex}{" & _label & "}")
      scaled connectorlabelscale;
    p := p shifted - center(p);
  fi;
  p
enddef;

vardef makeconnectorlabeloffset(expr _label, _path) =
  save p, u, offset, xoff, yoff;
  pair u, offset;
  picture p;
  numeric xoff, yoff;

  if picture _label:
    p = _label;
  else:
    p = makeconnectorlabel(_label);
  fi;

  u = unitvector(direction 1 of _path)
    rotated 90;

  xoff = connectorlabeloff + .5(xpart(lrcorner p - llcorner p));
  yoff = connectorlabeloff + .5(ypart(urcorner p - lrcorner p));

  offset =
    (xpart u) * xoff * right +
    (ypart u) * yoff * up;

  offset
enddef;

string _connector_label[];
numeric _connector_label_i;
_connector_label_i := 0;

vardef connectorlabel@#(expr _label) =
  _connector_label_index@# := incr _connector_label_i;
  _connector_label[_connector_label_index@#] = _label;
enddef;

vardef connectorrawlabel@# =
  save _label;
  string _label;
  if known _connector_label_index@#:
    _label = _connector_label[_connector_label_index@#];
  fi;
  _label
enddef;

boolean _connector_label_flag;
_connector_label_flag = true;

vardef withoutlabel text _instr =
  save _connector_label_flag;
  boolean _connector_label_flag;
  _connector_label_flag = false;
  _instr;
enddef;

vardef maybedrawconnectorcontrolpoints@# =
  if known z@#a and known z@#b:
    markconnectornode(z@#a, z@#b);
  fi;
enddef;

vardef markconnectornode(text _suffixes) =
  forsuffixes $ = _suffixes:
    draw $ withpen pensquare scaled 2bp;
  endfor;
enddef;


%%% Lollipop

numeric lollipopsize;

lollipopsize = componentvisualstereotypeheight;

vardef lollipop@#(expr _label) =
  connectorlabel@#(_label);
enddef;

vardef makelollipopstick(suffix a, b, c, d) =
  makeconnectorstick(a, b, c, d)
enddef;

vardef makelollipoptip(suffix d) =
  fullcircle
    scaled lollipopsize
    shifted d
enddef;

vardef makelollipoplabel(expr _label) =
  makeconnectorlabel(_label)
enddef;

vardef lollipopstick@# =
  makelollipopstick(z@#anchor, z@#a, z@#b, z@#tip)
enddef;

vardef lollipoptip@# =
  makelollipoptip(z@#tip)
enddef;

vardef lollipoplabel@# =
  save _rawlabel, p;
  string _rawlabel;
  picture p;
  _rawlabel = connectorrawlabel@#;
  if known _rawlabel:
    p = makelollipoplabel(_rawlabel);
  else:
    p = nullpicture;
  fi;
  p shifted (z@#tip + makeconnectorlabeloffset(p, lollipopstick@#))
enddef;

vardef drawlollipop@#=
  save _stick, _tip;
  path _stick, _tip;
  _stick = lollipopstick@#;
  _tip = lollipoptip@#;
  draw _stick cutafter _tip;
  draw _tip;
  maybedrawconnectorcontrolpoints@#;
  if _connector_label_flag and known connectorrawlabel@#:
    draw lollipoplabel@#;
  fi;
enddef;

vardef drawlollipops(text _suffixes) =
  forsuffixes $ = _suffixes:
    drawlollipop$;
  endfor;
enddef;


%%% Sockets

numeric socketsize;
string _socket_label[][];
socketsize = connectorlabeloff;

vardef socket@#(expr _label) =
  connectorlabel@#(_label);
enddef;

vardef socketstick@# =
  makeconnectorstick(z@#anchor, z@#a, z@#b, z@#tip)
enddef;

vardef sockettip@# =
  save theta;
  transform theta;

  theta = rotation(down, - direction 0 of reverse socketstick@#);

  halfcircle
    transformed theta
    scaled socketsize
    shifted z@#tip
enddef;

vardef socketlabel@# =
  save _rawlabel, p;
  string _rawlabel;
  picture p;
  _rawlabel = connectorrawlabel@#;
  if known _rawlabel:
    p = makeconnectorlabel(_rawlabel);
  else:
    p = nullpicture;
  fi;
  p shifted (z@#tip + makeconnectorlabeloffset(p, socketstick@#))
enddef;

vardef drawsocket@#=
  save _stick, _tip;
  path _stick, _tip;
  _stick = socketstick@#;
  _tip = sockettip@#;
  draw _stick cutafter _tip;
  draw _tip;
  maybedrawconnectorcontrolpoints@#;
  if _connector_label_flag and known connectorrawlabel@#:
    draw socketlabel@#;
  fi;
enddef;

vardef drawsockets(text _suffixes) =
  forsuffixes $ = _suffixes:
    drawsocket$;
  endfor;
enddef;


%%% Component ticks

numeric componenttickgap;
componenttickgap = 4bp;

vardef _component_tick(suffix _a, _b)(expr _dir, _theta) =
  save alpha, beta, u;
  pair u, alpha, beta;
  u = componenttickgap * _dir;
  alpha = _a + u;
  beta = _b - u;
  _theta[alpha,beta]
enddef;

vardef componenttick.bot@#(text _argv) =
  _component_tick(z@#ll, z@#lr, right, _argv)
enddef;

vardef componenttick.top@#(text _argv) =
  _component_tick(z@#ul, z@#ur, right, _argv)
enddef;

vardef componenttick.lft@#(text _argv) =
  _component_tick(z@#ul, z@#ll, down, _argv)
enddef;

vardef componenttick.rt@#(text _argv) =
  _component_tick(z@#ur, z@#lr, down, _argv)
enddef;



%%% Diagram

def componentdiagramorigin(suffix z) =
  z = (0,0);
enddef;

def begincomponentdiagram =
  begingroup;
  _component_prologue;

  save tick;
  def tick = componenttick enddef;
enddef;

def endcomponentdiagram =
  forsuffixlist($, _component_list):
    maybedrawcomponent$;
  endfor;
  endgroup;
enddef;

%%% End of file `component.mp`
