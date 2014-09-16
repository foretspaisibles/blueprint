%%% schedule.mp -- Project schedules

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

% A schedule project displays cards on a wand, arranged along two
% axis: a time axis and a domain axis.  Each card represent either an
% activity or a milestone.

numeric domainsep, legendsep, headersep;
numeric activityahjump;
numeric milestoneanchorwidth;
numeric milestoneanchorsep;

string legendtitle, legendactivity, legendmilestone;

string activitystyle, domainstyle;

activitystyle = "\normalfont";
domainstyle = "\normalfont";


domainsep = 12pt;
legendsep = 3 domainsep;
headersep = 18pt;
activityahjump = 4bp;
milestoneanchorwidth = 5bp;
milestoneanchorsep = 6pt;

legendtitle = "Legend";
legendactivity = "Activity";
legendmilestone = "Milestone";

def beginschedule =
  domainprologue;
  activityprologue;
  milestoneprologue;
enddef;

def endschedule =
  domainepilogue;
  activityepilogue;
  milestoneepilogue;
  drawtimescale;
enddef;

%
% Timerange
%

def timeprologue =
  save a, b;
enddef;

def timescale(expr _u) text TIME =
  timeprologue;
  save timeunit, timefirst, timelast;
  numeric timeunit, timefirst, timelast;
  timeunit = _u;
  vardef TIME[] =
    if unknown timefirst: timefirst := @; fi;
    if unknown timelast: timelast := @; fi;
    if timefirst > @: timefirst := @; fi;
    if timelast < @: timelast := @; fi;
    (@)*timeunit
  enddef;
enddef;

vardef timebegin@# =
  @# - timeunit
enddef;

vardef timeend@# =
  @#
enddef;

vardef drawtimescale =
  for _i = timefirst upto timelast:
    label(TEX(decimal(_i)), ((_i-1/2)*timeunit,domainsep));
  endfor;
enddef;

%
% Domain
%

def domainprologue =
  suffixlist _domain_list;
  suffixalist(domainbox, picture);
  suffixalist(domaincolor, color);
  save domainupperline, domainbottomline, domainsepline, domainleftline;
  numeric domainupperline[], domainbottomline[], domainsepline[];
  numeric domainleftline[];
enddef;

def domainepilogue =
  save _deepest, _leftmost;
  numeric _deepest, _leftmost;
  forsuffixlist($, _domain_list):
    domainresolve$;
    draw domainbox$
      shifted z$c;
  endfor;
  _leftmost = mins.domainleftline(scantokens _domain_list._contents);
  _deepest = mins.domainbottomline(scantokens _domain_list._contents);
  drawlegend(_leftmost, _deepest - legendsep);
enddef;

vardef domainresolve[] =
  save _deepest, _seen;
  numeric _deepest;
  boolean _seen;
  if known domainbottomline[@-1]:
    domainupperline@ = domainbottomline[@-1] - domainsep;
  else:
    domainupperline@ = 0;
  fi;
  domainresolve_activity@;
  domainresolve_sepline@;
  domainresolve_milestone@;
  domainresolve_bottomline@;
  domainresolve_leftline@;
enddef;

vardef domainresolve_activity[] =
  save _seen;
  boolean _seen;
  _seen := false;
  forsuffixlist($, _activity_list):
    if (activitydomain$ = @) and unknown activityupperline$:
      if _seen:
	activityupperline$ = findactivityfirstfit$;
      else:
	activityupperline$ = domainupperline@;
	_seen := true;
      fi;
    fi;
  endfor;
enddef;

vardef domainresolve_sepline[] =
  save _deepest;
  numeric _deepest;
  _deepest = domainupperline@;
  forsuffixlist($, _activity_list):
    if (activitydomain$ = @) and known activitybottomline$:
      if activitybottomline$ < _deepest:
	_deepest := activitybottomline$;
      fi;
    fi;
  endfor;
  if known _deepest:
    domainsepline@ = _deepest;
  fi;
enddef;

vardef domainresolve_milestone[] =
  forsuffixlist($, _milestone_list):
    if (milestonedomain$ = @) and unknown milestoneupperline$:
      milestoneupperline$ = domainsepline@;
    fi;
  endfor;
enddef;

vardef domainresolve_bottomline[] =
  save _deepest;
  numeric _deepest;
  _deepest = domainsepline@;
  forsuffixlist($, _milestone_list):
    if (milestonedomain$ = @) and known milestonebottomline$:
      if milestonebottomline$ < _deepest:
	_deepest := milestonebottomline$;
      fi;
    fi;
  endfor;
  if known _deepest:
    domainbottomline@ = _deepest;
  fi;
enddef;

vardef domainresolve_leftline[] =
  domainleftline@ = xpart(z@midleft);
enddef;


vardef domain[](expr _label, _color) =
  domaincolor@ = _color;
  domainbox@ = makedomainbox(_label);
  boxrelations@(whatever, picturewidth(domainbox@));

  ypart(z@ul) = domainupperline@;
  ypart(z@ll) = domainbottomline@;
  xpart(z@midright) = -headersep;
  _domain_list.push(@);
enddef;

vardef makedomainbox(expr _text) =
  save _label, _style;
  picture _label;
  string _style;
  _style = domainstyle;
  _label = TEX("\begin{tabular}{>{"&_style&"}c}"& _text &"\end{tabular}");
  _label := _label shifted - center(_label);
  _label
enddef;


%
% Activities
%

def activityprologue =
  save _activity_list;
  save activitybox;

  suffixlist _activity_list;
  suffixalist(activitybox, picture);
enddef;

def activityepilogue =
  forsuffixlist($, _activity_list):
    if unknown activityupperline$:
      activityupperline$ = findactivityfirstfit$;
    fi;
  endfor;
  forsuffixlist($, _activity_list):
    maybedrawactivity$;
  endfor;
enddef;

vardef activity[]@#(expr _label)(suffix _start, _end) =
  timeprologue;
  _activity_list.push(@ @#);
  _activity_domain @ @# = @;
  xpart(z@ @# midleft) = timebegin _start;
  xpart(z@ @# midright) = timeend _end;
  makeactivity@ @#(_label);
enddef;

vardef makeactivity@#(expr _label) =
  save _height;
  numeric _height;
  activitybox@# = makeactivitybox(_label);
  _height = magstep4*pictureheight(activitybox@#);
  boxrelations@#(_height+ magstep2*2activityahjump, activitywidth@#);
  timeperiod@#(z@#midleft, z@#midright, _height);
enddef;

vardef drawactivity@# =
  save _pigment;
  color _pigment;
  if known activitydomain@#:
    _pigment = 1/2[white,domaincolor[activitydomain@#]];
  fi;
  if known _pigment:
    fill timeperiodpath@#
      withcolor _pigment;
  fi;
  draw timeperiodpath@#
    withpen pencircle scaled 1bp;
  draw activitybox@#
    shifted z@#c;
enddef;

vardef maybedrawactivity@# =
  if known z@#c:
    drawactivity@#;
  else:
    message("maybedrawactivity." & str @# & ": unknwon position");
  fi;
enddef;

vardef activityupperline@# =
  ypart(z@#midupper)
enddef;

vardef activitybottomline@# =
  ypart(z@#midbottom)
enddef;

vardef activitystart@# =
  xpart(z@#midleft)
enddef;

vardef activityend@# =
  xpart(z@#midright)
enddef;

vardef activitywidth@# =
  xpart(z@#midright - z@#midleft)
enddef;

vardef activityheight@# =
  ypart(z@#midupper - z@#midbottom)
enddef;

vardef activitydomain@# =
  _activity_domain@#
enddef;

vardef makeactivitybox(expr _text) =
  save _label, _style;
  string _style;
  picture _label;
  _style = activitystyle;
  _label = TEX("\begin{tabular}{>{"& _style &"}c}"& _text &"\end{tabular}");
  _label := _label shifted - center(_label);
  _label
enddef;

vardef doactivityoverlap(suffix $, $$) =
  (activityend$ > activitystart$$) and
    (activityend$$ > activitystart$)
enddef;

vardef findactivityfirstfit@# =
  save _firstfit;
  numeric _firstfit;
  forsuffixlist($, _activity_list):
    if activitydomain$ = activitydomain@#:
      if unknown _firstfit:
	_firstfit := activityupperline$;
      fi;
      if known activityupperline$:
	if (_firstfit <= activityupperline$) and
	  doactivityoverlap($,@#):
	    _firstfit := activitybottomline$;
	fi;
      fi;
    fi;
  endfor;
  if unknown _firstfit:
    _firstfit := 0;
  fi;
  _firstfit
enddef;


%
% Milestones
%

def milestoneprologue =
  save _milestone_list;
  save milestonebox;

  suffixlist _milestone_list;
  suffixalist(milestonebox, picture);
enddef;

def milestoneepilogue =
  forsuffixlist($, _milestone_list):
    maybedrawmilestone$;
  endfor;
enddef;

def milestonepreamble =
  save spot;
enddef;

vardef milestone[]@#(expr _label)(suffix _spot) =
  timeprologue;
  milestonepreamble;
  _milestone_list.push(@ @#);
  _milestone_domain @ @# = @;
  xpart(z@ @#spot) = timebegin _spot;
  makemilestone@ @#(_label);
enddef;

vardef makemilestone@#(expr _label) =
  milestonepreamble;
  save _height, _width;
  numeric _height, _width;
  z@#spot = z@#midleft;
  milestonebox@# = makemilestonebox(_label);
  _height = magstep2 * pictureheight(milestonebox@#) + domainsep;
  _width = picturewidth(milestonebox@#)
    + milestoneanchorwidth
    + milestoneanchorsep;
  boxrelations@#(_height, _width);
enddef;

vardef drawmilestone@# =
  save _bullet;
  path _bullet[];

  _bullet0 := fullcircle
    scaled milestoneanchorwidth
    shifted z@#spot;

  _bullet1 := fullcircle
    scaled (1/2milestoneanchorwidth)
    shifted z@#spot;

  unfill _bullet0;
  draw _bullet0 withpen pencircle scaled 1bp;
  fill _bullet1;
  draw milestonebox@#
    shifted(z@#c - 1/2(milestoneanchorwidth + milestoneanchorsep)*right);
enddef;

vardef maybedrawmilestone@# =
  if known z@#c:
    drawmilestone@#;
  else:
    message("maybedrawmilestone." & str @# & ": unknwon position");
  fi;
enddef;

vardef milestoneupperline@# =
  ypart(z@#midupper)
enddef;

vardef milestonebottomline@# =
  ypart(z@#midbottom)
enddef;

vardef milestonestart@# =
  xpart(z@#midleft)
enddef;

vardef milestoneend@# =
  xpart(z@#midright)
enddef;

vardef milestonewidth@# =
  xpart(z@#midright - z@#midleft)
enddef;

vardef milestoneheight@# =
  ypart(z@#midupper - z@#midbottom)
enddef;

vardef milestonedomain@# =
  _milestone_domain@#
enddef;

vardef makemilestonebox(expr _text) =
  save _label;
  picture _label;
  _label = TEX("\begin{tabular}{c}"& _text &"\end{tabular}");
  _label := _label shifted - center(_label);
  _label
enddef;


%
% Timeperiod
%

numeric _timeperiod.ahlength, _timeperiod.ahtension, _timeperiod.ahangle;

def _timeperiod_preamble =
  save ahlength, ahweight, ahjump;
  numeric ahlength, ahweight, ahjump;
  ahlength = 9pt;
  ahweight = 3/8;
  ahjump = activityahjump;
enddef;

def _timeperiod_prologue =
  save _timeperiod_list;
  suffixlist _timeperiod_list;
  suffixalist(timeperiodcolor, color)
enddef;

def _timeperiod_epilogue =
  forsuffixlist($, _timeperiod_list):
    maybedrawtimeperiod$;
  endfor;
enddef;

vardef timeperiodheadrelations@#(expr _base, _direction, _height) =
  _timeperiod_preamble;

  save _normal;
  pair _normal;

  _normal = _direction rotated 90;
  z@#h = _base;
  z@#h = z@#b + ahlength*_direction;
  z@#tu - z@#td = _height*_normal;
  1/2[z@#td, z@#tu] = z@#b;
  z@#hu = z@#tu + ahjump*_normal;
  z@#hd = z@#td - ahjump*_normal;

  revealpoints@#(b, tu, hu, h, hd, td);

enddef;

vardef timeperiodheadpath@# =
  _timeperiod_preamble;
  z@#tu -- z@#hu{(ahweight[z@#b, z@#h] - z@#hu)}
    .. {-((ahweight[z@#b, z@#hu] - z@#h))}z@#h{(ahweight[z@#b, z@#hd] - z@#h)}
    .. {-((ahweight[z@#b, z@#h] - z@#hd))}z@#hd
    -- z@#td
enddef;

vardef maybedrawtimeperiod@# =
  if known z @# c:
    drawtimeperiod@#;
  else:
    message("drawtimeperiod." & str @# &": unknown position");
  fi;
enddef;

vardef drawtimeperiod@# =
  save _pigment, _alphaweight;
  color _pigment;
  numeric _alphaweight;

  _alphaweight = 0.25;

  if known timeperiodcolor@#:
    _pigment = timeperiodcolor@#;
  else:
    _pigment = red;
  fi;

  fill z@#ut -- z@#ua-- z@#ub -- z@#hd -- z@#db -- z@#da -- z@#dt -- cycle
    withcolor _alphaweight[white, _pigment];
enddef;

vardef timeperiod@#(expr _a, _b, _height) =
  save heada, headb;
  timeperiodheadrelations@# heada(_a, left, _height);
  timeperiodheadrelations@# headb(_b, right, _height);
enddef;

vardef timeperiodpath@# =
  save heada, headb;
  ((z@#heada td -- z@#headb tu)
      & timeperiodheadpath@#headb
      & (z@#headb td -- z@#heada tu)
      & timeperiodheadpath@#heada) -- cycle
enddef;


%
% Legend
%

def drawlegend(expr _xoffset, _yoffset) =
  save _legend, _activity, _milestone;
  save _legendlabel;
  picture _legendlabel;

  _legendlabel = makeactivitybox("\textbf{"&legendtitle&"}");

  boxrelations _legend
    (pictureheight(_legendlabel), picturewidth(_legendlabel));

  xpart(z _legend midleft) = _xoffset;
  ypart(z _legend midupper) = _yoffset;

  z _activity ul = z _legend ll + 7bp*down;
  z _milestone ul = z _activity ll;

  draw _legendlabel
    shifted (z _legend c - (10pt*right));

  activitywidth _activity = 2timeunit;
  makeactivity _activity(legendactivity);
  makemilestone _milestone(legendmilestone);
  drawactivity _activity;
  drawmilestone _milestone;
enddef;

%%% End of file `schedule.mp'
