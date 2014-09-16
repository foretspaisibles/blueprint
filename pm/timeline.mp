%%% timeline.mp -- Timeline graphics

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

% Required LaTeX packages: array.
% Required METAPOST packages: pervasives.

% TODO
% * Define distinct lengthes instead of using some `gap` over and over.

%%% Simple transparency

% alphacolor
%  Color used to fill transparent paths
%
% alphaweight
%  Weight used to mix colors
%
% alphafill PATH
%  Fill PATH with transparent alphacolor in currentpicture

color alphacolor;
picture _alpha_picture;
numeric alphaweight;

alphacolor := blue;
_alpha_picture := nullpicture;
alphaweight := 0.25;

def alphafill expr _c =
  _alpha_picture := nullpicture;
  _alpha_process(currentpicture, _c);
  addto currentpicture also _alpha_picture;
enddef;

def _alpha_process(expr _p, _c) =
  begingroup
    save _canvas;
    save _c_xmax, _c_xmin, _c_ymax, _c_ymin;
    save _item_xmin, _item_xmax, _item_ymin, _item_ymax;

    picture _canvas;
    _canvas = nullpicture;
    (_c_xmin, _c_ymin) = llcorner _c;
    (_c_xmax, _c_ymax) = urcorner _c;
    addto _canvas contour _c
      withcolor alphaweight[background, alphacolor];
    for _item within _p:
      numeric _item_xmin, _item_xmax, _item_ymin, _item_ymax;
      (_item_xmin, _item_ymin) = llcorner _item;
      (_item_xmax, _item_ymax) = urcorner _item;
      if (_item_xmax<= _c_xmin)	or (_item_xmin >= _c_xmax)
	or (_item_ymax<= _c_ymin) or (_item_ymin >= _c_ymax):
	  % The bounding boxes of _c and _item do not meet.
      else:
	if (not clipped _item) and (not bounded _item):
	  addto _canvas also _item withcolor
	    alphaweight[
	    (redpart _item, greenpart _item, bluepart _item),
	    alphacolor
	    ];
	else:
	  begingroup save _alpha_picture;
	    picture _alpha_picture; _alpha_picture = nullpicture;
	    _alpha_process(_item, pathpart _item);
	    addto _canvas also _alpha_picture;
	  endgroup;
	fi
      fi
    endfor
    clip _canvas to _c;
    addto _alpha_picture also _canvas;
  endgroup;
enddef;

%%% Timeline graphics

% A timeline graphics can represent the timeline of a project in a
% pleasant and impressive manner.  The milestones of the project are
% positioned on a timeline and represented as circular stickers
% whose attributes
% identify the phase, the importance and the involvement required by
% the milestone.
%
%
% Phase
%
%  A project phase consists of several milestones. It is identified by
%  an annotation and a colour.
%
%
% Milestone
%
%  A project milestone is characterised by the phase it belongs to,
%  the starting time, its importance and the involvement it requires.
%  A milestone can bear several annotations, which are formatted
%  according to its importance.
%
%
% Reference time
%
%  A reference time is an important date which must be displayed on
%  the timeline.

% roadmap_origin
%  The origin of the roadmap
%
% roadmap_t
%  The time unit
%
% roadmap_w
%  The work unit
%
% roadmap_fuzz
%  The roadmap fuzz
%
% roadmap_anchor_width
%  The size of milestone annotation anchors

pair roadmap_origin;
numeric roadmap_t;
numeric roadmap_w;
numeric roadmap_fuzz;
numeric roadmap_anchor_width;
string roadmap_style[];

roadmap_t :=  8.0mm;
roadmap_w := 12.0mm;
roadmap_anchor_width := 5bp;


% roadmap_save
%  Declare global constants for a roadmap

def roadmap_save =
  save roadmap_origin;
  save roadmap_t;
  save roadmap_w;
  save roadmap_fuzz;
  save roadmap_anchor_width
  pair roadmap_origin;
  numeric roadmap_t;
  numeric roadmap_w;
  numeric roadmap_fuzz;
  numeric roadmap_anchor_width
enddef;


roadmap_style0 = maketextfont("\small");
roadmap_style1 = maketextfont("\bfseries");
roadmap_style2 = maketextfont("\bfseries\Large");
roadmap_style3 = maketextfont("\Large");		% For untertitle
roadmap_style4 = maketextfont("\bfseries\Huge");
roadmap_style5 = maketextfont("\bfseries");		% For time annotation

% roadmap_make_generic_label(TEXT, STYLE, POSITION, CENTER)
%  Prepare a label containing TEXT
%
% The text is typesetted in a tabular with the given POSITION ("c" or
% "l" or "r") and the given STYLE.  Text spreading over multiple lines
% is supported through the use of "\\" to indicate new lines.
%
% If CENTER is true, then the hot spot of the picture is changed to
% its center.

vardef roadmap_make_generic_label(expr _text, _style, _position, _center) =
  begingroup
    save _actual_text;
    save _label;

    string _actual_text;
    picture _label;

    _actual_text := "\begin{tabular}{>{"
      & _style
      & "}"
      & _position
      & "}"
      & _text
      & "\end{tabular}";

    _label := TEX(_actual_text);

    if _center:
      _label := _label shifted -center(_label);
    fi;

    _label
  endgroup
enddef;


vardef roadmap_make_milestone_label(expr _text, _importance) =
  roadmap_make_generic_label(_text, roadmap_style[_importance], "c", true)
enddef;

vardef roadmap_make_title_label(expr _text, _importance) =
  roadmap_make_generic_label(_text, roadmap_style[_importance], "l", false)
enddef;

vardef roadmap_make_legend_label(expr _text) =
  roadmap_make_generic_label(_text, roadmap_style0, "l", false)
enddef;



def roadmap_picture_center(suffix _picture) =
  _picture := _picture shifted - center(_picture);
enddef;


vardef roadmap_bbox_spread(expr _picture, _spread) =
  begingroup
    save _bbox;
    save _p;
    path _bbox;
    pair _p[];

    _p0 = center _picture;
    _p1 = _p0 + _spread * (llcorner _picture - _p0);
    _p2 = _p0 + _spread * (lrcorner _picture - _p0);
    _p3 = _p0 + _spread * (urcorner _picture - _p0);
    _p4 = _p0 + _spread * (ulcorner _picture - _p0);
    _bbox = _p1 --- _p2 --- _p3 --- _p4 --- cycle;
    _bbox
  endgroup
enddef;


% roadmap_phase
%  Project phase
%
% The annotation is typeset in a tabular with one column.

def roadmap_phase text PHASE =

  string PHASE.annot;
  color PHASE.pigment;

  vardef PHASE.init(expr _annot, _pigment) =
    #@.annot := _annot;
    #@.pigment := _pigment;
  enddef;

enddef;


% roadmap_milestone
%  Milestone
%
% A milestone has two parts, a big coloured circular sticker and
% eventually some annotations.

numeric roadmap_milestone_h;
roadmap_milestone_h := 1/12;


def roadmap_milestone text MILESTONE =

  color MILESTONE.pigment;
  numeric MILESTONE.importance;
  numeric MILESTONE.involvement;
  numeric MILESTONE.start;

  numeric MILESTONE.diameter;
  path MILESTONE.sticker;
  pair MILESTONE.origin;

  % Anatomy of annotations
  %
  % An annotation has an anchor point, which looks like a handle
  % pinned on the milestone sticker. It also has a tag holding the
  % text and a connector joining the anchor and the tag.
  %
  % Note that the graphic elements themselves are not saved in the
  % structure, only their positions.

  numeric MILESTONE.annot_n;
  string MILESTONE.annot_text[];
  pair MILESTONE.annot_anchor[];
  pair MILESTONE.annot_position[];

  vardef MILESTONE.init(suffix _phase)(expr _importance, _involvement, _start) =
    #@.pigment = _phase.pigment;
    #@.importance = _importance;
    #@.involvement = _involvement;
    #@.start = _start;

    #@.diameter = _involvement * roadmap_w;
    #@.origin = _start * roadmap_t * right;
    #@.sticker = fullcircle
      scaled #@.diameter
      shifted #@.origin;

    #@.annot_n := 0;
  enddef;

  vardef MILESTONE.paint =
    begingroup
      save _milestone;
      path _milestone;

      alphacolor := #@.pigment;
      alphaweight := 0.50;

      alphafill #@.sticker;
    endgroup
  enddef;

  vardef MILESTONE.annotate(expr _text, _anchor, _position) =
    begingroup
      % The actual anchor is computed so that _anchor is a `time
      % coordinate', that is 0.00 corresponds to noon, 0.25 to 3
      % o'clock and so on.
      save _actual_anchor;
      _actual_anchor := 0.25 - _anchor;

      #@.annot_n := #@.annot_n + 1;
      #@.annot_text[#@.annot_n] := _text;
      #@.annot_anchor[#@.annot_n] :=
        point (_actual_anchor * length(#@.sticker)) of #@.sticker;
      #@.annot_position[#@.annot_n] :=
	_position + #@.origin;

    endgroup;
  enddef;

  vardef MILESTONE.paint_annotation =
    for i = 1 upto #@.annot_n:
      #@._paint_annot(
	  #@.annot_text[i],
	  #@.annot_anchor[i],
	  #@.annot_position[i]
      );
    endfor;
  enddef;

  vardef MILESTONE._paint_annot(expr _text, _anchor, _position) =
    begingroup
      save _bullet;
      save _label;
      save _connect;
      save _cartouche;
      save _offset;

      path _bullet[];
      path _cartouche;
      picture _label;
      string _actual_text;
      pair _offset;

      _bullet0 := fullcircle
	scaled roadmap_anchor_width
	shifted _anchor;

      _bullet1 := fullcircle
	scaled (0.5 * roadmap_anchor_width)
	shifted _anchor;

      unfill _bullet0;
      draw _bullet0 withpen pencircle scaled 1bp;
      fill _bullet1;

      _label := roadmap_make_milestone_label(_text, #@.importance);
      _cartouche := roadmap_bbox_spread(_label, 1.41);
      draw _label shifted _position;

      draw _anchor -- _position
	cutbefore _bullet0
	cutafter( _cartouche shifted _position);


    endgroup;
  enddef;

enddef;

% roadmap_timeline
%  Timeline with annotations for important times

def roadmap_timeline text TIMELINE =

  string TIMELINE.annot[];
  numeric TIMELINE.annot_n;
  numeric TIMELINE.offset[];
  pair TIMELINE.z[];

  pair TIMELINE.origin;
  numeric TIMELINE.height;
  numeric TIMELINE.thickness;
  color TIMELINE bgcolor;
  color TIMELINE fgcolor;

  vardef TIMELINE.init =
    #@.origin := (0,0);
    #@.annot_n := 0;
    #@.height := .25 roadmap_w;
    #@.width := 0mm;
    #@.thickness := 2bp;
    #@.bgcolor := black;
    #@.fgcolor := white;
  enddef;

  vardef TIMELINE.pack(expr _annot, _offset) =
    #@.annot_n := #@.annot_n + 1;
    #@.annot[#@.annot_n] := _annot;
    #@.offset[#@.annot_n] := _offset;
    #@.z[#@.annot_n] = #@.origin + _offset*right;
  enddef;

  vardef TIMELINE.reveal =
    reveal_drawoptions;
    for i = 1 upto #@.annot_n:
      dotlabel.bot("annot"&decimal i, #@.z[i]);
    endfor;
    drawoptions();
  enddef;

  vardef TIMELINE.paint_cartouche(suffix _a, _b) =
    begingroup
      save makepoint;
      save _contour;
      save _side;
      save _s;
      path _contour;
      path _side[];
      numeric _s[];

      % makepoint(_x, _h, _v)
      %
      %  Compute the intersection time on _x.sticker of a radius of
      %  _x.sticker looking in the direction _h and shifted by the
      %  timeline height in direction _v.

      vardef makepoint(suffix _x)(expr _h, _v) =
	xpart(_x.sticker intersectiontimes (_x.origin + #@.height * _v
	      --- _x.origin + .5 _x.diameter * _h + #@.height * _v))
      enddef;

      _s0 = makepoint(_a, left, up);
      _s1 = makepoint(_a, left, down);
      _s2 = makepoint(_b, right, down);
      _s3 = makepoint(_b, right, up);

      _side0 = subpath (_s0,_s1) of _a.sticker;
      _side1 = point _s1 of _a.sticker --- point _s2 of _b.sticker;
      _side2 = subpath(_s2, length _b.sticker +_s3) of _b.sticker;
      _side3 = point _s3 of _b.sticker --- point _s0 of _a.sticker;

      _contour = _side0 & _side1 & _side2 & _side3 .. cycle;

      fill _contour
	withcolor #@.bgcolor;

      for i = 1, 3:
	undraw _side[i]
	  withpen pencircle scaled #@.thickness;
      endfor;

    endgroup;
  enddef;

  vardef TIMELINE.paint =
    begingroup
      save _label, _width;
      picture _label;
      numeric _width;

      for i = 1 upto #@.annot_n:
	_label := TEX(roadmap_style5 & " " & #@.annot[i]);
	roadmap_picture_center(_label);
	_width := length(#@.z[i] --- #@.origin)
	  + length(lrcorner _label --- llcorner _label);
	if _width > #@.width: #@.width := _width; fi
	draw _label
	  shifted #@.z[i]
	  withcolor white;
      endfor;

      drawoptions();
    endgroup;
  enddef;

enddef;




% roadmap_canvas
%  Canvas for roadmaps

color _roadmap_canvas_palette[];

_roadmap_canvas_palette[1] := black;
_roadmap_canvas_palette[2] := (1.0, 0.2, 0.0);
_roadmap_canvas_palette[3] := (1.0, 0.8, 0.2);
_roadmap_canvas_palette[4] := (0.8, 0.8, 0.2);
_roadmap_canvas_palette[5] := (0.0, 0.6, 0.8);
_roadmap_canvas_palette[6] := (0.8, 0.2, 1.0);



def roadmap_canvas text CANVAS =

  numeric CANVAS.phase_n;		% Number of phases
  numeric CANVAS.milestone_n;		% Number of milestones
  numeric CANVAS.milestone_s;		% Last milestone offset
  numeric CANVAS.height;		% Height of the canvas

  roadmap_phase CANVAS.phase[];		% List of phases
  roadmap_milestone CANVAS.milestone[];	% List of milestones
  roadmap_timeline CANVAS.timeline;	% Timelime

  numeric CANVAS.rolegap;		% Gap between roadmap and roles
  numeric CANVAS.legendsep;		% Gap between legend and underline
  numeric CANVAS.legendgap;		% Gap between legend elements
  numeric CANVAS.legendwidth;		% Minimal width of legend elements
  numeric CANVAS.legendannot;		% Length of annotation connectors
  numeric CANVAS.lineskip;		% Gap between lines

  string CANVAS.title;			% Graphic title
  string CANVAS.subtitle;		%  and subtitle
  string CANVAS.owner;			% Project owner
  string CANVAS.stakeholder;		% Project stakeholder
  string CANVAS.milestone_occurence;	% Text for "Milestone occurence"
  string CANVAS.involvement_less;	% Text for "Less"
  string CANVAS.involvement_more;	% Text for "More"
  string CANVAS.importance_average;	% Text for "Average"
  string CANVAS.importance_important;	% Text for "Important"

  string CANVAS.legend_milestones_head;	% Text for "Milestones
  string CANVAS.legend_involvement_head;% Text for "Involvement"
  string CANVAS.legend_importance_head;	% Text for "Importance"
  string CANVAS.legend_phase_head;	% Text for "Phase"


  vardef CANVAS.init =
    #@.phase_n := 0;
    #@.milestone_n := 0;
    #@.milestone_s := 0;
    #@.height := 50mm;
    #@.rolegap := 10mm;
    #@.legendsep := 4mm;
    #@.legendgap := 10mm;
    #@.legendwidth := 50mm;
    #@.legendannot := 20mm;
    #@.lineskip := 1mm;

    #@.title := "Project";
    #@.subtitle := "Milestones, involvement, importance \& timeline";
    #@.owner := "Owner";
    #@.stakeholder := "Stakeholder";
    #@.milestone_occurence := "Milestone\\Occurence";
    #@.legend_milestones_head := "Milestones";
    #@.legend_involvement_head := "Involvement";
    #@.legend_importance_head := "Importance";
    #@.legend_phase_head := "Phase";
    #@.involvement_less := "Less";
    #@.involvement_more := "More";
    #@.importance_average := "Average";
    #@.importance_important := "Important";

    #@.timeline.init;
  enddef;

  vardef CANVAS.current_phase =
    #@.phase[#@.phase_n]
  enddef;

  vardef CANVAS.current_milestone =
    #@.milestone[#@.milestone_n]
  enddef;

  vardef CANVAS.add_phase(expr _text) =
    #@.phase_n := #@.phase_n + 1;
    #@.phase[#@.phase_n].init(
     	_text,
     	_roadmap_canvas_palette[#@.phase_n]);
  enddef;

  vardef CANVAS.add_milestone(expr _importance, _involvement, _offset) =
    #@.milestone_s := #@.milestone_s + _offset;
    #@.milestone_n := #@.milestone_n + 1;
    #@.milestone[#@.milestone_n].init(
	#@.phase[#@.phase_n],
	_importance,
	_involvement,
	#@.milestone_s);
  enddef;

  vardef CANVAS.add_annotation(expr _text, _anchor, _position) =
    #@.milestone[#@.milestone_n].annotate(_text, _anchor, _position);
  enddef;

  vardef CANVAS.add_time(expr _annot, _offset) =
    #@.timeline.pack(_annot, _offset * roadmap_t);
  enddef;

  vardef CANVAS.paint_roadmap =
    % Maybe we need to render milestones stickers in an extra picture
    for i = 1 upto #@.milestone_n:
      #@.milestone[i].paint;
    endfor;

    for i = 1 upto #@.milestone_n:
      #@.milestone[i].paint_annotation;
    endfor;

    #@.timeline.paint_cartouche(#@.milestone[1], #@.milestone[#@.milestone_n]);
    #@.timeline.paint;
    #@.paint_role(#@.owner, up);
    #@.paint_role(#@.stakeholder, down);
  enddef;


  vardef CANVAS.paint_role(expr _role, _direction) =
    begingroup
      save _label;
      save _height;
      picture _label;

      _label = roadmap_make_title_label(_role, 3);
      roadmap_picture_center(_label);

      draw _label
	rotated 90
	shifted (2 #@.rolegap * left + .5 #@.height * _direction);
    endgroup;
  enddef;

  %
  % Roadmap graphic elements
  %

  vardef CANVAS.roadmap =
    begingroup;
      interimpicture;
      #@.paint_roadmap;
      currentpicture
    endgroup
  enddef;

  vardef CANVAS.cartouche =
    begingroup
      interimpicture;
      save _title;
      save _subtitle;
      picture _title;
      picture _subtitle;

      _title = roadmap_make_title_label(#@.title, 4);
      _subtitle = roadmap_make_title_label(#@.subtitle, 3);

      draw _title;
      draw _subtitle
	shifted (llcorner _title - ulcorner _subtitle + #@.lineskip * down);

      maybe_reveal_bbox;
      currentpicture
    endgroup
  enddef;

  vardef CANVAS.legend_annotation(expr _text, _main) =
    begingroup
      interimpicture;
      save _main_width;
      save _actual_width;
      numeric _main_width;
      numeric _actual_width;

      _main_width = length(lrcorner _main - llcorner _main);

      if _main_width > #@.legendwidth:
	_actual_width = _main_width
      else:
	_actual_width = #@.legendwidth;
      fi;

      draw roadmap_make_title_label(_text, 3)
	shifted (#@.legendsep * up);
      draw (0,0) --- (_actual_width * right);

      currentpicture
    endgroup
  enddef;


  vardef CANVAS.legend_compose(expr _text, _main) =
    save _annotation;
    picture _annotation;
    _annotation = #@.legend_annotation(_text, _main);
    draw _main shifted (#@.rolegap * down - ulcorner _main);
    draw _annotation;
  enddef;

  vardef CANVAS.legend_milestones =
    begingroup;
      interimpicture
      save _main;
      picture _main;

      _main = begingroup;
	interimpicture
	save _milestone;
	save _origin;
	pair _origin;

	_origin = (roadmap_anchor_width, 0);
	roadmap_milestone _milestone;
	_milestone.init(#@.phase[0], 0, 0, 0);
	_milestone._paint_annot
	  (#@.milestone_occurence, _origin,
	    _origin + #@.legendannot * right rotated -45);

	maybe_reveal_bbox;
	currentpicture
      endgroup;

      #@.legend_compose(#@.legend_milestones_head, _main);
      currentpicture
    endgroup
  enddef;


  vardef CANVAS.legend_involvement =
    begingroup;
      interimpicture;
      save _main;
      picture _main;

      _main = begingroup;
	interimpicture
	save _sticker;
	save _offset;
	save _label;
	save _room;
	path _sticker[];
	pair _offset;
	pair _anchor[];
	picture _label[];
	path _room[];

	_offset = .5 * 3 * roadmap_w * down;

	_sticker1 = fullcircle
	  scaled (1 * roadmap_w)
	  shifted (_offset + 1.25 * roadmap_w * right);

	_sticker2 = fullcircle
	  scaled (3 * roadmap_w)
	  shifted (_offset + 2.75 * roadmap_w * right);

	_anchor1 = point (1.50 * 0.25 * length _sticker1) of _sticker1
	  - (0.25 * roadmap_w * up rotated 45);

	_anchor2 = point (- 0.50 * 0.25 * length _sticker2) of _sticker2
	  + (0.25 * roadmap_w * up rotated 45);

	_anchor3 = _anchor1 + (#@.legendannot * up rotated 45);
	_anchor4 = _anchor2 + (#@.legendannot * right rotated - 45);

	_label1 = roadmap_make_milestone_label(#@.involvement_less, 0)
	  shifted _anchor3;

	_label2 = roadmap_make_milestone_label(#@.involvement_more, 0)
	  shifted _anchor4;

	_room1 = roadmap_bbox_spread(_label1, 1.41);
	_room2 = roadmap_bbox_spread(_label2, 1.41);


	alphacolor := _roadmap_canvas_palette[4];
	alphaweight := 0.50;

	for i = 1,2:
	  alphafill _sticker[i];
	endfor;

	for i = 1,2:
	  draw _anchor[i] -- _anchor[i+2]
	    cutafter _room[i];
	  draw _label[i];
	endfor;

	maybe_reveal_bbox;

	currentpicture := currentpicture
	  shifted (xpart(ulcorner currentpicture) * left);

	currentpicture
      endgroup;

      #@.legend_compose(#@.legend_involvement_head, _main);
      currentpicture
    endgroup
  enddef;

  vardef CANVAS.legend_importance =
    begingroup;
      interimpicture;
      save _main;
      picture _main;

      _main = begingroup
	interimpicture;
	save _label;
	picture _label[];

	_label1 =
	  roadmap_make_generic_label
	  (#@.importance_average, roadmap_style0, "l", false);

	_label2 =
	  roadmap_make_generic_label
	  (#@.importance_important, roadmap_style2, "l", false);

	draw _label1;
	draw _label2
	  shifted (llcorner _label1 - ulcorner _label2 + #@.lineskip * down);

	maybe_reveal_bbox;
	currentpicture
      endgroup;

      #@.legend_compose(#@.legend_importance_head, _main);
      currentpicture
    endgroup
  enddef;

  vardef CANVAS.legend_phase =
    begingroup;
      interimpicture;
      save _main;
      picture _main;

      _main = begingroup
	interimpicture;
	save _center;
	save _sticker;
	save _label;
	pair _center[];
	path _sticker[];
	picture _label[];

	for i = 1 upto #@.phase_n:
	  _center[i] = (-0.25 + 1.6 *i) * roadmap_w * right
	    + 0.5 * roadmap_w * down;
	  _sticker[i] = fullcircle
	    scaled (2 * roadmap_w)
	    shifted _center[i];

	  alphacolor := #@.phase[i].pigment;
	  alphaweight := 0.50;
	  alphafill _sticker[i];
	endfor;

	for i = 1 upto #@.phase_n:
	  _label[i] = roadmap_make_milestone_label(#@.phase[i].annot, 1)
	    shifted _center[i];
	  draw _label[i] withcolor white;
	endfor;

	maybe_reveal_bbox;
	currentpicture
      endgroup;

      #@.legend_compose(#@.legend_phase_head, _main);
      currentpicture
    endgroup
  enddef;


  vardef CANVAS.paint =
    begingroup
      save _item;
      picture _item[];

      _item1 = #@.roadmap;
      _item2 = #@.cartouche;
      _item3 = #@.legend_milestones;
      _item4 = #@.legend_involvement;
      _item5 = #@.legend_importance;
      _item6 = #@.legend_phase;

      draw _item1;
      draw _item2 shifted (ulcorner _item1 + 2*#@.rolegap*up);
      draw _item3 shifted (llcorner _item1 + llcorner _item3 + 2*#@.rolegap*down);
      draw _item4
	shifted (llcorner _item1
	  + llcorner _item3 + 2*#@.rolegap*down
	  + (lrcorner _item3 - llcorner _item3 + #@.legendgap*right));
      draw _item5
	shifted (llcorner _item1
	  + llcorner _item3 + 2*#@.rolegap*down
	  + (lrcorner _item3 - llcorner _item3 + #@.legendgap*right)
	  + (lrcorner _item4 - llcorner _item4 + #@.legendgap*right));

      draw _item6
	shifted (llcorner _item1
	  + llcorner _item3 + 2*#@.rolegap*down
	  + (lrcorner _item3 - llcorner _item3 + #@.legendgap*right)
	  + (lrcorner _item4 - llcorner _item4 + #@.legendgap*right)
	  + (lrcorner _item5 - llcorner _item5 + #@.legendgap*right));
    endgroup;
  enddef;

enddef;


def beginroadmap =
  begingroup;
  save _canvas;
  save overline;
  save underline;
  save h;
  save u;

  save phase;
  save milestone;
  save annotation;
  save time;

  pair overline[];
  pair underline[];
  for i = 1 upto 3:
    overline[i] = (2 + i) * roadmap_w * up;
    underline[i] = (2 + i) * roadmap_w * down;
  endfor;

  numeric h;
  h = roadmap_milestone_h;

  numeric u;
  u = roadmap_w / 8;

  roadmap_canvas _canvas;
  _canvas.init;

  vardef phase text _t =
    _canvas.add_phase(_t);
  enddef;

  vardef milestone text _t =
    _canvas.add_milestone(_t);
  enddef;

  vardef annotation text _t =
    _canvas.add_annotation(_t);
  enddef;

  vardef time text _t =
    _canvas.add_time(_t);
  enddef;

  vardef title text _t =
    _canvas.title := _t;
  enddef;

  vardef subtitle text _t =
    _canvas.subtitle := _t;
  enddef;

  vardef owner text _t =
    _canvas.owner := _t;
  enddef;

  vardef stakeholder text _t =
    _canvas.stakeholder := _t;
  enddef;

  vardef milestone_occurence text _t =
    _canvas.milestone_occurence := _t;
  enddef;

  vardef involvement_less text _t =
    _canvas.involvement_less := _t;
  enddef;

  vardef involvement_more text _t =
    _canvas.involvement_more := _t;
  enddef;

  vardef importance_average text _t =
    _canvas.importance_average := _t;
  enddef;

  vardef importance_important text _t =
    _canvas.importance_important := _t;
  enddef;

  vardef legend_milestones_head text _t =
    _canvas.legend_milestones_head := _t;
  enddef;

  vardef legend_involvement_head text _t =
    _canvas.legend_involvement_head := _t;
  enddef;

  vardef legend_importance_head text _t =
    _canvas.legend_importance_head := _t;
  enddef;

  vardef legend_phase_head text _t =
    _canvas.legend_phase_head := _t;
  enddef;

enddef;


def endroadmap =
  _canvas.paint;
  endgroup;
enddef;

%%% End of file `timeline.mp'
