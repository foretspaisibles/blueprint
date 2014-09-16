%%% legend.mp -- Drawing graph legends

% Author: Michael Grünewald
% Date: Tue Sep 16 13:33:01 CEST 2014

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

%%% Dashpattertns

picture longshort;
longshort := dashpattern(on 6bp off 2bp on 2bp off 2bp);


%%% Miscellaneous Definitions

vardef pictureheight(expr p) =
  abs(ulcorner p - llcorner p)
enddef;


%%% Producing legends

% The legend is a small box (without frame) containing samples for the
% pen used for each graph and a legend.

pair legendbasepoint;
numeric legendlinesep;
numeric legendsamplewidth;
numeric legendsamplesep;
numeric legendheight;
picture legendpicture;

legendbasepoint := (0,0);	% Position of the first legend
legendlinesep := 4pt;		% Interline between legends
legendsamplewidth := 1cm;	% Width of the sample line
legendsamplesep := 6bp;		% Gutter between legend sample and text

legendheight := 		% The height of a M
  pictureheight("M" infont defaultfont);

def beginlegend =
  begingroup
    save currentpicture;
    picture currentpicture;
    currentpicture := nullpicture;
enddef;


def endlegend =
  legendpicture := currentpicture;
  endgroup;
  glabel.rt(legendpicture, OUT);
  legendpicture := nullpicture;
enddef;

def legenditem(expr label)(text p) =
  legendpicture := label;
  % This will not work well for multiline legends.

  draw (legendbasepoint + legendheight/2*up)
    --(legendbasepoint + legendheight/2*up + legendsamplewidth*right)
    p;
  draw label shifted (
      (legendbasepoint + legendsamplewidth*right)
      + (legendsamplesep*right));
  legendbasepoint :=
    legendbasepoint + (legendheight + legendlinesep) * down;
enddef;

%%% End of file `legend.mp'
