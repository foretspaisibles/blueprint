%%% example_legend.mp -- Example of a graph with legends

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


%%% Loading macro packages

input TEX;
input preamble;
input graph;
input legend;

%%% Definition of figures

beginfig(0)
  draw begingraph(90mm, 60mm);
    glabel.bot(TEX("Size of the dictionary"), OUT);
    glabel.lft(TEX("Execution time (s)"), OUT);
    gdraw "./times.d" dashed evenly;
    gdraw "./times.d";
    beginlegend;
    legenditem(TEX("Monadic"), dashed evenly);
    legenditem(TEX("Higher-Order"), withpen pencircle);
    endlegend;
  endgraph;
endfig;
bye;

%%% End of file `example_legend.mp'
