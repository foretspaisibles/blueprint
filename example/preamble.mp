%%% preamble.mp -- Our preamble

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

TEXPRE("%&latex"
  & char(10)
  & "\documentclass{article}"
  & "\usepackage{mathpazo}"
  & "\usepackage[utf8]{inputenc}"
  & "\usepackage[T1]{fontenc}"
  & "\usepackage{array}"
  & "\begin{document}"
  );
TEXPOST("\end{document}");

prologues := 3;
outputtemplate := "%j-%c.mps";
defaultfont := "uplr8t";

%%% End of file `preamble.mp'
