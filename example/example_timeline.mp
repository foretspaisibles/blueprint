%%% example_timeline.mp -- Example of a timeline graphic

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

input TEX;
input preamble;
input pervasives;
input reveal;
input timeline;

beginfig(0)

  beginroadmap;

  % Important time points
  time "WEEK 1",  7.0;
  time "WEEK 2", 14.0;
  time "WEEK 3", 21.0;
  time "WEEK 4", 28.0;
  time "WEEK 5", 35.0;


  phase "Initial\\Contact";

  % Initial Meeeting
  milestone 0, 1.0,  1;
  annotation "Initial Meeting",			12h, overline2;
  annotation "Initial Meeting",			 6h, underline2;


  phase "Planning";

  % Assessment
  milestone 1, 2.0,  2;
  annotation "Needs\\Assessment",		11h, overline1;
  annotation "Needs Assessment\\Feedback",	 6h, underline1 + u*right;

  % Research
  milestone 2, 3.0,  2;
  annotation "Research",			11h, overline3 + u*right;

  % Site Outline
  milestone 0, 2.0,  2;
  annotation "Site Outline",			11h, overline2 + u*left;
  annotation "Site Outline\\Review",		 5h, underline2;

  % Site Map
  milestone 0, 2.5,  1;
  annotation "Site Map",			12h, overline1;
  annotation "Site Map\\Review",		 5h, underline1 + 2u*right;

  % Wireframes
  milestone 2, 3.5, 2;
  annotation "Wireframes",			12h, overline2;
  annotation "Wireframes\\Review",		 5h, underline3;


  phase "Content";


  % Content Outline
  milestone 0, 2.0, 2;
  annotation "Content Outline",			 1h, overline1;

  % Content Submission
  milestone 1, 3.0, 2;
  annotation "Site Content\\Submission",	 5h, underline2;


  phase "Design";

  % Landing Page Artwork
  milestone 2, 4.5, 5;
  annotation "Landing Page\\Artwork",		 11h, overline3;
  annotation "Landing Page\\Artwork Review",	  6h, underline3;

  % Inner Page's Artwork
  milestone 1, 4.0, 3;
  annotation "Inner Page's\\Artwork",		 12h, overline2;
  annotation "Inner Page's\\Artwork Review",	  5h, underline2;


  phase "Development";

  % Coding
  milestone 2, 4.0, 5;
  annotation "Coding",				11h, overline3;

  % Validation
  milestone 2, 2.0, 2;
  annotation "Validation",			11h, overline1 + u*left;

  % Content Population
  milestone 0, 3.0, 2;
  annotation "Content Population",		11h, overline2;

  % Cross Platform & Browser Testing
  milestone 2, 2.5, 3;
  annotation "Cross Platform\\\& Browser Testing",12h, overline3;
  annotation "Final Review",			 7h, underline1;

  phase "Launch";

  % Refinement
  milestone 0, 1.0, 3;
  annotation "Refinement",			11h, overline2 + 2u*left;

  % Launch
  milestone 2, 1.5, 1.5;
  annotation "Launch",				11h, overline1;
  annotation "Search Engine\\Submission",	 1h, overline3 + 9u*right;

  endroadmap;


endfig;
bye;

%%% End of file `example_timeline.mp'
