%%% example_schedule.mp -- Example of a schedule graphic

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
input schedule;

color schedulepalette[];

schedulepalette[0] := black;
schedulepalette[1] := (1.0, 0.2, 0.0);
schedulepalette[2] := (1.0, 0.8, 0.2);
schedulepalette[3] := (0.8, 0.8, 0.2);
schedulepalette[4] := (0.0, 0.6, 0.8);
schedulepalette[5] := (0.8, 0.2, 1.0);

beginfig(0)

  beginschedule;
  timescale(35mm) month;

  domain1("Project phases", schedulepalette1);
  activity1 1("Project planning", month1, month2);
  activity1 2("Pilot", month3, month3);
  activity1 3("Wave 1", month4, month7);
  activity1 4("Wave 2", month8, month10);

  domain2("Technical implementation", schedulepalette2);
  activity2 1("Setup", month2, month2);
  activity2 2("Test", month3, month3);
  milestone2 deploy1("Deploy 1", month5);
  milestone2 deploy2("Deploy 2", month7);

  domain3("Integration", schedulepalette3);
  activity3 1("Design", month1, month1);
  activity3 2("Development of critical functions", month2, month7);
  activity3 3("Complete Platform", month8, month10);
  activity3 4("Plop-Fizz Integration", month2, month5);
  activity3 5("Core Integration", month6, month 8);
  activity3 6("Web Plugins", month9, month10);

  domain4("Documentation", schedulepalette4);
  activity4 1("Train the trainer", month3, month6);
  activity4 2("Look for trainees", month3, month6);
  activity4 3("Bootcamp", month7, month10);
  milestone4 announce("Announce Bootcamp", month3);
  endschedule;

endfig;
bye;

%%% End of file `example_schedule.mp'
