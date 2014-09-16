%%% example_umlcomponent.mp -- Example of a schedule graphic

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
input component;

beginfig(0)

  numeric u;
  numeric v;
  u = 22mm;
  v = 10mm; % For text measure

  %
  % Rulers,
  %  used to balance the picture
  %

  ruler.ui = 0;
  ruler.interface = 1u;
  ruler.data = 4u;
  ruler.infra = 9u;

  ruler.join.security = ruler.data + 3/2u;
  ruler.join.persistence = ruler.data + 5/2u;

  vardef dataprovider[](expr _label) =
    save _interface;
    string _interface;
    _interface = _label;
    component@("<Data Provider>\\" &_label);
    lollipop@ access("Data Access");
    lollipop@ interface(_interface);
    socket@ security("Access Control");
    socket@ persistence("Persistence");

    xpart(z@c) = ruler data;

    z@ access anchor =
      tick.bot@(1/2);
    z@ interface anchor =
      tick.lft@(1/2);
    z@ security anchor =
      tick.rt@(2/3);
    z@ persistence anchor =
      tick.rt@(1/3);

    z@ access tip =
      z@ access anchor + 1/2 u*down;
    z@ interface tip =
      z@ interface anchor + u*left;
    z@ security tip =
      z7 access tip;
    z@ persistence tip =
      z8 persistence tip;

    z@ security a =
      z@ security anchor + whatever * right;
    z@ security b =
      z7 access tip + whatever * left;
    ruler join security =
      xpart(z@ security a) =
      xpart(z@ security b);

    z@ persistence a =
      z@ persistence anchor + whatever * right;
    z@ persistence b =
      z8 persistence tip + whatever * left;
    ruler join persistence =
      xpart(z@ persistence a) =
      xpart(z@ persistence b);
  enddef;

  begincomponentdiagram;
  %
  % Market Data & Scenarios
  %

  component1("<User Interface>\\Student Administration");
  socket1 facilities("Facilities");
  socket1 student("Student");
  socket1 seminar("Seminar");
  socket1 schedule("Schedule");

  forsuffixes $ = facilities, student, seminar, schedule:
    z1 $ anchor = tick.rt1(1/2);
    z1 $ a = z1 $ anchor + 1/2 u * right;
    xpart(z1 $ b) = xpart(z1 $ a);
    ypart(z1 $ b) = ypart(z1 $ tip);
  endfor;

  z1 facilities tip = z3 interface tip;
  z1 student tip = z4 interface tip;
  z1 seminar tip = z5 interface tip;
  z1 schedule tip = z6 interface tip;


  component2("<User Interface>\\Seminar Management");
  socket2 seminar("Seminar");
  z2 seminar anchor =
    tick.rt2(1/2);
  z2 seminar tip =
    z1 seminar tip;

  x1c = x2c = ruler ui;
  y1c = - y2c = 2u;


  dataprovider3("Facilities");
  dataprovider4("Student");
  dataprovider5("Seminar");
  dataprovider6("Schedule");

  forsuffixes $ = 3, 4, 5, 6:
    y$c = (2*$ -9)*u;
  endfor;


  component7("<Infrastructure>\\Security");
  lollipop7 encrypt("Encryption");
  lollipop7 access("Access Control");

  component8("<Infrastructure>\\Persistence");
  lollipop8 persistence("Persistence");
  socket8 jdbc("JDBC");

  component9("<Database>\\University Database");
  lollipop9 jdbc("JDBC");


  z7 access anchor =
    tick.lft7(1/2);
  z7 encrypt anchor =
    tick.top7(1/2);
  z8 persistence anchor =
    tick.lft8(1/2);

  forsuffixes $ = 7access, 8persistence:
    z $ tip = z $ anchor + u*left;
  endfor;

  z7 encrypt tip = z7 encrypt anchor + 1/2u * up;

  forsuffixes $ = 7, 8, 9:
    x$c = ruler infra;
  endfor;

  y7c = y1c;
  y8c = y2c;
  y9c = 0;

  z8 jdbc anchor =
    tick.top8(1/2);

  z9 jdbc anchor =
    tick.bot9(1/2);

  z8 jdbc tip =
    z9 jdbc tip =
    1/2[z8 jdbc anchor, z9 jdbc anchor];

  % Draw sockets
  forsuffixes $ = facilities, student, seminar, schedule:
    withoutlabel drawsocket1 $;
  endfor;

  forsuffixes $ = 3, 4, 5, 6:
    drawlollipop $ access;
    drawlollipop $ interface;
    withoutlabel drawsocket $ security;
    withoutlabel drawsocket $ persistence;
  endfor;

  drawlollipop7 access;
  drawlollipop7 encrypt;
  drawlollipop8 persistence;

  withoutlabel drawsocket8 jdbc;
  drawlollipop9 jdbc;

  endcomponentdiagram;

endfig;

bye;

%%% End of file `example_umlcomponent.mp'
