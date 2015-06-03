(* ChartProperties -- Chart properties

   Blueprint (https://github.com/michipili/blueprint)
   This file is part of Blueprint

   Copyright © 2014–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

type major_tick_marks =
  | MAJOR_TICK_MARKS_NONE
  | MAJOR_TICK_MARKS_INSIDE
  | MAJOR_TICK_MARKS_CENTERED
  | MAJOR_TICK_MARKS_OUTSIDE

type axis_and_borders =
  | AXIS_AND_BORDERS_CHART_BORDERS
  | AXIS_AND_BORDERS_XY of bool * bool

type y_axis = {
  y_axis_show_value_labels: bool;
  y_axis_show_minimum_value: bool;
  y_axis_show_value_title: bool;
  y_axis_show_major_tick_marks: major_tick_marks;
  y_axis_show_minor_tick_marks: bool;
  y_axis_show_gridline: bool;
  y_axis_scale: (float -> float) option;
}
