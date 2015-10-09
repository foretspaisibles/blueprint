(* Chart -- Charts for OCaml and Gtk2

   Blueprint (https://github.com/michipili/blueprint)
   This file is part of Blueprint

   Copyright © 2014–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)


module Tooltip =
struct

  type t = {
    value_prefix: string option;
    value_suffix: string option;
  }

  let value_prefix prefix = {
    value_prefix = Some(prefix);
    value_suffix = None;
  }

  let value_suffix suffix = {
    value_prefix = None;
    value_suffix = Some(suffix);
  }

  let make prefix suffix = {
    value_prefix = Some(prefix);
    value_suffix = Some(suffix);
  }
end

module Legend =
struct
  type t = {
    position : position;
  }
  and position =
    | Right
    | Bottom
end

module Axis =
struct
  type t = {
    title: string option;
    range_min: float option;
    range_max: float option;
    range_step: float option;
    categories: (float * float) array option;
    logarithmic: bool;
    grid: bool;
    tick_layout: tick_layout;
    tick_labels: tick_labels option;
    tick_mark_style: tick_mark_style option;
    tick_label_angle: tick_label_angle;
  }

  and tick_layout =
    | Auto
    | Fixed of float array

  and tick_mark_style =
    | Inside
    | Centered
    | Outside

  and tick_labels =
    | Numeric
    | Text of (string * float) array

  and tick_label_angle =
    | Horizontal
    | Vertical
    | Diagonal

  let make ?title
    ?range_min ?range_max ?range_step
    ?categories
    ?(logarithmic = false) ?(grid = false)
    ?(tick_layout = Auto) ?tick_labels
    ?tick_mark_style ?(tick_label_angle = Horizontal)
    () = {
    title;
    range_min;
    range_max;
    range_step;
    categories;
    logarithmic;
    grid;
    tick_layout;
    tick_labels;
    tick_mark_style;
    tick_label_angle;
  }

  let text_tick_labels ?(origin = 0.0) ?(step = 1.0) labels =
    Text(Array.init
           (Array.length labels)
           (fun i -> ( labels.(i), origin +. ((float_of_int i) *. step))))
end

module Data =
struct
  type t =
    | Points of (float * float) array
    | Series of float array
end

module Style =
struct
  type t =
    | Line
    | Marker
    | LineMarker
    | Impulse
    | Histogram
    | VBar of float
    | HBar of float
end

module Series =
struct
  type t = {
    name: string;
    style: Style.t;
    data: Data.t;
  }
end

type t = {
  title: string option;
  subtitle: string option;
  x_axis: Axis.t;
  y_axis: Axis.t;
  legend: Legend.t option;
  tooltip: Tooltip.t option;
  series: Series.t list;
}
