(* Chart -- Plotting and charting library for OCaml

   Blueprint (https://github.com/michipili/blueprint)
   This file is part of Blueprint

   Copyright © 2014–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)


(** Chart tooltip specifications. *)
module Tooltip :
sig

  (** The type of tooltips. *)
  type t = {
    value_prefix: string option;
    value_suffix: string option;
  }

  val value_prefix : string -> t
  (** Create a tooltip specification with just a value prefix. *)

  val value_suffix : string -> t
  (** Create a tooltip specification with just a value suffix. *)

  val make : string -> string -> t
  (** Create a tooltip specification with both a value prefix and a
      value suffix. *)
end

(** Chart specifications. *)
module Legend :
sig

  (** The type of legend specifications. *)
  type t = {
    position : position;
  }

  (** The type of legend positions. *)
  and position =
    | Right
    | Bottom
end


(** Axis specification *)
module Axis :
sig

  (** The type of numeric axis specifications. *)
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

  val make :
    ?title:string ->
    ?range_min:float -> ?range_max:float -> ?range_step:float ->
    ?categories:(float*float) array ->
    ?logarithmic:bool -> ?grid:bool ->
    ?tick_layout:tick_layout -> ?tick_labels:tick_labels ->
    ?tick_mark_style:tick_mark_style -> ?tick_label_angle:tick_label_angle ->
    unit -> t

  val text_tick_labels :
    ?origin:float ->
    ?step:float ->
    string array -> tick_labels
end

(** Data specification. *)
module Data :
sig
  type t =
    | Points of (float * float) array
    | Series of float array
end

(** Style specification. *)
module Style :
sig
  type t =
    | Line
    | Marker
    | LineMarker
    | Impulse
    | Histogram
    | VBar of float
    | HBar of float
end

(** Series specification. *)
module Series :
sig
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
