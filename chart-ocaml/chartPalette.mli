(* ChartPalette -- Palettes used for charts

   Blueprint (https://github.com/michipili/blueprint)
   This file is part of Blueprint

   Copyright © 2014–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open ChartMVC

type palette
and color = int * int * int
and t = palette
val default : unit -> palette
val get : palette -> int -> int * int * int
val get_as_string : palette -> int -> string
val list : unit -> palette list
val of_index : int -> palette
val to_index : palette -> int

val model : unit -> palette model

class virtual abstract_observer : ?model:palette model -> unit ->
  [palette] ChartMVC.abstract_observer

val observer :
  callback:(palette -> unit) -> ?model:palette model -> unit ->
  palette ChartMVC.observer

val editor :
  ?packing:(GObj.widget -> unit) ->
  ?model:palette PatternMVC.model -> unit -> palette widget

val controller : model:palette model -> unit -> (palette, unit) controller
