(* GChart -- Plotting and charting library for OCaml

   Blueprint (https://github.com/michipili/blueprint)
   This file is part of Blueprint

   Copyright © 2014–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

class group : GnomeCanvas.group Gtk.obj ->
  object
    inherit GnoCanvas.group
    method chart : Chart.t
    method set_chart : Chart.t -> unit
  end

val group : ?chart:Chart.t -> #GnoCanvas.canvas -> group

class chart : GnomeCanvas.canvas Gtk.obj ->
  object
    inherit GnoCanvas.canvas
    method chart : Chart.t
    method set_chart : Chart.t -> unit
  end

val chart :
  ?chart:Chart.t ->
  ?aa:bool ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) ->
  ?show:bool -> unit -> chart
