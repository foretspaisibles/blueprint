(* GChart -- Plotting and charting library for OCaml

   Blueprint (https://github.com/michipili/blueprint)
   This file is part of Blueprint

   Copyright © 2014–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

let dummy = Chart.{
    title = None;
    subtitle = None;
    x_axis = Axis.make ();
    y_axis = Axis.make ();
    legend = None;
    tooltip = None;
    series = [];
  }

let render chart (group : #GnoCanvas.group) =
  ()

class group gtkgroup =
  object(self)
    inherit GnoCanvas.group gtkgroup
    val mutable _chart = dummy
    method chart = _chart
    method set_chart newchart =
      _chart <- newchart;
      List.iter (fun item -> item#destroy ()) self#get_items;
      render newchart self
  end

let maybe_set_chart s = function
  | None -> s
  | Some(c) -> (s#set_chart c; s)


let group ?chart canvas=
  maybe_set_chart (new group canvas#root#as_group) chart

class chart gtkcanvas =
  let _mgroup = ref None in
  let get_group canvas =
    match !_mgroup with
    | Some(_group) -> _group
    | None -> let _group = group canvas in
      (_mgroup := Some(_group); _group)
  in
  object(self)
    inherit GnoCanvas.canvas gtkcanvas
    method chart =
      (get_group self)#chart
    method set_chart c =
      (get_group self)#set_chart c
  end

let chart ?chart ?(aa = false) =
  let create plist =
    let widget =
      let open GnomeCanvas.Canvas in
      if aa then new_canvas_aa () else new_canvas ()
    in
    Gobject.set_params widget plist;
    maybe_set_chart (new chart widget) chart
  in
  GContainer.pack_container [] ~create
