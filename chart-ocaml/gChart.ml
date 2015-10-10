(* GChart -- Plotting and charting library for OCaml

   Blueprint (https://github.com/michipili/blueprint)
   This file is part of Blueprint

   Copyright © 2014–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

module Messages =
struct
  let empty_chart () =
    "Empty chart"
end

let dummy = Chart.{
    title = None;
    subtitle = None;
    x_axis = Axis.make ();
    y_axis = Axis.make ();
    legend = None;
    tooltip = None;
    series = [];
  }

class empty group =
  object(self)
    inherit GnoCanvas.group group#as_group
    val _text =
      GnoCanvas.text
        ~props:[
          `FILL_COLOR("gray");
          `ANCHOR(`CENTER);
          `SIZE_POINTS(12.0);
          `MARKUP("<i>"^Messages.empty_chart()^"</i>")
        ]
        group
    method update_position =
      let (x, y) =
        group#canvas#window_to_world
          ~winx:(float_of_int(group#canvas#width / 2))
          ~winy:(float_of_int(group#canvas#height / 2))
      in
      self#set [ `X(x); `Y(y) ]
    initializer
      _text#reparent (self :> GnoCanvas.group);
      self#update_position;
      ignore [
        group#canvas#misc#connect#size_allocate
          ~callback:(fun _ -> self#update_position);
      ]
  end

let empty parent =
  let group = GnoCanvas.group parent in
  let answer = new empty group in
  answer

class title group =
  object(self)
    inherit GnoCanvas.group group#as_group
    val _title = GnoCanvas.text
        ~props:[
          `FILL_COLOR("black");
          `ANCHOR(`NORTH);
          `TEXT("title");
          `SIZE_POINTS(20.0);
        ]
        group

    val _subtitle = GnoCanvas.text
        ~props:[
          `FILL_COLOR("gray");
          `ANCHOR(`NORTH);
          `MARKUP("<i>subtitle</i>");
          `SIZE_POINTS(17.0);
        ]
        group

    method set_title text =
      _title#set [ `TEXT(text) ]

    method set_subtitle text =
      _subtitle#set [ `MARKUP("<i>"^text^"</i>") ]

    method update_position =
      let (x, y) =
        group#canvas#window_to_world
          ~winx:(float_of_int(group#canvas#width / 2))
          ~winy:(0.0)
      in
      _title#set [ `X(x); `Y(y) ];
      _subtitle#set [ `X(x); `Y(y +. 1.20*. _title#text_height) ]
    initializer
      _title#reparent (self :> GnoCanvas.group);
      _subtitle#reparent (self :> GnoCanvas.group);
      self#update_position;
      ignore [
        group#canvas#misc#connect#size_allocate
          ~callback:(fun _ -> self#update_position);
      ]
  end


let title parent =
  let group = GnoCanvas.group parent in
  let answer = new title group in
  answer



let render chart (group : #GnoCanvas.group) =
  ()

class chart gnome_canvas =
  let canvas = new GnoCanvas.canvas gnome_canvas in
  let group = GnoCanvas.group canvas#root in
  object(self)
    inherit GObj.widget canvas#as_widget
    val mutable _chart = dummy
    val _title = title group
    val _empty = empty group
    method private redraw =
      if _chart == dummy then
        begin
          _title#hide ();
          _empty#show ();
          ()
        end
      else
        begin
          _title#show ();
          _empty#hide ();
          ()
        end
    method chart =
      _chart
    method set_chart c =
      _chart <- c;
      self#redraw
  end

let maybe_set_chart widget = function
  | None -> widget
  | Some(c) -> (widget#set_chart c; widget)

let chart ?chart ?(aa = false) =
  let create plist =
    let gnome_canvas =
      let open GnomeCanvas.Canvas in
      if aa then new_canvas_aa () else new_canvas ()
    in
    Gobject.set_params gnome_canvas plist;
    maybe_set_chart (new chart gnome_canvas) chart
  in
  GContainer.pack_container [] ~create
