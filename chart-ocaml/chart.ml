(* Chart -- Charts for OCaml and Gtk2

Author: Michael Grünewald
Date: Sun Jan  4 23:09:24 CET 2015

Blueprint (https://github.com/michipili/blueprint)
This file is part of Blueprint

Copyright © 2014–2015 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open Printf

let series1 = [| 65.; 59.; 80.; 81.; 56.; 55.; 40.; |]
let series2 = [| 28.; 48.; 40.; 19.; 86.; 27.; 90.; |]

(* Canvas properties *)
module CanvasProperties =
struct
  let canvas_properties_scale_unit =
    4.0
  type t = {
    bg: GDraw.color;
    scale: float;
  }

class ['observer] subject =
object(self)
  inherit ['observer] PatternSubjectObserver.subject
  val mutable bg = `WHITE
  val mutable scale = 1.0
  method get = {
    bg;
    scale;
  }
  method notify_change =
    self#notify (fun obs -> obs#canvas_properties_changed)
  method set props =
    bg <- props.bg;
    scale <- props.scale;
    self#notify_change
  method set_bg x =
    bg <- x;
    self#notify_change
  method set_scale x =
    scale <- x;
    self#notify_change
end

class virtual ['subject] observer s =
object(self : 'selftype)
  inherit ['subject] PatternSubjectObserver.observer
  val mutable subject = s
  method virtual canvas_properties_changed : 'selftype subject -> unit
  method set_canvas_properties (s :  'selftype subject) =
    subject <- s;
    subject#add self;
    self#canvas_properties_changed subject
end

class ['subject] user canvas s =
object(self)
  inherit ['subject] observer s
  method canvas_properties_changed s =
    let canvas_properties = s#get in
    canvas#set_pixels_per_unit
	     (canvas_properties_scale_unit *. canvas_properties.scale);
    canvas#misc#modify_bg [`NORMAL, canvas_properties.bg];
end

class ['subject] editor s =
  let canvas_properties = s#get in
  let container = GPack.hbox () in
  let scale =
    GData.adjustment
      ~value:canvas_properties.scale
      ~lower:0.01
      ~upper:8.0
      ~step_incr:0.01
      ~page_incr:0.1
      ~page_size:0.0
      ()
  in
  let scaleselect =
    GRange.scale
      `HORIZONTAL
      ~adjustment:scale
      ~digits:2
      ~packing:container#add () in
  let bgselect =
    GButton.color_button
      ~color:(GDraw.color canvas_properties.bg)
      ~packing:container#add () in
  object(self)
  inherit GObj.widget container#as_widget
  inherit ['subject] observer s
  method canvas_properties_changed s =
    let canvas_properties = s#get in
    scale#set_value canvas_properties.scale;
    bgselect#set_color (GDraw.color canvas_properties.bg);
  method private callback_changed () =
    let canvas_properties = {
      scale = scale#value;
      bg = (`COLOR bgselect#color);
    } in
    subject#set canvas_properties
  initializer
    ignore [
	scale#connect#value_changed ~callback:self#callback_changed;
	bgselect#connect#color_set ~callback:self#callback_changed;
      ]
  end

  let editor
      ?packing
      ?canvas_properties
      () =
  let open GHelper.Maybe.Operator in
  let apply_on_widget f x =
    f (x :> GObj.widget)
  in
  let actual_canvas_properties =
    match canvas_properties with
    | Some(p) -> p
    | None -> new subject
  in
  new editor actual_canvas_properties
  |> GHelper.maybe_callback (packing >>= apply_on_widget)
end

let series_add_x series =
  let f k =
    match (k mod 2 = 0, k/2) with
    | true, i -> float_of_int (i * 20)
    | false, i -> 100. -.(series.(i))
  in
  let n = Array.length series in
  Array.init (2*n) f

let lineprops color =
  [`SMOOTH(true); `WIDTH_PIXELS(4); `FILL_COLOR(color)]
class ['subject] chart () =
  let canvas_properties = new CanvasProperties.subject in
  let vbox = GPack.vbox () in
  let view = GBin.scrolled_window
               ~packing:vbox#add
               ~hpolicy:`AUTOMATIC
               ~vpolicy:`AUTOMATIC
               ()
  in
  let canvas = GnoCanvas.canvas ~aa:true ~packing:view#add () in
  let line1 = GnoCanvas.line
                ~points:(series_add_x series1)
                ~props:(lineprops "#2669a0") canvas#root
  in
  let line2 = GnoCanvas.line
                ~points:(series_add_x series2)
                ~props:(lineprops "#e12637" ) canvas#root
  in
  object
    inherit GObj.widget vbox#as_widget
    inherit ['subject] CanvasProperties.user canvas canvas_properties
    initializer
      canvas#set_pixels_per_unit 5.0;
  end

let chart
      ?packing
      ?canvas_properties
      () =
  let open GHelper.Maybe.Operator in
  let apply_on_widget f x =
    f (x :> GObj.widget)
  in
  new chart ()
  |> GHelper.maybe_callback (packing >>= apply_on_widget)
  |> GHelper.maybe_apply canvas_properties (fun x -> x#set_canvas_properties)
