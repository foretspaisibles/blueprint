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


(* Smart variable *)
module SmartVariable =
struct
  class type ['a] observer =
  object
    inherit GObj.widget
    method attach : 'a GUtil.variable -> unit
    method detach : unit
  end

  class virtual ['a] observer_trait ?variable () =
  object (self)
    val mutable subject = None
    val mutable signalids = []
    method virtual callback_changed : 'a -> unit
    method virtual callback_set : 'a -> unit
    method private disconnect =
      match subject with
      | None -> ()
      | Some(s) -> ( List.iter s#connect#disconnect signalids;
		     signalids <- []; )
    method attach (s : 'a GUtil.variable) =
      self#disconnect;
      subject <- Some(s);
      signalids <- [
	s#connect#set ~callback:self#callback_set;
	s#connect#changed ~callback:self#callback_changed;
      ];
      self#callback_changed s#get
    method detach =
      self#disconnect;
      subject <- None
    initializer
      match variable with
      | None -> ()
      | Some(s) -> self#attach s
  end

  class virtual ['a] viewable value =
  object (self)
    inherit ['a] GUtil.variable value
    method as_variable =
      (self :> 'a GUtil.variable)
    method virtual create_view : 'a observer
    method private finalize_observer (v : 'a observer) =
      v#attach self#as_variable
    method view ?packing ?show () =
      self#create_view
      |> GObj.pack_return ?packing ~show
      |> self#finalize_observer
  end

  class virtual ['a] editable value =
  object (self)
    inherit ['a] GUtil.variable value
    method as_variable =
      (self :> 'a GUtil.variable)
    method virtual create_editor : 'a observer
    method private finalize_editor (v : 'a observer) =
      v#attach self#as_variable
    method editor ?packing ?show () =
      self#create_editor
      |> GObj.pack_return ?packing ~show
      |> self#finalize_editor
  end
end

(* Canvas properties *)
module CanvasProperties =
struct

  let canvas_properties_scale_unit =
    4.0
  type t = {
    bg: GDraw.color;
    scale: float;
  }

  class virtual observer ?variable () =
    object (self)
      inherit [t] SmartVariable.observer_trait ?variable ()
      method virtual canvas_properties_changed : t -> unit
      method callback_changed props =
	self#canvas_properties_changed props
      method callback_set props =
	self#canvas_properties_changed props
    end

  class editor props =
    let canvas_properties = props#get in
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
      inherit observer ()

      method canvas_properties_changed props =
	scale#set_value props.scale;
	bgselect#set_color (GDraw.color props.bg);
      method notify_changed () =
	let canvas_properties = {
	  scale = scale#value;
	  bg = (`COLOR bgselect#color);
	} in
	Gaux.may (fun props -> props#set canvas_properties) subject
      initializer
	self#attach props;
	ignore [
	    scale#connect#value_changed ~callback:self#notify_changed;
	    bgselect#connect#color_set ~callback:self#notify_changed;
	  ];
  end

  class subject =
    let defaults = {
      bg = `WHITE;
      scale = 1.0;
    } in
  object(self)
    inherit [t] SmartVariable.editable defaults
    method as_variable =
      (self :> 'a GUtil.variable)
    method set_bg x =
      self#set { self#get with bg = x }
    method set_scale x =
      self#set { self#get with scale = x }
    method private equal a b =
      let unpack x =
	(Gdk.Color.pixel (GDraw.color x.bg), x.scale)
      in
      (unpack a) = (unpack b)
    method create_editor =
      (new editor self#as_variable :> t SmartVariable.observer)
  end

  class user canvas canvas_properties =
  object(self)
    inherit observer ~variable:(canvas_properties) ()
    method canvas_properties_changed props =
      canvas#set_pixels_per_unit
	       (canvas_properties_scale_unit *. props.scale);
      canvas#misc#modify_bg [`NORMAL, props.bg];
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
  new editor actual_canvas_properties#as_variable
  |> GHelper.maybe_callback (packing >>= apply_on_widget)
end


module Palette =
struct
  let predefined = [
    "Blue", [|
      081, 130, 188;
      154, 198, 227;
      000, 053, 123;
      082, 169, 222;
      001, 005, 017;
      000, 091, 164;
     |];

    "Brown", [|
      165, 127, 091;
      212, 198, 153;
      080, 055, 035;
      196, 161, 105;
      041, 013, 000;
      141, 079, 038;
     |];

    "Gray", [|
      134, 134, 134;
      203, 205, 202;
      061, 061, 061;
      159, 159, 159;
      001, 001, 001;
      237, 237, 237;
     |];

    "Green", [|
      020, 123, 054;
      150, 201, 134;
      000, 062, 025;
      079, 177, 078;
      000, 013, 003;
      000, 098, 028;
     |];

    "Spectrum", [|
      038, 105, 160;
      082, 172, 085;
      249, 177, 059;
      225, 038, 055;
      142, 169, 142;
      144, 146, 145;
     |];
  ]

  type t = color array
   and color = int * int * int

  let find key =
    List.assoc key predefined

  let names () =
    List.map fst predefined

  let default_name () =
    "Blue"

  let default () =
    find (default_name())

  let get_as_string palette i =
    let (r,g,b) = Array.get palette i in
    sprintf "#%02x%02x%02x" r g b

  class stylist palette =
  object (self)
    inherit [t] GUtil.variable palette
  end

  let stylist ?palette () =
    let actual_palette =
      match palette with
      | Some(p) -> p
      | None -> default()
    in
    new stylist actual_palette

  class picker ?packing ?stylist =
    let (popdown, (store, colum)) =
      GEdit.combo_box_text
	?packing
	~strings:(names())
	()
    in
  object (self)
    inherit GObj.widget popdown#as_widget
    val mutable subject = stylist
  end

  let picker ?packing ?stylist () =
    new picker ?packing ?stylist
end

module Line =
struct
  class stylist palette_name i =
    let palette =
      try Palette.find palette_name
      with Not_found -> Palette.default ()
    in
  object
    method props : GnomeCanvas.line_p list = [
      `WIDTH_PIXELS(4);
      `FILL_COLOR(Palette.get_as_string palette i);
    ]
  end

  let stylist palette_name i =
    new stylist palette_name i

  class series (stylist : stylist) parent =
    let myself = GnoCanvas.group parent in
    let unpack xypoints =
      let loop i =
	match i mod 2 = 0, i/2 with
	| true, k -> Array.get xypoints k |> fst
	| false, k -> Array.get xypoints k |> snd |> (~-.)
      in
      Array.init (2*(Array.length xypoints)) loop
    in
    object (self)
    inherit GnoCanvas.group myself#as_group
    val mutable size = 0
    val mutable points = [| |]
    val mutable line = GnoCanvas.line myself
    val mutable stylist = stylist
    method private update =
      line#destroy ();
      line <- GnoCanvas.line
		~points
		~props:stylist#props
		myself
    method set_points xypoints =
      points <- unpack xypoints;
      self#update
    method set_stylist newstylist =
      stylist <- newstylist;
      self#update
  end

  let series ?stylist ?points parent =
    let actualstylist =
      match stylist with
      | Some(s) -> s
      | None -> new stylist (Palette.default_name()) 0
    in
    let answer =
      new series actualstylist parent
    in
    match points with
    | Some(p) -> answer#set_points p; answer
    | None -> answer
end

let series_add_x series =
  let f k =
    (float_of_int (k * 20), series.(k))
  in
  let n = Array.length series in
  Array.init (Array.length series) f

let lineprops color =
  [`SMOOTH(true); `WIDTH_PIXELS(4); `FILL_COLOR(color)]

class ['subject] chart () =
  let palette_name = "Spectrum" in
  let canvas_properties = new CanvasProperties.subject in
  let vbox = GPack.vbox () in
  let view = GBin.scrolled_window
               ~packing:vbox#add
               ~hpolicy:`AUTOMATIC
               ~vpolicy:`AUTOMATIC
               ()
  in
  let canvas = GnoCanvas.canvas ~aa:true ~packing:view#add () in
  let stylist1 = Line.stylist palette_name 0 in
  let stylist2 = Line.stylist palette_name 1 in
  let line1 =
    Line.series
      ~stylist:stylist1
      ~points:(series_add_x series1)
      canvas#root
  in
  let line2 =
    Line.series
      ~stylist:stylist2
      ~points:(series_add_x series2)
      canvas#root
  in
  object
    inherit GObj.widget vbox#as_widget
    inherit CanvasProperties.user canvas canvas_properties#as_variable
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
  |> GHelper.maybe_apply canvas_properties (fun x -> x#attach)
