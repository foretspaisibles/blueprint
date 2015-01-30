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

  class ['a] observer_signals
      ~(attached : 'a GUtil.variable GUtil.signal)
      ~(detached : unit GUtil.signal) =
    let disconnect_list = [
      attached#disconnect;
      detached#disconnect;
    ]
    in
  object
    inherit GUtil.ml_signals disconnect_list
    method attached = attached#connect ~after
    method detached = detached#connect ~after
  end

  class virtual ['a] observer_trait ?variable () =
    let attached = new GUtil.signal () in
    let detached = new GUtil.signal () in
  object (self)
    val mutable subject = None
    val mutable signalids = []
    method connect =
      new observer_signals ~attached ~detached
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
      attached#call s;
      self#callback_changed s#get
    method detach =
      self#disconnect;
      subject <- None;
      detached#call ()
    initializer
      match variable with
      | None -> ()
      | Some(s) -> self#attach s
  end

  class virtual observer_detach_on_destroy
    ?(widget : GObj.widget option)
    () =
  object (self)
    method virtual detach : unit
    initializer
      match widget with
      | None -> ()
      | Some(w) ->
	 w#misc#connect#destroy ~callback:(fun () -> self#detach)
	 |> ignore
  end

  class virtual ['a] observer_handle_changed_as_set =
  object (self)
    method virtual callback_set : 'a -> unit
    method callback_changed x =
      self#callback_set x
  end

  class virtual ['a, 'b] controller ?variable () =
  object (self)
    inherit ['a] observer_trait ?variable () as super
    method virtual create_view : 'b -> 'a observer
    method private finalize_view (v : 'a observer) =
      self#connect#attached
        ~callback:(fun model -> v#attach model)
      |> ignore;
      v#coerce
    method view ?packing ?show typ =
      self#create_view typ
      |> GObj.pack_return ~packing ~show
      |> self#finalize_view
  end

  class controllable_item
    ~(name : string)
    ~(item : ('a, 'b)  controller)
    typ
    =
  object
    method view ?packing ?show () =
      (item#view ?packing ?show typ)#coerce
    method name =
      name
  end

  let controllable_item ~name ~item typ =
    new controllable_item ~name ~item typ

  class controllable_store =
    let create_and_pack ?packing (item : controllable_item) =
      GMisc.label ~text:item#name ?packing ~show:true ()
      |> ignore;
      item#view ?packing ~show:true ()
      |> ignore
    in
  object (self)
    val mutable store = []
    method add (item : controllable_item) =
      store <- item :: store
    method private create_view =
      let box = GPack.vbox () in
      List.iter (create_and_pack ~packing:box#add) store;
      box
    method view ?packing ?show () =
      self#create_view
      |> GObj.pack_return ~packing ~show
    method popup =
      let w =
	GWindow.window
	  ~type_hint:`UTILITY
	  ~show:true
	  ()
      in
      self#view ~packing:w#add ~show:true ()
      |> ignore
  end

  let controllable_store () =
    new controllable_store

  let pack ~store ~item ~name typ =
    store#add (controllable_item ~item ~name typ)
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

  class virtual observer ?variable ?widget () =
    object (self)
      inherit [t] SmartVariable.observer_trait ?variable ()
      inherit [t] SmartVariable.observer_handle_changed_as_set
      inherit SmartVariable.observer_detach_on_destroy ?widget ()
    end

  class editor ~variable =
    let canvas_properties = variable#get in
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
    let _scaleselect =
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
      inherit GObj.widget container#as_widget as widget
      inherit observer ~widget:container#coerce ()

      method callback_set props =
	scale#set_value props.scale;
	bgselect#set_color (GDraw.color props.bg);

      method notify_changed () =
	let canvas_properties = {
	  scale = scale#value;
	  bg = (`COLOR bgselect#color);
	} in
	Gaux.may (fun props -> props#set canvas_properties) subject
      initializer
	self#attach variable;
	ignore [
	    scale#connect#value_changed ~callback:self#notify_changed;
	    bgselect#connect#color_set ~callback:self#notify_changed;
	  ];
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

  class subject =
    let defaults = {
      bg = `WHITE;
      scale = 1.0;
    } in
  object(self)
    inherit [t] GUtil.variable defaults
    method private equal a b =
      let unpack x =
	(Gdk.Color.pixel (GDraw.color x.bg), x.scale)
      in
      (unpack a) = (unpack b)
  end

  let subject () =
    new subject

  class controller ?variable () =
  object
    inherit [t, unit] SmartVariable.controller ?variable ()
    method create_view () =
      match subject with
      | Some(v) -> (new editor v :> t SmartVariable.observer)
      | None -> failwith "not implemented"
    method callback_set v =
      ()
    method callback_changed v =
      ()
  end

  let controller ~(variable : t #GUtil.variable) () =
    new controller ~variable:(variable :> t GUtil.variable) ()

  class user canvas canvas_properties =
  object(self)
    inherit observer ~variable:canvas_properties ()
    method callback_set props =
      canvas#set_pixels_per_unit
	       (canvas_properties_scale_unit *. props.scale);
      canvas#misc#modify_bg [`NORMAL, props.bg];
  end
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

  class picker ?packing ?stylist () =
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
  Array.init (Array.length series) f

let lineprops color =
  [`SMOOTH(true); `WIDTH_PIXELS(4); `FILL_COLOR(color)]

class ['subject] chart () =
  let palette_name = "Spectrum" in
  let controllable_store = SmartVariable.controllable_store () in
  let canvas_properties = CanvasProperties.subject () in
  let canvas_properties_controller =
    CanvasProperties.controller ~variable:canvas_properties ()
  in
  let () = SmartVariable.pack
	     ~store:controllable_store
	     ~name:"Canvas properties"
	     ~item:canvas_properties_controller
	     ()
  in
  let vbox = GPack.vbox () in
  let _editor = controllable_store#view ~packing:vbox#add () in
  let view = GBin.scrolled_window
               ~packing:vbox#add
               ~hpolicy:`AUTOMATIC
               ~vpolicy:`AUTOMATIC
               ()
  in
  let canvas = GnoCanvas.canvas ~aa:true ~packing:view#add () in
  let stylist1 = Line.stylist palette_name 0 in
  let stylist2 = Line.stylist palette_name 1 in
  let _line1 =
    Line.series
      ~stylist:stylist1
      ~points:(series_add_x series1)
      canvas#root
  in
  let _line2 =
    Line.series
      ~stylist:stylist2
      ~points:(series_add_x series2)
      canvas#root
  in
  object
    inherit GObj.widget vbox#as_widget
    val canvas_properties_user =
      new CanvasProperties.user canvas canvas_properties
    method attach_canvas_properties props =
      canvas_properties_user#attach props;
      canvas_properties_controller#attach props
    initializer
      canvas#set_pixels_per_unit 5.0;
      controllable_store#popup;
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
  |> GHelper.maybe_apply canvas_properties (fun x -> x#attach_canvas_properties)
