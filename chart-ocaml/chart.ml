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

module Controled =
struct

  class item
    ~(name : string)
    ~(item : ('a, 'b) ChartMVC.controller)
    typ
    =
  object
    method view ?packing ?show () =
      (item#view ?packing ?show typ)#coerce
    method name =
      name
  end

  class store =
    let create_and_pack ?packing (item : item) =
      GMisc.label ~text:item#name ?packing ~show:true ()
      |> ignore;
      item#view ?packing ~show:true ()
      |> ignore
    in
  object (self)
    val mutable store = []
    method add (item : item) =
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

  let item ~name ~item typ =
    new item ~name ~item typ

  let store () =
    new store

  let pack ~store ~item ~name typ =
    store#add (new item ~item ~name typ)

end


(* Canvas properties *)
module CanvasProperties =
struct

  let canvas_properties_scale_unit =
    4.0

  type canvas_properties = {
    bg: GDraw.color;
    scale: float;
  }

  let defaults = {
    bg = `WHITE;
    scale = 1.0;
  }

  type t = canvas_properties
  module Internal =
    ChartMVC.Macro(struct type t = canvas_properties end)

  class model =
  object(self)
    inherit Internal.model defaults
    method private equal a b =
      let unpack x =
        (Gdk.Color.pixel (GDraw.color x.bg), x.scale)
      in
      (unpack a) = (unpack b)
  end

  let model () =
    new model

  class virtual observer =
    Internal.observer

  class virtual widget =
    Internal.widget

  class editor ?model () =
    let container = GPack.hbox () in
    let scale =
      GData.adjustment
        ~value:defaults.scale
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
        ~color:(GDraw.color defaults.bg)
        ~packing:container#add () in
    object(self)
      inherit widget ?model container#as_widget () as widget

      method callback_set props =
        scale#set_value props.scale;
        bgselect#set_color (GDraw.color props.bg);

      method private notify_changed () =
        let canvas_properties = {
          scale = scale#value;
          bg = (`COLOR bgselect#color);
        } in
        Gaux.may (fun props -> props#set canvas_properties) currentmodel
      initializer
        ignore [
            scale#connect#value_changed ~callback:self#notify_changed;
            bgselect#connect#color_set ~callback:self#notify_changed;
          ];
  end

  let editor =
    Internal.widget (new editor)

  class controller ?model () =
  object
    inherit [t, unit] ChartMVC.controller ?model ()
    method create_view () =
      (editor ?model:currentmodel () :> Internal.widget)
    method callback_set v =
      ()
    method callback_changed v =
      ()
  end

  let controller ~(model : t #GUtil.variable) () =
    new controller ~model:(model :> t GUtil.variable) ()

  class consumer_canvas canvas model =
  object(self)
    inherit observer ~model ()
    method callback_set props =
      canvas#set_pixels_per_unit
        (canvas_properties_scale_unit *. props.scale);
      canvas#misc#modify_bg [`NORMAL, props.bg];
  end

  let consumer_canvas canvas model =
    new consumer_canvas canvas model
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

  type palette = string
   and color = int * int * int

  let default () =
    "Blue"

  type t = palette
  module Internal =
    ChartMVC.Macro(struct type t = palette end)

  class model =
    Internal.model

  let model () =
    new model (default())

  class virtual observer =
    Internal.observer

  class virtual widget =
    Internal.widget

  let get palette i =
    let table = List.assoc palette predefined in
    Array.get table i

  let get_as_string palette i =
    let (r,g,b) = get palette i in
    sprintf "#%02x%02x%02x" r g b

  let list () =
    List.map fst predefined

  let of_index i =
    List.nth (list()) i

  let to_index palette =
    let rec loop left i =
      match left with
      | [] -> raise Not_found
      | hd :: tl -> (if hd = palette then i else loop tl (succ i))
    in
    loop (list()) 0

  class editor ?packing ?model () =
    let (popdown, (store, colum)) =
      GEdit.combo_box_text
        ?packing
        ~strings:(list())
        ()
    in
  object (self)
    inherit Internal.widget popdown#as_widget ?model ()

    method callback_set newpalette =
      to_index newpalette
      |> popdown#set_active

    method private notify_changed () =
      let newpalette = of_index popdown#active in
      Gaux.may (fun v -> v#set newpalette) currentmodel
    initializer
      ignore [
          popdown#connect#changed ~callback:self#notify_changed;
        ];
  end

  let editor =
    Internal.widget (new editor)

  class controller ?model () =
  object
    inherit [t, unit] ChartMVC.controller ?model ()
    method create_view () =
      (editor ?model:currentmodel () :> Internal.widget)
    method callback_set v =
      ()
    method callback_changed v =
      ()
  end

  let controller ~(model : t #GUtil.variable) () =
    new controller ~model:(model :> t GUtil.variable) ()

  class consumer_stylist stylist model =
  object(self)
    inherit Internal.observer ~model ()
    method callback_set palette =
      stylist#set_palette palette
  end

  let consumer stylist palette_variable =
    new consumer_stylist stylist palette_variable
end


module Series_store =
struct
  
end


module Line =
struct
  class stylist ?line palette i =
  object (self)
    val mutable line_option = line
    inherit Palette.observer ~model:palette ()
    method props : GnomeCanvas.line_p list = [
      `WIDTH_PIXELS(4);
      `FILL_COLOR(Palette.get_as_string palette#get i);
    ]
    method callback_set _ =
      match line_option with
      | None -> ()
      | Some(line) -> line#set self#props
    method set_line (line : GnoCanvas.line) =
      line_option <- Some(line)
  end

  let stylist palette i =
    new stylist palette i

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
      stylist#set_line line;
      self#update
  end

  let series ?stylist ?points parent =
    let actualstylist =
      match stylist with
      | Some(s) -> s
      | None -> new stylist (Palette.model()) 0
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
  let controled_store = Controled.store () in
  (* Canvas properties *)
  let canvas_properties = CanvasProperties.model () in
  let canvas_properties_controller =
    CanvasProperties.controller ~model:canvas_properties ()
  in
  let () = Controled.pack
             ~store:controled_store
             ~name:"Canvas properties"
             ~item:canvas_properties_controller
             ()
  in
  (* Palette *)
  let palette = Palette.model () in
  let palette_controller =
    Palette.controller ~model:palette ()
  in
  let () = Controled.pack
             ~store:controled_store
             ~name:"Palette"
             ~item:palette_controller
             ()
  in
  let vbox = GPack.vbox () in
  let _editor = controled_store#view ~packing:vbox#add () in
  let palette_editor = palette_controller#view ~packing:vbox#add () in
  let view = GBin.scrolled_window
               ~packing:vbox#add
               ~hpolicy:`AUTOMATIC
               ~vpolicy:`AUTOMATIC
               ()
  in
  let canvas = GnoCanvas.canvas ~aa:true ~packing:view#add () in
  let stylist1 = Line.stylist palette 0 in
  let stylist2 = Line.stylist palette 1 in
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
    val canvas_properties_consumer =
      CanvasProperties.consumer_canvas canvas canvas_properties
    method attach_canvas_properties props =
      canvas_properties_consumer#attach props;
      canvas_properties_controller#attach props
    method attach_palette palette =
      palette_controller#attach palette
    method disconnect () =
      palette_editor#detach
    initializer
      canvas#set_pixels_per_unit 5.0;
      controled_store#popup;
  end

let chart
      ?packing
      ?canvas_properties
      ?palette
      () =
  let open GHelper.Maybe.Operator in
  let apply_on_widget f x =
    f (x :> GObj.widget)
  in
  new chart ()
  |> GHelper.maybe_callback (packing >>= apply_on_widget)
  |> GHelper.maybe_apply canvas_properties (fun x -> x#attach_canvas_properties)
  |> GHelper.maybe_apply palette (fun x -> x#attach_palette)
