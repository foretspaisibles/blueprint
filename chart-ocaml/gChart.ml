(* GChart -- Plotting and charting library for OCaml

   Blueprint (https://github.com/michipili/blueprint)
   This file is part of Blueprint

   Copyright © 2014–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

let maybe_do f = function
  | Some(x) -> f x
  | None -> ()

let maybe_map f = function
  | Some(x) -> Some(f x)
  | None -> None

let maybe_filtermap f lst =
  List.fold_right
    (fun x acc -> match f x with Some(y) -> y :: acc | None -> acc)
    lst
    []

let maybe_filter lst =
  maybe_filtermap (fun x -> x) lst

let bounds_as_props bounds = [
  `X1(bounds.(0));
  `Y1(bounds.(1));
  `X2(bounds.(2));
  `Y2(bounds.(3));
]

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

let palette_data = [
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

class palette _name =
  object
    val name = _name
    val colors = List.assoc _name palette_data

    method get_as_string i =
      let (r,g,b) = colors.(i mod Array.length colors) in
      Printf.sprintf "#%02x%02x%02x" r g b
  end

let palette name =
  new palette name

let dot ?x ?y ?(text = "\xe2\x9d\x96") ?size ?fill_color ?(props = []) parent =
  let actual_props =
    props @ maybe_filter [ maybe_map (fun s -> `FILL_COLOR(s)) fill_color]
  in
  GnoCanvas.text ?x ?y ?size ~text ~anchor:`CENTER ~props:actual_props parent

let bbox item =
  let group = GnoCanvas.group item#canvas#root in
  let box = GnoCanvas.rect
      ~props:(bounds_as_props item#get_bounds @ [
          `FILL_COLOR_RGBA(0x2669A033l);
        ])
      group
  in
  let hotspot =
    let (x,y) = item#i2w ~x:0.0 ~y:0.0 in
    dot ~x ~y ~props:[ `FILL_COLOR_RGBA(0x2669A066l) ] group
  in
  let on_motion = function
    |`MOTION_NOTIFY(_) ->
      box#set (bounds_as_props item#get_bounds);
      hotspot#set (let (x,y) = item#i2w ~x:0.0 ~y:0.0 in [`X(x); `Y(y)]);
      false
    | _ -> false
  in
  let on_destroy _ =
    box#destroy();
    hotspot#destroy();
  in
  ignore [
    item#connect#destroy ~callback:on_destroy;
    item#connect#event ~callback:on_motion;
  ];
  group

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
    val _title =
      GnoCanvas.text
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

    method bounds =
      let title_bounds = _title#get_bounds in
      let subtitle_bounds = _subtitle#get_bounds in
      [|
        min title_bounds.(0) subtitle_bounds.(0);
        min title_bounds.(1) subtitle_bounds.(1);
        max title_bounds.(2) subtitle_bounds.(2);
        max title_bounds.(3) subtitle_bounds.(3);
      |]

    method width =
      let bounds = self#bounds in
      abs_float(bounds.(0) -. bounds.(1))

    method height =
      let bounds = self#bounds in
      abs_float(bounds.(2) -. bounds.(3))

    method private set_title text =
      _title#set [ `TEXT(text) ]

    method private set_subtitle text =
      _subtitle#set [ `MARKUP("<i>"^text^"</i>") ]

    method set_chart chart =
      maybe_do self#set_title chart.Chart.title;
      maybe_do self#set_subtitle chart.Chart.subtitle;
      _title#set [ `X(0.0); `Y(0.0); `ANCHOR(`NORTH) ];
      _subtitle#set [ `X(0.0); `Y(1.20*. _title#text_height); `ANCHOR(`NORTH) ];

    method update_position =
      let (x, y) =
        group#canvas#window_to_world
          ~winx:(float_of_int(group#canvas#width / 2))
          ~winy:(0.0)
      in
      self#set [`X(x); `Y(y) ];

    initializer
      ignore [
        group#canvas#misc#connect#size_allocate
          ~callback:(fun _ -> self#update_position);
      ]
  end


let title parent =
  let group = GnoCanvas.group parent in
  let answer = new title group in
  answer

class legend group =
  let palette =
    palette "Blue"
  in
  let half_hint_size = 5.0 in
  let sep1 = 10.0 in
  let sep2 = 15.0 in
  let make_hint (i, name) =
    GnoCanvas.rect ~props:[
      `FILL_COLOR(palette#get_as_string i);
      `OUTLINE_COLOR("black");
      `WIDTH_PIXELS(1);
      `X1(~-. half_hint_size);
      `Y1(~-. half_hint_size);
      `X2(    half_hint_size);
      `Y2(    half_hint_size);
    ] group,
    GnoCanvas.text ~props:[
      `TEXT(name);
      `ANCHOR(`WEST);
      `SIZE_POINTS(12.0);
      `FILL_COLOR("black");
    ] group
  in
  let max_height hint =
    let rec loop current = function
      | [] -> current
      | hd :: tl -> loop (max hd current) tl
    in
    loop (2. *. half_hint_size) (List.map (fun (_,t) -> t#text_height) hint)
  in
  let total_width hint =
    let n = float_of_int(List.length hint) in
    let n_mone = n -. 1.0 in
    List.fold_left ( +. ) 0.0 (List.map (fun (_,t) -> t#text_width) hint)
    +. n_mone *. (2.0 *. half_hint_size +. sep2)
    +. n *. sep1
  in
  let spread hint =
    let y = ~-. (3.0 *. max_height hint /. 2.0) in
    let w = total_width hint /. 2.0 in
    let rec loop x = function
      | [] -> ()
      | (r, t) :: tl ->
        r#set [
          `X1(x -. half_hint_size);
          `Y1(y -. half_hint_size);
          `X2(x +. half_hint_size);
          `Y2(y +. half_hint_size);
        ];
        t#set [ `X(x+.sep1); `Y(y) ];
        loop (x +. sep1 +. 2.0 *. half_hint_size +. sep2 +. t#text_width) tl
    in
    loop (~-. w) hint
  in
  object(self)
    inherit GnoCanvas.group group#as_group
    val mutable hint = []
    method set_chart c =
      let name = Chart.(List.map (fun s -> s.Series.name) c.series) in
      let iname =
        name
        |> Array.of_list
        |> Array.mapi (fun i s -> (i,s))
        |> Array.to_list
      in
      List.iter (fun i -> i#destroy()) self#get_items;
      hint <- List.map make_hint iname;
      spread hint

    method update_position =
      let (x, y) =
        group#canvas#window_to_world
          ~winx:(float_of_int(group#canvas#width / 2))
          ~winy:(float_of_int(group#canvas#height))
      in
      self#set [`X(x); `Y(y) ];

    initializer
      ignore [
        group#canvas#misc#connect#size_allocate
          ~callback:(fun _ -> self#update_position);
      ]


  end

let legend parent =
  let group = GnoCanvas.group parent in
  let answer = new legend group in
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
    val _legend = legend group
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
      _title#set_chart c;
      _legend#set_chart c;
      self#redraw
    initializer
      self#misc#modify_bg [
        `NORMAL, `WHITE;
      ];
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
