(* ChartPalette -- Palettes used for charts

   Blueprint (https://github.com/michipili/blueprint)
   This file is part of Blueprint

   Copyright © 2014–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open Printf

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

module Internal =
  ChartMVC.Macro(struct type t = palette end)

class model =
  Internal.macro_model

let model () =
  new model (default())

class virtual observer =
  Internal.macro_observer

class virtual widget =
  Internal.macro_widget


class virtual abstract_observer ?model () =
  object
    inherit [palette] ChartMVC.abstract_observer ?model ()
  end

class editor ?packing ?model () =
  let (popdown, (store, colum)) =
    GEdit.combo_box_text
      ?packing
      ~strings:(list())
        ()
  in
  object (self)
    inherit Internal.macro_widget popdown#as_widget ?model ()

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
    inherit [t, unit] ChartMVC.abstract_controller ?model ()
    method create_view () =
      (editor ?model:currentmodel () :> Internal.macro_widget)
    method callback_set v =
      ()
    method callback_changed v =
      ()
  end

let controller
    ~(model : palette ChartMVC.model) () =
  (new controller ~model ()
       :> (palette, unit) ChartMVC.controller)

let observer ~callback ?model () =
  object(self)
    inherit Internal.macro_observer ?model ()
    method callback_set palette =
      callback palette
  end
