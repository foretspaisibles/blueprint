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

let series1 = [| 65.; 59.; 80.; 81.; 56.; 55.; 40.; |]
let series2 = [| 28.; 48.; 40.; 19.; 86.; 27.; 90.; |]

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
class chart () =
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
    initializer
      canvas#set_pixels_per_unit 5.0;
  end

let chart ?packing () =
  let answer = new chart () in
  ( match packing with
    | None -> ()
    | Some(p) -> p answer
  );
  answer
