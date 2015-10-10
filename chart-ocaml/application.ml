(* Application -- An application to test OCaml charts

   Blueprint (https://github.com/michipili/blueprint)
   This file is part of Blueprint

   Copyright © 2014–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open GMain
open GdkKeysyms


let chart_temperature = Chart.{
    title = Some("Monthly average temperature");
    subtitle = Some("Source: Wordclimate.com");
    x_axis =
      Axis.make ~tick_labels:(Axis.text_tick_labels[|
        "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
        "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"
        |]) ();
    y_axis =
      Axis.make ~title:"Temperature (°C)" ();
    tooltip = Some(Tooltip.(value_suffix "°C"));
    legend = Some(Legend.{ position = Bottom; });
    series = [];
  }

let mainwindow () =
  let window = GWindow.window ~width:320 ~height:240 ~title:"Test OCaml charts" () in
  let vbox = GPack.vbox ~packing:window#add () in
  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in

  let canvas =
    GnoCanvas.canvas
      ~aa:true
      ~packing:(vbox#pack ~expand:true)
      ()
  in

  let rectangle n =
    let offset s =
      float_of_int(n*10) +. s
    in
    GnoCanvas.rect
      ~x1:(offset 0.0)
      ~x2:(offset 50.0)
      ~y1:(offset 0.0)
      ~y2:(offset 50.0)
      ~fill_color:"cadet blue"
      canvas#root
  in

  let count = ref 0 in
  let pack () =
    ignore(rectangle !count);
    incr count;
  in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  let _ = factory#add_item "Quit" ~key:_Q ~callback: Main.quit in

  let _ = factory#add_item "Connect" ~key:_C
      ~callback:(fun () -> pack();
                  List.iter (fun item -> Printf.printf "OID %d\n" item#get_oid) canvas#root#get_items;
                  print_endline "Connect!")
  in
  let _ = factory#add_item "Disconnect" ~key:_D
      ~callback:(fun () ->
          List.iter (fun item -> item#destroy ()) canvas#root#get_items;
          print_endline "Disconnect")
  in

  (* Chart *)
  let _gchart =
    GChart.chart
      ~aa:true
      ~chart:chart_temperature
      ~packing:(vbox#pack ~expand:true)
      ()
  in
  (* Display the windows and enter Gtk+ main loop *)
  let _ =   window#connect#destroy ~callback:Main.quit in
  window#add_accel_group accel_group;
  window#show ();
  window

let main () =
  let _ = Main.init () in
  let _ = mainwindow () in
  Main.main ()

let () = main ()
