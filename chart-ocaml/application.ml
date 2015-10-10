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
    series = Series.[{
        name = "Tokyo";
        style = Style.Line;
        data = Data.Series [| 7.0; 6.9; 9.5; 14.5; 18.2; 21.5; 25.2; 26.5; 23.3; 18.3; 13.9; 9.6 |];
      }; {
         name = "New York";
         style = Style.Line;
         data = Data.Series [| -0.2; 0.8; 5.7; 11.3; 17.0; 22.0; 24.8; 24.1; 20.1; 14.1; 8.6; 2.5 |];
       }; {
         name = "Berlin";
         style = Style.Line;
         data = Data.Series [| -0.9; 0.6; 3.5; 8.4; 13.5; 17.0; 18.6; 17.9; 14.3; 9.0; 3.9; 1.0 |];
       }; {
         name = "London";
         style = Style.Line;
         data = Data.Series [| 3.9; 4.2; 5.7; 8.5; 11.9; 15.2; 17.0; 16.6; 14.2; 10.3; 6.6; 4.8 |];
  }]
}

let mainwindow () =
  let window = GWindow.window ~width:320 ~height:240 ~title:"Test OCaml charts" () in
  let vbox = GPack.vbox ~packing:window#add () in
  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in

  (* Chart *)
  let gchart =
    GChart.chart
      ~aa:true
      ~chart:chart_temperature
      ~packing:(vbox#pack ~expand:true)
      ~border_width:500
      ()
  in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  let _ = factory#add_item "Quit" ~key:_Q ~callback: Main.quit in

  let _ = factory#add_item "Connect" ~key:_C
      ~callback:(fun () -> gchart#set_chart chart_temperature; print_endline "Connect!")
  in
  let _ = factory#add_item "Disconnect" ~key:_D
      ~callback:(fun () -> gchart#set_chart GChart.dummy; print_endline "Disconnect")
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
