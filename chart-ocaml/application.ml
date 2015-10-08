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

let mainwindow () =
  let window = GWindow.window ~width:320 ~height:240 ~title:"Test OCaml charts" () in
  let vbox = GPack.vbox ~packing:window#add () in
  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  let _ = factory#add_item "Quit" ~key:_Q ~callback: Main.quit in

  (* Chart *)
  let chart =
    Chart.chart
      ~packing:vbox#add
      ()
  in
  let _editor =
    Chart.CanvasProperties.editor ~packing:(vbox#pack ~fill:false) ()
  in
  let _ = factory#add_item "Connect" ~key:_C
      ~callback:(fun () -> print_endline "Connect!")
  in
  let _ = factory#add_item "Disonnect" ~key:_D
      ~callback:(fun () -> chart#disconnect())
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
