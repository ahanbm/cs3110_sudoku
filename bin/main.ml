open Gtk

let create_window () =
  let window = GWindow.window ~title:"Sudoku" ~width:300 ~height:300 () in
  window#connect#destroy ~callback:Main.quit;
  window

let create_grid () =
  let grid = GPack.grid ~rows:9 ~columns:9 ~row_spacing:2 ~column_spacing:2 () in
  for i = 0 to 8 do
    for j = 0 to 8 do
      let entry = GEdit.entry ~width_chars:1 ~max_length:1 () in
      grid#attach entry ~left:j ~top:i
    done
  done;
  grid

let main () =
  ignore (GtkMain.Main.init ());
  let window = create_window () in
  let grid = create_grid () in
  window#add grid;
  window#show ();
  GtkMain.Main.main ()

let () = main ()
