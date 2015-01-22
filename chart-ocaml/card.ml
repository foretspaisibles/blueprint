open Printf

type card = {
  name: string;
  surname: string;
}

module CardMVC = PatternMVC.Make (struct
end)

let pango_markup_buffer_sz =
  100

let pango_style_name = [
  "size", "large";
]

let pango_style_surname = [
  "size", "medium";
]

let pango_style_not_set = [
  "foreground", "#999999"
]

let pango_markup ?(attr = []) text =
  let attr_to_string (k,v) =
    sprintf "%s = \"%s\"" k v
  in
  let attrlist_to_string lst =
    String.concat " " (List.map attr_to_string lst)
  in
  match attr with
  | [] -> text
  | _ -> sprintf "<span %s>%s</span>" (attrlist_to_string attr) text

let maybe_pack ?packing w =
  ( match packing with
    | None -> ()
    | Some(p) -> p (w :> GObj.widget)
  );
  w


class view () =
  let cardref = ref None in
  let name () =
    match !cardref with
    | None -> pango_markup
		~attr:(pango_style_not_set @ pango_style_name)
		"not set"
    | Some(c) -> pango_markup
		   ~attr:pango_style_name
		   c.name
  in
  let surname () =
    match !cardref with
    | None -> pango_markup
		~attr:(pango_style_not_set @ pango_style_surname)
		"not set"
    | Some(c) -> pango_markup
		   ~attr:pango_style_surname
		   c.surname
  in
  let vbox = GPack.vbox () in
  let label init =
    GMisc.label ~markup:(init()) ~packing:vbox#add ~xalign:0.0 ()
  in
  let labelname =
    label name
  in
  let labelsurname =
    label surname
  in
  object(self)
    inherit GObj.widget vbox#as_widget

    method update () =
      labelname#set_text (name());
      labelname#set_use_markup true;
      labelsurname#set_text (surname());
      labelsurname#set_use_markup true
    method set_card c =
      cardref := Some(c);
      self#update()
  end

let view ?packing ?card () =
  let w = new view () in
  (
    match card with
    | None -> ()
    | Some(c) -> w#set_card c
  );
  maybe_pack ?packing w


class edit () =
  let cardref = ref None in
  let name () =
    match !cardref with
    | None -> ""
    | Some(c) -> c.name
  in
  let surname () =
    match !cardref with
    | None -> ""
    | Some(c) -> c.surname
  in
  let vbox = GPack.vbox () in
  let label text =
    GMisc.label ~text ~packing:vbox#add ~xalign:1.0 ()
  in
  let entry () =
    GEdit.entry ~packing:vbox#add ()
  in
  let labelname =
    label "Name"
  in
  let entryname =
    entry ()
  in
  let labelsurname =
    label "Surname"
  in
  let entrysurname =
    entry ()
  in
  object(self)
    inherit GObj.widget vbox#as_widget

    method update () =
      entryname#set_text (name());
      entrysurname#set_text (surname());
    method set_card c =
      cardref := Some(c);
      self#update()
    method get_card =
      match !cardref with
      | None -> { name = ""; surname = "" }
      | Some(c) -> c
    method callback_changed () =
      let newcard = {
	name = entryname#text;
	surname = entrysurname#text;
      }
      in
      printf "Changed: %s, %s\n%!" newcard.surname newcard.name;
      self#set_card newcard
    initializer
      List.iter ignore [
        entryname#connect#activate ~callback:self#callback_changed;
        entrysurname#connect#activate ~callback:self#callback_changed;
      ]
  end

let edit ?packing ?card () =
  let w = new edit () in
  (
    match card with
    | None -> ()
    | Some(c) -> w#set_card c
  );
  maybe_pack ?packing w
