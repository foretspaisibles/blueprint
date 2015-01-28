open Printf

type card = {
  name: string;
  surname: string;
}

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

class ['observer] model c =
object (self : 'selftype)
  inherit ['observer] PatternSubjectObserver.subject
  val mutable card = c
  method notify_change =
    self#notify (fun obs -> obs#changed)
  method set c =
    card <- c;
    self#notify_change
  method get =
    card
  method name =
    card.name
  method surname =
    card.surname
end

class virtual ['subject] view =
object(self : 'selftype)
  inherit ['subject] PatternSubjectObserver.observer
  val mutable model =
    new model { name = ""; surname = ""; }
  method virtual changed : 'selftype model -> unit
  method set_model (s :  'selftype model) =
    model <- s;
    model#add self;
    self#changed model
end

class ['subject] presenter () =
  let name s =
    pango_markup ~attr:pango_style_name s#name
  in
  let surname s =
    pango_markup ~attr:pango_style_surname s#surname
  in
  let vbox = GPack.vbox () in
  let label init =
    GMisc.label ~markup:"not set"
		~packing:(vbox#pack ~expand:false)
		~xalign:0.0 ()
  in
  let labelname =
    label name
  in
  let labelsurname =
    label surname
  in
  object(self : 'selftype)
    inherit GObj.widget vbox#as_widget
    inherit ['subject] view
    method changed (s : 'selftype model) =
      labelname#set_text (name s);
      labelname#set_use_markup true;
      labelsurname#set_text (surname s);
      labelsurname#set_use_markup true
  end


let presenter ?packing ?model () =
  let w = new presenter () in
  (
    match model with
    | None -> ()
    | Some(m) -> w#set_model m
  );
  maybe_pack ?packing w


class ['subject] edit () =
  let vbox = GPack.vbox () in
  let label text =
    GMisc.label ~text ~packing:(vbox#pack ~expand:false) ~xalign:1.0 ()
  in
  let entry () =
    GEdit.entry ~packing:(vbox#pack ~expand:false) ()
  in
  let _labelname =
    label "Name"
  in
  let entryname =
    entry ()
  in
  let _labelsurname =
    label "Surname"
  in
  let entrysurname =
    entry ()
  in
  object(self)
    inherit GObj.widget vbox#as_widget
    inherit ['subject] view
    method changed s =
      entryname#set_text s#name;
      entrysurname#set_text s#surname;
    method private callback_changed () =
      let card = {
	name = entryname#text;
	surname = entrysurname#text;
      }
      in
      model#set card
    initializer
      List.iter ignore [
        entryname#connect#activate ~callback:self#callback_changed;
        entrysurname#connect#activate ~callback:self#callback_changed;
      ]
  end

let edit ?packing ?model () =
  let w = new edit () in
  (
    match model with
    | None -> ()
    | Some(m) -> w#set_model m
  );
  maybe_pack ?packing w
