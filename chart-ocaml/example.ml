class ['observer] subject =
object (self : 'mytype)
  val mutable observers : 'observer list = []
  method add obs = observers <- obs :: observers
  method notify (message : 'observer -> 'mytype -> unit) =
    List.iter (fun obs -> message obs self) observers
end

class ['subject] observer =
object
end

class ['observer] window =
object (self : 'mytype)
  inherit ['observer] subject
  val mutable position = 0
  method move d = position <- position + d; self#notify (fun x -> x#moved)
  method draw = Printf.printf "[Position = %d]" position;
end

class ['subject] manager =
object inherit ['subject] observer
  method moved (s : 'subject) : unit = s#draw
end
;;

let w = new window in w#add (new manager); w#move 1
;;

class ['observer] large_window =
object (self)
  inherit ['observer] window as super
  val mutable size = 1
  method resize x =
    size <- size + x;
    self#notify (fun x -> x#resized)
  method draw =
    super#draw;
    Printf.printf "[Size = %d]" size;
end

class ['subject] big_manager =
object
  inherit ['subject] manager as super
  method resized (s:'subject) = s#draw
end

class ['subject] spy =
object inherit ['subject] observer
  method resized (s:'subject) =
    print_string "<R>"
  method moved (s:'subject)
    = print_string "<M>"
end
;;

let w = new large_window
    in w#add (new big_manager);
       w#add (new spy); w#resize 2;
       w#move 1
;;
