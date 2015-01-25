class ['observer] subject =
object (self : 'selftype)
  val mutable observers : 'observer list = []
  method add obs =
    observers <- obs :: observers
  method notify (message : 'observer -> 'selftype -> unit) =
    List.iter (fun obs -> message obs self) observers
end

class ['subject] observer =
object
end


class ['observer, 'data] model d =
object (self : 'selftype)
  inherit ['observer] subject
  val mutable data = d
  method notify_change =
    self#notify (fun obs -> obs#changed)
  method set d =
    data <- d;
    self#notify_change
  method get =
    data
end
