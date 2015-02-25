(* ChartMVC -- Cooked version of Model-View-Controller

Blueprint (https://github.com/michipili/blueprint)
This file is part of Blueprint

Copyright © 2014–2015 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

class ['a] model =
  ['a] PatternMVC.model

class virtual ['a] observer ?model () =
object (self)
  inherit ['a] PatternMVC.abstract_observer ?model ()
  inherit ['a] PatternMVC.trait_handle_changed_as_set
end

class virtual ['a] widget root ?model () =
object
  inherit ['a] observer ?model () as observer
  inherit GObj.widget root as widget
  inherit PatternMVC.trait_detach_on_destroy
  initializer
    observer#connect#attached
      ~callback:(fun _ -> widget#misc#set_sensitive true)
    |> ignore;
    observer#connect#detached
      ~callback:(fun _ -> widget#misc#set_sensitive false)
    |> ignore;
end


(** The type of controllers synchronising views and models
of type ['a]. It also acts as a view factory, parametrised by
a value of type ['b]. *)
class virtual ['a, 'b] controller ?model () =
object (self)
  inherit ['a] PatternMVC.abstract_observer ?model () as super
  method virtual create_view : 'b -> 'a widget
  method private finalize_view (v : 'a widget) =
    self#connect#attached
      ~callback:(fun model -> v#attach model)
    |> ignore;
    v
  method view ?packing ?show typ =
    self#create_view typ
    |> GObj.pack_return ~packing ~show
    |> self#finalize_view
end


module type P =
sig
  type t
end

module Macro(Parameter:P) =
struct
  class virtual _model var =
  object
    inherit [Parameter.t] model var
  end

  class virtual _observer ?model () =
  object
    inherit [Parameter.t] observer ?model ()
  end

  class virtual _widget root ?model () =
  object
    inherit [Parameter.t] widget root ?model ()
  end

  class virtual model = _model
  class virtual observer = _observer
  class virtual widget = _widget
end
