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

class type ['a] observer =
  object
    inherit ['a] PatternMVC.observer
    method callback_changed : 'a -> unit
    method callback_set : 'a -> unit
  end

class type ['a] widget =
  object
    inherit ['a] PatternMVC.widget
    method callback_changed : 'a -> unit
    method callback_set : 'a -> unit
  end

class type ['a, 'b] controller =
  object
    inherit ['a] PatternMVC.observer
    method callback_changed : 'a -> unit
    method callback_set : 'a -> unit
    method create_view : 'b -> 'a widget
    method view : ?packing:(GObj.widget -> unit) -> ?show:bool -> 'b -> 'a widget
  end

class virtual ['a] abstract_observer ?model () =
  object (self)
    inherit ['a] PatternMVC.abstract_observer ?model ()
    inherit ['a] PatternMVC.trait_handle_changed_as_set
  end

class virtual ['a] abstract_widget root ?model () =
  object
    inherit ['a] abstract_observer ?model () as observer
    inherit GObj.widget root as widget
    inherit PatternMVC.trait_detach_on_destroy
    initializer
      observer#connect#attached
        ~callback:(fun _ -> widget#misc#set_sensitive true)
      |> ignore;
      observer#connect#detached
        ~callback:(fun _ -> widget#misc#set_sensitive false)
      |> ignore;
      match model with
      | None -> widget#misc#set_sensitive false
      | Some(_) -> ()
  end


(** The type of controllers synchronising views and models
    of type ['a]. It also acts as a view factory, parametrised by
    a value of type ['b]. *)
class virtual ['a, 'b] abstract_controller ?model () =
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
  class virtual macro_model var =
    object
      inherit [Parameter.t] model var
    end

  class virtual macro_observer ?model () =
    object
      inherit [Parameter.t] abstract_observer ?model ()
    end

  class virtual macro_widget root ?model () =
    object
      inherit [Parameter.t] abstract_widget root ?model ()
    end

  let apply_observer_params ~cont finally ?model =
    let finally_attach =
      match model with
      | None -> finally
      | Some(m) -> (fun observer -> observer#attach m) :: finally
    in
    cont finally_attach

  let apply_widget_params ~cont finally ?packing =
    let finally_pack =
      match packing with
      | None -> finally
      | Some(callback) -> (fun w -> callback w#coerce) :: finally
    in
    apply_observer_params ~cont finally_pack

  let create_and_prepare create () =
    let cont finally () =
      let answer = create () in
      List.iter (fun f -> f answer) finally;
      answer
    in
    cont

  let widget create =
    let cont finally () =
      let answer = create () in
      List.iter (fun f -> f answer) finally;
      answer
    in
    apply_widget_params ~cont []
end
