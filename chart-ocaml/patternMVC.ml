(* PatternMVC -- Model-View-Controller

   Blueprint (https://github.com/michipili/blueprint)
   This file is part of Blueprint

   Copyright © 2014–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** Model-View-Controller *)

(** The type of models representing a value of type ['a]. *)
class ['a] model =
  ['a] GUtil.variable

(** The type of signals for observers of a value of type ['a].

    An observer is usually attached to a model, but it also might be
    detached, usually displaying nothing or being inactive.

    There is two connectible signals:
    - The attached signal, parametrised by the model being attached.
    - The detached signal, with a unit parameter. *)
class ['a] observer_signals
    ~(attached : 'a model GUtil.signal)
    ~(detached : unit GUtil.signal) =
  let disconnect_list = [
    attached#disconnect;
    detached#disconnect;
  ]
  in
  object
    inherit GUtil.ml_signals disconnect_list
    method attached = attached#connect ~after
    method detached = detached#connect ~after
  end

(** The type of observers, watching a value of type ['a]. *)
class type ['a] observer =
  object
    method attach : 'a model -> unit
    method detach : unit
    method connect : 'a observer_signals
  end

(** The type of observer widgets, watching a value of type ['a]. *)
class type ['a] widget =
  object
    inherit GObj.widget
    inherit ['a] observer
  end

class virtual ['a] abstract_observer ?model () =
  let attached = new GUtil.signal () in
  let detached = new GUtil.signal () in
  object (self)
    val mutable currentmodel = None
    val mutable signalids = []
    method connect : 'a observer_signals =
      new observer_signals ~attached ~detached
    method virtual callback_changed : 'a -> unit
    method virtual callback_set : 'a -> unit
    method private disconnect =
      match currentmodel with
      | None -> ()
      | Some(m) -> ( List.iter m#connect#disconnect signalids;
                     signalids <- []; )
    method attach (m : 'a model) =
      self#disconnect;
      currentmodel <- Some(m);
      signalids <- [
        m#connect#set ~callback:self#callback_set;
        m#connect#changed ~callback:self#callback_changed;
      ];
      attached#call m;
      self#callback_changed m#get
    method detach =
      self#disconnect;
      currentmodel <- None;
      detached#call ()
    initializer
      Gaux.may self#attach model
  end

class virtual trait_detach_on_destroy =
  object (self)
    method virtual coerce : GObj.widget
    method virtual detach : unit
    initializer
      self#coerce#misc#connect#destroy ~callback:(fun () -> self#detach)
      |> ignore
  end

class virtual ['a] trait_handle_changed_as_set =
  object (self)
    method virtual callback_set : 'a -> unit
    method callback_changed x =
      self#callback_set x
  end

(** The type of controllers synchronising views and models
    of type ['a]. It also acts as a view factory, parametrised by
    a value of type ['b]. *)
class virtual ['a, 'b] controller ?model () =
  object (self)
    inherit ['a] abstract_observer ?model () as super
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
