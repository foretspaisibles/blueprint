(* ChartMVC -- Cooked version of Model-View-Controller

   Blueprint (https://github.com/michipili/blueprint)
   This file is part of Blueprint

   Copyright © 2014–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

class ['a] model : 'a -> ['a] PatternMVC.model

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

class virtual ['a] abstract_observer :
  ?model:'a model ->
  unit ->
  object
    inherit ['a] PatternMVC.observer
    method callback_changed : 'a -> unit
    method virtual callback_set : 'a -> unit
  end

(** This is a [ ['a] PatternMVC.widget] with two more methods,
    [callback_changed] and [callback_set]. *)
class virtual ['a] abstract_widget :
  ([> Gtk.widget ] as 'b) Gtk.obj ->
  ?model:'a model ->
  unit -> object
    val mutable currentmodel : 'a PatternMVC.model option
    val obj : 'b Gtk.obj
    val mutable signalids : GtkSignal.id list
    method as_widget : Gtk.widget Gtk.obj
    method attach : 'a PatternMVC.model -> unit
    method callback_changed : 'a -> unit
    method virtual callback_set : 'a -> unit
    method coerce : GObj.widget
    method connect : 'a PatternMVC.observer_signals
    method destroy : unit -> unit
    method detach : unit
    method drag : GObj.drag_ops
    method get_oid : int
    method misc : GObj.misc_ops
  end

class virtual ['a, 'b] abstract_controller :
  ?model:'a PatternMVC.model ->
  unit ->
  object
    inherit ['a] PatternMVC.abstract_observer
    method virtual create_view : 'b -> 'a widget
    method private finalize_view : 'a widget -> 'a widget
    method view : ?packing:(GObj.widget -> unit) -> ?show:bool -> 'b -> 'a widget
  end

module type P = sig type t end
module Macro :
  functor (Parameter : P) ->
    sig
      class virtual macro_model :
        Parameter.t -> [ Parameter.t ] model

      class virtual macro_observer :
        ?model:Parameter.t PatternMVC.model ->
        unit ->
        [ Parameter.t ] abstract_observer

      class virtual macro_widget :
        ([> Gtk.widget ] as 'a) Gtk.obj ->
        ?model:Parameter.t PatternMVC.model ->
        unit ->
        object
          inherit [Parameter.t] abstract_widget
          val obj : 'a Gtk.obj
        end

      val apply_observer_params :
        cont:((macro_observer -> unit) list -> 'a) ->
        (macro_observer -> unit) list -> ?model:macro_model -> 'a

      val apply_widget_params :
        cont:((macro_widget -> unit) list -> 'a) ->
        (macro_widget -> unit) list ->
        ?packing:(GObj.widget -> unit) -> ?model:macro_model -> 'a

      val create_and_prepare :
        (unit -> 'a) ->
        unit -> ('a -> unit) list -> unit -> 'a

      val widget :
        (unit ->
         (< attach : 'b -> unit; coerce : 'c; .. > as 'a)) ->
        ?packing:('c -> unit) -> ?model:'b -> unit -> 'a
    end
