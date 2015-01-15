(* Copyright (C) 2004 Julien SIGNOLES *)

(*i $Id: MVC.mli,v 1.2 2004/12/01 11:02:56 signoles Exp $ i*)

(** Generic Model-View-Controller. *)

(** {1 Model} *)

(** Signature allowing the use of [Map.Make], [Set.Make] and [Hashtbl.Make] *)
module type COMPARABLE = sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val hash: t -> int
end

type view
  (** Should not be used by the user. *)

(** Signature of the model *)
module type MODEL = sig

  type domain
    (** type of the model domain *)

  include COMPARABLE

  val create : domain -> t
    (** create a model from its domain *)

  val domain : t -> domain
    (** domain of a model *)

  val set : t -> domain -> unit
    (** set the domain of a model *)

  (** The following operations should not be used by the user.
    They are only required by the functors [Aggregate] and [View] below *)

  val add_view : t -> view -> (unit -> unit) -> unit
  val new_view : ?display:bool -> t -> (unit -> unit) -> view
  val del_view : t -> view -> unit

end

(** Model implementations *)
module Model : sig

  (** [Basic] implements a model from a given domain *)
  module Basic(D: COMPARABLE) : MODEL with type domain = D.t

  (** [Aggregate] implements an aggregate from a submodel and a domain.
    The domain of an aggregate uses another model. *)
  module Aggregate
    (M: MODEL)
    (D: sig
       include COMPARABLE
       val iter_submodel : (M.t -> unit) -> t -> unit
     end):
    MODEL with type domain = D.t

end

(** {1 View} *)

(** Common signature to the view domain and to the view *)
module type GENERIC_VIEW = sig

  type model
    (** type of the underlying model *)

  type t
    (** type of the view *)

  val destroy: t -> unit
    (** destroy a view *)

  val change: t -> model -> unit
    (** change the model of a view *)

end

(** Signature of the view domain *)
module type DOMAIN_VIEW = sig

  include GENERIC_VIEW

  val update: t -> unit
    (** update a view *)

  val model: t -> model
    (** underlying model of a view *)

end

(** Signature of the view *)
module type VIEW = sig

  include GENERIC_VIEW

  type domain
    (** type of the view model *)

  val domain: t -> domain
    (** domain of a view *)

  val create: ?display:bool -> (model -> domain) -> model -> t
    (** [create d f m] creates a view from the model [m].
      [f] is a function building the domain view from a model.
      The view is updated now if [d] is [true] (default is [true]. *)

end

(** View implementation *)
module View
  (M: MODEL)
  (V: DOMAIN_VIEW with type model = M.t) :
  VIEW with type model = M.t and type domain = V.t
