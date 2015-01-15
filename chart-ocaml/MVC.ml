(* Copyright (C) 2004 Julien SIGNOLES *)

(*i $Id: MVC.ml,v 1.2 2004/12/01 11:02:56 signoles Exp $ i*)

module type COMPARABLE = sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val hash: t -> int
end

(* Set of views *)

module type VIEWS = sig
  type view
  type t
  val create: unit -> t
  val add: t -> view -> (unit -> unit) -> unit
  val add_new: ?display:bool -> t -> (unit -> unit) -> view
  val del: t -> view -> unit
  val update: t -> unit
  val iter: (view -> (unit -> unit) -> unit) -> t -> unit
end

module Views: VIEWS = struct

  type view = int

  type t = (view, unit -> unit) Hashtbl.t

  let create () = Hashtbl.create 7

  let add = Hashtbl.add

  let add_new =
    let cpt = ref 0 in
    fun ?(display=true) views up ->
      if display then up ();
      incr cpt;
      add views !cpt up;
      !cpt

  let del = Hashtbl.remove

  let iter = Hashtbl.iter

  let update = iter (fun _ up -> up ())

end

type view = Views.view

(* Model *)

module type MODEL = sig
  type domain
  include COMPARABLE
  val create : domain -> t
  val domain : t -> domain
  val set : t -> domain -> unit
  val add_view : t -> Views.view -> (unit -> unit) -> unit
  val new_view : ?display:bool -> t -> (unit -> unit) -> Views.view
  val del_view : t -> Views.view -> unit
end

module Model = struct

  module Basic(D: COMPARABLE) = struct

    type domain = D.t

    type t = { mutable domain: domain; views: Views.t }

    let domain m = m.domain

    let create d = { domain = d; views = Views.create () }

    let set m d =
      m.domain <- d;
      Views.update m.views

    let add_view m = Views.add m.views

    let new_view ?display m = Views.add_new ?display m.views

    let del_view m = Views.del m.views

    let compare m1 m2 = D.compare m1.domain m2.domain

    let equal m1 m2 = D.equal m1.domain m2.domain

    let hash m = D.hash m.domain

  end

  module Aggregate
    (M: MODEL)
    (D: sig
       include COMPARABLE
       val iter_submodel : (M.t -> unit) -> t -> unit
     end) =
  struct

    include Basic(D)

    (* auxiliary functions *)

    let apply f m = D.iter_submodel f m.domain

    let add_to_submodels m v up = apply (fun m -> M.add_view m v up) m

    let del_from_submodels m v = apply (fun m -> M.del_view m v) m

    (* *** *)

    let add_view m v update =
      add_view m v update;
      add_to_submodels m v update

    let new_view ?display m update =
      let v = new_view ?display m update in
      add_to_submodels m v update;
      v

    let del_view m v =
      del_view m v;
      del_from_submodels m v

    let set m d =
      Views.iter (fun v _ -> del_from_submodels m v) m.views;
      m.domain <- d;
      Views.iter (fun v up -> add_to_submodels m v up; up ()) m.views

  end

end

(* View *)

module type GENERIC_VIEW = sig
  type model
  type t
  val destroy: t -> unit
  val change: t -> model -> unit
end

module type DOMAIN_VIEW = sig
  include GENERIC_VIEW
  val update: t -> unit
  val model: t -> model
end

module type VIEW = sig
  include GENERIC_VIEW
  type domain
  val domain: t -> domain
  val create: ?display:bool -> (model -> domain) -> model -> t
end

module View
  (M : MODEL)
  (V : DOMAIN_VIEW with type model = M.t) :
  VIEW with type domain = V.t and type model = M.t =
struct

  type model = M.t

  type domain = V.t

  type t = { domain : V.t; mutable id : Views.view }

  let domain v = v.domain

  let new_domain ?display m d = M.new_view ?display m (fun () -> V.update d)

  let create ?display create_domain m =
    let d = create_domain m in
    { domain = d; id = new_domain ?display m d }

  let del_from_model v = M.del_view (V.model v.domain) v.id

  let destroy v =
    del_from_model v;
    V.destroy v.domain

  let change v m =
    del_from_model v;
    V.change v.domain m;
    v.id <- new_domain m v.domain

end
