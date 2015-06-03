(* GHelper -- Grab pack of utilities for Lablgtk

   Blueprint (https://github.com/michipili/blueprint)
   This file is part of Blueprint

   Copyright © 2014–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

module Maybe =
struct
  type 'a t = 'a option
  let bind opt f =
    match opt with
    | None -> None
    | Some(x) -> Some(f x)
  let return x =
    Some(x)
  module Operator =
  struct
    let ( >>= ) = bind
    let ( >> ) m f = bind m (fun _ -> f ())
  end
end

let chain_callback (f:'a->unit) x =
  f x; x

let maybe_callback opt x =
  match opt with
  | None -> x
  | Some(f) -> chain_callback f x

let maybe_apply opt f x =
  match opt with
  | None -> x
  | Some(v) -> f x v; x
