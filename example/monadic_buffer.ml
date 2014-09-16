(* MonadicBuffers -- Monadic output to a buffer

Author: Michael Grünewald
Date: Tue Sep 16 13:33:01 CEST 2014

Blueprint (https://bitbucket.org/michipili/blueprint)
This file is part of Blueprint

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

module MonadicBuffer : sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val add_char : char -> unit t
  val add_string : string -> unit t
  val contents : int -> unit t -> string
  val lift : ('a -> 'b) -> ('a t -> 'b t)
  val ( >>= ) :'a t -> ('a -> 'b t) -> 'b t
  val ( >> ) : 'a t -> 'b t -> 'b t
end = struct

  type 'a t = Buffer.t -> 'a

  let return x =
    fun _ -> x

  let bind m f =
    fun buf -> f (m buf) buf

  let add_string s buf =
    Buffer.add_string buf s

  let add_char c buf =
    Buffer.add_char buf c

  let contents sz m =
    let buf = Buffer.create sz in
    m buf;
    Buffer.contents buf

  let lift f m =
    bind m (fun x -> return (f x))

  let ( >>= ) =
    bind

  let ( >> ) m1 m2 =
    bind m1 (fun _ -> m2)
end


let buffer_sz = 1000

let add_key k v =
  let open MonadicBuffer in
  return ()
  >> add_string k
  >> add_string ": "
  >> add_string v
  >> add_char '\n'

let rec add_alist lst =
  let open MonadicBuffer in
  match lst with
    | [] -> return ()
    | (k,v) :: tl -> add_key k v >> add_alist tl

let hackers_alist =  [
  "From", "DECWRL::\"EKUNS@cancer.rutgers.edu\" \"Eddie Kuns  30-May-89 2216 EST\" 30-MAY-1989 19:19:47.60";
  "To", "rnd%mts@itsgw.rpi.edu, schechterml@VTCC1.BITNET, santiago@ygdrsl.dec.com, dave%foo@whuxr.att.com, lloyd!sunfs3!doug@husc6.harvard.edu, giovanni@FNALD.BITNET";
  "CC", "";
  "Subj", "Enjoy the following!  I did!  <grin>";
  "Return-path", "CTDAY@B0VS02.FNAL.GOV";
  "Received", "from B0VS02.FNAL.GOV by cancer.rutgers.edu; Tue, 30 May 89 10:34 EST";
  "Date", "Sat, 27 May 1989 8:39:29 CDT";
  "From", "CTDAY@B0VS02.FNAL.GOV";
  "To", "EKuns@zodiac.rutgers.edu";
  "Message-Id", "<890527083929.2540009a@B0VS02.FNAL.GOV>";
  "X-Vmsmail-To", "SMTP%\"EKuns@zodiac.rutgers.edu\""; 
  "Received", "from decwrl.dec.com by LBL.Gov with INTERNET ; Thu, 25 May 89 11:05:38 PDT";
  "Received", "by decwrl.dec.com (5.54.5/4.7.34) id AA04313; Thu, 25 May 89 11:05:36 PDT";
  "Message-Id", "<8905251805.AA04313@decwrl.dec.com>";
  "Received", "by decwrl.dec.com (5.54.5/4.7.34) for ctday%b0host.hepnet@lbl.gov; id AA04313; Thu, 25 May 89 11:05:36 PDT";
  "From", "cvi%hpsrad.DEC@decwrl.dec.com (C. van Ingen, HPS A/D, DTN 297-6186)";
  "Date", "25 May 89 13:53";
  "To", "russell%hpsrad.DEC@decwrl.dec.com, ctday%hpsrad.DEC@decwrl.dec.com";
  "Subject", "Nerd Alert";
]


let hackers_monad =
  let open MonadicBuffer in
  return hackers_alist
  >>= add_alist
  |> contents buffer_sz

let rec buffer_add_alist buf alist =
  match alist with
    | [] -> ()
    | (k,v) :: tl -> buffer_add_key buf k v; buffer_add_alist buf tl
and buffer_add_key buf k v =
  let open Buffer in
  add_string buf k;
  add_string buf ": ";
  add_string buf v;
  add_char buf '\n'

let with_buffer sz f x =
  let buf = Buffer.create sz in
  f buf x;
  Buffer.contents buf

let hackers_with_buffer =
  with_buffer buffer_sz buffer_add_alist hackers_alist

let long_list sz =
  let rec loop ax n =
    if n = 0 then ax else loop (("XXXX", "YYYYY") :: ax) (n-1)
  in
  loop [] sz

let monadic alist =
  let open MonadicBuffer in
  return alist
  >>= add_alist
  |> contents buffer_sz

let higher_order alist =
  with_buffer buffer_sz buffer_add_alist alist

let time f x =
  let start = Sys.time () in
  let stop = f x; Sys.time () in
  stop -. start

let sample n step =
  let rec loop ax i =
    if i > n then List.rev ax else loop (i::ax) (i + step)
  in
  loop [] 0

let () =
  let open Printf in
  let time_method f label sample =
    let comment = ref(sprintf "\t%% %s" label) in
    let loop n =
      let lst = long_list n in
      printf "%d\t%f%s\n" n (time f lst) !comment;
      comment := "";
    in
    List.iter loop sample;
    printf "\n";
  in
  let loop sample (f, label) =
    time_method f label sample
  in
  List.iter (loop (sample 100_000 500)) [
    monadic, "Monadic";
    higher_order, "Higher-Order";
  ]
