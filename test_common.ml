(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let () = Random.self_init ()
let pp ppf style h = Fmt.pf ppf "%a " Fmt.(styled style string) h

let pad n x =
  if String.length x > n then x else x ^ String.make (n - String.length x) ' '

let reporter () =
  let report src level ~over k msgf =
    let k _ = over () ; k () in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      let dt = 0.0 in
      Fmt.kpf k ppf
        ("\r%+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        dt
        Fmt.(styled `Magenta string)
        (pad 10 @@ Logs.Src.name src)
        Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k fmt
  in
  {Logs.report}

let setup_log level =
  Logs.set_level level ;
  Logs.set_reporter (reporter ()) ;
  ()

let verbose () = setup_log (Some Logs.Debug)
let quiet () = setup_log None

let () =
  Printexc.record_backtrace true ;
  Lwt.async_exception_hook := fun e -> Fmt.kstrf failwith "%a" Fmt.exn e

let compare_option fn x y =
  match x, y with
  | Some x, Some y -> fn x y
  | None, None -> true
  | Some _, None | None, Some _ -> false

let printer_option fn = function None -> "<none>" | Some v -> fn v

let rec compare_list fn x y =
  match x, y with
  | xh :: xt, yh :: yt -> fn xh yh && compare_list fn xt yt
  | [], [] -> true
  | _ -> false

let printer_list pp_data = Fmt.hvbox (Fmt.Dump.list pp_data)

module Make (S : Git.S) = struct
  let compare_list equal compare l1 l2 =
    compare_list equal (List.sort compare l1) (List.sort compare l2)

  let mk equal compare pp =
    let aux (type a) compare pp msg =
      let testable : a Alcotest.testable =
        let module M = struct type t = a

                              let equal = compare
                              let pp = pp end in
        (module M)
      in
      Alcotest.check testable msg
    in
    ( aux equal pp
    , aux (compare_option equal) Fmt.(option pp)
    , aux (compare_list equal compare) Fmt.(list pp) )

  let assert_key_equal, assert_key_opt_equal, assert_keys_equal =
    mk S.Hash.equal S.Hash.compare S.Hash.pp

  let assert_value_equal, assert_value_opt_equal, assert_values_equal =
    mk S.Value.equal S.Value.compare S.Value.pp

  let assert_tag_equal, assert_tag_opt_equal, assert_tags_equal =
    mk S.Value.Tag.equal S.Value.Tag.compare S.Value.Tag.pp

  let assert_ref_equal, assert_ref_opt_equal, assert_refs_equal =
    mk S.Reference.equal S.Reference.compare S.Reference.pp

  let ( assert_head_contents_equal
      , assert_head_contents_opt_equal
      , assert_heads_contents_equal ) =
    mk S.Reference.equal_head_contents S.Reference.compare_head_contents
      S.Reference.pp_head_contents

  let assert_cstruct_equal, assert_cstruct_opt_equal, assert_cstructs_equal =
    mk ( = ) compare (Fmt.of_to_string Cstruct.debug)

  let ( assert_cstruct_data_equal
      , assert_cstruct_data_opt_equal
      , assert_cstructs_data_equal ) =
    mk
      (fun a b -> String.equal (Cstruct.to_string a) (Cstruct.to_string b))
      (fun a b -> String.compare (Cstruct.to_string a) (Cstruct.to_string b))
      (Fmt.hvbox
         (Encore.Lole.pp_scalar ~get:Cstruct.get_char ~length:Cstruct.len))

  let ( assert_ref_and_hash_equal
      , assert_ref_and_hash_opt_equal
      , assert_refs_and_hashes_equal ) =
    mk
      (fun (r, h) (r', h') -> S.Reference.equal r r' && S.Hash.equal h h')
      (fun (r, h) (r', h') ->
        match S.Reference.compare r r' with 0 -> S.Hash.compare h h' | n -> n
        )
      (Fmt.Dump.pair S.Reference.pp S.Hash.pp)

  module Map = Map.Make (S.Hash)

  let ( assert_index_pack_equal
      , assert_index_pack_opt_equal
      , assert_index_packs_equal ) =
    let equal_value (h1, (c1, o1)) (h2, (c2, o2)) =
      S.Hash.equal h1 h2 && Optint.equal c1 c2 && Int64.equal o1 o2
    in
    let pp_value : (S.Hash.t * (Checkseum.Crc32.t * int64)) Fmt.t =
      Fmt.pair
        ~sep:(Fmt.const Fmt.string " -> ")
        S.Hash.pp
        Fmt.(Dump.pair Optint.pp int64)
    in
    let compare a b =
      if compare_list equal_value (fun (a, _) (b, _) -> S.Hash.compare a b) a b
      then 0
      else 1
    in
    let equal a b =
      compare_list equal_value (fun (a, _) (b, _) -> S.Hash.compare a b) a b
    in
    let pp = Fmt.Dump.list pp_value in
    mk
      (fun a b -> equal (Map.bindings a) (Map.bindings b))
      (fun a b -> compare (Map.bindings a) (Map.bindings b))
      (fun ppf a -> pp ppf (Map.bindings a))
end

