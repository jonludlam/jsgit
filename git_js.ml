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

type endpoint = Http.E.t = {uri: Uri.t; headers: Cohttp.Header.t}

let endpoint ?headers uri =
  let headers =
    match headers with None -> Cohttp.Header.of_list [] | Some h -> h
  in
  {headers; uri}

module Endpoint = struct type t = endpoint

                         let uri t = t.uri end

module Sync (G : Git.S) = struct
  module Http = Http.Make (G)
  module Endpoint = Endpoint

  type error = Http.error

  let pp_error ppf = function
    | x -> Http.pp_error ppf x

  type command =
    [ `Create of G.Hash.t * Git.Reference.t
    | `Delete of G.Hash.t * Git.Reference.t
    | `Update of G.Hash.t * G.Hash.t * Git.Reference.t ]

  let http_error x =
    Lwt.map (function Ok _ as x -> x | Error e -> Error e) x

  let push t ~push ?capabilities e =
    Http.push t ~push ?capabilities e |> http_error

  let ls t ?capabilities e =
    Http.ls t ?capabilities e |> http_error

  let fetch t ?shallow ?capabilities ~notify ~negociate ~have ~want ?deepen e =
    Http.fetch t ?shallow ?capabilities ~notify ~negociate ~have ~want
      ?deepen e |> http_error

  let clone t ?capabilities ~reference e =
    Http.clone t ?capabilities ~reference e |> http_error

  let fetch_some t ?capabilities ~references e =
    Http.fetch_some t ?capabilities ~references e |> http_error

  let fetch_all t ?capabilities ~references e =
    Http.fetch_all t ?capabilities ~references e |> http_error

  let fetch_one t ?capabilities ~reference e =
    Http.fetch_one t ?capabilities ~reference e |> http_error

  let update_and_create t ?capabilities ~references e =
    Http.update_and_create t ?capabilities ~references e |> http_error
end

