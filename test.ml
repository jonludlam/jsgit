(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 * and Romain Calascibetta <romain.calascibetta@gmail.com>
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


module Http (Store : Test_store.S) = Test_sync.Make (struct
  module Sync = Git_js.Sync (Store)
  module M = Sync.Http
  module Store = Store

  type error = M.error
  type endpoint = M.Endpoint.t

  let pp_error = M.pp_error
  let clone t ~reference uri = M.clone t ~reference:(reference, reference) uri
  let endpoint_of_uri uri = Git_js.endpoint uri

  let fetch_all t ~references uri =
    let open Lwt.Infix in
    M.fetch_all t ~references uri
    >>= function
    | Error _ as err -> Lwt.return err | Ok _ -> Lwt.return (Ok ())

  let update t ~reference uri =
    M.update_and_create t
      ~references:(Store.Reference.Map.singleton reference [reference])
      uri
end)


module Mem_store = struct include Git.Mem.Store

                          let v root = v root end

(* XXX(dinosaure): we replace [move] to be a /forced move/: on [move a b], if
   [b] already exists, we delete it properly (Windows raises an error if [b]
   already exists). This case appear only on tests where we use
   [Store.Pack.from] on already existing PACK files. In this situation,
   [Store.Pack.from] wants to make a fresh new IDX file on [.git/objects/pack/]
   but it already exists because the PACK file comes from [.git/objects/pack/]
   too (and any PACK files on [.git/objects/pack/] have an IDX file - it's
   mandatory for Git).

   To avoid error on Windows, we tun [Git_unix.Fs] so. *)


module Http1 = Http (Mem_store)

let () =
  Printf.printf "Setting log level\n%!";
  Logs.set_level (Some Debug);
  Logs.set_reporter (Logs_browser.console_reporter ());
  Alcotest.run "git-unix"
    [ Http1.test_clone "mem-http-sync"
        [Uri.of_string "https://cors.isomorphic-git.org/github.com/mirage/ocaml-git.git", "gh-pages"]
    ]
