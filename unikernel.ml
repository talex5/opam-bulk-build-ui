(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt
open V1_LWT

let () = Log.(set_log_level INFO)

(* Never used, but needed to create the store. *)
let task s = Irmin.Task.create ~date:0L ~owner:"Server" s

module Store = Irmin.Basic(Irmin_unix.Irmin_git.FS)(Irmin.Contents.String)
let config = Irmin_unix.Irmin_git.config ~root:"logs" ~bare:true ()

let show_head = function
  | None -> "(none)"
  | Some head -> String.sub (Irmin.Hash.SHA1.to_hum head) 0 6

let re_dash = Str.regexp_string "-"

let rec filter_map fn = function
  | [] -> []
  | (x::xs) ->
      match fn x with
      | None -> filter_map fn xs
      | Some y -> y :: filter_map fn xs

let opam_repo_config = Irmin_unix.Irmin_git.config ~root:"opam-repository" ~bare:true ()
let opam_repo = Store.create opam_repo_config task

(* Newest first *)
let revs = Yojson.Basic.(
  from_file "revs.json"
  |> Util.to_list
  |> List.map Util.to_string
  |> Array.of_list
)

let date_of_rev = Hashtbl.create (Array.length revs)

type build_result = {
  success : bool;
  platform : string;
  build_commit : Irmin.Hash.SHA1.t;
  date : Unix.tm;
}

let successes =
  let hash = Hashtbl.create 1000 in
  Yojson.Basic.(
    from_file "status.json"
    |> Util.to_assoc
    |> List.iter (fun (rev, rev_info) ->
        Util.to_assoc rev_info
        |> List.iter (fun (platform, platform_info) ->
            platform_info
            |> Util.member "ok"
            |> Util.to_list
            |> List.iter (fun pkg ->
                let key = (Util.to_string pkg, rev) in
                let successes =
                  try Hashtbl.find hash key
                  with Not_found -> [] in
                Hashtbl.replace hash key (platform :: successes)
            )
       )
    )
  );
  hash

let platforms = [
  "local-centos-7-ocaml-4.02.1", "CentOS-7 / OCaml 4.02.1";
  "local-debian-stable-ocaml-4.01.0", "Debian-stable / OCaml 4.01.0";
  "local-debian-stable-ocaml-4.02.1", "Debian-stable / OCaml 4.02.1";
  "local-ubuntu-14.04-ocaml-4.01.0", "Ubuntu-14.04 / OCaml 4.01.0";
  "local-ubuntu-14.04-ocaml-4.02.1", "Ubuntu-14.04 / OCaml 4.02.1";
]

let n_platforms = List.length platforms

module Main (S:Cohttp_lwt.Server) (FS:KV_RO) = struct
  (* Split a URI into a list of path segments *)
  let split_path uri =
    let path = Uri.path uri in
    let rec aux = function
      | [] | [""] -> []
      | hd::tl -> hd :: aux tl
    in
    List.filter (fun e -> e <> "")
      (aux (Re_str.(split_delim (regexp_string "/") path)))

  let count_failures ~rev pkg =
    try
      let ok = Hashtbl.find successes (pkg, rev) |> List.length in
      n_platforms - ok
    with Not_found -> n_platforms

  let view_package_list ~tags () =
    let shown_revs =
      let r = ref [] in
      for i = 0 to 19 do
        r := revs.(i) :: !r
      done;
      !r in
    let open Html5.M in
    [
      let header =
        tr (
          th [pcdata "Month"; br (); pcdata "Day"] :: (
          shown_revs |> List.map (fun rev ->
            let date = Hashtbl.find date_of_rev rev in
            th [
              pcdata (Printf.sprintf "%02d" (date.Unix.tm_mon + 1));
              br ();
              pcdata (Printf.sprintf "%02d" date.Unix.tm_mday);
            ]
          ))
        ) in
      let rows = tags |> filter_map (fun tag ->
        match Str.bounded_split_delim re_dash tag 2 with
        | ["pkg"; pkg] ->
            let href = Printf.sprintf "/pkg/%s" pkg in
            let results =
              shown_revs |> List.map (fun rev ->
                let failures = count_failures ~rev pkg in
                let cl =
                  match failures with
                  | 0 -> ["fullsuccess"]
                  | 5 -> ["allfail"]
                  | _ -> ["somesuccess"] in
                td ~a:[a_class cl] (
                  if failures = 0 then [] else [pcdata (string_of_int failures)]
                )
              ) in
            Some (tr ~a:[a_class ["clkrow"]; a_onclick (Printf.sprintf "document.location=%S" href)] (
              td [a ~a:[a_href href] [pcdata pkg]] :: results
            ))
        | _ -> None
      ) in
      table (header :: rows)
    ]

  let view_package ~pkg ~history () =
    let open Html5.M in
    let header =
      tr (
        th [pcdata "Month"; br (); pcdata "Day"] :: (
        history |> List.rev |> List.map (fun rev ->
          let date = rev.(0).date in
          th [
            pcdata (Printf.sprintf "%02d" (date.Unix.tm_mon + 1));
            br ();
            pcdata (Printf.sprintf "%02d" date.Unix.tm_mday);
          ]
        ))
      ) in
    let rows =
      platforms |> List.mapi (fun i (platform, platform_disp) ->
        let results =
          history |> List.map (fun rev ->
            let any_failed =
              Array.fold_left (fun acc build ->
                acc || build.success
              ) false rev in
            let cl =
              if rev.(i).success then "fullsuccess"
              else if any_failed then "somesuccess"
              else "allfail" in
            let href =
              Printf.sprintf "/pkg/%s/platform/%s/build/%s"
                pkg
                rev.(i).platform
                (Irmin.Hash.SHA1.to_hum rev.(i).build_commit) in
            td ~a:[a_class [cl]] [
              a ~a:[a_href href] [pcdata (
                if rev.(i).success then "✔" else "✘"
              )]
            ]
          ) in
        let href = Printf.sprintf "/pkg/%s/platform/%s" pkg platform in
        tr ~a:[a_class ["pkgrow"]] (
          td [a ~a:[a_href href] [pcdata platform_disp]] :: (List.rev results)
        )
      ) in
    [
      table (header :: rows)
    ]

  let view_build ~log ~actions ~buildtime ~info () =
    let open Html5.M in
    [
      p [pcdata (Printf.sprintf "Build time: %ss" buildtime)];
      pre [pcdata info];
      hr ();
      pre [pcdata actions];
      hr ();
      pre [pcdata log];
    ]

  let respond_ok ~title:t content =
    let open Html5.M in
    let t = pcdata t in
    let b = Buffer.create 1000 in
    Html5.P.print ~output:(Buffer.add_string b) (
      html
        (head (title t) [
          meta ~a:[a_charset "UTF-8"] ();
          link ~rel:[`Stylesheet] ~href:"/css/theme.css" ();
        ])
        (body (
          p [a ~a:[a_href "/"] [pcdata "OPAM Bulk Builds"]] ::
          h1 [t] ::
          content))
    );
    S.respond_string ~status:`OK ~body:(Buffer.contents b) ()

  let read fs name =
    FS.size fs name >>= function
    | `Error (FS.Unknown_key _) -> fail (Failure ("read " ^ name))
    | `Ok size ->
      FS.read fs name 0 (Int64.to_int size) >>= function
      | `Error (FS.Unknown_key _) -> fail (Failure ("read " ^ name))
      | `Ok bufs -> return (Cstruct.copyv bufs)

  let get_package_history ~depth pkg =
    Store.of_tag config task ("pkg-" ^ pkg) >>= fun store ->
    let store = store "get_package_history" in
    Store.history ~depth store >>= fun history ->
    let module Top = Graph.Topological.Make_stable(Store.History) in
    let hashes = ref [] in
    let platforms = Array.of_list platforms in
    history |> Top.iter (fun hash -> hashes := hash :: !hashes);
    !hashes |> Lwt_list.map_s (fun hash ->
(*       Store.of_head config task hash >>= fun store -> *)
      Store.task_of_head store hash >>= fun task ->
      let opam_rev = List.hd (Irmin.Task.messages task) |> String.trim in
      let key = (pkg, opam_rev) in
      Printf.printf "key = (%S, %S)\n%!" pkg opam_rev;
      let ok = try Hashtbl.find successes key with Not_found -> [] in
      let date = Hashtbl.find date_of_rev opam_rev in
      platforms |> Array.map (fun (platform, _disp) ->
        { platform; build_commit = hash; success = List.mem platform ok; date }
      )
      |> Lwt.return
    )

  let return_build ~pkg ~platform store =
    get_package_history ~depth:20 pkg >>= fun history ->
    let header = view_package ~pkg ~history () in
    let store = store "get build details" in
    Store.read_exn store [platform; pkg; "log"] >>= fun log ->
    Store.read_exn store [platform; pkg; "actions"] >>= fun actions ->
    Store.read_exn store [platform; pkg; "buildtime"] >>= fun buildtime ->
    Store.read_exn store [platform; pkg; "info"] >>= fun info ->
    header @ [Html5.M.hr ()] @ view_build ~log ~actions ~buildtime ~info ()
    |> respond_ok ~title:(Printf.sprintf "Build %s on %s" pkg platform)

  let handle_request ~resources _conn_id request _body =
    Lwt.catch (fun () ->
      match Cohttp.Request.meth request, split_path (Cohttp.Request.uri request) with
      | `GET, [] ->
          Store.create config task >>= fun store ->
          let store = store "list branches" in
          Store.tags store >>= fun tags ->
          view_package_list ~tags:(List.sort String.compare tags) ()
          |> respond_ok ~title:"Package index"
      | `GET, ["css"; file] ->
          read resources ("css/" ^ file) >>= fun data ->
          S.respond_string ~status:`OK ~body:data ()
(*       | `GET, ["pkg"; pkg; "rev"; rev] -> *)
      | `GET, ["pkg"; pkg] ->
          get_package_history ~depth:20 pkg >>= fun history ->
          view_package ~pkg ~history ()
          |> respond_ok ~title:pkg
      | `GET, ["pkg"; pkg; "platform"; platform; "build"; build_commit] ->
          Store.of_head config task (Irmin.Hash.SHA1.of_hum build_commit) >>= fun store ->
          return_build ~pkg ~platform store
      | `GET, ["pkg"; pkg; "platform"; platform] ->
          Store.of_tag config task ("pkg-" ^ pkg) >>= fun store ->
          return_build ~pkg ~platform store
      | _ -> S.respond_error ~status:`Method_not_allowed ~body:"Invalid request" ()
    ) (fun ex ->
      Log.warn "Unhandled exception processing HTTP request: %s" (Printexc.to_string ex);
      fail ex
    )

  let start http resources =
    opam_repo >>= fun opam_repo ->
    revs |> Array.to_list |> Lwt_list.iter_p (fun rev ->
      let hash = Irmin.Hash.SHA1.of_hum rev in
      Store.task_of_head (opam_repo "get dates") hash >|= fun task ->
      Printf.printf "%s -> %Ld\n%!" rev (Irmin.Task.date task);
      let date = Irmin.Task.date task |> Int64.to_float |> Unix.gmtime in
      Hashtbl.add date_of_rev rev date
    ) >>= fun () ->
    http (`TCP 29211) (S.make ~conn_closed:ignore ~callback:(handle_request ~resources) ())
end
