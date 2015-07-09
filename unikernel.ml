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
  |> List.map (fun json -> Util.to_string json |> Irmin.Hash.SHA1.of_hum)
  |> Array.of_list
)

let date_of_rev = Hashtbl.create (Array.length revs)

type build_result = {
  outcome : Status.outcome option;
  platform : string;
  build_commit : Irmin.Hash.SHA1.t;
  date : Unix.tm;
}

let format_date date =
  Printf.sprintf "%04d-%02d-%02d %02d:%02d"
    (date.Unix.tm_year + 1900)
    (date.Unix.tm_mon + 1)
    (date.Unix.tm_mday)
    (date.Unix.tm_hour)
    (date.Unix.tm_min)

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
      let pkg_info = Status.pkg_info ~rev pkg in
      n_platforms - pkg_info.Status.pass
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
                acc || (build.outcome <> Some Status.Pass)
              ) false rev in
            let cl =
              match rev.(i).outcome with
              | Some (Status.Pass) -> "fullsuccess"
              | Some (Status.Fail) -> if any_failed then "somesuccess" else "allfail"
              | None -> "unknown" in
            let href =
              Printf.sprintf "/pkg/%s/platform/%s/build/%s"
                pkg
                rev.(i).platform
                (Irmin.Hash.SHA1.to_hum rev.(i).build_commit) in
            td ~a:[a_class [cl]] [
              match rev.(i).outcome with
              | None -> pcdata "-"
              | Some outcome ->
                  a ~a:[a_href href] [pcdata (
                    match outcome with
                    | Status.Pass -> "✔"
                    | Status.Fail -> "✘"
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

  let view_build ~log ~actions ~info () =
    let open Html5.M in
    [
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
      let opam_rev = List.hd (Irmin.Task.messages task) |> String.trim |> Irmin.Hash.SHA1.of_hum in
      let date = Hashtbl.find date_of_rev opam_rev in
      platforms |> Array.map (fun (platform, _disp) ->
        let outcome = Status.build_outcome ~rev:opam_rev ~platform pkg in
        { platform; build_commit = hash; outcome; date }
      )
      |> Lwt.return
    )

  let return_build ~pkg ~platform store =
    get_package_history ~depth:20 pkg >>= fun history ->
    let header = view_package ~pkg ~history () in
    let store = store "get build details" in
    Store.head store >>= function
    | None -> assert false
    | Some head ->
    Store.task_of_head store head >>= fun task ->
    let opam_rev_str = List.hd (Irmin.Task.messages task) |> String.trim in
    let opam_rev = Irmin.Hash.SHA1.of_hum opam_rev_str in
    let date = Hashtbl.find date_of_rev opam_rev in
    Store.read_exn store [platform; pkg; "log"] >>= fun log ->
    Store.read_exn store [platform; pkg; "actions"] >>= fun actions ->
    Store.read_exn store [platform; pkg; "buildtime"] >>= fun buildtime ->
    Store.read_exn store [platform; pkg; "info"] >>= fun info ->
    let result, buildtime, log =
      let open Html5.M in
      match Status.build_outcome ~platform ~rev:opam_rev pkg with
      | None ->
          pcdata "Unknown", "-", []
      | Some outcome ->
          let status =
            match outcome with
            | Status.Pass -> span ~a:[a_class ["buildok"]] [pcdata "Pass"]
            | Status.Fail -> span ~a:[a_class ["buildfail"]] [pcdata "FAIL"] in
          status,
          Printf.sprintf "Build time: %s seconds" buildtime,
          view_build ~log ~actions ~info () in
    let meta =
      let open Html5.M in
      [
        hr ();
        h1 [pcdata platform];
        table [
          tr [ th [pcdata "OPAM repository head"]; td [
            a ~a:[a_href ("https://github.com/ocaml/opam-repository/commit/" ^ opam_rev_str)] [
              pcdata opam_rev_str
            ]
          ] ];
          tr [ th [pcdata "Commit date"]; td [pcdata (format_date date)] ];
          tr [ th [pcdata "Build time"]; td [pcdata buildtime] ];
          tr [ th [pcdata "Result"]; td [result] ];
        ];
        hr ();
      ] in
    header @ meta @ log
    |> respond_ok ~title:(Printf.sprintf "Builds for %s" pkg)

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
      Store.task_of_head (opam_repo "get dates") rev >|= fun task ->
      let date = Irmin.Task.date task |> Int64.to_float |> Unix.gmtime in
      Hashtbl.add date_of_rev rev date
    ) >>= fun () ->
    http (`TCP 29211) (S.make ~conn_closed:ignore ~callback:(handle_request ~resources) ())
end
