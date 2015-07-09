(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

type outcome = Pass | Fail
type pkg = {
  pass : int;
  fail : int;
}

type key = {
  pkg : string;
  rev : Irmin.Hash.SHA1.t;
  platform : string;
}

let build_status = Hashtbl.create 1000
let pkg_status = Hashtbl.create 1000

let process ~rev ~platform ~platform_info dir result =
  let open Yojson.Basic in
  platform_info
  |> Util.member dir
  |> Util.to_list
  |> List.iter (fun pkg ->
      let key = {pkg = Util.to_string pkg; rev; platform} in
      Hashtbl.replace build_status key result
  )

let () =
  Yojson.Basic.(
    from_file "status.json"
    |> Util.to_assoc
    |> List.iter (fun (rev, rev_info) ->
        let rev = Irmin.Hash.SHA1.of_hum rev in
        Util.to_assoc rev_info
        |> List.iter (fun (platform, platform_info) ->
            process ~rev ~platform ~platform_info "ok" Pass;
            process ~rev ~platform ~platform_info "err" Fail;
       )
    )
  );
  build_status |> Hashtbl.iter (fun key result ->
    let pkg_key = (key.pkg, key.rev) in
    let pkg_info =
      try Hashtbl.find pkg_status pkg_key
      with Not_found -> {pass = 0; fail = 0} in
    let pkg_info =
      match result with
      | Pass -> {pkg_info with pass = pkg_info.pass + 1}
      | Fail -> {pkg_info with fail = pkg_info.fail + 1} in
    Hashtbl.replace pkg_status pkg_key pkg_info
  )

let pkg_info ~rev pkg =
  try Hashtbl.find pkg_status (pkg, rev)
  with Not_found -> {pass = 0; fail = 0}

let build_outcome ~rev ~platform pkg =
  let key = {pkg; rev; platform} in
  try Some (Hashtbl.find build_status key)
  with Not_found -> None
