(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Mirage

let net =
  match get_mode () with
  | `Xen -> `Direct
  | `Unix | `MacOSX ->
      try match Sys.getenv "NET" with
        | "direct" -> `Direct
        | "socket" -> `Socket
        | _        -> `Direct
      with Not_found -> `Socket

let dhcp =
  try match Sys.getenv "DHCP" with
    | "" -> false
    | _  -> true
  with Not_found -> true

let ipv4_conf =
  let i = Ipaddr.V4.of_string_exn in
  {
    address  = i "10.0.0.2";
    netmask  = i "255.255.255.0";
    gateways = [i "10.0.0.1"];
  }

let stack console =
  match net, dhcp with
  | `Direct, true  -> direct_stackv4_with_dhcp console tap0
  | `Direct, false -> direct_stackv4_with_static_ipv4 console tap0 ipv4_conf
  | `Socket, _     -> socket_stackv4 console [Ipaddr.V4.any]

let main =
  foreign
    ~libraries:["irmin.unix"; "tyxml"; "str"; "yojson"; "patience_diff"]
    ~packages:["irmin"; "tyxml"; "yojson"; "git"; "patience_diff"]
    "Unikernel.Main" (http @-> kv_ro @-> job)

let http_srv = http_server (conduit_direct (stack default_console))

let resources = crunch "resources"

let () =
  register "bulk-build-www" [
    main $ http_srv $ resources
  ]
