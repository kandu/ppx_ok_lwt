open Migrate_parsetree
open OCaml_403.Ast

open Asttypes
open Parsetree

open Ast_helper
open Ast_mapper
open Location

let debug= ref true
let strict= ref true

let lwts mapper expr=
  match expr with
  | [%expr [%e? lhs]; [%e? rhs]] ->
    let pat = if !strict then [%pat? ()] else [%pat? _] in
    let rec gen_sequence mapper expr=
      match expr with
      | [%expr [%e? lhs]; [%e? rhs]] ->
        let lhs, rhs= mapper.expr mapper lhs, gen_sequence mapper rhs in
        if !debug then
          [%expr Lwt.backtrace_bind
            (fun exn -> try raise exn with exn -> exn)
            [%e lhs]
            (fun [%p pat] -> [%e rhs])]
            [@metaloc expr.pexp_loc]
        else
          [%expr Lwt.bind
            [%e lhs]
            (fun [%p pat] -> [%e rhs])]
            [@metaloc expr.pexp_loc]
      | _ -> mapper.expr mapper expr
    in
    gen_sequence mapper expr

  | _ -> mapper.expr mapper expr


let lwts_mapper _config _cookies=
  { default_mapper with
    expr= fun mapper expr->
      match expr with
      | [%expr [%lwts [%e? expr]]] ->
        lwts mapper expr
      | _ -> default_mapper.expr mapper expr
  }

let args = Arg.[
  "-no-debug", Clear debug, "disable debug mode";
  "-no-strict", Clear strict, "allow non-unit sequence operations";
]

let ()= Driver.register
  ~name:"ppx_lwts"
  ~args
  (module OCaml_403)
  lwts_mapper

