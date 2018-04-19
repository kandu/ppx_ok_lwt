open Migrate_parsetree
open OCaml_403.Ast

open Asttypes
open Parsetree

open Ast_helper
open Ast_mapper
open Location

let debug= ref true
let strict= ref true

let with_loc loc f=
  let tmp_loc= !default_loc in
  default_loc:= loc;
  let r= f () in
  default_loc:= tmp_loc;
  r

let lwts mapper expr trigger_loc=
  match expr with
  | [%expr [%e? lhs]; [%e? rhs]] ->
    let pos_trigger= trigger_loc.Location.loc_start.Lexing.pos_cnum
    and pos_lhs= lhs.pexp_loc.Location.loc_start.Lexing.pos_cnum in
    let bind expr lhs rhs=
      default_loc:= lhs.pexp_loc;
      let pat=
        with_loc lhs.pexp_loc 
          (fun ()-> if !strict then [%pat? ()] else [%pat? _])
      in
      if !debug then
        [%expr
          let module Reraise =
            struct external reraise : exn -> 'a = "%reraise" end
          in
          Lwt.backtrace_bind
            (fun exn -> try Reraise.reraise exn with exn -> exn)
            [%e lhs]
            (fun [%p pat] -> [%e rhs])]
            [@metaloc expr.pexp_loc]
      else
        [%expr Lwt.bind
          [%e lhs]
          (fun [%p pat] -> [%e rhs])]
          [@metaloc expr.pexp_loc]
    in
    let rec gen_sequence mapper expr=
      match expr with
      | [%expr [%e? lhs]; [%e? rhs]] ->
        let lhs, rhs= mapper.expr mapper lhs, gen_sequence mapper rhs in
        bind expr lhs rhs
      | _ -> mapper.expr mapper expr
    in
    if pos_trigger > pos_lhs then (* wag tail syntax *)
      let lhs, rhs= mapper.expr mapper lhs, mapper.expr mapper rhs in
      bind expr lhs rhs
    else
      gen_sequence mapper expr
  | _ -> mapper.expr mapper expr


let lwtc mapper exp=
  let exp= mapper.expr mapper exp in
  let exp=
    if !debug then
      [%expr
        let module Reraise =
          struct external reraise : exn -> 'a = "%reraise" end
        in
        Lwt.backtrace_catch
          (fun exn -> try Reraise.reraise exn with exn -> exn)
          (fun () -> [%e exp]) Lwt.fail]
    else
      [%expr Lwt.catch (fun () -> [%e exp]) Lwt.fail]
  in
  exp [@metaloc exp.pexp_loc]


let ok_lwt_mapper _config _cookies=
  { default_mapper with
    expr= fun mapper expr->
      match expr with
      | { pexp_desc=
            Pexp_extension (
              {txt="lwts"; loc= trigger_loc},
              PStr[{pstr_desc= Pstr_eval (exp, _);_}]);
          _
        }->
        lwts mapper exp trigger_loc
      | [%expr [%lwtc [%e? exp]]]-> lwtc mapper exp
      | _ -> default_mapper.expr mapper expr
  }

let args = Arg.[
  "--ol-no-debug", Clear debug, " disable debug mode";
  "--ol-no-strict", Clear strict, " allow non-unit sequence operations";
]

let ()= Driver.register
  ~name:"ppx_ok_lwt"
  ~args
  (module OCaml_403)
  ok_lwt_mapper

