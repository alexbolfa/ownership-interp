open! Base

type target =
  { root : AbsLang.ident
  ; kind : Value.ref_kind
  }

type owner =
  | Stored of AbsLang.ident
  | Temporary

type edge =
  { owner : owner
  ; target : target
  }

type t = edge list

let empty = []
let key (AbsLang.Ident name) = name
let same_ident a b = String.equal (key a) (key b)

let add_edge edge graph =
  let same_edge a b =
    let same_owner =
      match a.owner, b.owner with
      | Temporary, Temporary -> true
      | Stored a, Stored b -> same_ident a b
      | Temporary, Stored _ | Stored _, Temporary -> false
    in
    let same_target =
      same_ident a.target.root b.target.root
      && Value.equal_ref_kind a.target.kind b.target.kind
    in
    same_owner && same_target
  in
  if List.exists graph ~f:(same_edge edge) then graph else edge :: graph
;;

let add_temporary target graph = add_edge { owner = Temporary; target } graph
let add_owned ~owner target graph = add_edge { owner = Stored owner; target } graph

let add_owned_all ~owner targets graph =
  List.fold targets ~init:graph ~f:(fun graph target -> add_owned ~owner target graph)
;;

let remove_owner owner graph =
  List.filter graph ~f:(fun edge ->
    match edge.owner with
    | Stored stored -> not (same_ident owner stored)
    | Temporary -> true)
;;

let owned_by owner graph =
  List.filter_map graph ~f:(fun edge ->
    match edge.owner with
    | Stored stored when same_ident owner stored -> Some edge.target
    | Stored _ | Temporary -> None)
;;

let active ?(ignore = []) root graph =
  let owner_ignored = function
    | Stored owner -> List.exists ignore ~f:(same_ident owner)
    | Temporary -> false
  in
  List.filter_map graph ~f:(fun edge ->
    if (not (owner_ignored edge.owner)) && same_ident root edge.target.root
    then Some edge.target
    else None)
;;

let merge a b = List.fold b ~init:a ~f:(fun graph edge -> add_edge edge graph)

let stored_edges graph =
  List.filter graph ~f:(function
    | { owner = Stored _; _ } -> true
    | { owner = Temporary; _ } -> false)
;;

let temporary_edges graph =
  List.filter graph ~f:(function
    | { owner = Temporary; _ } -> true
    | { owner = Stored _; _ } -> false)
;;

let merge_stored a b = merge (stored_edges a) (stored_edges b)

let keep_outer ~outer ~inner =
  let outer_owners =
    List.filter_map outer ~f:(function
      | { owner = Stored owner; _ } -> Some owner
      | { owner = Temporary; _ } -> None)
  in
  let kept_inner =
    List.filter inner ~f:(function
      | { owner = Stored owner; _ } -> List.exists outer_owners ~f:(same_ident owner)
      | { owner = Temporary; _ } -> false)
  in
  merge kept_inner (temporary_edges outer)
;;

let clear_temporaries = stored_edges
let temporaries graph = List.map (temporary_edges graph) ~f:(fun edge -> edge.target)

let set_temporaries graph targets =
  List.fold targets ~init:(clear_temporaries graph) ~f:(fun graph target ->
    add_temporary target graph)
;;
