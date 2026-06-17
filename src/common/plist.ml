open! Base

type 'a t = 'a list

let bounds_error i n = Printf.sprintf "index %d out of bounds (length %d)" i n

let get xs i =
  match List.nth xs i with
  | Some v -> Ok v
  | None -> Error (bounds_error i (List.length xs))
;;

let set xs i v =
  let n = List.length xs in
  if i >= 0 && i < n
  then Ok (List.mapi xs ~f:(fun j x -> if j = i then v else x))
  else Error (bounds_error i n)
;;

let push xs v = xs @ [ v ]

let insert xs i v =
  let n = List.length xs in
  if i >= 0 && i <= n
  then Ok (List.take xs i @ (v :: List.drop xs i))
  else Error (bounds_error i n)
;;

let remove xs i =
  let n = List.length xs in
  if i >= 0 && i < n
  then Ok (List.take xs i @ List.drop xs (i + 1))
  else Error (bounds_error i n)
;;
