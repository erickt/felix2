open Format

(* A simple hashtable to store profile times of arbitrary blocks of code. *)
let statistics = Hashtbl.create 100

(** A wrapper function to profile a function call. *)
let call name f =
  let start_time = Unix.gettimeofday () in
  let result = f () in
  let end_time = Unix.gettimeofday () in
  let old_times =
    try Hashtbl.find statistics name
    with Not_found -> []
  in
  Hashtbl.replace statistics name ((end_time -. start_time) :: old_times);

  result


(** Print out our gathered statistics. *)
let print ppf =
  let keys, key_length = Hashtbl.fold
    (fun k _ (ks, key_length) -> k :: ks, max key_length (String.length k))
    statistics
    ([], 0)
  in
  let keys = List.sort compare keys in

  let sum = List.fold_left (+.) 0. in
  let mean xs = (sum xs) /. (float_of_int (List.length xs)) in
  let std xs =
    let m = mean xs in
    sqrt (mean (List.map (fun x -> let d = x -. m in d *. d) xs))
  in

  List.iter begin fun key ->
    let times = Hashtbl.find statistics key in

    fprintf ppf "%*s: count=%d total=%f mean=%f std=%f@."
      key_length key
      (List.length times)
      (sum times)
      (mean times)
      (std times)
  end keys
