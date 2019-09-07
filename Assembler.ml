(* Made by Atte Torri on Friday 6 September 2019 *)

let encode i =
  match List.hd i with
  | "NOP" -> 0
  | "EXIT" -> 1 lsl 26
  | _ -> 0

let () =
  let out = open_out_bin Sys.argv.(2) in
  let file = Sys.argv.(1) in
  let f = open_in file in
  try
    while true do
      let instr = Str.split (Str.regexp "[ \t]+") (input_line f) in
      let instr = List.filter (fun e -> e<>"") instr in
      output_binary_int out (encode instr)
    done
  with
  | End_of_file -> close_in f; close_out out
  
