(* Made by Atte Torri on Friday 6 September 2019 *)

let memory = Array.make 65536
let registers = Array.make 32
let pc = ref 0
let branch_flag = ref false

let op_code i = i lsr 26 (* 6 first bits reserved for opcode *)
let r_dest i = (i lsr 21) mod 32 (* 5 bits for each register *)
let r_arg1 i = (i lsr 16) mod 32
let r_arg2 i = (i lsr 11) mod 32
let imm i = i mod 2097152 (* 21 bits for immediate *)
                
let execute i =
  match op_code i with
  | 0 -> (* NOP *)
     ()
  | 1 -> (* EXIT *)
     exit 0
  | 2 -> (* LOAD *)
     registers.(r_dest i) <- memory.(registers.(r_arg1 i))
  | 3 -> (* STORE *)
     memory.(registers.(r_dest i)) <- registers.(r_arg1 i)
  | 4 -> (* PRINT *)
     Printf.printf (char_of_int registers.(r_arg1 i))
     
  | _ -> (* UNASSIGNED *)
     ()
       
let () =
  let file = Sys.argv.(1) in
  let f = open_in file in
  try
    while true do
      let instr = int_of_string (input_line f) in
      execute instr;
      if not (!branch_flag) then
        incr pc;
      branch_flag := false
    done
  with
  | End_of_file -> close_in f
