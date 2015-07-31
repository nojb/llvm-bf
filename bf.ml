open Llvm;;

ignore (Llvm_executionengine.initialize_native_target ())

let context = global_context ()
let prog = create_module context ""
let int8 = i8_type context
let int32 = i32_type context
let putchar = declare_function "putchar"
  (function_type (i32_type context) [|int8|]) prog
let getchar = declare_function "getchar"
  (function_type int8 [||]) prog
let memset = declare_function "memset"
  (function_type (pointer_type int8)
    [|pointer_type int8; int32; int32|]) prog

let compile b mem p inch =
  let rec compile_char p = function
  | '>' ->
      build_add p (const_int int32 1) "" b
  | '<' ->
      build_sub p (const_int int32 1) "" b
  | '+' ->
      let a = build_gep mem [|const_int int32 0; p|] "" b in
      ignore (build_store
        (build_add (build_load a "" b) (const_int int8 1) "" b) a b);
      p
  | '-' ->
      let a = build_gep mem [|const_int int32 0; p|] "" b in
      ignore (build_store
        (build_sub (build_load a "" b) (const_int int8 1) "" b) a b);
      p
  | '.' ->
      let a = build_gep mem [|const_int int32 0; p|] "" b in
      ignore (build_call putchar [|build_load a "" b|] "" b);
      p
  | ',' ->
      let a = build_gep mem [|const_int int32 0; p|] "" b in
      ignore (build_store (build_call getchar [||] "" b) a b);
      p
  | '[' ->
      let bb = insertion_block b in
      let f = block_parent bb in
      let body = append_block context "" f in
      let after = append_block context "" f in
      let a = build_gep mem [|const_int int32 0; p|] "" b in
      ignore (build_cond_br
        (build_icmp Icmp.Ne (build_load a "" b)
        (const_int int8 0) "" b) body after b);
      position_at_end body b;
      let body_start_p = build_phi [p, bb] "" b in
      let rec loop p =
        let c = input_char inch in
        match c with
        | ']' -> p
        | '>' | '<' | '+' | '-' | '.' | ',' | '[' -> loop (compile_char p c)
        | _ -> loop p
      in
      let body_p = loop body_start_p in
      (* body may have more than one basic block; get the last one *)
      let body_end = insertion_block b in
      add_incoming (body_p, body_end) body_start_p;
      let a = build_gep mem [|const_int int32 0; body_p|] "" b in
      ignore (build_cond_br
        (build_icmp Icmp.Ne (build_load a "" b)
        (const_int int8 0) "" b) body after b);
      position_at_end after b;
      build_phi [p, bb; body_p, body_end] "" b
  | ']' ->
      assert false
  | _ ->
      compile_char p (input_char inch)
  in compile_char p (input_char inch)

let memsize = 30000

let compile_chan inch =
  let main = define_function "main"
    (function_type (i32_type context) [||]) prog in
  let b = builder_at_end context (entry_block main) in
  let mem = build_alloca (array_type int8 memsize) "" b in
  ignore (build_call memset [|build_pointercast mem (pointer_type int8) "" b;
    const_int int32 0; const_int int32 memsize|] "" b);
  try let rec loop p =
    loop (compile b mem p inch)
  in loop (const_int int32 0)
  with End_of_file -> (ignore (build_ret (const_int int32 0) b); main)

(* let execute main =
  let eng = Llvm_executionengine.ExecutionEngine.create prog in
  ignore (Llvm_executionengine.ExecutionEngine.run_function main [||] eng) *)

let () =
  ignore (compile_chan stdin);
  (* execute (compile_chan stdin); *)
  Llvm.dump_module prog;
  Llvm.dispose_module prog
