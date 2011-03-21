open Llvm

let context = global_context ()
let prog = create_module context ""
let int8 = i8_type context
let int32 = i32_type context
let putchar = declare_function "putchar"
  (function_type (i32_type context) [|int8|]) prog
let getchar = declare_function "getchar"
  (function_type int8 [||]) prog

let bbs = Stack.create ()

let compile_char b mem pc = function
  | '>' ->
      ignore (build_store
        (build_add (build_load pc "" b) (const_int int32 1) "" b)
        pc b)
  | '<' ->
      ignore (build_store
        (build_sub (build_load pc "" b) (const_int int32 1) "" b)
        pc b)
  | '+' ->
      let a = build_gep mem [|const_int int32 0; build_load pc "" b|] "" b in
      ignore (build_store
        (build_add (build_load a "" b) (const_int int8 1) "" b) a b)
  | '-' ->
      let a = build_gep mem [|const_int int32 0; build_load pc "" b|] "" b in
      ignore (build_store
        (build_sub (build_load a "" b) (const_int int8 1) "" b) a b)
  | '.' ->
      let a = build_gep mem [|const_int int32 0; build_load pc "" b|] "" b in
      ignore (build_call putchar [|build_load a "" b|] "" b)
  | ',' ->
      let a = build_gep mem [|const_int int32 0; build_load pc "" b|] "" b in
      ignore (build_store
        (build_call getchar [||] "" b) a b)
  | '[' ->
      let f = block_parent (insertion_block b) in
      let test = append_block context "" f in
      let body = append_block context "" f in
      let after = append_block context "" f in
      Stack.push after bbs;
      Stack.push test bbs;
      ignore (build_br test b);
      position_at_end test b;
      let a = build_gep mem [|const_int int32 0; build_load pc "" b|] "" b in
      ignore (build_cond_br
        (build_icmp Icmp.Ne (build_load a "" b)
        (const_int int8 0) "" b) body after b);
      position_at_end body b
  | ']' ->
      ignore (build_br (Stack.pop bbs) b);
      position_at_end (Stack.pop bbs) b
  | _ -> ()

let memsize = 30000

let compile inch =
  let main = define_function "main"
    (function_type (i32_type context) [||]) prog in
  let b = builder_at_end context (entry_block main) in
  let mem = build_alloca (array_type int8 memsize) "" b in
  let pc = build_alloca int32 "" b in
  ignore (build_store (const_int int32 0) pc b);
  try while true do
    compile_char b mem pc (input_char inch)
  done with End_of_file -> ignore (build_ret (const_int (i32_type context) 0) b)

let main () =
  compile stdin;
  Llvm.dump_module prog;
  Llvm.dispose_module prog

let () =
  main ()
