open GoblintCil.Cil

let compile = Fun.id

let pp = Obj.magic ()

let write_out c_ast out =
  Format.fprintf out "%a" pp c_ast