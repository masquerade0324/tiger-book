type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list
     and exp = IdExp of id
	     | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp

val prog = 
  CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
    CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
              OpExp(NumExp 10, Times, IdExp"a"))),
    PrintStm[IdExp "b"]))

fun maxargs (CompoundStm (s1, s2)) = Int.max (maxargs s1, maxargs s2)
  | maxargs (AssignStm (_, e)) = maxargs' e
  | maxargs (PrintStm es) = length es
and maxargs' (OpExp (e1, _, e2)) = Int.max (maxargs' e1, maxargs' e2)
  | maxargs' (EseqExp (s, e)) = Int.max (maxargs s, maxargs' e)
  | maxargs' _ = 0
