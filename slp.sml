type id = string

type table = (id * int) list

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list
     and exp = IdExp of id
	     | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp

exception Exception

val prog = 
  CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
    CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
              OpExp(NumExp 10, Times, IdExp"a"))),
    PrintStm[IdExp "b"]))

fun maxargs (CompoundStm (s1, s2)) = Int.max (maxargs s1, maxargs s2)
  | maxargs (AssignStm (_, e))     = maxargs' e
  | maxargs (PrintStm es)          = length es
and maxargs' (OpExp (e1, _, e2)) = Int.max (maxargs' e1, maxargs' e2)
  | maxargs' (EseqExp (s, e))    = Int.max (maxargs s, maxargs' e)
  | maxargs' _                   = 0

fun update (t, x, i) = (x, i)::t

fun lookup ([] : table, _) = raise Exception
  | lookup ((x, i)::t, y)  = if x = y then i else lookup (t, y)

fun interpStm (CompoundStm (s1, s2), t) = interpStm (s2, interpStm (s1, t))
  | interpStm (AssignStm (x, e), t) =
    let
      val (i, t1) = interpExp (e, t)
    in
      update (t1, x, i)
    end
  | interpStm (PrintStm es, t) =
    case es of
        []     => raise Exception
      | [e]    => let val (i, t1) = interpExp (e, t)
                  in  print (Int.toString i ^ "\n");
                      t1
                  end
      | e::es' => let val (i, t1) = interpExp (e, t)
                  in  print (Int.toString i ^ " ");
                      interpStm (PrintStm es', t1)
                  end
and interpExp (IdExp x, t)  = (lookup (t, x), t)
  | interpExp (NumExp i, t) = (i, t)
  | interpExp (OpExp (e1, bop, e2), t) =
    let
      val (i1, t1) = interpExp (e1, t)
      val (i2, t2) = interpExp (e2, t1)
    in
      case bop of
          Plus  => (i1  +  i2, t2)
        | Minus => (i1  -  i2, t2)
        | Times => (i1  *  i2, t2)
        | Div   => (i1 div i2, t2)
    end
  | interpExp (EseqExp (s, e), t) = interpExp (e, interpStm (s, t))

fun interp s = ignore (interpStm (s, []))
