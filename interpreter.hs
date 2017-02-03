import Data.Char

data Op = Plus | Minus | Mult | Div | Mod deriving(Eq , Show)
data Exp = Binary_op Op Exp Exp | Variable Char | Number Integer  deriving(Eq,Show)
data Bexp = Logical_op Bop Bexp Bexp | Not Bexp | Bool_val Bool deriving(Eq ,Show)
data Bop = Or | And | Xor deriving(Eq ,Show)
data Ctype = Assign Char Exp | While Bexp [Ctype] | If Bexp [Ctype] [Ctype]  deriving(Eq, Show)

-- Evaluation:
-- Assignmnt statment = Assign variable_name expression

statmnt = Assign ('x') (Binary_op Plus (Number 5) (Binary_op Mult (Number 6) (Number 7)))
state = [('x',0),('y',0),('z',0)]

evaluate (Number y) = y
evaluate (Binary_op x y z) = result x (evaluate y) (evaluate z)
result (Plus) x y = x + y
result (Minus) x y = x - y
result (Mult) x y = x * y
result (Div) x y= div x y

--Boolean expression evaluator:

statmnt1 =  Logical_op And (Bool_val False) (Not (Bool_val False))              ---(Logical_op (Or) (Bool_val True) (Bool_val False))

eval_bool (Bool_val y) = y
eval_bool (Not x) = not (eval_bool x)
eval_bool (Logical_op x y z) = result_bool x (eval_bool y) (eval_bool z)

result_bool (Or) x y =  x || y
result_bool (And) x y = x && y
result_bool (Xor) x y = if ((x || y) == (x && y)) then False else True


-- while statment = Bexp [statement] :
list_statemnt = [Assign ('y') (Binary_op Plus (Number 5) (Binary_op Mult (Number 6) (Number 7))) , Assign ('x') (Binary_op Plus (Number 5) (Binary_op Mult (Number 6) (Number 7)))
  ]
list_statemnt2 = [Assign ('z') (Binary_op Plus (Number 5) (Binary_op Mult (Number 6) (Number 7))) , Assign ('x') (Binary_op Plus (Number 5) (Binary_op Mult (Number 6) (Number 7)))
  ]


interpreter (Assign x exp) state = [if (z == x) then (z,(evaluate exp)) else (z,y ) |(z,y) <- state] 
interpreter (While exp list_statemnt) state = if (eval_bool exp) then update_state list_statemnt state else state
interpreter (If exp list_statemnt list_statemnt2) state = if (eval_bool exp) then update_state list_statemnt state else update_state list_statemnt2 state 


update_state ([]) state = state
update_state (x:list_statemnt) state = update_state list_statemnt (interpreter x state)


