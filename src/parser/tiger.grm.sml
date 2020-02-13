functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure A = Absyn

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\000\000\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\092\000\007\000\092\000\009\000\092\000\015\000\027\000\
\\016\000\026\000\017\000\025\000\018\000\024\000\026\000\092\000\
\\027\000\092\000\031\000\092\000\032\000\092\000\035\000\092\000\
\\036\000\092\000\000\000\
\\001\000\001\000\093\000\007\000\093\000\009\000\093\000\015\000\027\000\
\\016\000\026\000\017\000\025\000\018\000\024\000\026\000\093\000\
\\027\000\093\000\031\000\093\000\032\000\093\000\035\000\093\000\
\\036\000\093\000\000\000\
\\001\000\001\000\094\000\007\000\094\000\009\000\094\000\015\000\027\000\
\\016\000\026\000\017\000\025\000\018\000\024\000\026\000\094\000\
\\027\000\094\000\031\000\094\000\032\000\094\000\035\000\094\000\
\\036\000\094\000\000\000\
\\001\000\001\000\095\000\007\000\095\000\009\000\095\000\015\000\027\000\
\\016\000\026\000\017\000\025\000\018\000\024\000\026\000\095\000\
\\027\000\095\000\031\000\095\000\032\000\095\000\035\000\095\000\
\\036\000\095\000\000\000\
\\001\000\001\000\096\000\007\000\096\000\009\000\096\000\015\000\027\000\
\\016\000\026\000\017\000\025\000\018\000\024\000\026\000\096\000\
\\027\000\096\000\031\000\096\000\032\000\096\000\035\000\096\000\
\\036\000\096\000\000\000\
\\001\000\001\000\097\000\007\000\097\000\009\000\097\000\015\000\027\000\
\\016\000\026\000\017\000\025\000\018\000\024\000\026\000\097\000\
\\027\000\097\000\031\000\097\000\032\000\097\000\035\000\097\000\
\\036\000\097\000\000\000\
\\001\000\002\000\015\000\003\000\014\000\004\000\013\000\008\000\012\000\
\\016\000\011\000\030\000\010\000\033\000\009\000\034\000\008\000\
\\037\000\007\000\041\000\006\000\042\000\005\000\000\000\
\\001\000\002\000\029\000\000\000\
\\001\000\008\000\054\000\000\000\
\\001\000\009\000\051\000\015\000\027\000\016\000\026\000\017\000\025\000\
\\018\000\024\000\019\000\023\000\020\000\022\000\021\000\021\000\
\\022\000\020\000\023\000\019\000\024\000\018\000\026\000\017\000\
\\027\000\016\000\000\000\
\\001\000\009\000\063\000\000\000\
\\001\000\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\026\000\017\000\027\000\016\000\
\\031\000\050\000\000\000\
\\001\000\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\026\000\017\000\027\000\016\000\
\\035\000\061\000\000\000\
\\001\000\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\026\000\017\000\027\000\016\000\
\\036\000\049\000\000\000\
\\001\000\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\026\000\017\000\027\000\016\000\
\\036\000\068\000\000\000\
\\001\000\028\000\048\000\000\000\
\\001\000\038\000\047\000\000\000\
\\001\000\039\000\058\000\000\000\
\\071\000\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\026\000\017\000\027\000\016\000\000\000\
\\072\000\000\000\
\\073\000\000\000\
\\074\000\007\000\064\000\015\000\027\000\016\000\026\000\017\000\025\000\
\\018\000\024\000\019\000\023\000\020\000\022\000\021\000\021\000\
\\022\000\020\000\023\000\019\000\024\000\018\000\026\000\017\000\
\\027\000\016\000\000\000\
\\075\000\000\000\
\\076\000\028\000\034\000\000\000\
\\077\000\000\000\
\\078\000\000\000\
\\079\000\000\000\
\\080\000\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\026\000\017\000\027\000\016\000\000\000\
\\081\000\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\026\000\017\000\027\000\016\000\000\000\
\\082\000\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\026\000\017\000\027\000\016\000\000\000\
\\083\000\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\026\000\017\000\027\000\016\000\000\000\
\\084\000\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\026\000\017\000\027\000\016\000\000\000\
\\085\000\000\000\
\\086\000\000\000\
\\087\000\017\000\025\000\018\000\024\000\000\000\
\\088\000\017\000\025\000\018\000\024\000\000\000\
\\089\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\\098\000\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\000\000\
\\099\000\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\026\000\017\000\000\000\
\\100\000\000\000\
\"
val actionRowNumbers =
"\008\000\028\000\020\000\027\000\
\\035\000\000\000\009\000\008\000\
\\008\000\008\000\008\000\024\000\
\\043\000\025\000\008\000\008\000\
\\008\000\008\000\008\000\008\000\
\\008\000\008\000\008\000\008\000\
\\008\000\008\000\018\000\017\000\
\\015\000\013\000\040\000\011\000\
\\008\000\042\000\041\000\006\000\
\\004\000\007\000\005\000\003\000\
\\002\000\039\000\038\000\037\000\
\\036\000\010\000\008\000\008\000\
\\008\000\026\000\031\000\019\000\
\\008\000\014\000\032\000\030\000\
\\034\000\012\000\023\000\008\000\
\\008\000\021\000\008\000\016\000\
\\029\000\022\000\008\000\033\000\
\\001\000"
val gotoT =
"\
\\001\000\002\000\002\000\068\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\026\000\000\000\
\\000\000\
\\001\000\028\000\006\000\001\000\000\000\
\\001\000\029\000\006\000\001\000\000\000\
\\001\000\030\000\006\000\001\000\000\000\
\\001\000\031\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\033\000\006\000\001\000\000\000\
\\001\000\034\000\006\000\001\000\000\000\
\\001\000\035\000\006\000\001\000\000\000\
\\001\000\036\000\006\000\001\000\000\000\
\\001\000\037\000\006\000\001\000\000\000\
\\001\000\038\000\006\000\001\000\000\000\
\\001\000\039\000\006\000\001\000\000\000\
\\001\000\040\000\006\000\001\000\000\000\
\\001\000\041\000\006\000\001\000\000\000\
\\001\000\042\000\006\000\001\000\000\000\
\\001\000\043\000\006\000\001\000\000\000\
\\001\000\044\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\050\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\051\000\000\000\
\\001\000\053\000\006\000\001\000\000\000\
\\001\000\054\000\006\000\001\000\000\000\
\\001\000\055\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\058\000\004\000\057\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\063\000\006\000\001\000\000\000\
\\001\000\064\000\006\000\001\000\000\000\
\\000\000\
\\001\000\058\000\004\000\065\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\067\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 69
val numrules = 30
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string)
end
type svalue = MlyValue.svalue
type result = unit
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 32) => true | (T 33) => true | (T 34) => true | (T 40) => true
 | (T 36) => true | (T 37) => true | (T 38) => true | (T 42) => true
 | (T 43) => true | (T 44) => true | (T 28) => true | (T 29) => true
 | (T 30) => true | (T 31) => true | (T 35) => true | (T 39) => true
 | (T 41) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 30))::
(nil
,nil
 $$ (T 31))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "DOT"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "TIMES"
  | (T 17) => "DIVIDE"
  | (T 18) => "EQ"
  | (T 19) => "NEQ"
  | (T 20) => "LT"
  | (T 21) => "LE"
  | (T 22) => "GT"
  | (T 23) => "GE"
  | (T 24) => "UMINUS"
  | (T 25) => "AND"
  | (T 26) => "OR"
  | (T 27) => "ASSIGN"
  | (T 28) => "ARRAY"
  | (T 29) => "IF"
  | (T 30) => "THEN"
  | (T 31) => "ELSE"
  | (T 32) => "WHILE"
  | (T 33) => "FOR"
  | (T 34) => "TO"
  | (T 35) => "DO"
  | (T 36) => "LET"
  | (T 37) => "IN"
  | (T 38) => "END"
  | (T 39) => "OF"
  | (T 40) => "BREAK"
  | (T 41) => "NIL"
  | (T 42) => "FUNCTION"
  | (T 43) => "VAR"
  | (T 44) => "TYPE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38)
 $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31)
 $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (
exp as exp1) = exp1 ()
 in (exp)
end; ()))
 in ( LrTable.NT 1, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID 
exp_list1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  exp_list1 = 
exp_list1 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.ntVOID exp_list1, _, exp_list1right)) :: _
 :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp_list1 = exp_list1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, exp1left, exp_list1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, exp1left, exp1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.STRING STRING1, (STRINGleft as STRING1left),
 STRING1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  (STRING as STRING1) = STRING1 ()
 in (A.StringExp(STRING, STRINGleft))
end; ()))
 in ( LrTable.NT 0, ( result, STRING1left, STRING1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, ID1left, ID1right), rest671)
end
|  ( 6, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID exp1,
 _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.ntVOID (fn _ => ( let val  (exp as exp1) = exp1 ()
 in (exp)
end; ()))
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 7, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => (A.NilExp))
 in ( LrTable.NT 0, ( result, NIL1left, NIL1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.ntVOID mathExp1, mathExp1left, mathExp1right
)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let
 val  (mathExp as mathExp1) = mathExp1 ()
 in (mathExp)
end; ()))
 in ( LrTable.NT 0, ( result, mathExp1left, mathExp1right), rest671)

end
|  ( 9, ( ( _, ( MlyValue.ntVOID exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp2, _, _)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _
)) :: ( _, ( _, (IFleft as IF1left), _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (
A.IfExp({test = exp1, then' = exp2, else' = SOME(exp3), pos = IFleft})
)
end; ()))
 in ( LrTable.NT 0, ( result, IF1left, exp3right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, _, _)) :: ( _, ( _, (IFleft as IF1left), _)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.IfExp({test = exp1, then' = exp2, else' = NONE, pos = IFleft}))

end; ()))
 in ( LrTable.NT 0, ( result, IF1left, exp2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, ID1left, exp1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, _, _)) :: ( _, ( _, (WHILEleft as WHILE1left), _
)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.WhileExp({test = exp1, body = exp2, pos = WHILEleft}))
end; ())
)
 in ( LrTable.NT 0, ( result, WHILE1left, exp2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.ntVOID exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp2, _, _)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _
)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FORleft as 
FOR1left), _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (
A.ForExp({var = Symbol.symbol(ID), escape = ref true, lo = exp1, hi = exp2, body = exp3, pos = FORleft})
)
end; ()))
 in ( LrTable.NT 0, ( result, FOR1left, exp3right), rest671)
end
|  ( 14, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.ntVOID exp_seq1
, _, _)) :: _ :: ( _, ( MlyValue.ntVOID decs1, _, _)) :: ( _, ( _, 
LET1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  decs1 = decs1 ()
 val  exp_seq1 = exp_seq1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 15, ( ( _, ( _, (BREAKleft as BREAK1left), BREAK1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => (
A.BreakExp(BREAKleft)))
 in ( LrTable.NT 0, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp({left = exp1, oper = A.PlusOp, right = exp2, pos = exp1left}))

end; ()))
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp({left = exp1, oper = A.MinusOp, right = exp2, pos = exp1left})
)
end; ()))
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp({left = exp1, oper = A.TimesOp, right = exp2, pos = exp1left})
)
end; ()))
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp({left = exp1, oper = A.DivideOp, right = exp2, pos = exp1left})
)
end; ()))
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.ntVOID exp1, expleft, exp1right)) :: ( _, (
 _, MINUS1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  (exp as exp1) = exp1 ()
 in (
A.OpExp({A.IntExp(0), oper = A.MinusOp, right = exp, pos = expleft))

end; ()))
 in ( LrTable.NT 5, ( result, MINUS1left, exp1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp({left = exp1, oper = A.EqOp, right = exp2, pos = exp1left}))

end; ()))
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp({left = exp1, oper = A.NeqOp, right = exp2, pos = exp1left}))

end; ()))
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp({left = exp1, oper = A.GtOp, right = exp2, pos = exp1left}))

end; ()))
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp({left = exp1, oper = A.LtOp, right = exp2, pos = exp1left}))

end; ()))
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp({left = exp1, oper = A.GeOp, right = exp2, pos = exp1left}))

end; ()))
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp({left = exp1, oper = A.LeOp, right = exp2, pos = exp1left}))

end; ()))
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.IfExp({test = exp1, then' = exp2, else' = SOME(A.IntExp(0)), pos = exp1left})
)
end; ()))
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.IfExp({test = exp1, then' = A.IntExp(1), else' = SOME(exp2), pos = exp1left})
)
end; ()))
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (INT as 
INT1) = INT1 ()
 in (A.IntExp(INT))
end; ()))
 in ( LrTable.NT 5, ( result, INT1left, INT1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ntVOID x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
end
end
