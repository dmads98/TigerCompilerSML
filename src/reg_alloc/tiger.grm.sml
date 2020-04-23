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

fun updateTypeDec (t, (A.TypeDec d)::ls) = A.TypeDec(t :: d):: ls
  | updateTypeDec (t, d::ls) = A.TypeDec([])::[]
  | updateTypeDec (t, []) = A.TypeDec([t])::[];

fun updateFuncDec (t, (A.FunctionDec d)::ls) = A.FunctionDec(t :: d):: ls
  | updateFuncDec (t, d::ls) = A.FunctionDec([])::[]
  | updateFuncDec (t, []) = A.FunctionDec([t])::[];



end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\224\000\005\000\224\000\007\000\224\000\009\000\224\000\
\\011\000\224\000\013\000\224\000\015\000\033\000\016\000\032\000\
\\017\000\031\000\018\000\030\000\026\000\224\000\027\000\224\000\
\\031\000\224\000\032\000\224\000\035\000\224\000\036\000\224\000\
\\038\000\224\000\039\000\224\000\043\000\224\000\044\000\224\000\
\\045\000\224\000\000\000\
\\001\000\001\000\225\000\005\000\225\000\007\000\225\000\009\000\225\000\
\\011\000\225\000\013\000\225\000\015\000\033\000\016\000\032\000\
\\017\000\031\000\018\000\030\000\026\000\225\000\027\000\225\000\
\\031\000\225\000\032\000\225\000\035\000\225\000\036\000\225\000\
\\038\000\225\000\039\000\225\000\043\000\225\000\044\000\225\000\
\\045\000\225\000\000\000\
\\001\000\001\000\226\000\005\000\226\000\007\000\226\000\009\000\226\000\
\\011\000\226\000\013\000\226\000\015\000\033\000\016\000\032\000\
\\017\000\031\000\018\000\030\000\026\000\226\000\027\000\226\000\
\\031\000\226\000\032\000\226\000\035\000\226\000\036\000\226\000\
\\038\000\226\000\039\000\226\000\043\000\226\000\044\000\226\000\
\\045\000\226\000\000\000\
\\001\000\001\000\227\000\005\000\227\000\007\000\227\000\009\000\227\000\
\\011\000\227\000\013\000\227\000\015\000\033\000\016\000\032\000\
\\017\000\031\000\018\000\030\000\026\000\227\000\027\000\227\000\
\\031\000\227\000\032\000\227\000\035\000\227\000\036\000\227\000\
\\038\000\227\000\039\000\227\000\043\000\227\000\044\000\227\000\
\\045\000\227\000\000\000\
\\001\000\001\000\228\000\005\000\228\000\007\000\228\000\009\000\228\000\
\\011\000\228\000\013\000\228\000\015\000\033\000\016\000\032\000\
\\017\000\031\000\018\000\030\000\026\000\228\000\027\000\228\000\
\\031\000\228\000\032\000\228\000\035\000\228\000\036\000\228\000\
\\038\000\228\000\039\000\228\000\043\000\228\000\044\000\228\000\
\\045\000\228\000\000\000\
\\001\000\001\000\229\000\005\000\229\000\007\000\229\000\009\000\229\000\
\\011\000\229\000\013\000\229\000\015\000\033\000\016\000\032\000\
\\017\000\031\000\018\000\030\000\026\000\229\000\027\000\229\000\
\\031\000\229\000\032\000\229\000\035\000\229\000\036\000\229\000\
\\038\000\229\000\039\000\229\000\043\000\229\000\044\000\229\000\
\\045\000\229\000\000\000\
\\001\000\002\000\018\000\003\000\017\000\004\000\016\000\008\000\015\000\
\\009\000\046\000\016\000\014\000\030\000\013\000\033\000\012\000\
\\034\000\011\000\037\000\010\000\041\000\009\000\042\000\008\000\000\000\
\\001\000\002\000\018\000\003\000\017\000\004\000\016\000\008\000\015\000\
\\016\000\014\000\030\000\013\000\033\000\012\000\034\000\011\000\
\\037\000\010\000\041\000\009\000\042\000\008\000\000\000\
\\001\000\002\000\041\000\000\000\
\\001\000\002\000\051\000\000\000\
\\001\000\002\000\073\000\000\000\
\\001\000\002\000\074\000\000\000\
\\001\000\002\000\075\000\000\000\
\\001\000\002\000\113\000\012\000\112\000\029\000\111\000\000\000\
\\001\000\002\000\115\000\000\000\
\\001\000\002\000\140\000\000\000\
\\001\000\002\000\145\000\000\000\
\\001\000\002\000\146\000\000\000\
\\001\000\002\000\150\000\000\000\
\\001\000\002\000\155\000\000\000\
\\001\000\005\000\131\000\009\000\130\000\000\000\
\\001\000\005\000\131\000\013\000\141\000\000\000\
\\001\000\006\000\095\000\028\000\094\000\000\000\
\\001\000\006\000\132\000\000\000\
\\001\000\006\000\144\000\019\000\143\000\000\000\
\\001\000\006\000\151\000\000\000\
\\001\000\007\000\080\000\009\000\079\000\015\000\033\000\016\000\032\000\
\\017\000\031\000\018\000\030\000\019\000\029\000\020\000\028\000\
\\021\000\027\000\022\000\026\000\023\000\025\000\024\000\024\000\
\\026\000\023\000\027\000\022\000\000\000\
\\001\000\008\000\096\000\000\000\
\\001\000\009\000\104\000\000\000\
\\001\000\009\000\135\000\000\000\
\\001\000\011\000\086\000\015\000\033\000\016\000\032\000\017\000\031\000\
\\018\000\030\000\019\000\029\000\020\000\028\000\021\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\026\000\023\000\
\\027\000\022\000\000\000\
\\001\000\011\000\103\000\015\000\033\000\016\000\032\000\017\000\031\000\
\\018\000\030\000\019\000\029\000\020\000\028\000\021\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\026\000\023\000\
\\027\000\022\000\000\000\
\\001\000\013\000\101\000\000\000\
\\001\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\026\000\023\000\027\000\022\000\
\\031\000\078\000\000\000\
\\001\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\026\000\023\000\027\000\022\000\
\\035\000\118\000\000\000\
\\001\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\026\000\023\000\027\000\022\000\
\\036\000\077\000\000\000\
\\001\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\026\000\023\000\027\000\022\000\
\\036\000\147\000\000\000\
\\001\000\019\000\093\000\000\000\
\\001\000\019\000\102\000\000\000\
\\001\000\019\000\154\000\000\000\
\\001\000\028\000\076\000\000\000\
\\001\000\028\000\129\000\000\000\
\\001\000\038\000\065\000\000\000\
\\001\000\039\000\107\000\000\000\
\\001\000\040\000\126\000\000\000\
\\159\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\026\000\023\000\027\000\022\000\000\000\
\\160\000\000\000\
\\161\000\007\000\109\000\015\000\033\000\016\000\032\000\017\000\031\000\
\\018\000\030\000\019\000\029\000\020\000\028\000\021\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\026\000\023\000\
\\027\000\022\000\000\000\
\\162\000\005\000\136\000\015\000\033\000\016\000\032\000\017\000\031\000\
\\018\000\030\000\019\000\029\000\020\000\028\000\021\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\026\000\023\000\
\\027\000\022\000\000\000\
\\163\000\000\000\
\\164\000\002\000\082\000\000\000\
\\165\000\000\000\
\\166\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\026\000\023\000\027\000\022\000\000\000\
\\167\000\000\000\
\\168\000\002\000\018\000\003\000\017\000\004\000\016\000\008\000\015\000\
\\016\000\014\000\030\000\013\000\033\000\012\000\034\000\011\000\
\\037\000\010\000\041\000\009\000\042\000\008\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\005\000\106\000\015\000\033\000\016\000\032\000\017\000\031\000\
\\018\000\030\000\019\000\029\000\020\000\028\000\021\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\026\000\023\000\
\\027\000\022\000\000\000\
\\172\000\008\000\049\000\010\000\048\000\012\000\047\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\040\000\122\000\000\000\
\\176\000\000\000\
\\177\000\010\000\021\000\014\000\020\000\028\000\019\000\000\000\
\\178\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\026\000\023\000\027\000\022\000\000\000\
\\179\000\000\000\
\\180\000\000\000\
\\181\000\000\000\
\\182\000\000\000\
\\183\000\000\000\
\\184\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\026\000\023\000\027\000\022\000\000\000\
\\185\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\026\000\023\000\027\000\022\000\
\\032\000\119\000\000\000\
\\186\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\026\000\023\000\027\000\022\000\000\000\
\\187\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\026\000\023\000\027\000\022\000\000\000\
\\188\000\000\000\
\\189\000\000\000\
\\190\000\000\000\
\\191\000\000\000\
\\192\000\000\000\
\\193\000\000\000\
\\194\000\000\000\
\\195\000\043\000\040\000\044\000\039\000\045\000\038\000\000\000\
\\196\000\000\000\
\\197\000\000\000\
\\198\000\043\000\040\000\044\000\039\000\000\000\
\\199\000\000\000\
\\200\000\000\000\
\\201\000\044\000\039\000\045\000\038\000\000\000\
\\202\000\000\000\
\\203\000\002\000\018\000\003\000\017\000\004\000\016\000\008\000\015\000\
\\016\000\014\000\030\000\013\000\033\000\012\000\034\000\011\000\
\\037\000\010\000\041\000\009\000\042\000\008\000\000\000\
\\204\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\026\000\023\000\027\000\022\000\
\\043\000\040\000\000\000\
\\205\000\000\000\
\\206\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\026\000\023\000\027\000\022\000\
\\043\000\040\000\000\000\
\\207\000\000\000\
\\208\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\026\000\023\000\027\000\022\000\000\000\
\\209\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\026\000\023\000\027\000\022\000\000\000\
\\210\000\045\000\038\000\000\000\
\\211\000\000\000\
\\212\000\000\000\
\\213\000\000\000\
\\214\000\000\000\
\\215\000\000\000\
\\216\000\002\000\117\000\000\000\
\\216\000\002\000\117\000\013\000\128\000\000\000\
\\217\000\000\000\
\\218\000\000\000\
\\219\000\017\000\031\000\018\000\030\000\000\000\
\\220\000\017\000\031\000\018\000\030\000\000\000\
\\221\000\000\000\
\\222\000\000\000\
\\223\000\000\000\
\\230\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\000\000\
\\231\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\026\000\023\000\000\000\
\\232\000\000\000\
\"
val actionRowNumbers =
"\008\000\076\000\064\000\078\000\
\\070\000\046\000\069\000\075\000\
\\082\000\009\000\008\000\008\000\
\\008\000\007\000\063\000\114\000\
\\059\000\008\000\010\000\008\000\
\\008\000\008\000\008\000\008\000\
\\008\000\008\000\008\000\008\000\
\\008\000\008\000\008\000\008\000\
\\043\000\082\000\088\000\085\000\
\\011\000\012\000\013\000\041\000\
\\036\000\034\000\111\000\027\000\
\\067\000\051\000\008\000\055\000\
\\065\000\061\000\031\000\113\000\
\\112\000\005\000\003\000\006\000\
\\004\000\002\000\001\000\110\000\
\\109\000\108\000\107\000\090\000\
\\081\000\079\000\082\000\085\000\
\\080\000\082\000\088\000\038\000\
\\023\000\028\000\008\000\008\000\
\\008\000\066\000\008\000\033\000\
\\039\000\032\000\029\000\058\000\
\\060\000\044\000\048\000\086\000\
\\087\000\083\000\084\000\014\000\
\\008\000\015\000\103\000\035\000\
\\073\000\072\000\048\000\052\000\
\\008\000\062\000\054\000\056\000\
\\008\000\077\000\089\000\008\000\
\\097\000\045\000\104\000\100\000\
\\096\000\042\000\021\000\024\000\
\\008\000\008\000\030\000\049\000\
\\008\000\058\000\048\000\098\000\
\\016\000\022\000\101\000\008\000\
\\025\000\017\000\018\000\037\000\
\\071\000\068\000\051\000\053\000\
\\057\000\047\000\099\000\102\000\
\\095\000\008\000\019\000\026\000\
\\105\000\008\000\050\000\093\000\
\\040\000\020\000\074\000\094\000\
\\008\000\106\000\091\000\092\000\
\\000\000"
val gotoT =
"\
\\001\000\005\000\002\000\156\000\004\000\004\000\005\000\003\000\
\\014\000\002\000\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\035\000\012\000\034\000\013\000\033\000\016\000\032\000\000\000\
\\000\000\
\\001\000\040\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\001\000\041\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\001\000\042\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\001\000\043\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\048\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\000\000\
\\001\000\050\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\001\000\051\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\001\000\052\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\001\000\053\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\001\000\054\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\001\000\055\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\001\000\056\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\001\000\057\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\001\000\058\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\001\000\059\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\001\000\060\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\001\000\061\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\001\000\062\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\000\000\
\\011\000\035\000\012\000\034\000\013\000\033\000\016\000\064\000\000\000\
\\011\000\067\000\013\000\066\000\019\000\065\000\000\000\
\\012\000\070\000\013\000\069\000\018\000\068\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\079\000\000\000\
\\001\000\081\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\001\000\083\000\004\000\004\000\005\000\003\000\006\000\082\000\
\\014\000\002\000\015\000\001\000\000\000\
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
\\000\000\
\\000\000\
\\000\000\
\\001\000\086\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\020\000\085\000\000\000\
\\000\000\
\\000\000\
\\011\000\035\000\012\000\034\000\013\000\033\000\016\000\087\000\000\000\
\\012\000\070\000\013\000\069\000\018\000\088\000\000\000\
\\000\000\
\\011\000\035\000\012\000\034\000\013\000\033\000\016\000\089\000\000\000\
\\011\000\067\000\013\000\066\000\019\000\090\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\095\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\001\000\096\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\001\000\097\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\000\000\
\\001\000\098\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\103\000\000\000\
\\000\000\
\\000\000\
\\003\000\106\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\108\000\000\000\
\\001\000\112\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\000\000\
\\010\000\114\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\118\000\000\000\
\\000\000\
\\001\000\119\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\121\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\122\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\011\000\123\000\000\000\
\\000\000\
\\010\000\125\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\131\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\001\000\132\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\135\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\007\000\136\000\000\000\
\\003\000\137\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\140\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\146\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\147\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\150\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\000\000\
\\012\000\151\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\154\000\004\000\004\000\005\000\003\000\014\000\002\000\
\\015\000\001\000\000\000\
\\000\000\
\\012\000\155\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 157
val numrules = 74
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
 | ID of unit ->  (string) | let_body of unit ->  (A.exp)
 | nfuncdec of unit ->  (A.dec list)
 | ntypedec of unit ->  (A.dec list) | ty of unit ->  (A.ty)
 | decList of unit ->  (A.dec list)
 | structBuilder of unit ->  (A.exp) | l_value of unit ->  (A.var)
 | vardec of unit ->  (A.dec) | funcdec of unit ->  (A.dec list)
 | typedec of unit ->  (A.dec list)
 | type_fields of unit ->  (A.field list)
 | record_args of unit ->  ( ( A.symbol * A.exp * A.pos )  list)
 | fun_args_seq of unit ->  (A.exp list)
 | func_args_seq of unit ->  (A.exp list)
 | func_args of unit ->  (A.exp list) | func_exp of unit ->  (A.exp)
 | mathExp of unit ->  (A.exp)
 | exp_seq of unit ->  ( ( A.exp * A.pos )  list)
 | program of unit ->  (A.exp) | exp of unit ->  (A.exp)
end
type svalue = MlyValue.svalue
type result = A.exp
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
of  ( 0, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)
) => let val  result = MlyValue.program (fn _ => let val  (exp as exp1
) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 1, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.exp_seq exp_seq1, _, exp_seq1right)) :: ( _,
 ( MlyValue.exp exp1, expleft, _)) :: ( _, ( _, SEMICOLON1left, _)) ::
 rest671)) => let val  result = MlyValue.exp_seq (fn _ => let val  (
exp as exp1) = exp1 ()
 val  (exp_seq as exp_seq1) = exp_seq1 ()
 in ((exp, expleft) :: exp_seq)
end)
 in ( LrTable.NT 2, ( result, SEMICOLON1left, exp_seq1right), rest671)

end
|  ( 2, ( rest671)) => let val  result = MlyValue.exp_seq (fn _ => ([]
))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let val  
result = MlyValue.record_args (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in ([(Symbol.symbol(ID), exp, IDleft)])
end)
 in ( LrTable.NT 8, ( result, ID1left, exp1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.record_args record_args1, _, 
record_args1right)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: (
 _, ( MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let
 val  result = MlyValue.record_args (fn _ => let val  (ID as ID1) = 
ID1 ()
 val  (exp as exp1) = exp1 ()
 val  (record_args as record_args1) = record_args1 ()
 in ((Symbol.symbol(ID), exp, IDleft) :: record_args)
end)
 in ( LrTable.NT 8, ( result, ID1left, record_args1right), rest671)

end
|  ( 5, ( rest671)) => let val  result = MlyValue.record_args (fn _ =>
 ([]))
 in ( LrTable.NT 8, ( result, defaultPos, defaultPos), rest671)
end
|  ( 6, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.record_args 
record_args1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as 
ID1left), _)) :: rest671)) => let val  result = MlyValue.structBuilder
 (fn _ => let val  (ID as ID1) = ID1 ()
 val  (record_args as record_args1) = record_args1 ()
 in (
A.RecordExp({fields = record_args, typ = Symbol.symbol(ID), pos = IDleft})
)
end)
 in ( LrTable.NT 14, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: _ :: ( _, (
 MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as 
ID1left), _)) :: rest671)) => let val  result = MlyValue.structBuilder
 (fn _ => let val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.ArrayExp({typ = Symbol.symbol(ID), size = exp1, init = exp2, pos = IDleft})
)
end)
 in ( LrTable.NT 14, ( result, ID1left, exp2right), rest671)
end
|  ( 8, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.func_args 
func_args1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left)
, _)) :: rest671)) => let val  result = MlyValue.func_exp (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  (func_args as func_args1) = func_args1 ()
 in (
A.CallExp({func = Symbol.symbol(ID), args = func_args, pos = IDleft}))

end)
 in ( LrTable.NT 4, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 9, ( rest671)) => let val  result = MlyValue.func_args (fn _ => (
[]))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 10, ( ( _, ( MlyValue.func_args_seq func_args_seq1, _, 
func_args_seq1right)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: 
rest671)) => let val  result = MlyValue.func_args (fn _ => let val  (
exp as exp1) = exp1 ()
 val  (func_args_seq as func_args_seq1) = func_args_seq1 ()
 in (exp :: func_args_seq)
end)
 in ( LrTable.NT 5, ( result, exp1left, func_args_seq1right), rest671)

end
|  ( 11, ( ( _, ( MlyValue.func_args_seq func_args_seq1, _, 
func_args_seq1right)) :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _,
 COMMA1left, _)) :: rest671)) => let val  result = 
MlyValue.func_args_seq (fn _ => let val  (exp as exp1) = exp1 ()
 val  (func_args_seq as func_args_seq1) = func_args_seq1 ()
 in (exp :: func_args_seq)
end)
 in ( LrTable.NT 6, ( result, COMMA1left, func_args_seq1right), 
rest671)
end
|  ( 12, ( rest671)) => let val  result = MlyValue.func_args_seq (fn _
 => ([]))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 13, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.l_value (fn _ => let val  (ID
 as ID1) = ID1 ()
 in (A.SimpleVar(Symbol.symbol(ID), IDleft))
end)
 in ( LrTable.NT 13, ( result, ID1left, ID1right), rest671)
end
|  ( 14, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: _ :: ( _, ( MlyValue.l_value l_value1, (l_valueleft as 
l_value1left), _)) :: rest671)) => let val  result = MlyValue.l_value
 (fn _ => let val  (l_value as l_value1) = l_value1 ()
 val  (exp as exp1) = exp1 ()
 in (A.SubscriptVar(l_value, exp, l_valueleft))
end)
 in ( LrTable.NT 13, ( result, l_value1left, RBRACK1right), rest671)

end
|  ( 15, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( 
MlyValue.l_value l_value1, (l_valueleft as l_value1left), _)) :: 
rest671)) => let val  result = MlyValue.l_value (fn _ => let val  (
l_value as l_value1) = l_value1 ()
 val  (ID as ID1) = ID1 ()
 in (A.FieldVar(l_value, Symbol.symbol(ID), l_valueleft))
end)
 in ( LrTable.NT 13, ( result, l_value1left, ID1right), rest671)
end
|  ( 16, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left), _)) :: 
rest671)) => let val  result = MlyValue.l_value (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (
A.SubscriptVar(A.SimpleVar(Symbol.symbol(ID), IDleft), exp, IDleft))

end)
 in ( LrTable.NT 13, ( result, ID1left, RBRACK1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.STRING STRING1, (STRINGleft as STRING1left)
, STRING1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (STRING as STRING1) = STRING1 ()
 in (A.StringExp(STRING, STRINGleft))
end)
 in ( LrTable.NT 0, ( result, STRING1left, STRING1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.l_value l_value1, l_value1left, 
l_value1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (l_value as l_value1) = l_value1 ()
 in (A.VarExp(l_value))
end)
 in ( LrTable.NT 0, ( result, l_value1left, l_value1right), rest671)

end
|  ( 19, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.l_value l_value1, (l_valueleft as l_value1left), _)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (l_value
 as l_value1) = l_value1 ()
 val  (exp as exp1) = exp1 ()
 in (A.AssignExp({var = l_value, exp = exp, pos = l_valueleft}))
end)
 in ( LrTable.NT 0, ( result, l_value1left, exp1right), rest671)
end
|  ( 20, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 21, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( _, LPAREN1left, _))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => (A.SeqExp([])
))
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 22, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp_seq 
exp_seq1, _, _)) :: ( _, ( MlyValue.exp exp2, exp2left, _)) :: _ :: (
 _, ( MlyValue.exp exp1, exp1left, _)) :: ( _, ( _, LPAREN1left, _))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1
 = exp1 ()
 val  exp2 = exp2 ()
 val  (exp_seq as exp_seq1) = exp_seq1 ()
 in (A.SeqExp((exp1, exp1left) :: (exp2, exp2left) :: exp_seq))
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 23, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => (A.NilExp))
 in ( LrTable.NT 0, ( result, NIL1left, NIL1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.mathExp mathExp1, mathExp1left, 
mathExp1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (mathExp as mathExp1) = mathExp1 ()
 in (mathExp)
end)
 in ( LrTable.NT 0, ( result, mathExp1left, mathExp1right), rest671)

end
|  ( 25, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: 
( _, ( _, (IFleft as IF1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (
A.IfExp({test = exp1, then' = exp2, else' = SOME(exp3), pos = IFleft})
)
end)
 in ( LrTable.NT 0, ( result, IF1left, exp3right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (IFleft as IF1left), _)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = 
exp1 ()
 val  exp2 = exp2 ()
 in (A.IfExp({test = exp1, then' = exp2, else' = NONE, pos = IFleft}))

end)
 in ( LrTable.NT 0, ( result, IF1left, exp2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (WHILEleft as WHILE1left), _))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1
 = exp1 ()
 val  exp2 = exp2 ()
 in (A.WhileExp({test = exp1, body = exp2, pos = WHILEleft}))
end)
 in ( LrTable.NT 0, ( result, WHILE1left, exp2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) ::
 _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FORleft as FOR1left
), _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (
A.ForExp({var = Symbol.symbol(ID), escape = ref false, lo = exp1, hi = exp2, body = exp3, pos = FORleft})
)
end)
 in ( LrTable.NT 0, ( result, FOR1left, exp3right), rest671)
end
|  ( 29, ( ( _, ( _, (BREAKleft as BREAK1left), BREAK1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => (
A.BreakExp(BREAKleft)))
 in ( LrTable.NT 0, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.structBuilder structBuilder1, 
structBuilder1left, structBuilder1right)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => let val  (structBuilder as 
structBuilder1) = structBuilder1 ()
 in (structBuilder)
end)
 in ( LrTable.NT 0, ( result, structBuilder1left, structBuilder1right)
, rest671)
end
|  ( 31, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.let_body 
let_body1, _, _)) :: _ :: ( _, ( MlyValue.decList decList1, _, _)) :: 
( _, ( _, (LETleft as LET1left), _)) :: rest671)) => let val  result =
 MlyValue.exp (fn _ => let val  (decList as decList1) = decList1 ()
 val  (let_body as let_body1) = let_body1 ()
 in (A.LetExp({decs = decList, body = let_body, pos = LETleft}))
end)
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.func_exp func_exp1, func_exp1left, 
func_exp1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (func_exp as func_exp1) = func_exp1 ()
 in (func_exp)
end)
 in ( LrTable.NT 0, ( result, func_exp1left, func_exp1right), rest671)

end
|  ( 33, ( ( _, ( MlyValue.nfuncdec nfuncdec1, _, nfuncdec1right)) :: 
( _, ( MlyValue.funcdec funcdec1, funcdec1left, _)) :: rest671)) =>
 let val  result = MlyValue.decList (fn _ => let val  (funcdec as 
funcdec1) = funcdec1 ()
 val  (nfuncdec as nfuncdec1) = nfuncdec1 ()
 in (funcdec @ nfuncdec)
end)
 in ( LrTable.NT 15, ( result, funcdec1left, nfuncdec1right), rest671)

end
|  ( 34, ( ( _, ( MlyValue.ntypedec ntypedec1, _, ntypedec1right)) :: 
( _, ( MlyValue.typedec typedec1, typedec1left, _)) :: rest671)) =>
 let val  result = MlyValue.decList (fn _ => let val  (typedec as 
typedec1) = typedec1 ()
 val  (ntypedec as ntypedec1) = ntypedec1 ()
 in (typedec @ ntypedec)
end)
 in ( LrTable.NT 15, ( result, typedec1left, ntypedec1right), rest671)

end
|  ( 35, ( ( _, ( MlyValue.decList decList1, _, decList1right)) :: ( _
, ( MlyValue.vardec vardec1, vardec1left, _)) :: rest671)) => let val 
 result = MlyValue.decList (fn _ => let val  (vardec as vardec1) = 
vardec1 ()
 val  (decList as decList1) = decList1 ()
 in (vardec :: decList)
end)
 in ( LrTable.NT 15, ( result, vardec1left, decList1right), rest671)

end
|  ( 36, ( rest671)) => let val  result = MlyValue.decList (fn _ => (
[]))
 in ( LrTable.NT 15, ( result, defaultPos, defaultPos), rest671)
end
|  ( 37, ( ( _, ( MlyValue.decList decList1, _, decList1right)) :: ( _
, ( MlyValue.vardec vardec1, vardec1left, _)) :: rest671)) => let val 
 result = MlyValue.ntypedec (fn _ => let val  (vardec as vardec1) = 
vardec1 ()
 val  (decList as decList1) = decList1 ()
 in (vardec :: decList)
end)
 in ( LrTable.NT 17, ( result, vardec1left, decList1right), rest671)

end
|  ( 38, ( ( _, ( MlyValue.nfuncdec nfuncdec1, _, nfuncdec1right)) :: 
( _, ( MlyValue.funcdec funcdec1, funcdec1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntypedec (fn _ => let val  (funcdec as 
funcdec1) = funcdec1 ()
 val  (nfuncdec as nfuncdec1) = nfuncdec1 ()
 in (funcdec @ nfuncdec)
end)
 in ( LrTable.NT 17, ( result, funcdec1left, nfuncdec1right), rest671)

end
|  ( 39, ( rest671)) => let val  result = MlyValue.ntypedec (fn _ => (
[]))
 in ( LrTable.NT 17, ( result, defaultPos, defaultPos), rest671)
end
|  ( 40, ( ( _, ( MlyValue.decList decList1, _, decList1right)) :: ( _
, ( MlyValue.vardec vardec1, vardec1left, _)) :: rest671)) => let val 
 result = MlyValue.nfuncdec (fn _ => let val  (vardec as vardec1) = 
vardec1 ()
 val  (decList as decList1) = decList1 ()
 in (vardec :: decList)
end)
 in ( LrTable.NT 18, ( result, vardec1left, decList1right), rest671)

end
|  ( 41, ( ( _, ( MlyValue.ntypedec ntypedec1, _, ntypedec1right)) :: 
( _, ( MlyValue.typedec typedec1, typedec1left, _)) :: rest671)) =>
 let val  result = MlyValue.nfuncdec (fn _ => let val  (typedec as 
typedec1) = typedec1 ()
 val  (ntypedec as ntypedec1) = ntypedec1 ()
 in (typedec @ ntypedec)
end)
 in ( LrTable.NT 18, ( result, typedec1left, ntypedec1right), rest671)

end
|  ( 42, ( rest671)) => let val  result = MlyValue.nfuncdec (fn _ => (
[]))
 in ( LrTable.NT 18, ( result, defaultPos, defaultPos), rest671)
end
|  ( 43, ( ( _, ( MlyValue.exp_seq exp_seq1, _, exp_seq1right)) :: ( _
, ( MlyValue.exp exp1, (expleft as exp1left), _)) :: rest671)) => let
 val  result = MlyValue.let_body (fn _ => let val  (exp as exp1) = 
exp1 ()
 val  (exp_seq as exp_seq1) = exp_seq1 ()
 in (A.SeqExp((exp, expleft) :: exp_seq))
end)
 in ( LrTable.NT 19, ( result, exp1left, exp_seq1right), rest671)
end
|  ( 44, ( rest671)) => let val  result = MlyValue.let_body (fn _ => (
A.SeqExp([])))
 in ( LrTable.NT 19, ( result, defaultPos, defaultPos), rest671)
end
|  ( 45, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, ID2left, _)) :: _ :: _ :: ( _, ( MlyValue.type_fields
 type_fields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, (
 _, (FUNCTIONleft as FUNCTION1left), _)) :: rest671)) => let val  
result = MlyValue.funcdec (fn _ => let val  ID1 = ID1 ()
 val  (type_fields as type_fields1) = type_fields1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 in (
A.FunctionDec([{name=Symbol.symbol(ID1), params = type_fields, result = SOME((Symbol.symbol(ID2), ID2left)), body = exp, pos = FUNCTIONleft}]) :: []
)
end)
 in ( LrTable.NT 11, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.funcdec funcdec1, _, funcdec1right)) :: ( _
, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID2, ID2left,
 _)) :: _ :: _ :: ( _, ( MlyValue.type_fields type_fields1, _, _)) ::
 _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FUNCTIONleft as 
FUNCTION1left), _)) :: rest671)) => let val  result = MlyValue.funcdec
 (fn _ => let val  ID1 = ID1 ()
 val  (type_fields as type_fields1) = type_fields1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 val  (funcdec as funcdec1) = funcdec1 ()
 in (
updateFuncDec({name=Symbol.symbol(ID1), params = type_fields, result = SOME((Symbol.symbol(ID2), ID2left)), body = exp, pos = FUNCTIONleft}, funcdec)
)
end)
 in ( LrTable.NT 11, ( result, FUNCTION1left, funcdec1right), rest671)

end
|  ( 47, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: _ :: ( _, 
( MlyValue.type_fields type_fields1, _, _)) :: _ :: ( _, ( MlyValue.ID
 ID1, _, _)) :: ( _, ( _, (FUNCTIONleft as FUNCTION1left), _)) :: 
rest671)) => let val  result = MlyValue.funcdec (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (type_fields as type_fields1) = type_fields1 ()
 val  (exp as exp1) = exp1 ()
 in (
A.FunctionDec([{name=Symbol.symbol(ID), params = type_fields, result = NONE, body = exp, pos = FUNCTIONleft}]) :: []
)
end)
 in ( LrTable.NT 11, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.funcdec funcdec1, _, funcdec1right)) :: ( _
, ( MlyValue.exp exp1, _, _)) :: _ :: _ :: ( _, ( MlyValue.type_fields
 type_fields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, (
 _, (FUNCTIONleft as FUNCTION1left), _)) :: rest671)) => let val  
result = MlyValue.funcdec (fn _ => let val  (ID as ID1) = ID1 ()
 val  (type_fields as type_fields1) = type_fields1 ()
 val  (exp as exp1) = exp1 ()
 val  (funcdec as funcdec1) = funcdec1 ()
 in (
updateFuncDec({name=Symbol.symbol(ID), params = type_fields, result = NONE, body = exp, pos = FUNCTIONleft}, funcdec)
)
end)
 in ( LrTable.NT 11, ( result, FUNCTION1left, funcdec1right), rest671)

end
|  ( 49, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, ID2left, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _))
 :: ( _, ( _, (VARleft as VAR1left), _)) :: rest671)) => let val  
result = MlyValue.vardec (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 in (
A.VarDec({name = Symbol.symbol(ID1), escape = ref false, typ = SOME((Symbol.symbol(ID2), ID2left)), init = exp, pos = VARleft})
)
end)
 in ( LrTable.NT 12, ( result, VAR1left, exp1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (VARleft as VAR1left), _)) :: 
rest671)) => let val  result = MlyValue.vardec (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (
A.VarDec({name = Symbol.symbol(ID), escape = ref false, typ = NONE, init = exp, pos = VARleft})
)
end)
 in ( LrTable.NT 12, ( result, VAR1left, exp1right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.ty ty1, _, ty1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (TYPEleft as TYPE1left), _)) :: 
rest671)) => let val  result = MlyValue.typedec (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (ty as ty1) = ty1 ()
 in (
A.TypeDec([{name = Symbol.symbol(ID), ty = ty, pos = TYPEleft}]) :: []
)
end)
 in ( LrTable.NT 10, ( result, TYPE1left, ty1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.typedec typedec1, _, typedec1right)) :: ( _
, ( MlyValue.ty ty1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: 
( _, ( _, (TYPEleft as TYPE1left), _)) :: rest671)) => let val  result
 = MlyValue.typedec (fn _ => let val  (ID as ID1) = ID1 ()
 val  (ty as ty1) = ty1 ()
 val  (typedec as typedec1) = typedec1 ()
 in (
updateTypeDec({name = Symbol.symbol(ID), ty = ty, pos = TYPEleft}, typedec)
)
end)
 in ( LrTable.NT 10, ( result, TYPE1left, typedec1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, (
ARRAYleft as ARRAY1left), _)) :: rest671)) => let val  result = 
MlyValue.ty (fn _ => let val  (ID as ID1) = ID1 ()
 in (A.ArrayTy(Symbol.symbol(ID), ARRAYleft))
end)
 in ( LrTable.NT 16, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.ty (fn _ => let val  (ID as 
ID1) = ID1 ()
 in (A.NameTy(Symbol.symbol(ID), IDleft))
end)
 in ( LrTable.NT 16, ( result, ID1left, ID1right), rest671)
end
|  ( 55, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( _, LBRACE1left, _))
 :: rest671)) => let val  result = MlyValue.ty (fn _ => (
A.RecordTy([])))
 in ( LrTable.NT 16, ( result, LBRACE1left, RBRACE1right), rest671)

end
|  ( 56, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.type_fields 
type_fields1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.ty (fn _ => let val  (type_fields as 
type_fields1) = type_fields1 ()
 in (A.RecordTy(type_fields))
end)
 in ( LrTable.NT 16, ( result, LBRACE1left, RBRACE1right), rest671)

end
|  ( 57, ( rest671)) => let val  result = MlyValue.type_fields (fn _
 => ([]))
 in ( LrTable.NT 9, ( result, defaultPos, defaultPos), rest671)
end
|  ( 58, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.type_fields (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in (
[{name = Symbol.symbol(ID1), escape = ref false, typ = Symbol.symbol(ID2), pos = ID1left}]
)
end)
 in ( LrTable.NT 9, ( result, ID1left, ID2right), rest671)
end
|  ( 59, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: _ :: ( _, ( MlyValue.type_fields 
type_fields1, type_fields1left, _)) :: rest671)) => let val  result = 
MlyValue.type_fields (fn _ => let val  (type_fields as type_fields1) =
 type_fields1 ()
 val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in (
type_fields @ [{name = Symbol.symbol(ID1), escape = ref false, typ = Symbol.symbol(ID2), pos = ID1left}]
)
end)
 in ( LrTable.NT 9, ( result, type_fields1left, ID2right), rest671)

end
|  ( 60, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.mathExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp({left = exp1, oper = A.PlusOp, right = exp2, pos = exp1left}))

end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.mathExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp({left = exp1, oper = A.MinusOp, right = exp2, pos = exp1left})
)
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.mathExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp({left = exp1, oper = A.TimesOp, right = exp2, pos = exp1left})
)
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 63, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.mathExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp({left = exp1, oper = A.DivideOp, right = exp2, pos = exp1left})
)
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 64, ( ( _, ( MlyValue.exp exp1, expleft, exp1right)) :: ( _, ( _,
 MINUS1left, _)) :: rest671)) => let val  result = MlyValue.mathExp
 (fn _ => let val  (exp as exp1) = exp1 ()
 in (
A.OpExp({left = A.IntExp(0), oper = A.MinusOp, right = exp, pos = expleft})
)
end)
 in ( LrTable.NT 3, ( result, MINUS1left, exp1right), rest671)
end
|  ( 65, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.mathExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp({left = exp1, oper = A.EqOp, right = exp2, pos = exp1left}))

end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 66, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.mathExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp({left = exp1, oper = A.NeqOp, right = exp2, pos = exp1left}))

end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 67, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.mathExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp({left = exp1, oper = A.GtOp, right = exp2, pos = exp1left}))

end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 68, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.mathExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp({left = exp1, oper = A.LtOp, right = exp2, pos = exp1left}))

end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 69, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.mathExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp({left = exp1, oper = A.GeOp, right = exp2, pos = exp1left}))

end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 70, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.mathExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp({left = exp1, oper = A.LeOp, right = exp2, pos = exp1left}))

end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 71, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.mathExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.IfExp({test = exp1, then' = exp2, else' = SOME(A.IntExp(0)), pos = exp1left})
)
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 72, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.mathExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.IfExp({test = exp1, then' = A.IntExp(1), else' = SOME(exp2), pos = exp1left})
)
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 73, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.mathExp (fn _ => let val  (INT as INT1
) = INT1 ()
 in (A.IntExp(INT))
end)
 in ( LrTable.NT 3, ( result, INT1left, INT1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
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
