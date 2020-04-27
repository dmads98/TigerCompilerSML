type svalue = Tokens.svalue
type pos = int
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue,pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val nestedCom = ref 0;
val commentStart = ref ~1;
val strStart = ref ~1;
val strBuilder = ref "";
fun err(p1,p2) = ErrorMsg.error p1


  
fun eof() = let val pos = hd(!linePos)
	    in
		if !nestedCom = 0 then () else ErrorMsg.error(!commentStart) (" open comment");
		if !strStart = ~1 then () else ErrorMsg.error(!strStart) (" open string");
		nestedCom := 0;
		commentStart := ~1;
		strStart := ~1;
		strBuilder := "";
		Tokens.EOF(pos,pos)
	    end

%%

%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));

%s COMMENT STR FORMATSEQ;
		
%%


    
<INITIAL> "/*" => (YYBEGIN COMMENT;
		   commentStart := yypos;
		   nestedCom := !nestedCom + 1;
		   continue());
<COMMENT> "*/" => (if !nestedCom > 1 then () else YYBEGIN INITIAL;
		   nestedCom := !nestedCom - 1;
		   continue());
<COMMENT> "/*" => (nestedCom := !nestedCom + 1;
		   continue());
<COMMENT> . => (continue());

<INITIAL> \" => (YYBEGIN STR;
		 strStart := yypos;
		 strBuilder := "";
		 continue());
<STR> \" => (YYBEGIN INITIAL;
	     let val temp = !strStart
	     in
		 strStart := ~1;
		 Tokens.STRING(!strBuilder, temp, yypos+1)
	     end);
<STR> \\\^[@-_] => (strBuilder := !strBuilder ^ String.str(chr(ord(String.sub(yytext, 2)) - 64)); continue());
<STR> \\[0-1][0-9][0-9]|\\2[0-4][0-9]|\\25[0-5] => (strBuilder := !strBuilder ^ String.str(chr(valOf(Int.fromString(String.substring(yytext, 1, 3))))); continue());
<STR> \\\" => (strBuilder := !strBuilder ^ "\\\""; continue());
<STR> \\\\ => (strBuilder := !strBuilder ^ "\\"; continue());
<STR> \\t => (strBuilder := !strBuilder ^ yytext; continue());
<STR> [\\n] => (strBuilder := !strBuilder ^ yytext; continue());

<STR> \\[\012\t\n\r ] => (YYBEGIN FORMATSEQ;
			if (String.substring(yytext, 1, 1) = "\n")
			then (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue())
			else continue());

<FORMATSEQ> \n => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<FORMATSEQ> [\012\t\r ] => (continue());
<FORMATSEQ> \\ => (YYBEGIN STR; continue());
<FORMATSEQ> . => (ErrorMsg.error yypos (" illegal format sequence character"); continue());

<STR> \\ => (ErrorMsg.error yypos ("illegal escape string character"); continue());
<STR> [ -~] => (strBuilder := !strBuilder ^ yytext; continue());
<STR> . => (ErrorMsg.error yypos (" illegal string character"); continue());
<STR> \n => (strBuilder := !strBuilder ^ yytext; lineNum := !lineNum+1; linePos := yypos :: !linePos; ErrorMsg.error yypos (" illegal newline in string"); continue());

\n => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());

<INITIAL> \t|" " => (continue());
<INITIAL> var  	=> (Tokens.VAR(yypos,yypos+3));
<INITIAL> type  => (Tokens.TYPE(yypos,yypos+4));
<INITIAL> function  => (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL> break => (Tokens.BREAK(yypos,yypos+5));
<INITIAL> of    => (Tokens.OF(yypos,yypos+2));
<INITIAL> end   => (Tokens.END(yypos,yypos+3));
<INITIAL> in    => (Tokens.IN(yypos,yypos+2));
<INITIAL> nil   => (Tokens.NIL(yypos,yypos+3));
<INITIAL> let   => (Tokens.LET(yypos,yypos+3));
<INITIAL> do    => (Tokens.DO(yypos,yypos+2));
<INITIAL> to    => (Tokens.TO(yypos,yypos+4));
<INITIAL> for   => (Tokens.FOR(yypos,yypos+3));
<INITIAL> while => (Tokens.WHILE(yypos,yypos+5));
<INITIAL> else  => (Tokens.ELSE(yypos,yypos+4));
<INITIAL> then  => (Tokens.THEN(yypos,yypos+4));
<INITIAL> if    => (Tokens.IF(yypos,yypos+2));
<INITIAL> array => (Tokens.ARRAY(yypos,yypos+5));
<INITIAL> ":="    => (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL> "|"     => (Tokens.OR(yypos,yypos+1));
<INITIAL> "&"     => (Tokens.AND(yypos,yypos+1));
<INITIAL> ">="    => (Tokens.GE(yypos,yypos+2));
<INITIAL> ">"     => (Tokens.GT(yypos,yypos+1));
<INITIAL> "<="    => (Tokens.LE(yypos,yypos+2));
<INITIAL> "<"     => (Tokens.LT(yypos,yypos+1));
<INITIAL> "<>"    => (Tokens.NEQ(yypos,yypos+2));
<INITIAL> "="     => (Tokens.EQ(yypos,yypos+1));
<INITIAL> "/"     => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL> "*"     => (Tokens.TIMES(yypos,yypos+1));
<INITIAL> "-"     => (Tokens.MINUS(yypos,yypos+1));
<INITIAL> "."     => (Tokens.DOT(yypos,yypos+1));
<INITIAL> "}"     => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL> "{"     => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL> "]"     => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL> "["     => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL> ")"     => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL> "("     => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL> ";"     => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL> ":"     => (Tokens.COLON(yypos,yypos+1));
<INITIAL> ","	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL> "+"     => (Tokens.PLUS(yypos,yypos+1));
<INITIAL> [a-zA-Z][a-zA-Z_0-9]* => (Tokens.ID(yytext, yypos, yypos+size yytext));
<INITIAL> [0-9]+ => (Tokens.INT(valOf(Int.fromString(yytext)), yypos, yypos+size yytext));
<INITIAL> .       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
