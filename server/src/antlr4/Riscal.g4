grammar Riscal;

module
    : modules*
    ;

modules
    : pipeModule
    | regModule
    | memModule
    | operModule
    | paraModule
    | fuModule
    | instrModule
    | configModule
    | includeModule
    ;

includeModule
    : '#' IncludeKW '"' other '"'
    ;
other : baseName=Identifier ('.' extName=Identifier) ;

// parameter
paraModule : ParaKW paraName '=' Digit ';'?  ;
paraName : Identifier;


// pipeline
pipeModule : PipeKW pipeName '{' pipeStage (',' pipeStage)* ';'? '}' ;
pipeName : Identifier;
pipeStage : Identifier ( '=' pipeName '.' pipeStage)?;


// regfile
regModule : RegKW regName regSize '{' regBody* '}' ;
regName : Identifier;
regSize : '[' Digit ']' '[' Digit ']';
regBody : regReadPort | regWritePort;
regReadPort : ReadPort rwPortName (',' rwPortName)* ';' ;
regWritePort : WritePort rwPortName (',' rwPortName)* ';' ;
rwPortName : Identifier;


// operand
operModule : OperKW operName '{' operBody* '}' ;
operName : Identifier;
operBody : operVarDefine
         | operBinaryDefine
         | operSyntaxDefine
         | operSematicDefine ;
operVarDefine : UintType Identifier ';';
operBinaryDefine  : Binary '{' operExpr '}' ;
operSyntaxDefine  : Syntax '{' operExpr '}' ;
operSematicDefine : Sematic '{' (operExpr | operRegExpr) '}' ;
operRegExpr : regName '[' operExpr ']' ;
operExpr : '(' operExpr ')'                   # OperBrachet
         | operExpr Concat operExpr           # OperConcat
         | operExpr ( Star | Div ) operExpr   # OperMuldiv
         | operExpr ( Plus | Minus ) operExpr # OperPlusMinus
         | ('"' Identifier '"')               # OperString
         | ('"' Digit '"')                    # OperString
         | BinaryData                         # OperBinary
         | Identifier                         # OperVariable
         | Digit                              # OperDigit
         ;


// memory
memModule : MemKW memName memSize  '{' memBody* '}' ;
memName : Identifier;
memSize : '[' Digit ']' '[' Digit ']';
memBody : memAttrExpr
        | memInterface
        ;
memAttrExpr : memAttrExpr '=' memAttrExpr ';'  # MemEqual
            | Identifier                       # MemAttr
            | Digit                            # MemValue
            ;
memInterface : MemIf memIfName '{' busType* '}' ';'  ;
memIfName : Identifier ;
busType   : 'AHB_LITE'
          | 'AXI'
          | 'APB'
          ;

fuModule : FuKW operName parameters '{' fuBody* '}' ;
parameters : '(' param_list+=parameter (',' param_list+=parameter)* ')' ;
parameter : FuIn type Identifier
          | FuOut type Identifier
          | InOut type Identifier
          ;
type : UintType
     | IntType
     | Void
     | Char
     | Short
     | Int
     | UInt
     | Long
     ;
fuBody : varDecl ';'
       | fuStage
       ;
varDecl : type Identifier (',' Identifier)* ;
fuStage : Stage single=stagePipe '{' stageBody* '}'
        | Stage left=stagePipe '..' right=stagePipe '{' stageBody* '}';
stagePipe : left=Identifier '.' right=Identifier ;
stageBody : suite
          | Commit ';'
          ;
suite : expression ';'
      | assign
      | ifStatement
      | forIteration
      ;
assign : left=assignTarget '=' right=expression ';' ;
assignTarget : Identifier
             | assignTarget assignTrialer
             ;
assignTrialer : '[' subscript ']'
              | '.' assignTarget
              ;

expression : single=concatExpr 
            | res=concatExpr '?' first=concatExpr ':' second=concatExpr 
            ;

concatExpr : logicalAndExpr Concat concatExpr
           | logicalAndExpr
           ;
logicalAndExpr : logicalOrExpr AndAnd logicalAndExpr
               | logicalOrExpr
               ;
logicalOrExpr : notExpr OrOr logicalOrExpr
              | notExpr
              ;
notExpr : Not notExpr
        | comparison
        ;
comparison : orExpr compOp comparison
           | orExpr
           ;
compOp : Less
       | LessEqual
       | Greater
       | GreaterEqual
       | Equality
       | NotEqual
       ;
orExpr : xorExpr Or orExpr
        | xorExpr
        ;
xorExpr : andExpr Caret xorExpr
        | andExpr
        ;
andExpr : shiftExpr And andExpr
        | shiftExpr
        ;
shiftExpr : arithExpr (LeftShift | RightShift) shiftExpr
          | arithExpr
          ;
arithExpr : term (Plus | Minus) arithExpr
          | term
          ;
term : power (Star | Div | Mod) term
     | power
     ;
power : factor (PlusPlus | MinusMinus)
      | single=factor
      ;
factor : (Minus | Tilde | Or | Caret | And) factor
       | castExpr
       ;
// todo: CastOp is Hign-Level Operator Precedence
castExpr : '(' type ')' castExpr
        | atomExpr
        ;
atomExpr : atomExpr trailer
         | atom 
         ;
atom : '(' exprList ')'
     | ('"' stringId=Identifier '"')
     | ('"' stringNum=Digit '"')    
     | BinaryData
     | id=Identifier
     | num=Digit
     ;
exprList : expression (',' expression)* ;
trailer : '(' arglist? ')'
        | '[' subscript ']'
        | '.' atomExpr
        ;
arglist : logicalAndExpr (',' logicalAndExpr)* ;
// subscriptlist : subscript (',' subscript)* ','? ;
subscript : single=logicalAndExpr 
          | first=logicalAndExpr ':' sencond=logicalAndExpr
          | lower=logicalAndExpr ':' upper=logicalAndExpr ':' step=logicalAndExpr 
          ;

ifStatement : If '(' expression ')' '{' thenBody+=suite* '}' (Else '{' elBody+=suite* '}')? ;

forIteration : For '(' forIterBody ')' '{' suite* '}' ;
forIterBody : (varDecl '=' lowerBound=expression)? ';' upBound=comparison? ';' iter=arithExpr? ;

instrModule : InstrKW Identifier '(' instrParameters ')' '{' instrBody* '}';
instrParameters : instrPara (',' instrPara)*;
instrPara : paraType=Identifier paramId=Identifier ;
instrBody : varDecl ';'
          | instrBinary
          | instrSyntax
          | instrSematic
          ;
instrBinary : Binary '{' instrExpr '}' ;
instrExpr : instrAtom (Concat instrAtom)* ;
instrAtom : Identifier
          | BinaryData
          | Digit
          ;
instrSyntax : Syntax '{' '"' syntaxBody '"' '}';
syntaxBody : instrName syntaxArguments ;
instrName : Identifier
          | Identifier '.' Identifier
          ;
syntaxArguments : syntaxArg (',' syntaxArg)* ;
syntaxArg : '%' Identifier
          | '%' offset=Identifier '(' '%' Identifier ')'
          ;
instrSematic : Sematic '{' sematicBody* '}';
sematicBody : fuStage
            | atomExpr ';'
            ;

configModule : 'Config' Identifier '{' configEntry* '}' ;
configEntry : Identifier '=' configValue ';'? ;
configValue : Digit
            | Identifier
            | DigitId
            | HexDigit
            | 'APB'
            | '{' configValue (',' configValue)* '}'
            ;

// Lexer Part
PipeKW : 'Pipeline';
RegKW : 'Regfile';
MemKW : 'Memory';
OperKW : 'Operand';
ParaKW : 'Parameter';
FuKW : 'FuncUnit';
InstrKW : 'Instruction';
IncludeKW : 'include' ;
ReadPort : 'readport';
WritePort : 'writeport';
MemIf : 'memif';
Stage : 'stage';

FuIn : 'in';
FuOut : 'out';
InOut : 'inout';

Binary : 'binary' ;
Syntax : 'syntax' ;
Sematic : 'sematic' ;

If : 'if';
Else : 'else';
For : 'for';
Commit : 'commit';

Less : '<';
LessEqual : '<=';
Greater : '>';
GreaterEqual : '>=';
LeftShift : '<<';
RightShift : '>>';
Equality : '==';
NotEqual : '!=';

Plus : '+';
PlusPlus : '++';
Minus : '-';
MinusMinus : '--';
Star : '*';
Div : '/';
Mod : '%';

And : '&';
Or : '|';
AndAnd : '&&';
OrOr : '||';
Caret : '^';
Not : '!';
Tilde : '~';
Concat : '::';

UintType : 'uint' Digit+;
IntType : 'int' Digit+;
Void : 'void';
Char : 'char';
Short : 'short';
Int : 'int';
UInt : 'unsigned' 'int';
Long : 'long';

BinaryData : [0-9]+ ['][bB] [0-1]+ ;
Identifier : [a-zA-Z][a-zA-Z0-9_]* ;
Digit : [0-9]+ ;
DigitId : [0-9]+[a-zA-Z]+ ;
HexDigit : [0][xX][0-9a-fA-F]+ ;


WS : [ \r\n\t] -> skip ;              // skip spaces, tabs, newlines
LineComment : '//' .*? '\n' ->skip ;
BlockComment : '/*' .*? '*/' ->skip ;

