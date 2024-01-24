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

expression : res=expression '?' first=expression ':' second=expression
           | '(' type ')' expression
           | inside=expression '.' bee=expression
        //    | '[' exprList? ']'
        //    | '(' exprList? ')'
           | expression '[' subscript ']'
           | callee=expression '(' arguments=exprList? ')'
           | expression logicalOp expression
           | expression Concat expression
           | expression OrOr expression
           | expression shiftOp expression
           | expression binaryOp expression
           | expression compareOp expression
           | unaryOp expression
           | expression (PlusPlus | MinusMinus)
           | atom
           ;

logicalOp : AndAnd
          | OrOr
          ;

shiftOp: LeftShift
       | RightShift
       ;

binaryOp : Plus
         | Minus
         | Star
         | Div
         | Mod
         | And
         | Or
         | Caret
         ;

compareOp : Less
          | LessEqual
          | Greater
          | GreaterEqual
          | Equality
          | NotEqual
          ;

unaryOp : Or
        | Caret
        | And
        | Minus
        | Tilde
        | Not
        ;

atom : id=Identifier
     | ('"' stringId=Identifier '"')
     | ('"' stringNum=Digit '"')    
     | BinaryData
     | num=Digit
     ;
exprList : expression
         | exprList Comma expression
         | subscript
         ;
arglist : expression (',' expression)* ;
// subscriptlist : subscript (',' subscript)* ','? ;
subscript : low = expression? Colon uper = expression? (Colon step=expression)?
          ;

ifStatement : If '(' expression ')' '{' thenBody+=suite* '}' (Else '{' elBody+=suite* '}')? ;

forIteration : For '(' forIterBody ')' '{' suite* '}' ;
forIterBody : (varDecl '=' lowerBound=expression)? ';' upBound=expression? ';' iter=expression? ;

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
            | expression ';'
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
Comma : ',';
Colon : ':';

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

