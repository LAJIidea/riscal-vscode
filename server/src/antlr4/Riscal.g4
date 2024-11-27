grammar Riscal;

// @parser::members {
// private int myself_results = 0;
// }

modules: module*;

module : NAMESPACE Identifier LBRACE (module | module_body)* RBRACE;

module_body : pipeline
            | regfile
            | operand
            | instruction
            | funcunit
            | issue
            | forward
            | addrspace
            | interface
            | memory
            | function
            | struct
            | enum
            | statement
            | trait
            | impl
            | typedef
            | prototype
            ;

pipeline : PIPELINE Identifier LBRACE expr_list RBRACE ;

regfile: 'ArchRegFile' generic Identifier LBRACE port_statement* RBRACE;

operand : OPERAD Identifier LBRACE (declaration SEMI)* arch_body* RBRACE;

instruction : INST Identifier LPAREN parameters? RPAREN LBRACE arch_body* RBRACE ;

funcunit: FUNCUNIT Identifier SEMI
        | FUNCUNIT Identifier LBRACE (issue_body (',' issue_body)* ','?)? RBRACE;

issue: ISSUE Identifier LBRACE issue_body (',' issue_body)* ','? RBRACE ('<=' expr)? SEMI;

issue_body : (INPUT | OUTPUT ) type=expr content=expr;

forward : FORWARD LBRACE statement* RBRACE ;

addrspace : ADDRSPACE Identifier LBRACE statement* RBRACE ;

memory : MEMORY Identifier LBRACE statement* memif* RBRACE ;

memif: 'MemIf' Identifier LBRACE statement* RBRACE ;

interface: INTERFACE Identifier LBRACE statement* RBRACE ;

struct : STRUCT Identifier generic? LBRACE (declaration SEMI)* RBRACE ;

enum : ENUM Identifier generic? LBRACE enum_body* RBRACE ;

enum_body: Identifier SEMI
         | Identifier LPAREN parameters? RPAREN SEMI
         ;

function : EXTERN? ty Identifier generic? LPAREN parameters? RPAREN ('requiring' LPAREN arguments RPAREN)? expr_block;

trait : 'trait' Identifier (LPAREN parameters RPAREN)? ('requiring' LPAREN arguments RPAREN)? LBRACE trait_body* RBRACE;
impl: 'impl' Identifier (LPAREN arguments RPAREN)? ('requiring' LPAREN arguments RPAREN)? LBRACE impl_body* RBRACE;
typedef: 'typedef' Identifier ty SEMI;

trait_body: declaration ';'
          | proto
          ;
proto: ty Identifier generic? LPAREN parameters? RPAREN ('requiring' LPAREN arguments RPAREN)?;
impl_body: declaration ';'
         | function
         ;
prototype: EXTERN? proto ';';

ty : ADDRSPACE
   | ISSUE
   | INTERFACE
   | MEMORY
   | FUNCUNIT
   | Identifier generic?
   | PCREG generic
   | AUTO
   | BASE
   ;

parameters: ('{' implicit=param (',' implicit=param)* '}' ',')? param (',' param)* ;

param: ty Identifier ;

//type: expr generic?
//    | AUTO
//    ;
//
//type_params: type_param (',' type_param)* ;
//
//type_param: type
//          | expr
//          ;

generic : '(' expr_list ')' ;

port_statement : (READPORT | WRITEPORT) expr_list SEMI;

//==============================
alias_statement : 'alias_to';
//==============================

arch_body : semantics
          | binary
          | syntax
          ;

semantics: SEMATIC expr_block ;

binary: BINARY expr_block ;

syntax: SYNTAX expr_block ;

arguments : expr (',' expr)* ;

expr_block: LBRACE statement* expr? RBRACE ;

block: LBRACE statement* RBRACE ;

statement : declaration SEMI
          | assignment SEMI
          | condition
          | loop
          | iteration
          | try_statement
          | THROW expr SEMI
          | expression=expr SEMI
          | RETURN expr SEMI
          | import_statement
          | stage_statement
          ;

declaration : EXTERN ? CONST? ty name=Identifier
            | EXTERN ? CONST? vret=ty name=Identifier ASSIGN expr
            ;

assignment: lhs=expr ASSIGN rhs=expr ;

condition : IF LPAREN cond=expr RPAREN then=block (ELSE IF LPAREN elcond=expr RPAREN other=block)*
            (ELSE el=block) ? ;

loop : WHILE LPAREN end_expr=expr RPAREN LBRACE statement* RBRACE
     | FOR LPAREN (declaration | assignment)? SEMI cond=expr? SEMI iter=expr? RPAREN LBRACE statement* RBRACE
     ;

iteration : FOR LPAREN declaration COLON expr RPAREN block;

try_statement : TRY LBRACE statement* RBRACE (CACHE LPAREN expr RPAREN block)*;

import_statement : IMPORT expr SEMI
                 | USING IMPORT expr SEMI
                 ;

stage_statement : 'stage' cond=expr block ;

expr_list : expr (',' expr) * ;

expr : '_'
     | struct_init=Identifier '{' expr_list? '}'
     | left=expr ACCESS bee=Identifier
     |   deref='$' operand_expr=expr
     | list='[' expr_list? ']'
     | tuple=LPAREN expr_list? RPAREN
     | array=expr '[' vector_slice? ']'
     | inside=expr DOT bee=Identifier
     | callee=expr LPAREN arguments? RPAREN
     | <assoc=right> op=unary_op operand_expr=expr
     | left=expr bin_factor_op right=expr
     | left=expr bin_sum_op right=expr
     | left=expr bin_shift_op right=expr
     | left=expr compare_op right=expr
     | left=expr bin_bit_op right=expr
     | left=expr logic_op right=expr
     | left=expr concat_op right=expr
     | left=expr bind_op right=expr
     | left=expr vars='..' right=expr
     | left=expr EXCLUDE right=expr
     | left=expr IN right=expr
     | left=expr stage_bind='bind_to' right=expr
    //  | unary_call=expr unary=expr
    //  | left=expr binary_call=Identifier left=expr
     | triple=expr QUE_MARK true_expr=expr COLON false_expr=expr
     | left=expr assign_op right=expr
     | lambda_expr
     | if_expr
     | reduction
     | for_expr
     | match_expr
     | left=expr AT right=expr
     | primary
     ;

match_expr : MATCH LPAREN expr_list RPAREN LBRACE (CASE LPAREN case=expr_list RPAREN expr_block)* RBRACE;

reduction : FOR LPAREN (declaration | assignment)* SEMI expr_list? SEMI expr_list? RPAREN
            LBRACE statement* RBRACE YIELD LBRACE Identifier RBRACE
;

lambda_expr : LPAREN parameters? RPAREN expr_block ;

if_expr : IF cond=expr THEN then=expr_block ELSE other=expr_block ;

for_expr : '[' val=expr '|' FOR iter=expr IN cond=expr ']' ;

vector_slice : base=expr
             | low = expr? COLON uper = expr? (COLON step=expr)?
             ;

bin_bit_op : '|' | '&' | '^';
bin_shift_op : '<<' | '>>';
bin_factor_op : '*' | '/' | '%' ;
bin_sum_op : '+' | '-';
unary_op : '!' | '~' | '`&' | '`|' | '`^';
compare_op : '==' | '!=' | '<' | '<=' | '>' | '>=';
logic_op : '&&' | '||';
bind_op : '<==' | '==>' ;
assign_op : ':=' | '+=' | '-='  ;
concat_op : '<+>' | '++' | '<:' ;

primary : Identifier
        | constant
        | STRINGLIT
        | CHARLIT
        | ('True' | 'False')
        | BASE
        ;

constant : Literal
         | Integer
         | Double
         | FullIntLit
         ;

IF : 'if';
ELSE : 'else';
WHILE : 'while';
FOR : 'for';
YIELD : 'yield';
RETURN : 'return';
STRUCT : 'struct';
TRAIT : 'trait';
IMPL : 'impl';
ENUM : 'enum';
AUTO : 'auto';
MATCH : 'match';
BASE: 'Type' ;
CASE : 'case';
TYPEDEF : 'typedef';
TRY : 'try';
CACHE : 'cache';
THROW : 'throw';
IMPORT : 'import';
THEN : 'then';
IN : 'in';
PIPELINE : 'Pipeline';
PCREG : 'PCReg';
WRITEPORT : 'Writeport';
READPORT : 'Readport';
OPERAD : 'Operand';
FUNCUNIT: 'FuncUnit';
INST: 'Instruction';
SEMATIC: 'Semantics';
BINARY: 'Binary';
SYNTAX: 'Syntax';
ISSUE : 'Issue';
INPUT : 'Input';
OUTPUT : 'Output';
FORWARD : 'Forward';
ADDRSPACE : 'AddrSpace';
MEMORY : 'Memory';
INTERFACE : 'Interface';
EXCLUDE : 'exclude';
NAMESPACE : 'namespace' ;
USING : 'using' ;
CONST : 'const' ;
EXTERN : 'extern' ;
ASSIGN : '=';
LBRACE : '{';
RBRACE : '}';
LPAREN : '(';
RPAREN : ')';
COMMA : ',';
SEMI : ';';
COLON : ':';
ACCESS: '::';
DOT : '.';
QUE_MARK : '?' ;
AT : '@' ;
LET : '<=' ;
LARROW : '<==' ;
GET : '>=' ;
RSARROW : '=>' ;
RARROW : '==>';

STRINGLIT
 : '"' ( '\\' [btnfr"'\\] | ~[\r\n\\"] )* '"'
 ;
CHARLIT
 : '\'' ( '\\' [btnfr"'\\] | ~[\r\n\\'] ) '\''
 ;

Identifier : IdentifierNotDigit (IdentifierNotDigit | Digit )*;

FullIntLit: '0x' '_'* [0-9a-fA-F] [0-9a-fA-F_]*
          | '0o' '_'* [0-7] [0-7_]*
          | '0b' '_'* [01] [01_]*
          ;

Literal : Integer_Literal ['] [bodx]? Integer_Expr [us]?
;

Integer: Integer_Literal;

Double: Double_Literal;

fragment
Integer_Expr : Integer_Literal  ( '_' | Integer_Literal) *
;

fragment
IdentifierNotDigit : [a-zA-Z_];

fragment
Digit : [0-9];

fragment
Integer_Literal
       : DigitNotZero Digit*
       | Digit
       ;

fragment
DigitNotZero : [1-9];

//fragment
//Any: '\u0021' .. '\u0024'
//   | '\u0026' .. '\u0027'
//   | '\u002A' .. '\u002B'
//   | '\u002D' .. '\u002F'
//   | '\u003C' .. '\u0040'
//   | '\u005B' .. '\u005C'
//   | '\u005E' .. '\u005F'
//   | '\u00C0'..'\u00D6'
//   | '\u00D8'..'\u00F6'
//   | '\u00F8'..'\u02FF'
//   | '\u0370'..'\u037D'
//   | '\u037F'..'\u1FFF'
//   | '\u200C'..'\u200D'
//   | '\u2070'..'\u218F'
//   | '\u2C00'..'\u2FEF'
//   | '\u3001'..'\uD7FF'
//   | '\uF900'..'\uFDCF'
//   | '\uFDF0'..'\uFFFD' ;

fragment
Double_Literal
       : (Digit+)? '.' Digit+ Digit+ [df]?
       | Digit+ '.' [df]?
       ;

Whitespace : [ \t\r\n]+ -> skip;
LineComment : '//' ~('\n'|'\r')* '\r'? '\n' -> channel(HIDDEN);
BlockComment : '/*' .*? '*/' -> channel(HIDDEN);