import { RiscalParser } from './../antlr4/gen/RiscalParser';
import { RiscalLexer } from './../antlr4/gen/RiscalLexer';
import { SemanticVisitor } from "../visitor/semantic";
import { Workspace } from "./workspace";
// import { SymbolTable } from "./symbol_table";
import { ErrorListener } from "./error_listener";
import {
  ANTLRInputStream,
  CommonTokenStream,
  ConsoleErrorListener
} from 'antlr4ts';
import { CompletionItem, CompletionItemKind, CompletionList, Diagnostic, DiagnosticSeverity, Location, Position, Range, TextDocument, TextDocumentPositionParams } from 'vscode-languageserver';
import { ParseErrorLevel, ParserError } from './types';
import { Scope, VariableDefinition } from './scopes';

/// Returns `true` if the character may start an identifier.
function is_identifier_start_char(c: string): boolean {
  return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_';
}


function is_identifier_char(c: string): boolean {
  return is_identifier_start_char(c) || ('0' <= c && c<= '9');
}

export class Compilation {

 workspace: Workspace;

 constructor() {
  this.workspace = new Workspace();
 }

 public update(document: TextDocument): Diagnostic[] {
  let input = new ANTLRInputStream(document.getText())
  let lexer = new RiscalLexer(input);
  lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);

  let token = new CommonTokenStream(lexer);
  let parser = new RiscalParser(token);
  let listener = new ErrorListener();
  parser.addErrorListener(listener)
  let tree = parser.modules();
  let visitor = new SemanticVisitor(this.workspace, document.uri);
  visitor.visit(tree);
  return this.launchCompilationErrors(listener.errors);
 }

 public complete(param: TextDocumentPositionParams): CompletionList {
  let symbols = this.workspace.dictionary.get(param.textDocument.uri);
  let scope = this.getOnwerScope(symbols.allScopes, param.position.line);
  let suggestion = this.getAllSymbolSuggestors(scope);
  return {isIncomplete: false, items: suggestion};
 }

 public find_defination(document: TextDocument, position: Position): Location | null {
  let word = this.get_word_under_cursor(document, position);
  if (!word) 
    return null;

  let symbols = this.workspace.dictionary.get(document.uri);
  let scope = this.getOnwerScope(symbols.allScopes, position.line);
  let var_define = this.getSymbolDefinition(scope, word);
  return {
    uri: document.uri,
    range: Range.create(Position.create(var_define.startLine, var_define.startCharacter), Position.create(var_define.endLine, var_define.endCharacter))
  };
 }

 private launchCompilationErrors(errors: ParserError[]): Diagnostic[] {
  let diagnostics: Diagnostic[] = [];
  errors.forEach(error => {
    diagnostics.push(this.errorToDiagnostics(error));
  });
  return diagnostics;
 }

 private errorToDiagnostics(error: ParserError) {
  return {
    serverity: error.level === ParseErrorLevel.ERROR ? DiagnosticSeverity.Error : DiagnosticSeverity.Warning,
    range: {
      start: {
        line: error.line,
        character: error.start
      },
      end: {
        line: error.line,
        character: error.end
      }
    },
    message: error.message,
    source: 'ex'
  };
 }

 private getOnwerScope(list: Scope[], line: number): Scope {
  let best: Scope = null;
  let score = 65536;
  for (const item of list) {
    if (line <= item.endLine && line >= item.startLine) {
      if (line - item.startLine < score) {
        best = item
        score = line - item.startLine
      }
    }
  }
  return best;
 }

 private getAllSymbolSuggestors(scope: Scope) {
  let suggestions = this.getCurrentScopeSymbols(scope);

  while (scope.parentScope) {
    suggestions.concat(this.getCurrentScopeSymbols(scope.parentScope));
    scope = scope.parentScope;
  }

  return suggestions;
 }

 private getCurrentScopeSymbols(scope: Scope) {
  let suggestions: CompletionItem[] = [];

  for (const [key, val] of scope.dictionary) {
    suggestions.push({
      label: val.name,
      kind: CompletionItemKind.Variable
    })
  }
  return suggestions;
 }

 private getSymbolDefinition(scope: Scope, name: string): VariableDefinition | null {
  return scope.lookup(name);
 }

 private find_position_offset(text: string, line: number, character: number): number {
  let offset = 0;
  while (line > 0) {
    while(text[offset] && text[offset] != '\n') offset += 1;
    if (text[offset] == '\n') offset += 1;
    line -= 1;
  }

  while (character > 0 && text[offset] && text[offset] != '\n') {
    offset += 1;
    character -= 1;
  }

  return offset;
 }

 private get_last_word_start(text: string, offset: number): number {
  let start = offset;
  while (start > 0 && is_identifier_char(text[start-1])) {
    start -= 1;
  }

  // If `text` was `123abc` and `offset` pointed at `b`, start would point at `1`.
  // We want to point to `a`, so advance past any characters that are not a
  // valid start of an identifier.
  while (start < offset && !is_identifier_start_char(text[start])) {
    start += 1;
  }

  return start;
 }

 private get_word_end(text: string, start: number): number {
  let end = start;
  while (text[end] && is_identifier_char(text[end])) end++;
  return end;
 }

 private get_word_under_cursor(document: TextDocument, position: Position): string | null {
  let offset = document.offsetAt(position);
  let word_start = this.get_last_word_start(document.getText(), offset);
  let word_end = this.get_word_end(document.getText(), word_start);
  let length = word_end - word_start;

  if (length <= 0)
    return null;

  return document.getText().substring(word_start, length);
 }
}