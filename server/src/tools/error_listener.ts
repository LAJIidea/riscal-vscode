import { ANTLRErrorListener, CommonToken, Token, Recognizer, RecognitionException } from "antlr4ts";
import { Override } from "antlr4ts/Decorators";
import { ParseErrorLevel, ParserError } from "./types";

export class ErrorListener implements ANTLRErrorListener<CommonToken> {
  errors: ParserError[] = [];

  addError(error: ParserError) {
    this.errors.push(error);
  }

  @Override
  syntaxError<T extends Token>(
    recognizer: Recognizer<T, any>, 
    offendingSymbol: T, 
    line: number, 
    charPositionInLine: number, 
    msg: string, 
    e: RecognitionException | undefined
  ): void {
    this.errors.push({
      line: line -1,
      start: charPositionInLine,
      end: charPositionInLine + offendingSymbol.text.length,
      message: msg,
      level: ParseErrorLevel.ERROR
    });
  }
}