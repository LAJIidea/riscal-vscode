import { RiscalVisitor } from "../antlr4/gen/RiscalVisitor";
import { AbstractParseTreeVisitor, TerminalNode } from "antlr4ts/tree";
import { Workspace } from "../tools/workspace";
import { SymbolTable } from "../tools/symbol_table";
// import { ErrorBuilder } from "../tools/error_builder";
import { AddrspaceContext, ExprContext, FuncunitContext, Import_statementContext, InstructionContext, InterfaceContext, IssueContext, MemifContext, MemoryContext, ModuleContext, ModulesContext, OperandContext, PipelineContext, PrimaryContext, RegfileContext } from "../antlr4/gen/RiscalParser";
import { Scope, VariableDefinition } from "../tools/scopes";

export class SemanticVisitor extends AbstractParseTreeVisitor<void> implements RiscalVisitor<void> {
  table: SymbolTable;

  constructor(private workspace: Workspace, private filename: string) {
    super();
    this.table = new SymbolTable();
  }
  
  protected defaultResult(): void {
      
  }

  visitModules(ctx: ModulesContext): void {
    if (ctx.module().length === 0) {
      return;
    }

    for (let mod of ctx.module())
      this.visitModule(mod);
    this.workspace.update(this.filename, this.table);
  }

  visitModule(ctx: ModuleContext): void {
    let name = ctx.Identifier().text
    let scope = new Scope(null, name, ctx.start.line, ctx.stop.line);
    this.table.enterScope(scope);
    this.visitChildren(ctx);
  }

  visitAddrspace(ctx: AddrspaceContext): void {
    let name = ctx.Identifier().text;
    let val = new VariableDefinition(name, ctx._start.line, ctx.Identifier()._symbol.charPositionInLine, ctx._stop.line, ctx._stop.charPositionInLine);
    this.table.insert_variable(name, val);
  }

  visitMemory(ctx: MemoryContext): void {
    let name = ctx.Identifier().text;
    let val = new VariableDefinition(name, ctx._start.line, ctx.Identifier()._symbol.charPositionInLine, ctx._stop.line, ctx._stop.charPositionInLine);
    this.table.insert_variable(name, val);
  }

  visitInterface(ctx: InterfaceContext): void {
    let name = ctx.Identifier().text;
    let val = new VariableDefinition(name, ctx._start.line, ctx.Identifier()._symbol.charPositionInLine, ctx._stop.line, ctx._stop.charPositionInLine);
    this.table.insert_variable(name, val);
  }

  visitRegfile(ctx: RegfileContext): void {
    let name = ctx.Identifier().text;
    let val = new VariableDefinition(name, ctx._start.line, ctx.Identifier()._symbol.charPositionInLine, ctx._stop.line, ctx._stop.charPositionInLine);
    this.table.insert_variable(name, val);
  }

  visitPipeline(ctx: PipelineContext): void {
    let name = ctx.Identifier().text;
    let val = new VariableDefinition(name, ctx._start.line, ctx.Identifier()._symbol.charPositionInLine, ctx._stop.line, ctx._stop.charPositionInLine);
    this.table.insert_variable(name, val);
  }

  visitFuncunit(ctx: FuncunitContext): void {
    let name = ctx.Identifier().text;
    let val = new VariableDefinition(name, ctx._start.line, ctx.Identifier()._symbol.charPositionInLine, ctx._stop.line, ctx._stop.charPositionInLine);
    this.table.insert_variable(name, val);
  }

  visitIssue(ctx: IssueContext): void {
    let name = ctx.Identifier().text;
    let val = new VariableDefinition(name, ctx._start.line, ctx.Identifier()._symbol.charPositionInLine, ctx._stop.line, ctx._stop.charPositionInLine);
    this.table.insert_variable(name, val);
  } 

  visitOperand(ctx: OperandContext): void {
    let name = ctx.Identifier().text;
    let val = new VariableDefinition(name, ctx._start.line, ctx.Identifier()._symbol.charPositionInLine, ctx._stop.line, ctx._stop.charPositionInLine);
    this.table.insert_variable(name, val);
  }

  visitInstruction(ctx: InstructionContext): void {
    let name = ctx.Identifier().text;
    let val = new VariableDefinition(name, ctx._start.line, ctx.Identifier()._symbol.charPositionInLine, ctx._stop.line, ctx._stop.charPositionInLine);
    this.table.insert_variable(name, val);
  }

  visitExpr(ctx: ExprContext): void {
    
  }

  visitPrimary(ctx: PrimaryContext): void {

  }

  visitImport_statement(ctx: Import_statementContext): void {

  }
}