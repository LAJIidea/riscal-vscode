import { Scope, VariableDefinition } from "./scopes";
import { Symbol } from "./symbols";

export class SymbolTable implements SymbolTable {

  allScopes: Scope[] = [];

  public enterScope(scope: Scope): void {
    this.allScopes.push(scope);
    if (this.currentScope)
      this.currentScope.children.push(scope);
    this.currentScope = scope;
  }

  public exitScope(): void {
    this.currentScope = this.currentScope.parentScope;
  }

  public insert_variable(name: string, val: VariableDefinition): void {
    this.currentScope.insert_variable(name, val);
  }

  public lookup(name: string): VariableDefinition {
    return this.currentScope.lookup(name);
  }

  currentScope: Scope;
}