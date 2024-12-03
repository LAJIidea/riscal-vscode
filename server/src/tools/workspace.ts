import { SymbolTable } from "./symbol_table";


export class Workspace {
  dictionary: Map<string, SymbolTable> = new Map();

  public update(name: string, table: SymbolTable): void {
    this.dictionary.set(name, table);
  }
}