// import { Symbol, BuiltinInTypeSymbol } from "./symbols";

export class VariableDefinition {
  // constructor(public symbol: Symbol, public startLine: number, public endLine: number) {}

  constructor(public name: string, public startLine: number, public startCharacter: number, public endLine: number, public endCharacter: number,) {}

  nameEquals(name: string): boolean {
    return this.name === name;
  }

  inScope(line: number): boolean {
    return line >= this.startLine && line <= this.endLine;
  }

  toString() {
    return `{ ${this.name} from: ${this.startLine} to: ${this.endLine} }`;
  }
}

const MAX_LINE = 65536;

export class Scope {
  dictionary: Map<string, VariableDefinition> = new Map();
  children: Scope[] = [];

  constructor(public parentScope: Scope, public name: string, public startLine = 1, public endLine = MAX_LINE,
    public args: string[] = null, public types: string[] = null) {}

  public insert_variable(name: string, val: VariableDefinition): void {
    this.dictionary.set(name, val);
  }

  public lookup(name: string): VariableDefinition {
    if (this.dictionary.has(name))
      return this.dictionary.get(name);

    if (this.parentScope) {
      let val = this.parentScope.lookup(name);
      if (val)
        return val
    }

    return null;
  }

}