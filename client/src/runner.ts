import * as vscode from "vscode";


export namespace Runner {
    export function registerCommands(context: vscode.ExtensionContext) {
        context.subscriptions.push(
            vscode.commands.registerCommand('riscal.runCode', () => {

            })
        );
    }
}