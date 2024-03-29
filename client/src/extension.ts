import path = require('path');
import { workspace, ExtensionContext } from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient';   

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    let serverModule = context.asAbsolutePath(
        path.join('server', 'out', 'server.js')
    );
    let debugOptions = {execArgv: ['--nolazy', '--inspect=6009']};

    let serverOptions: ServerOptions = {
        run: {module: serverModule, transport: TransportKind.ipc},
        debug: {
            module: serverModule,
            transport: TransportKind.ipc,
            options: debugOptions
        }
    };

    let clientOptions: LanguageClientOptions = {
        documentSelector: [{scheme: 'file', language: 'riscal'}],
        synchronize: {
            configurationSection: ['riscal'],
            fileEvents: workspace.createFileSystemWatcher('**/clientrc')
        }
    }

    // Create the language client and start the client
    client = new LanguageClient(
        'riscal',
        'Riscal Support',
        serverOptions,
        clientOptions
    );

    // Start the client. This will also launch the server
    client.start();

}

export function deactivate(): Thenable<void> {
    if (!client) {
        return undefined;
    }
    return client.stop();
}