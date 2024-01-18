"use strict";
exports.__esModule = true;
exports.deactivate = exports.activate = void 0;
var path = require("path");
var vscode_1 = require("vscode");
var vscode_languageclient_1 = require("vscode-languageclient");
var client;
function activate(context) {
    var serverModule = context.asAbsolutePath(path.join('server', 'out', 'server.js'));
    var debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };
    var serverOptions = {
        run: { module: serverModule, transport: vscode_languageclient_1.TransportKind.ipc },
        debug: {
            module: serverModule,
            transport: vscode_languageclient_1.TransportKind.ipc,
            options: debugOptions
        }
    };
    var clientOptions = {
        documentSelector: [{ scheme: 'file', language: 'riscal' }],
        synchronize: {
            configurationSection: ['riscal'],
            fileEvents: vscode_1.workspace.createFileSystemWatcher('**/clientrc')
        }
    };
    // Create the language client and start the client
    client = new vscode_languageclient_1.LanguageClient('riscal', 'Riscal Support', serverOptions, clientOptions);
    // Start the client. This will also launch the server
    client.start();
}
exports.activate = activate;
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
exports.deactivate = deactivate;
