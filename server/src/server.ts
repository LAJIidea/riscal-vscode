import { createConnection, TextDocuments, TextDocument, Diagnostic, ProposedFeatures, InitializeParams, InitializeResult, DidChangeConfigurationNotification, Range, Position } from "vscode-languageserver";
import { Compilation } from "./tools/compilation";
import { runSafe, runSafeAsync } from "./tools/runner";

// Create a connection for the server.
let connection = createConnection(ProposedFeatures.all);

// Create a document manager.
let documents: TextDocuments = new TextDocuments();

let compilation: Compilation = new Compilation();

let hasConfigurationCapability: boolean = false;

// Initialization
connection.onInitialize((params: InitializeParams): InitializeResult => {
    let capabilities = params.capabilities;

    hasConfigurationCapability = 
        capabilities.workspace && !!capabilities.workspace.configuration;
    
    return {
        capabilities: {
            textDocumentSync: documents.syncKind,
            completionProvider: {
                resolveProvider: true,
            },
            hoverProvider: false,
            documentHighlightProvider: false,
            definitionProvider: true
        }
    };
});

connection.onInitialized(() => {
    if (hasConfigurationCapability) {
        // Register for all configuration changes.
        connection.client.register(
            DidChangeConfigurationNotification.type,
            undefined
        );
    }
});

// The riscal settings.
interface RiscalSettings {
    maxNumberOfProblems: number;
}

// Cache the settings of all open documents.
let documentSettings: Map<string, Thenable<RiscalSettings>> = new Map();

connection.onDidChangeConfiguration(change => {
    if (hasConfigurationCapability) {
        // Reset all cached document settings.
        documentSettings.clear();
    }

    // Revalidate all open text documents.
    documents.all().forEach(triggerValidation);
});

const pendingValidationRequests: { [uri: string]: NodeJS.Timer} = {};
const validationDelayMs = 0;

// The context of a text document has changed. This event is emitted
// when the text document first opened or when its content has changed.
documents.onDidChangeContent(change => {
    change.document
    triggerValidation(change.document);
});

// a document has closed: clear all diagnostics
documents.onDidClose(event => {
    cleanPendingValidation(event.document);
    connection.sendDiagnostics({uri: event.document.uri, diagnostics: []});
});

function cleanPendingValidation(textDocument: TextDocument): void {
    const request = pendingValidationRequests[textDocument.uri];
    if (request) {
        clearTimeout(request);
        delete pendingValidationRequests[textDocument.uri];
    }
}

function triggerValidation(textDocument: TextDocument): void {
    cleanPendingValidation(textDocument);
    pendingValidationRequests[textDocument.uri] = setTimeout(() => {
        delete pendingValidationRequests[textDocument.uri];
        validateTextDocument(textDocument);
    }, validationDelayMs);
}

// This handler provides the validation of document region.
async function validateTextDocument(textDocument: TextDocument): Promise<void> {
    try {
        const version = textDocument.version;
        const diagnostics: Diagnostic[] = [];
        if (textDocument.languageId === 'riscal') {
            let diagnosticsIndex: Diagnostic[] = getRiscalValidation(textDocument);
            if (diagnosticsIndex.length > 0) {
                for (let c of diagnosticsIndex) {
                    if (c != null) {
                        diagnostics.push(c);
                    }
                }
            }
            const latestTextDocument = documents.get(textDocument.uri);
            if (latestTextDocument && latestTextDocument.version === version) {
                // Check no new version has come in after the async op.
                connection.sendDiagnostics({uri: latestTextDocument.uri, diagnostics});
            }
        }
    } catch (e) {

    }
}

function getRiscalValidation(textDocument: TextDocument): Diagnostic[] {
    return compilation.update(textDocument);
}

// This handler provides the initial list of the completion items.
connection.onCompletion(async (textDocumentPosition, token) => {
  return runSafeAsync(async () => {
    const document = documents.get(textDocumentPosition.textDocument.uri);
    if (!document) {
      return null;
    }
    
    // let caret = document.getText().substring(document.offsetAt(textDocumentPosition.position)-1);
    // console.log(textDocumentPosition.textDocument.uri)
    return compilation.complete(textDocumentPosition);
    // return null;
  }, null, `Error while computing completions for ${textDocumentPosition.textDocument.uri}`, token);
});

// This handler resolve additional information for the item selected in the completion list.
connection.onCompletionResolve((item, token) => {
  return runSafe(() => {
    const data = item.data;

    return item;
  }, item, `Error while resolving completion proposcal`, token);
});

connection.onHover((textDocumentPosition, token) => {
  return runSafe(() => {
    return null;
  }, null, `Error while computing hover for ${textDocumentPosition.textDocument.uri}`, token);
});

connection.onDefinition((definitionParams, token) => {
  return runSafe(() => {
    const document = documents.get(definitionParams.textDocument.uri);
    // return compilation.find_defination(document, definitionParams.position);
    return {uri: definitionParams.textDocument.uri, range: Range.create(Position.create(1, 0), Position.create(1, 1))}
  }, null, `Error while computing definitions for ${definitionParams.textDocument.uri}`, token);
});

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();

