import { createConnection, TextDocuments, TextDocument, Diagnostic, ProposedFeatures, InitializeParams, ConfigurationParams, ConfigurationRequest, InitializeResult, DiagnosticSeverity, DidChangeConfigurationNotification } from "vscode-languageserver";

// Create a connection for the server.
let connection = createConnection(ProposedFeatures.all);

// Create a document manager.
let documents: TextDocuments = new TextDocuments();

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
            hoverProvider: true,
            documentHighlightProvider: true,
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
    let diagnostics: Diagnostic[] = [];
    return diagnostics;
}

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();

