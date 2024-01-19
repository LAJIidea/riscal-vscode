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