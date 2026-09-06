import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;
let outputChannel: vscode.OutputChannel;
let statusBarItems: vscode.StatusBarItem[] = [];

export function activate(context: vscode.ExtensionContext) {
    outputChannel = vscode.window.createOutputChannel('typR');
    outputChannel.appendLine('typR extension activated');

    // Start LSP client if enabled
    const config = vscode.workspace.getConfiguration('typr');
    if (config.get<boolean>('enableLsp', true)) {
        startLanguageClient(context);
    }

    // Register commands
    context.subscriptions.push(
        vscode.commands.registerCommand('typr.check', () => runTyprCommand('check')),
        vscode.commands.registerCommand('typr.build', () => runTyprCommand('build')),
        vscode.commands.registerCommand('typr.run', () => runTyprCommand('run')),
        vscode.commands.registerCommand('typr.test', () => runTyprCommand('test')),
        vscode.commands.registerCommand('typr.checkFile', () => runTyprCommandOnFile('check')),
        vscode.commands.registerCommand('typr.buildFile', () => runTyprCommandOnFile('build')),
        vscode.commands.registerCommand('typr.runFile', () => runTyprCommandOnFile('run')),
        vscode.commands.registerCommand('typr.testFile', () => runTyprCommandOnFile('test')),
        vscode.commands.registerCommand('typr.restartLsp', () => restartLanguageClient(context)),
        // Menu commands for status bar buttons
        vscode.commands.registerCommand('typr.showCheckMenu', () => showCommandMenu('check')),
        vscode.commands.registerCommand('typr.showBuildMenu', () => showCommandMenu('build')),
        vscode.commands.registerCommand('typr.showRunMenu', () => showCommandMenu('run')),
        vscode.commands.registerCommand('typr.showTestMenu', () => showCommandMenu('test'))
    );

    // Create status bar items
    createStatusBarItems(context);

    // Watch for configuration changes
    context.subscriptions.push(
        vscode.workspace.onDidChangeConfiguration(e => {
            if (e.affectsConfiguration('typr.enableLsp')) {
                const newConfig = vscode.workspace.getConfiguration('typr');
                if (newConfig.get<boolean>('enableLsp', true)) {
                    if (!client) {
                        startLanguageClient(context);
                    }
                } else {
                    stopLanguageClient();
                }
            }
            if (e.affectsConfiguration('typr.showStatusBarButtons')) {
                createStatusBarItems(context);
            }
        })
    );

    outputChannel.appendLine('typR commands registered');
}

function startLanguageClient(context: vscode.ExtensionContext) {
    const config = vscode.workspace.getConfiguration('typr');
    const typrPath = config.get<string>('path', 'typr');

    const serverOptions: ServerOptions = {
        command: typrPath,
        args: ['lsp']
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'typr' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.ty')
        },
        outputChannel: outputChannel,
        traceOutputChannel: outputChannel,
        diagnosticCollectionName: 'typr'
    };

    client = new LanguageClient(
        'typrLanguageServer',
        'typR Language Server',
        serverOptions,
        clientOptions,
        false // Disable pull diagnostics (use push model only)
    );

    client.start().then(() => {
        outputChannel.appendLine('typR Language Server started');
    }).catch((error) => {
        outputChannel.appendLine(`Failed to start typR Language Server: ${error}`);
        vscode.window.showErrorMessage(
            `Failed to start typR Language Server. Make sure '${typrPath}' is installed and accessible.`
        );
    });

    context.subscriptions.push({
        dispose: () => {
            if (client) {
                client.stop();
            }
        }
    });
}

async function stopLanguageClient() {
    if (client) {
        await client.stop();
        client = undefined;
        outputChannel.appendLine('typR Language Server stopped');
    }
}

async function restartLanguageClient(context: vscode.ExtensionContext) {
    outputChannel.appendLine('Restarting typR Language Server...');
    await stopLanguageClient();
    startLanguageClient(context);
    vscode.window.showInformationMessage('typR Language Server restarted');
}

function getTyprPath(): string {
    const config = vscode.workspace.getConfiguration('typr');
    return config.get<string>('path', 'typr');
}

function getWorkspaceFolder(): string | undefined {
    const workspaceFolders = vscode.workspace.workspaceFolders;
    if (workspaceFolders && workspaceFolders.length > 0) {
        return workspaceFolders[0].uri.fsPath;
    }
    return undefined;
}

async function runTyprCommand(command: 'check' | 'build' | 'run' | 'test') {
    const typrPath = getTyprPath();
    const workspaceFolder = getWorkspaceFolder();

    if (!workspaceFolder) {
        vscode.window.showErrorMessage('No workspace folder open. Please open a folder containing your typR project.');
        return;
    }

    const terminal = getOrCreateTerminal();
    terminal.show();
    terminal.sendText(`cd "${workspaceFolder}" && ${typrPath} ${command}`);

    outputChannel.appendLine(`Running: ${typrPath} ${command} in ${workspaceFolder}`);
}

async function runTyprCommandOnFile(command: 'check' | 'build' | 'run' | 'test') {
    const editor = vscode.window.activeTextEditor;
    
    if (!editor) {
        vscode.window.showErrorMessage('No active editor. Please open a .ty file.');
        return;
    }

    const document = editor.document;
    
    if (document.languageId !== 'typr') {
        vscode.window.showErrorMessage('Current file is not a typR file (.ty).');
        return;
    }

    // Save the file before running
    if (document.isDirty) {
        await document.save();
    }

    const filePath = document.uri.fsPath;
    const typrPath = getTyprPath();

    const terminal = getOrCreateTerminal();
    terminal.show();
    terminal.sendText(`${typrPath} ${command} "${filePath}"`);

    outputChannel.appendLine(`Running: ${typrPath} ${command} "${filePath}"`);
}

let typrTerminal: vscode.Terminal | undefined;

function getOrCreateTerminal(): vscode.Terminal {
    // Check if our terminal still exists
    if (typrTerminal) {
        const terminals = vscode.window.terminals;
        if (terminals.includes(typrTerminal)) {
            return typrTerminal;
        }
    }

    // Create a new terminal
    typrTerminal = vscode.window.createTerminal({
        name: 'typR',
        iconPath: new vscode.ThemeIcon('terminal')
    });

    return typrTerminal;
}

// Status bar button definitions
interface ToolButton {
    command: string;
    tooltip: string;
    icon: string;
    text: string;
}

const toolButtons: ToolButton[] = [
    { command: 'typr.showCheckMenu', tooltip: 'typR Check', icon: '$(checklist)', text: 'Check' },
    { command: 'typr.showBuildMenu', tooltip: 'typR Build', icon: '$(package)', text: 'Build' },
    { command: 'typr.showRunMenu', tooltip: 'typR Run', icon: '$(play)', text: 'Run' },
    { command: 'typr.showTestMenu', tooltip: 'typR Test', icon: '$(beaker)', text: 'Test' }
];

function createStatusBarItems(context: vscode.ExtensionContext) {
    // Dispose existing items
    statusBarItems.forEach(item => item.dispose());
    statusBarItems = [];

    const config = vscode.workspace.getConfiguration('typr');
    if (!config.get<boolean>('showStatusBarButtons', true)) {
        return;
    }

    // Create status bar items with priority to keep them grouped
    // Higher priority = more to the left
    let priority = 100;
    
    for (const button of toolButtons) {
        const item = vscode.window.createStatusBarItem(
            vscode.StatusBarAlignment.Left,
            priority--
        );
        item.command = button.command;
        item.tooltip = button.tooltip;
        item.text = `${button.icon} ${button.text}`;
        context.subscriptions.push(item);
        statusBarItems.push(item);
    }

    // Update visibility based on active editor
    updateStatusBarVisibility();

    // Listen for editor changes
    context.subscriptions.push(
        vscode.window.onDidChangeActiveTextEditor(() => updateStatusBarVisibility())
    );
}

function updateStatusBarVisibility() {
    const editor = vscode.window.activeTextEditor;
    const isTyprFile = editor?.document.languageId === 'typr';

    for (const item of statusBarItems) {
        if (isTyprFile) {
            item.show();
        } else {
            item.hide();
        }
    }
}

async function showCommandMenu(command: 'check' | 'build' | 'run' | 'test') {
    const commandLabels: Record<string, string> = {
        check: 'Check',
        build: 'Build',
        run: 'Run',
        test: 'Test'
    };

    const items: vscode.QuickPickItem[] = [
        {
            label: `$(folder) ${commandLabels[command]} Project`,
            description: `typr ${command}`,
            detail: 'Run on the entire project'
        },
        {
            label: `$(file) ${commandLabels[command]} Current File`,
            description: `typr ${command} [current file]`,
            detail: 'Run on the currently open file'
        }
    ];

    const selected = await vscode.window.showQuickPick(items, {
        placeHolder: `Select ${commandLabels[command]} target`
    });

    if (!selected) {
        return;
    }

    if (selected.label.includes('Project')) {
        await runTyprCommand(command);
    } else {
        await runTyprCommandOnFile(command);
    }
}

export function deactivate(): Thenable<void> | undefined {
    // Dispose status bar items
    statusBarItems.forEach(item => item.dispose());
    statusBarItems = [];

    if (!client) {
        return undefined;
    }
    outputChannel.appendLine('typR extension deactivated');
    return client.stop();
}
