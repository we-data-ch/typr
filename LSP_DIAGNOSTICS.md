# LSP Diagnostics - Guide d'utilisation

## Nouvelles fonctionnalités

Le serveur LSP de TypR supporte maintenant les **diagnostics en temps réel** ! 🎉

### Fonctionnalités implémentées

✅ **Détection d'erreurs de syntaxe** - Le LSP détecte les erreurs de parsing
✅ **Détection d'erreurs de types** - Le LSP détecte les incompatibilités de types
✅ **Feedback en temps réel** - Les erreurs apparaissent pendant que vous tapez
✅ **Nettoyage automatique** - Les diagnostics disparaissent quand le code est corrigé

## Configuration dans votre éditeur

### VS Code

Ajoutez cette configuration à votre `.vscode/settings.json` :

```json
{
  "typr.lsp.diagnostics": true
}
```

### Neovim

Configuration avec `nvim-lspconfig` :

```lua
require('lspconfig').typr.setup({
  capabilities = vim.lsp.protocol.make_client_capabilities(),
})
```

## Exemples de diagnostics

### Erreur de type

```typr
// ❌ Erreur : Type mismatch
let x: int <- "hello";
//            ^^^^^^^ ERROR: Type error detected
```

### Variable non définie

```typr
// ❌ Erreur : Variable non définie
let y <- unknown_variable;
//       ^^^^^^^^^^^^^^^^ ERROR: Undefined variable
```

### Code valide

```typr
// ✅ Pas d'erreur
let z: int <- 42;
```

## Architecture technique

### Comment ça fonctionne ?

1. **did_open / did_change** : Quand un fichier est ouvert ou modifié
2. **compute_diagnostics** : Parse et type-check le code
3. **catch_unwind** : Capture les panics (erreurs)
4. **extract_diagnostic** : Convertit en diagnostic LSP
5. **publish_diagnostics** : Envoie au client

### ✅ Améliorations récentes (v2)

- ✅ **Extraction de position précise** : Les diagnostics extraient maintenant la ligne et colonne depuis les messages miette
- ✅ **Messages nettoyés** : Les messages d'erreur sont extraits proprement sans formatage ANSI
- ✅ **Support format miette** : Parse correctement les messages formatés comme `╭─[fichier:ligne:col]`

### Limitations actuelles

⚠️ **Une erreur à la fois** : Seule la première erreur qui cause un panic est capturée
⚠️ **Longueur du token** : La longueur exacte du token en erreur est approximative

### Améliorations futures possibles

- Modifier le type checker pour retourner une liste d'erreurs (au lieu de paniquer)
- Améliorer la détection de la longueur du token en erreur
- Ajouter des quick fixes et suggestions de correction

## Tests

Le LSP inclut maintenant des tests unitaires :

```bash
# Tester tous les tests LSP
cargo test interface::lsp::tests

# Tester un test spécifique
cargo test test_check_valid_code
```

### Tests disponibles

- ✅ `test_strip_ansi_codes` - Suppression des codes de couleur
- ✅ `test_strip_ansi_codes_no_ansi` - Texte sans ANSI
- ✅ `test_clean_error_message` - Nettoyage des messages d'erreur
- ✅ `test_clean_error_message_miette_format` - Parsing format miette (NEW)
- ✅ `test_extract_position_from_miette_error` - Extraction position depuis miette (NEW)
- ✅ `test_offset_to_position` - Conversion offset → position
- ✅ `test_check_valid_code` - Vérification de code valide
- ✅ `test_check_syntax_error` - Détection d'erreurs de syntaxe
- ✅ `test_check_type_error` - Détection d'erreurs de type

## Fichier de test

Un fichier de test est disponible : `test_lsp_errors.ty`

```bash
# Ouvrir avec le LSP pour voir les diagnostics
code test_lsp_errors.ty
```

## Commandes LSP

```bash
# Démarrer le serveur LSP
typr lsp

# Builder et démarrer
cargo build --release && ./target/release/typr lsp
```

## Contribution

Pour améliorer les diagnostics :

1. **Améliorer l'extraction de position** : Modifier `extract_position_from_error()`
2. **Ajouter plus de types de diagnostics** : Warnings, hints, etc.
3. **Ajouter des quick fixes** : Suggestions de correction automatique
4. **Parser les messages miette** : Extraire les vraies positions depuis les erreurs

## Questions / Problèmes

Si vous rencontrez des problèmes :

1. Vérifier que le LSP démarre : `typr lsp`
2. Vérifier les logs du serveur LSP
3. Tester avec le fichier `test_lsp_errors.ty`
4. Ouvrir une issue sur GitHub

---

**Note** : Cette fonctionnalité a été implémentée avec l'approche "Option A - Simple" qui capture les panics.
Une amélioration future (Option B) pourrait refactorer le type checker pour retourner des Result au lieu de paniquer.
