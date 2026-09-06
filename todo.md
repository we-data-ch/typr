# À faire — canaux éditeur

État au 2026-09-06, après la release v0.5.9.

Six canaux sur huit servent la version courante. Les deux qui restent concernent
tous les deux l'extension d'éditeur, et se traitent séparément parce qu'ils
passent par deux registres distincts qui ne se parlent pas.

| Canal | Sert aujourd'hui | Utilisateurs concernés |
|---|---|---|
| VS Code Marketplace | **0.1.8** | VS Code |
| Open VSX | **rien** | Positron, VSCodium, Cursor, Gitpod, Theia |

Tant que ces deux points ne sont pas faits, le job `VS Code Marketplace` de la
release se termine en `success` avec un avertissement : il empaquette bien le
`.vsix` et l'attache à la release, mais ne publie nulle part. C'est une
dégradation volontaire — un canal d'éditeur ne doit pas faire échouer la
publication du compilateur — mais elle est silencieuse, d'où ce fichier.

---

## 1. VS Code Marketplace — `VSCE_PAT`

L'extension est publiée sous l'éditeur `wedata-ch` (voir `publisher` dans
`editors/vscode/package.json`). Le jeton doit appartenir à un compte qui a des
droits sur cet éditeur.

### Obtenir le jeton

Le Marketplace n'a pas de gestion de jetons propre : il s'appuie sur Azure
DevOps, ce qui explique le détour.

1. Ouvrir `https://aex.dev.azure.com/me` — **en navigation privée**. C'est le
   point d'entrée qui contourne la boucle de redirection rencontrée sur
   `https://dev.azure.com` : cette dernière tente de deviner l'organisation et
   boucle quand le compte n'en a pas encore.
2. Créer une organisation si le compte n'en a aucune (n'importe quel nom, elle
   ne sert qu'à héberger le jeton).
3. Aller sur `https://dev.azure.com/<org>/_usersSettings/tokens`.
4. **New Token**, avec exactement :
   - *Organization* : **All accessible organizations** — indispensable, un jeton
     limité à une seule organisation est refusé par `vsce` ;
   - *Scopes* : **Custom defined** → **Marketplace: Manage** ;
   - *Expiration* : 1 an (le maximum), et noter la date.
5. Copier le jeton — il n'est affiché qu'une fois.

### Poser le secret

```
gh secret set VSCE_PAT --repo we-data-ch/typr
```

Coller le jeton à l'invite : la saisie est masquée et rien ne transite par un
fichier ni par un historique de shell.

### Vérifier

Aucune modification du workflow n'est nécessaire — le job teste déjà la présence
du secret. Rejouer la publication sur le tag courant :

```
gh workflow run release.yml -f tag=v0.5.9
```

Puis confirmer que le Marketplace a bougé :

```
https://marketplace.visualstudio.com/items?itemName=wedata-ch.typr-language
```

> **Si le compte n'a pas les droits sur l'éditeur `wedata-ch`** : le
> propriétaire actuel doit ajouter le compte comme membre depuis
> `https://marketplace.visualstudio.com/manage/publishers/wedata-ch`. Sans ça,
> `vsce publish` échoue sur un 403 même avec un jeton valide.

---

## 2. Open VSX — `OVSX_PAT` + job de publication

Open VSX est un registre indépendant, géré par la fondation Eclipse. Positron,
VSCodium et Cursor y cherchent leurs extensions et **n'ont pas accès au
Marketplace** — la licence de ce dernier l'interdit aux produits non-Microsoft.
Publier sur l'un ne publie donc rien sur l'autre : ce sont deux dépôts à
alimenter, pas une redondance.

À ce jour, un utilisateur de Positron ne peut pas installer l'extension TypR du
tout.

### Obtenir le jeton

Plus simple que le Marketplace — pas de tenant Azure, pas de redirection.

1. Se connecter sur `https://open-vsx.org` **avec le compte GitHub**.
2. Accepter l'*Eclipse Publisher Agreement* (obligatoire, une seule fois, dans
   les réglages du profil).
3. *Settings* → *Access Tokens* → **Generate New Token**.

### Créer le namespace

Le nom du namespace doit correspondre au champ `publisher` du manifeste, donc
`wedata-ch`. Il n'existe pas encore et se crée en ligne de commande :

```
npx ovsx create-namespace wedata-ch -p <le-jeton>
```

À faire **une seule fois**, avant la première publication. Sans lui, `ovsx
publish` échoue sur un namespace inconnu.

### Poser le secret

```
gh secret set OVSX_PAT --repo we-data-ch/typr
```

### Ajouter le job

Contrairement au Marketplace, ce canal n'existe pas encore dans
`.github/workflows/release.yml` — il faut l'écrire. Le plus simple est de
l'ajouter au job `vscode` existant, qui empaquette déjà le `.vsix` : deux étapes
de plus, sur le même modèle de dégradation que `VSCE_PAT`.

```yaml
    env:
      VSCE_PAT: ${{ secrets.VSCE_PAT }}
      OVSX_PAT: ${{ secrets.OVSX_PAT }}   # ← ajouter ici

      # ... après « Publish to the Marketplace » :

      - name: Publish to Open VSX
        if: env.OVSX_PAT != ''
        run: npx ovsx publish typr-${{ needs.verify.outputs.version }}.vsix -p "$OVSX_PAT"

      - name: OVSX_PAT absent — publication ignorée
        if: env.OVSX_PAT == ''
        run: |
          echo "::warning title=Open VSX non mis a jour::OVSX_PAT absent ; Positron et VSCodium ne verront pas cette version."
```

Le `env` doit rester au niveau du **job** : un `env` de step n'est pas visible
depuis le `if` de ce même step. C'est la raison de la structure actuelle, à ne
pas « simplifier ».

Réutiliser le `.vsix` déjà produit plutôt que d'en empaqueter un second garantit
que les deux registres servent un artefact **bit pour bit identique** à celui
attaché à la release.

### Vérifier

```
https://open-vsx.org/extension/wedata-ch/typr-language
```

---

## Point annexe repéré au passage — licences incohérentes

Deux choses à trancher avant la première publication Open VSX, qui affiche la
licence sur la page de l'extension.

1. **Le manifeste ne déclare pas de licence.** `editors/vscode/package.json` n'a
   pas de champ `license`, alors qu'un fichier `LICENSE` est présent à côté.
   Open VSX affiche « unlicensed » dans ce cas, ce qui décourage l'installation.

2. **Les deux licences ne concordent pas.** Le workspace Rust déclare
   `license = "Apache-2.0"` dans `Cargo.toml`, tandis que
   `editors/vscode/LICENSE` porte un texte **MIT** (« Copyright (c) 2024 typR »).
   L'extension et le compilateur qu'elle pilote sont donc sous deux licences
   différentes, sans que rien ne l'explique.

Ce n'est pas bloquant, mais c'est le genre d'écart qui se remarque une fois
publié et se corrige mal après coup — les deux registres archivent la licence
déclarée à chaque version. Décider laquelle fait foi, aligner le fichier et le
champ, puis ajouter au manifeste :

```json
"license": "Apache-2.0",
```

— ou `"MIT"`, selon la décision.
