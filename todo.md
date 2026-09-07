# À faire — canaux éditeur

État au 2026-09-07, après la release v0.5.9.

Six canaux sur huit servent la version courante. Les deux qui restent concernent
tous les deux l'extension d'éditeur, et se traitent séparément parce qu'ils
passent par deux registres distincts qui ne se parlent pas.

| Canal | Sert aujourd'hui | Utilisateurs concernés |
|---|---|---|
| VS Code Marketplace | **0.1.8** | VS Code |
| Open VSX | **rien** | Positron, VSCodium, Cursor, Gitpod, Theia |

Le travail de dépôt est fait : le job `vscode` de `release.yml` publie désormais
sur les deux registres, et la licence de l'extension est alignée. **Ne restent
que les actions manuelles ci-dessous** — obtenir deux jetons et poser deux
secrets ; elles passent par un navigateur et un compte, rien d'automatisable ici.

Tant qu'elles ne sont pas faites, le job se termine en `success` avec un
avertissement par canal : il empaquette bien le `.vsix` et l'attache à la
release, mais ne publie nulle part. C'est une dégradation volontaire — un canal
d'éditeur ne doit pas faire échouer la publication du compilateur — mais elle est
silencieuse, d'où ce fichier.

---

## 1. VS Code Marketplace — `VSCE_PAT`  *(à faire)*

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

## 2. Open VSX — `OVSX_PAT` + namespace  *(job fait, jeton à faire)*

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

### Le job — **fait**

Le job `vscode` de `.github/workflows/release.yml` porte maintenant les deux
canaux : `OVSX_PAT` est déclaré dans son `env` (au niveau du **job**, un `env` de
step n'étant pas visible depuis le `if` de ce même step), et deux étapes
`Publish to Open VSX` / avertissement suivent le même modèle de dégradation que
le Marketplace.

Elles republient le `.vsix` **déjà empaqueté** plutôt que d'en produire un
second : les deux registres servent ainsi un artefact bit pour bit identique à
celui attaché à la release.

`ovsx` est épinglé dans les `devDependencies` de l'extension, comme `@vscode/vsce`,
pour que `npm ci` en installe une version connue au lieu d'en tirer une au hasard
au moment de la release.

### Vérifier

```
https://open-vsx.org/extension/wedata-ch/typr-language
```

---

## Point annexe repéré au passage — licences  *(fait)*

Le manifeste ne déclarait pas de licence, et `editors/vscode/LICENSE` portait un
texte **MIT** là où le workspace Rust, le `LICENSE` racine et le README disent
`Apache-2.0`. Open VSX affiche la licence sur la page de l'extension et archive
celle déclarée à chaque version : l'écart se corrige mal après coup.

Tout est aligné sur **Apache-2.0** : `editors/vscode/LICENSE` reprend le texte du
dépôt et le manifeste déclare `"license": "Apache-2.0"`.

## Point annexe repéré au passage — `package-lock.json` désynchronisé  *(fait)*

Le lock de l'extension annonçait encore `vscode_extension` en `1.0.0` : `nu
publish.nu sync` mettait à jour `package.json` mais pas le lock, qui dérivait à
chaque version. `sync-editors` propage désormais la version aux deux, et le lock
a été régénéré.
