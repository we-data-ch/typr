# Canaux éditeur — état au 2026-09-07

Deux canaux sur huit servaient une vieille version ou rien ; ils passent par deux
registres distincts qui ne se parlent pas. Les deux servent désormais la version
courante, `v0.5.9`.

| Canal               | Sert aujourd'hui | Vérifié le                    | Utilisateurs concernés                    |
|---------------------|------------------|-------------------------------|-------------------------------------------|
| VS Code Marketplace | **0.5.9**        | 2026-09-07 (publié à 12:41 Z) | VS Code                                   |
| Open VSX            | **0.5.9**        | 2026-09-07 (publié à 13:15 Z) | Positron, VSCodium, Cursor, Gitpod, Theia |

## Ce qui a été fait

1. **Jetons posés** — `VSCE_PAT` et `OVSX_PAT` sont des secrets GitHub depuis le
   2026-09-07.
2. **Job** — `.github/workflows/release.yml` publie le `.vsix` déjà empaqueté sur
   les deux registres. Les étapes Open VSX portent `if: always()` : un échec du
   Marketplace (ex. republier une version déjà en ligne) ne doit pas bloquer la
   publication Open VSX. Vérifié en conditions réelles sur le run 34126270113 —
   Marketplace en échec, Open VSX en succès dans le même job.
3. **Licences** — `editors/vscode/LICENSE` et le manifeste sont alignés sur
   `Apache-2.0`. Voir la réserve ci-dessous : ce n'est pas encore visible en
   ligne.
4. **Lock** — `package-lock.json` porte la même version que `package.json`, et
   `nu publish.nu sync` l'y maintient.

## Reste à faire — la licence n'est pas encore visible en ligne

Open VSX affiche toujours l'extension comme **unlicensed** (`license: null` dans
`https://open-vsx.org/api/wedata-ch/typr-language`).

Ce n'est pas un oubli : le job de release fait un `checkout` **au tag**, et
`v0.5.9` est antérieur au commit qui ajoute `"license": "Apache-2.0"` au
manifeste. Le `.vsix` publié sur les deux registres est donc celui d'avant
l'alignement. Seul le workflow, lui, vient de la branche depuis laquelle la
release est relancée — d'où un job qui connaît Open VSX alors que le tag ne le
connaît pas.

**Rien à corriger** : republier `v0.5.9` avec la licence supposerait de déplacer
le tag, ce qui change un artefact déjà distribué. La `v0.5.10` portera la licence
sans intervention. À revérifier après cette release :

```
curl -s https://open-vsx.org/api/wedata-ch/typr-language | grep license
```

## Vérifier

Marketplace :

```
https://marketplace.visualstudio.com/items?itemName=wedata-ch.typr-language
```

Open VSX :

```
https://open-vsx.org/extension/wedata-ch/typr-language
```

En ligne de commande, sans navigateur ni jeton :

```
curl -s https://open-vsx.org/api/wedata-ch/typr-language | grep -E '"version"|"license"'

curl -s -X POST https://marketplace.visualstudio.com/_apis/public/gallery/extensionquery \
  -H 'Content-Type: application/json' \
  -H 'Accept: application/json;api-version=7.2-preview.1' \
  -d '{"filters":[{"criteria":[{"filterType":7,"value":"wedata-ch.typr-language"}],"pageSize":1,"pageNumber":1}],"flags":950}'
```

## Pièges passés, à connaître pour la prochaine rotation de jeton

### `VSCE_PAT` — compte et portée

- Un jeton Azure DevOps **limité à une seule organisation** est refusé par
  `vsce` : choisir **All accessible organizations**.
- La portée doit être **Marketplace: Manage**.
- Sans les droits sur l'éditeur `wedata-ch`, `vsce publish` échoue en 403 même
  avec un jeton valide. Le jeton doit appartenir au **même compte** que celui
  qui a créé l'éditeur `wedata-ch` ; un autre compte doit être ajouté comme
  membre par le propriétaire depuis
  `https://marketplace.visualstudio.com/manage/publishers/wedata-ch`.
- Créé le 2026-09-07 avec une expiration à un an : il tombe donc vers le
  **2027-09-07**. Le renouveler avant, en repassant par
  `https://aex.dev.azure.com/me` puis
  `https://dev.azure.com/<org>/_usersSettings/tokens`.

### `OVSX_PAT` et namespace

- Le namespace `wedata-ch` existe désormais sur Open VSX — ne pas le recréer.
- `ovsx` est épinglé dans les `devDependencies` de l'extension, pour que `npm ci`
  en installe une version connue plutôt que d'en tirer une au hasard au moment de
  la release.
- Sans namespace, `ovsx publish` échoue sur un namespace inconnu.
- La propagation du registre prend quelques secondes après la publication : ne
  pas s'alarmer d'un 404 immédiat.

### Rejouer une release

- `gh workflow run release.yml -f tag=v0.5.9` régénère le `.vsix`, le réattache à
  la release et republie. Comme v0.5.9 est déjà sur le Marketplace, `vsce publish`
  échoue (`wedata-ch.typr-language v0.5.9 already exists.`) : normal et attendu.
  Le job finit donc en `failure` sur un rejeu, mais le canal Open VSX continue de
  tourner grâce à `if: always()`.
- Le rejeu prend le **workflow de la branche** choisie pour le dispatch, mais le
  **code du tag**. Une correction de workflow prend donc effet tout de suite, une
  correction de source seulement au tag suivant.
- C'est le job `verify`, pas le job `vscode`, qui vérifie que le tag et les
  versions de `Cargo.toml`, de `package.json` et de la `DESCRIPTION` RStudio
  concordent ; les autres jobs en dépendent (`needs: verify`).
