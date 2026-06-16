# Rapporter un bug TypR — créer un cas reproductible

Ce guide explique comment transformer un bug rencontré en développant ton package TypR en un
**cas reproductible** (`<slug>.case/`) que tu peux joindre à ton rapport. Tu n'as besoin que de la
CLI `typr` — aucun accès au code source de `typr` n'est requis.

---

## 1. Reproduis le bug dans ton projet

Assure-toi que le problème se reproduit avec un projet TypR aussi petit que possible (idéalement
quelques fichiers dans `TypR/`). Plus la repro est minimale, plus le bug sera facile à corriger.

## 2. Annote le code fautif avec un marqueur `#@case`

Ajoute un commentaire TypR (les commentaires TypR commencent par `#`, **pas** `//`) juste à côté du
code qui pose problème :

```typr
#@case pub-fn-in-module: g.default émis au lieu de g dans un module
@pub let g <- fn(): int { 1 };
```

- avant le `:` → le **slug** (nom court du cas, en kebab-case)
- après le `:` → la **description** (deviendra le titre)
- la position du commentaire → la **localisation** (fichier + ligne), capturée automatiquement

## 3. (Optionnel) Détaille dans `.case.md`

Si tu veux écrire plus que le marqueur (ce qui *devrait* se passer, les symptômes…), crée un
fichier `.case.md` à la racine de ton projet. Si tu ne le crées pas, l'étape suivante en génère un
squelette que tu pourras compléter.

## 4. Crée le snapshot

Depuis **la racine de ton projet** :

```bash
typr case snapshot
# ou en forçant le slug :  typr case snapshot pub-fn-in-module
```

Ça produit un bundle auto-portant `<slug>.case/` dans ton projet :

```
pub-fn-in-module.case/
  repro/        # copie minimale de tes sources : TypR/ + DESCRIPTION/NAMESPACE
                #   (sans renv/, .git/, etc.)
  case.toml     # titre + commande de rejeu (build) + statut
  expect.md     # ta description + la localisation du marqueur
  expect.toml   # squelette de règles (laissé à remplir par un mainteneur)
  observed.txt  # le R réellement généré + les erreurs, capturés au moment du snapshot
```

## 5. Vérifie le bundle

Ouvre `<slug>.case/observed.txt` : tu dois y retrouver le R généré et/ou l'erreur qui illustrent
le bug. C'est la preuve que ton snapshot capture bien le problème.

## 6. Partage le cas

Joins le dossier `<slug>.case/` à ton rapport de bug (par exemple en l'archivant en `.zip` et en
l'attachant à une issue). Il contient tout le nécessaire pour reproduire le problème à l'identique.

---

**En résumé** : `#@case` dans le code → `typr case snapshot` → tu obtiens un `<slug>.case/`
complet → tu le partages. C'est tout.
