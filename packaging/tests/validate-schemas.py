#!/usr/bin/env python3
"""TypR — validation des manifestes WinGet et Scoop contre les schémas officiels.

    packaging/tests/validate-schemas.py <dossier-rendu>

WinGet refuse en amont toute pull request dont un manifeste ne passe pas son
JSON Schema, et le dépôt `scoop` attire des manifests invalides de toutes
sortes. Valider ici, avant la publication, coûte deux secondes ; se faire refuser
par un dépôt coûte une release.

Les schémas ne sont pas écrits dans ce dépôt : ce sont ceux de winget-pkgs et de
Scoop, telechargés une fois et mis en cache. `winget-pkgs` n'accepte que les
schemas qu'il publie lui-meme — un schema recopie dans notre dépôt deriverait
dès la version suivante, en validant moins que ce qui sera exige.

Sorties :
    0  tout est valide
    1  au moins un manifeste est invalide
    2  validation impossible (PyYAML absent, schema indisponible hors ligne)

Options :
    --schemas <dossier>   cache des schemas (defaut : $TYPR_SCHEMA_CACHE)
    --offline             n'echoue pas si un schema manque, il saute le controle
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path

SCOOP_SCHEMA_URL = (
    "https://raw.githubusercontent.com/ScoopInstaller/Scoop/master/schema.json"
)
WINGET_SCHEMA_TMPL = "https://aka.ms/winget-manifest.{kind}.{version}.schema.json"

# WinGet nomme ses trois manifestes `version`, `installer`, `defaultLocale`.
# `locale` est l'ancien nom, encore accepté par le dépôt : on le tolère aussi,
# pour qu'une version de Winget plus ancienne n'explique pas le refus autrement.
WINGET_KIND_BY_TYPE = {
    "version": "version",
    "installer": "installer",
    "defaultLocale": "defaultLocale",
    "locale": "defaultLocale",
}


def default_cache() -> Path:
    env = os.environ.get("TYPR_SCHEMA_CACHE")
    if env:
        return Path(env)
    base = os.environ.get("XDG_CACHE_HOME") or (Path.home() / ".cache")
    return Path(base) / "typr-packaging" / "schemas"


def fetch(url: str, dest: Path, offline: bool) -> Path | None:
    """Ramène le schéma `url` dans `dest`, ou None si impossible et hors ligne."""
    if dest.is_file():
        return dest
    if offline:
        print(f"   (hors ligne) schéma absent du cache : {url}")
        return None
    dest.parent.mkdir(parents=True, exist_ok=True)
    try:
        with tempfile.NamedTemporaryFile(
            dir=dest.parent, delete=False, suffix=".part"
        ) as tmp:
            tmp_path = Path(tmp.name)
        subprocess.run(
            ["curl", "-fsSL", url, "-o", str(tmp_path)],
            check=True,
            capture_output=True,
        )
    except (OSError, subprocess.CalledProcessError) as exc:
        detail = getattr(exc, "stderr", b"") or b""
        print(
            f"   téléchargement impossible : {url}\n"
            f"   {detail.decode(errors='replace').strip() or exc}",
            file=sys.stderr,
        )
        tmp_path.unlink(missing_ok=True)
        return None
    tmp_path.replace(dest)
    return dest


def load_schema(url: str, cache: Path, offline: bool) -> dict | None:
    dest = cache / url.rsplit("/", 1)[-1]
    path = fetch(url, dest, offline)
    if path is None:
        return None
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as exc:
        print(f"   schéma illisible : {path} ({exc})", file=sys.stderr)
        return None


def check(instance, schema: dict, label: str) -> bool:
    """Valide une instance ; renvoie True si elle est valide."""
    try:
        import jsonschema
    except ImportError:
        print("   jsonschema absent — pip install jsonschema", file=sys.stderr)
        raise

    validator_cls = jsonschema.validators.validator_for(schema)
    validator_cls.check_schema(schema)
    errors = sorted(
        validator_cls(schema).iter_errors(instance),
        key=lambda e: list(getattr(e, "absolute_path", [])),
    )
    if not errors:
        print(f"   ✓ {label}")
        return True
    print(f"   ✗ {label}")
    for err in errors[:20]:
        where = "/".join(str(p) for p in err.absolute_path) or "(racine)"
        print(f"       {where} : {err.message}")
    if len(errors) > 20:
        print(f"       … et {len(errors) - 20} autres")
    return False


def validate_winget(path: Path, cache: Path, offline: bool) -> bool | None:
    import yaml

    try:
        doc = yaml.safe_load(path.read_text(encoding="utf-8"))
    except yaml.YAMLError as exc:
        print(f"   ✗ {path.name} : YAML illisible — {exc}")
        return False
    if not isinstance(doc, dict):
        print(f"   ✗ {path.name} : la racine n'est pas un objet")
        return False

    kind = WINGET_KIND_BY_TYPE.get(doc.get("ManifestType", ""))
    if kind is None:
        print(
            f"   ✗ {path.name} : ManifestType inconnu "
            f"({doc.get('ManifestType')!r})"
        )
        return False

    version = str(doc.get("ManifestVersion", ""))
    url = WINGET_SCHEMA_TMPL.format(kind=kind, version=version)
    schema = load_schema(url, cache, offline)
    if schema is None:
        return None
    return check(doc, schema, f"{path.name} ({kind} {version or '?'})")


def validate_scoop(path: Path, cache: Path, offline: bool) -> bool | None:
    try:
        doc = json.loads(path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as exc:
        print(f"   ✗ {path.name} : JSON illisible — {exc}")
        return False
    schema = load_schema(SCOOP_SCHEMA_URL, cache, offline)
    if schema is None:
        return None
    return check(doc, schema, path.name)


def main() -> int:
    parser = argparse.ArgumentParser(add_help=True)
    parser.add_argument("directory", type=Path)
    parser.add_argument("--schemas", type=Path, default=None)
    parser.add_argument("--offline", action="store_true")
    args = parser.parse_args()

    try:
        import yaml  # noqa: F401
    except ImportError:
        print(
            "PyYAML absent : la validation des manifestes WinGet est sautée "
            "(pip install pyyaml).",
            file=sys.stderr,
        )
        return 2

    root = args.directory
    if not root.is_dir():
        print(f"dossier introuvable : {root}", file=sys.stderr)
        return 2

    cache = args.schemas or default_cache()
    offline = args.offline or bool(os.environ.get("TYPR_SCHEMA_OFFLINE"))

    winget = sorted((root / "winget").glob("*.yaml"))
    scoop = sorted((root / "scoop").rglob("*.json"))
    if not winget and not scoop:
        print(f"aucun manifeste à valider sous {root}", file=sys.stderr)
        return 2

    results: list[bool | None] = []
    if winget:
        print(f"WinGet — {len(winget)} manifeste(s)")
        for path in winget:
            results.append(validate_winget(path, cache, offline))
    if scoop:
        print(f"Scoop — {len(scoop)} manifeste(s)")
        for path in scoop:
            results.append(validate_scoop(path, cache, offline))

    failures = [r for r in results if r is False]
    skipped = [r for r in results if r is None]

    if failures:
        print(f"\n{len(failures)} manifeste(s) invalide(s).")
        return 1
    if skipped:
        print(
            f"\n{len(skipped)} manifeste(s) non contrôlé(s) : schéma indisponible."
        )
        return 2
    print(f"\n{len(results)} manifeste(s) valide(s).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
