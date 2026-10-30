#!/usr/bin/env python3
"""TypR — validation des manifestes Scoop contre le schéma officiel.

    packaging/tests/validate-schemas.py <dossier-rendu>

Scoop ne valide rien au commit : un manifeste invalide n'est découvert qu'à
l'installation, chez l'utilisateur. Valider ici, avant la publication, coûte
deux secondes.

Le schéma n'est pas écrit dans ce dépôt : c'est celui de Scoop, téléchargé une
fois et mis en cache. Un schéma recopié ici dériverait dès la version suivante,
en validant moins que ce que Scoop exige.

Sorties :
    0  tout est valide
    1  au moins un manifeste est invalide
    2  validation impossible (schéma indisponible hors ligne)

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

    root = args.directory
    if not root.is_dir():
        print(f"dossier introuvable : {root}", file=sys.stderr)
        return 2

    cache = args.schemas or default_cache()
    offline = args.offline or bool(os.environ.get("TYPR_SCHEMA_OFFLINE"))

    scoop = sorted((root / "scoop").rglob("*.json"))
    if not scoop:
        print(f"aucun manifeste à valider sous {root}", file=sys.stderr)
        return 2

    print(f"Scoop — {len(scoop)} manifeste(s)")
    results: list[bool | None] = [
        validate_scoop(path, cache, offline) for path in scoop
    ]

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
