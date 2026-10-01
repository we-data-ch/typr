#!/usr/bin/env python3
"""Sert une release GitHub factice, pour tester install.sh sans réseau.

L'installeur ne connaît que deux points d'entrée côté release : l'URL
`/releases/latest`, dont il lit l'en-tête `Location`, et `/releases.atom`, qu'il
parse pour le canal beta. Les deux sont servis ici depuis l'arborescence racine
en argument, avec une seule route réélée :

    <racine>/<repo>/releases/LATEST   contient le tag vers lequel rediriger
    <racine>/<repo>/releases/latest    absent : c'est la redirection
    <racine>/<repo>/releases.atom      le flux
    <racine>/<repo>/releases/download/<tag>/…  les artefacts

Sans ce serveur, vérifier que l'installeur *refuse* un SHA falsifié
supposerait de falsifier une vraie release publiée.

Usage :
    fixture-server.py <racine>
    → écrit « PORT <n> » sur stdout, puis sert jusqu'à l'arrêt.
"""

import sys
import os
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer


class Handler(BaseHTTPRequestHandler):
    root = "."

    def log_message(self, *_args):
        pass  # le test lit la sortie du script, pas les accès

    def resolve(self):
        """Renvoie (chemin, est_une_redirection) ou None si 404."""
        path = self.path.split("?", 1)[0].split("#", 1)[0]
        while "//" in path:
            path = path.replace("//", "/")
        parts = [p for p in path.split("/") if p and p != ".."]

        if parts and parts[-1] == "latest":
            tag_file = os.path.join(self.root, *(parts[:-1] + ["LATEST"]))
            if os.path.isfile(tag_file):
                with open(tag_file, encoding="utf-8") as handle:
                    tag = handle.read().strip()
                return ("/".join(parts + ["tag", tag]), True)

        target = os.path.normpath(os.path.join(self.root, *parts))
        if not os.path.abspath(target).startswith(os.path.abspath(self.root)):
            return None
        if os.path.isfile(target):
            return (target, False)
        return None

    def do_HEAD(self):
        self.respond(body=False)

    def do_GET(self):
        self.respond(body=True)

    def respond(self, body):
        found = self.resolve()
        if found is None:
            self.send_error(404, "Not Found")
            return

        target, redirect = found
        if redirect:
            self.send_response(302)
            self.send_header("Location", target)
            self.send_header("Content-Length", "0")
            self.end_headers()
            return

        with open(target, "rb") as handle:
            payload = handle.read()
        self.send_response(200)
        self.send_header("Content-Length", str(len(payload)))
        self.send_header("Content-Type", "application/octet-stream")
        self.end_headers()
        if body:
            self.wfile.write(payload)


def main():
    if len(sys.argv) != 2:
        print(__doc__, file=sys.stderr)
        return 2
    Handler.root = os.path.abspath(sys.argv[1])
    server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    print(f"PORT {server.server_port}", flush=True)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    return 0


if __name__ == "__main__":
    sys.exit(main())