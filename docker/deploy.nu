# Build local de l'image, pour tester le Dockerfile hors CI.
#
# La publication réelle sur Docker Hub est faite par le job `docker` de
# .github/workflows/release.yml, à partir du binaire de la release.
# Ce script sert uniquement à valider l'image avec le binaire courant.

def main [--push] {
  let version = (open ../Cargo.toml | get workspace.package.version)

  print $"Compilation du binaire local \(($version)\)..."
  cd ..
  cargo build --release
  cd docker

  mkdir bin
  cp -f ../target/release/typr bin/typr

  print $"Construction de l'image typr:($version)..."
  docker build -t $"fabricehategekimana/typr:($version)" -t fabricehategekimana/typr:latest .

  if $push {
    print "(ansi yellow)Publication manuelle — normalement c'est la CI qui pousse.(ansi reset)"
    docker push $"fabricehategekimana/typr:($version)"
    docker push fabricehategekimana/typr:latest
  } else {
    print $"Image construite. Test : docker run --rm -it fabricehategekimana/typr:($version) typr --version"
    print "Pour publier malgré tout : nu deploy.nu --push"
  }
}
