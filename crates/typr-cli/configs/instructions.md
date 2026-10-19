Pour commencer

  cd {{PACKAGE_NAME}}, name
  # Éditez TypR/main.ty avec votre code TypR"
  # Puis utilisez les commandes:"
  typr check    # Vérifier le code"
  typr build    # Compiler vers R"
  typr run      # Compiler et exécuter"
  typr test     # Lancer les tests"

Dépendances R (gérées par uvr)

  typr use dplyr   # ajoute le paquet à DESCRIPTION et à uvr.toml
  uvr sync         # réinstalle les paquets après un clone
  uvr run script.R # exécute un script R dans la bibliothèque du projet
  
Ou ouvrez le projet dans RStudio avec le fichier {{PACKAGE_NAME}}.Rproj", name
