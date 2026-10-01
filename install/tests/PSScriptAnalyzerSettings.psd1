# Règles PSScriptAnalyzer écartées pour install.ps1, et pourquoi.
#
# Ce fichier n'est pas lu automatiquement par PSScriptAnalyzer — voir la note en
# bas. Il documente les exclusions, et run-tests.ps1 en tire la liste exacte.
#
# Une règle écartée sans raison écrite est pire qu'une règle active : elle crée
# un sentiment de couverture qui n'existe pas.

@{
    # `Try-Save-File` renvoie un booléen. Le verbe approprié serait `Test-`,
    # qui dirait « tester si on peut sauvegarder » sans le faire. Le préfixe
    # `Try-` est la convention .NET pour « essayer, sinon renvoyer false ».
    #
    # $Version et $Channel sont déclarés dans le bloc `param` et lus plus bas,
    # dans `Resolve-Version`. PSScriptAnalyzer ne résout pas la portée : depuis
    # une fonction, il voit le $Version de la fonction, pas celui du script.
    # $Gnu est volontairement sans effet sur Windows — il existe pour que la
    # même ligne de commande fonctionne sur les deux scripts.
    #
    # Ces trois paramètres sont couverts par les tests : `-Version` épinglé et
    # `-Channel beta` ont chacun leurs propres scénarios.
    #
    # Stop-Install, Remove-TempDir et New-Client ne sont pas des cmdlets et
    # n'ont pas à implémenter -WhatIf. La règle vise les modules distribués, où
    # l'appelant doit pouvoir voir l'effet avant de le subir. Ici
    # l'installateur est le programme : « installer sans installer » n'a pas de
    # sens, et un -WhatIf trompeur serait pire que son absence.
    #
    # Write-Host est le bon choix, et le seul qui convienne. L'installateur
    # écrit son rapport sur le flux de l'hôte, pas dans le pipeline : sous
    # `irm … | iex`, un Write-Output de plus se retrouverait dans la valeur de
    # retour de l'utilisateur, dans un `$x =`, ou dans une boucle. Write-Host
    # va vers le flux d'information, que `2>&1` capture et que `iex` ne pollue
    # pas. L'échec passe par Write-Host puis `exit`, jamais par Write-Error :
    # sous `iex`, une erreur non terminante est écrite sans rien arrêter.
    # La couleur est voulue : c'est un outil de terminal, pas une bibliothèque.

    Excluded = @(
        'PSUseApprovedVerbs'
        'PSReviewUnusedParameter'
        'PSUseShouldProcessForStateChangingFunctions'
        'PSAvoidUsingWriteHost'
    )

    # Règles volontairement NON écartées, malgré leur pénalité : elles ont
    # attrapé de vrais défauts dans ce fichier.
    #
    #   PSUseBOMForUnicodeEncodedFile : a signalé l'absence de BOM, donc que le
    #   fichier aurait été lu en ANSI par PowerShell 5.1 — chaque accent cassé
    #   sur un Windows français ou allemand. Le BOM est posé depuis, et un test
    #   explicite le vérifie.
    #   PSAvoidAssignmentToAutomaticVariable : a signalé `$input`, qui est une
    #   variable automatique. Réécrite en `$source`.
    #   PSUseSingularNouns : a signalé Test-BinaryStarts, renommé
    #   Test-BinaryStart.
    #
    # Note d'implémentation : `-Settings` avec un dictionnaire est accepté par
    # l'API mais silencieusement ignoré dans PSScriptAnalyzer 1.25 — les
    # constatations changent d'un appel à l'autre. `-ExcludeRule` est le
    # mécanisme vérifié, et c'est lui que la suite utilise. Ce fichier reste la
    # source de vérité pour les raisons ; la liste effective y est lue par
    # run-tests.ps1.
}