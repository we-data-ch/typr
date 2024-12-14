Donc je m'explique les sciences de données c'est vraiment quelque chose qui est basé sur pas mal de choses donc l'élément de base c'est surtout les tableaux multidimensionnels donc c'est des tableaux comme des tenseurs, des vecteurs des matrices, des choses dans le genre, et en fait ce que j'ai pu tirer de mon travail de master, donc c'est surtout en fait les types dépendants je pourrais en parler vraiment longtemps parce que c'est vraiment cool et puissant mais je vais plutôt vous montrer parce que c'est plutôt une implémentation légère de ces types là donc sans aller trop en puissance parce que sinon je vais perdre aussi l'inférence de type mais voilà,


Donc,
il y a aussi un index,
donc 3 et num.
Donc,
c'est un tableau qui a deux lignes et trois colonnes.
Simple comme bonjour.
Donc après nous avons aussi la notion de alias,
donc on peut créer des types alias comme par exemple là je peux créer quelque chose comme mat pour matrice et je vais mettre m lignes n colonnes Donc ce serait l'équivalent au type tableau de tableau.

Donc ici je vais mettre num,
parce qu'habituellement les matrices c'est des éléments numériques,
mais ça peut être pour autre chose.

mais ici on va dire j'ai un peu de flemme,
donc là je peux mettre comme ça.

Et de cette façon là en fait j'ai créé une sorte de alias de type,
c'est pas vraiment un nouveau type mais c'est un raccourci donc si je fais maintenant matrice de deux lignes et de trois colonnes donc j'ai aussi ce typage là,
donc dans mon cas j'essaie toujours de garder le type nature et brute mais aussi non donc nous on peut utiliser maintenant des raccourcis comme ça dans la notation de notre langage donc ce qui veut dire que maintenant je peux aussi créer des fonctions comme par exemple let donc je vais la nommer transpose et avec l'aide des génériques c'est ce que vous voyez ici on peut créer des génériques,
les génériques se font avec des lettres majuscules moi je préfère plutôt fn.

Ici après je peux dire que c'est une matrice qui va être de type mat qui va avoir m lignes n colonnes et qui doit retourner une matrice qui fait donc n lignes m colonne donc ici pour la fonction je vais mettre trois petits points donc c'est pour dire que la fonction n'est pas implémentée parce qu'on s'intéresse uniquement au niveau des types donc après qui capte ce type d'informations.

Donc que la transposition doit marcher de cette façon-là.

Donc de cette façon-là,
on peut même créer le corps de la fonction et s'assurer à ce qu'on retourne bel et bien le bon élément.

Donc,
il y a encore plein d'autres choses qu'on peut faire dedans.

Donc j'ai pu créer comme par exemple de l'algèbre linéaire TypeSafe dedans pour créer aussi notamment des modèles comme par exemple pour les statistiques ou les sciences des données ou bien même l'intelligence artificielle.

vous êtes maintenant capable de pouvoir faire du typage sur certains et sereins et j'espère que vous appréciez la vidéo n'hésitez pas à mettre un pouce bleu si c'est le cas et à vous abonner si ce n'est pas encore le cas.

Et donc,
voilà,
je voulais juste vous présenter notre Discord.

Donc,
c'est là où vous pouvez en fait nous parler assez souvent,
constamment et donc avec toutes les informations que vous voulez.

ou de zone pour nous exercer donc il y a vraiment de tout ça fait depuis un moment que je devrais écrire un blog mais je ne l'ai pas encore fait donc je ferai un jour un blog post dessus et sinon nous avons aussi notre compte Insta donc c'est plutôt un compte là où on met pas mal de memes ou pas mal d'éléments juste pour chill donc vraiment c'est l'endroit où nous sommes aussi donc si jamais vous pouvez nous retrouver dans ces plateformes là,
donc sans plus tarder on va continuer la vidéo après nous avons aussi les data frames parce que je sais que l'utilisateur de R et pas mal de data analystes en fait,
utilise ça,
c'est vraiment la base,
la norme.

Donc en fait,
ce que j'ai pensé,
c'est la notion d'objet,
ou plutôt de record,
moi je mets ça plutôt des record,
donc moi je mets plutôt R,
donc record,
tac,
et nous avons comme par exemple on va créer un point donc position x 0 y 0 donc lorsque j'évalue le type du record donc je vois que c'est un type record donc il ya un champ x en tant que number y en tant que number donc j'ai dit vous pouvez utiliser le mot clé object si vous voulez donc ça peut aussi être utilisé sinon vous pouvez aussi en fait abuser donc pour les vrais nostalgiques de air vous pouvez en fait faire quelque chose comme ça parce que techniquement c'est bel et bien une liste labellisée que vous créez et de façon donc le typechecker va l'évaluer en tant record.

C'est pour ça que je préfère quand même la notation record.

Bon après c'est vraiment sur la préférence de chacun.

En plus de ça donc on peut capter justement le record.

Donc on pourrait mettre par exemple x c'est un nombre y c'est un nombre donc on peut capter le type tout simplement mais une chose que j'ai ajouté c'est ce qui s'appelle le raw polymorphisme donc dans la littérature c'est quelque chose qui va me permettre de faire ça donc en fait en termes sémantique ici on parle de sous-typage donc c'est à dire que ça c'est un sous-type de ça et on pourrait se demander pourquoi parce que ce truc là il est plus petit que l'autre quand même en fait vous pouvez imaginer ça comme une liste de restrictions c'est à dire que ici on parle des objets qui ont comme champ un x qui est un nombre et un y qui est un nombre donc ça fait moins d'objets possibles Alors qu'ici,
si on prend simplement les objets qui ont x,
ça prend aussi l'autre.

Donc là où nous avons y,
il y a un nombre tout autant.

Et donc ça,
ça pourrait nous permettre de faire des choses comme ça,
comme par exemple.

Donc,
on peut créer une fonction qui va,
comme par exemple,
prendre un dataFrame.

Et donc,
on demande cette restriction.

Donc,
ça doit être uniquement des dataFrames qui ont une colonne,
age,
qui est de type nombre.

du coup si le data frame a ça on va pouvoir travailler avec donc ça c'est quelque chose de beau je trouve et de pratique donc ça nous permettrait vraiment de créer des modules,
des packages qui sont spécifiques à certains éléments donc des data frames bien sûr c'est quelque chose que j'aimerais travailler parce que pour l'instant la notion des records et des data frames ne sont pas encore aussi synchronisés que je le voudrais donc je ne sais pas si j'arriverai à trouver quelque chose dessus mais je ferai aussi mes recherches pour avoir quelque chose de bien aussi.

Ça c'était les éléments qui nous permettent aussi d'avoir des data science un peu plus sécurisé et là un de mes points favoris donc qui vont avec cette voici l'aspect aisance et sécurité ben c'est aussi la réutilisation de code c'est quelque chose de très puissant la La programmation orientée objet avait voulu faire des choses aussi intéressantes comme ça,
comme par exemple l'héritage,
mais en fait,
vous verrez que je ne prendrai pas ça.

Donc il y a en fait un mouvement qui voulait que R puisse être de plus en plus orienté objet,
en fait je vais montrer aussi quelques solutions qui le rapprochent plus de l'orienté objet tout restant fonctionnel donc ça va être vraiment des constructions que vous avez peut-être bien apprécié parce que j'ai tiré ça de différents langages Rust,
Go,
Nim,
qui eux ne sont pas techniquement orienté objet mais prennent son essence sur l'orienté objet.

Donc je vous invite à regarder les quelques vidéos que j'ai aussi fait sur l'orienté objet et sur son origine,
sa façon d'être faite et parce qu'il y a pas mal d'infos qui pourraient vous intéresser dans l'usage et en fait même les vidéos que je vous ai envoyées précédemment donnent des indices sur comment utiliser TypeR Donc,
si jamais vous avez envie de voir,
n'hésitez pas.

Donc ici,
on va voir quelques éléments intéressants pour la réutilisation de code.

C'est notamment la notion de l'uniforme function call.

c'est quelque chose qui va nous permettre de faire quelque chose comme ça ici je vais définir comme par exemple je vais créer un type je vais faire un type point qui va plutôt être x numéro donc ici c'est quelque chose qu'on connaît déjà que j'ai que je fais continuer mais voilà donc ici nous avons un point et à partir de là en fait je vais créer une autre fonction je vais appeler add donc c'est une fonction addition donc je peux attribuer le truc comme ça donc fonction addition je vais créer quoi fn donc ça va prendre un premier élément donc ça va être un point je vais prendre un deuxième élément ça un point et je vais retourner un point.

Donc voilà,
ne se foule pas.

Et à partir de là,
ce qu'on va essayer de faire maintenant c'est plutôt d'additionner ça.

Donc on va faire un let,
Je vais créer un point,
donc je vais appeler P,
qui va prendre cette valeur-là,
donc c'est record X0,
comme par exemple Y2.

Et donc en fait,
je peux appeler P,
donc en faisant add P et P.

donc j'additionne P avec P et donc vous voyez nous nous retrouvons avec le résultat donc vu que c'est un point additionne un point qui donne un point on a ça,
donc à l'intérieur on aurait très bien pu définir,
je pense que même ici c'est bien de garder la fonction comme ça,
on aurait très bien pu mettre à l'intérieur un nom justement donc quelque chose qui nous permette de pouvoir faire le travail comme par exemple addition les coordonnées x et les coordonnées y mais ici donc on a add p et p qui nous donne ce résultat mais je peux aussi faire quelque chose comme ça p pipe add p et donc vous piper plusieurs éléments et donc tomber sur un résultat.

Ici vu que c'est simplement des points qui s'additionnent,
qui s'additionnent,
ça marche comme ça.

Vous avez aussi la possibilité en fait de noter les choses comme ça aussi .addP .addP là aussi ça fonctionne,
vous pouvez enchaîner les trucs comme ça ces deux notations donnent cette notation donc en fait j'ai simplement fait un sucre syntaxique qui nous permet en fait d'utiliser soit des pipes ou soit en fait une appellation par méthode comme on dirait donc de cette façon là en fait les personnes sont un peu plus libre.

Si vous avez une approche un petit peu plus fonctionnelle,
vous prendrez peut-être Raypipe.

Si vous avez une approche un peu plus orientée objet,
vous prendrez en fait cet appel là.

Mais ça donnera exactement la même chose.

Et d'ailleurs je vais vous montrer quelque chose de plus si je fais maintenant un p plus p.

Vous pensez que ça va donner quoi ? On penserait qu'il y a une mais en fait il n'y a pas d'erreur.

Pourquoi ? Parce qu'en fait c'est lié à une notion que j'appelle l'OperatorOverloading.

En fait j'ai triché,
add est en fait un élément qui est,
comment on appelle ça,
mais voilà est plutôt un élément qui est comme add pp donc si on transpile ou on compile dans du code R ça donnerait plutôt quelque chose dans ce genre là donc tout va être transformé en une forme un peu plus basique mais d'appel fonctionnel.

Donc de cette façon là en fait nous avons aussi la possibilité de créer notre propre type et de pouvoir en fait implémenter des opérateurs donc si on veut faire un petit peu notre propre arithmétique Je pourrais développer par exemple des nombres complexes et créer des opérations par-dessus.

Ce serait donc la liste des opérateurs que je voudrais réserver.

Donc ça veut dire que les plus vont se transformer en add Les plus plus vont se transformer en add2 Donc les moins vont se transformer en sub Les moins moins vont se transformer en sub2 Les mules vont se transformer en...

Les étoiles vont se transformer en mule,
je veux dire,
et les doubles étoiles vont se transformer en mule 2,
les hâte vont se transformer en hâte quoi,
et les doubles arrobases vont se transformer en hâte 2.

donc ce serait le but comme ça en fait ça laisserait à une personne de développer en fait son package avec en fait ses propres types sa propre arithmétique et sa propre façon de faire donc c'est vraiment pour une orientation un peu plus data science,
un peu plus dans ce sens là en fait,
c'est le but.

Aussi donc pour vous montrer que je ne blague pas,
en fait ici vous voyez une autre subtilité derrière c'est que je peux définir plusieurs fois la même fonction donc c'est aussi un petit peu lié uniform function call c'est juste qu'en fait le langage plutôt le type shaker est assez intelligent parce qu'en fait il va créer des sortes de pseudo namespace qui va vous permettre en fait de pouvoir définir deux fois le même truc parce que il va prendre le premier paramètre de la fonction en tant que tel comme objet ou comme target.

C'est pour ça que vous pouvez aussi noter les choses comme ça et donc il va être capable d'inférer par lui-même de quoi on parle.

donc ça c'est aussi un avantage lorsqu'on travaille avec ce type de système parce que comme je vous ai dit j'essaie vraiment de cacher quelques informations qui ne sont pas très utiles parce que ce n'est pas très utile pour utilisateurs parce que par exemple il y a aussi la notion de kindings dans les fonctions que j'ai aussi caché la notion de kindings parce que ce serait aussi contre la politique de simplicité et d'aisance de type R.

On peut créer un type,
on peut créer des fonctions qui lui sont propres et ensuite directement travailler avec.

Donc ça c'est vraiment beau.

Une autre fonctionnalité que je n'ai pas encore ajoutée parce que je suis encore en train de travailler dedans,
parce que là,
cette fois-ci,
ça va un peu plus loin dans la métaprogrammation,
mais qui est vraiment pratique et que je trouve beaucoup plus propre et beau que de l'héritage parce que vous verrez que même si dans Type R je cherche des moyens de pouvoir rapprocher les choses de la programmation de similaire aussi mais c'est du type embedding donc c'est quelque chose qui va permettre d'avoir une sorte d'héritage et qui reste quand même de la composition donc si vous n'avez rien compris c'est pas grave imaginez-vous que c'est juste de l'héritage mais plus propre et qui va vous apporter moins de problèmes donc là je crée un type point donc là je vais créer les fonctions right et left,
donc ça va déplacer le point donc ça prend un type point,
ça prend un nombre et ça déplace le point et après je peux créer comme par exemple un type caractère qui va prendre un point comme membre,
mais vous voyez qu'il y a une étoile,
c'est à dire que point est embeddé,
donc il est incrusté ou inclus ce qui veut dire qu'après je peux créer un C qui est de type caractère donc là j'ai mis trois petits points parce que le code il ne run pas,
je n'ai pas encore fini l'implémentation et bien évidemment à partir de là je peux appeler write qui provient de point,
parce qu'en fait ce que va faire le type embedding,
en tout cas celui que je vais implémenter c'est juste que lorsqu'on va faire l'embedding c'est juste qu'il va créer bon en fait je vais juste copier ça il va créer en fait une nouvelle implémentation de write qui va pas prendre point donc il va prendre caractère ensuite il va retourner ça et ici à voir s'il va retourner caractère mais normalement oui et en fait là dedans il va juste faire c.p qui est le point et ensuite il va faire justement write donc il va créer cette fonction par défaut donc ça va être une redirection automatique donc voilà c'est pour ça que c'est pas de l'héritage parce que vous verrez que en faisant l'embedding,
caractère ne devient pas un sous-type de point.

Donc tous les endroits où l'on attend point,
on ne pourra pas mettre caractère.

Par contre,
avec des interfaces comme le fake go,
vous pourrez très bien faire une interface qui a peut-être les éléments movable,
qui demande d'avoir les fonctions right,
left et tout.

donc ça veut dire que points et caractères ou tous les types qui font de l'embedding de points donc qui inclut points en leur sein vont pouvoir être passés dans l'interface donc si c'est une interface movable donc en fait on pourra mettre en fait dans toutes les fonctions d'être en movable on pourra mettre caractère ou point donc voilà donc ça c'est encore quelque chose pour laquelle je travailler parce qu'il y a encore pas mal d'éléments importants à mettre en place pour typer le tout.

Vraiment c'est pour présenter des idées parce qu'il y en a plein après j'ai aussi l'inférence d'interface c'est à dire que comme je l'ai montré tout à l'heure avec add ce que j'aimerais faire c'est que on lui donne déjà on donne déjà aux objets l'attribut comme par exemple de la classe de l'interface comme par exemple aidable,
si je crée une classe aidable qui utilise la notion de add en fait il suffit juste de l'implémenter ou d'écrire la fonction sans forcément faire attention à si on implémente telle ou telle interface ou sans le mentionner directement et nous serons capables de le faire donc en fait c'est comme Go,
littéralement donc il y a ça et j'aimerais aussi ajouter la notion de pseudo traits,
donc de Rust qui lui va en fait nous permettre de pouvoir avoir des sortes de classe abstraite donc ça veut dire qu'on pourra faire une interface qui aura justement à l'intérieur peut-être une fonction qui aura ces trucs là,
mais après on peut dans l'interface Donc,
si vous ne comprenez pas tout,
ce n'est pas grave.

C'est quelque chose qu'on pourra voir aussi,
peut-être,
si je finis bien le langage.

donc voilà une interface avec aussi une fonction qui elle va être implémentée directement et donc en fait tous les types qui implémentent l'interface vont avoir cette fonction gratuite donc tout ça vraiment c'est le genre de choses qui sont là pour aider en fait à faire de la réutilisation de code ça aussi ça fait partie de ma wishlist mais j'aimerais tout autant avoir la possibilité comme je vous ai montré de faire du pipe donc par exemple si j'ai une fonction mais en fait qu'on puisse faire quelque chose comme par exemple on a set pipe inc comme ça mais que comme par exemple si cette fois ci on traite avec un tableau par exemple 7 8 9 en fait qu'on puisse juste mettre ink par là et donc en fait le type shaker va comprendre qu'on traite ici avec un tableau donc en fait c'est juste qu'il va l'implémenter en tant que map de cette façon là vous n'avez pas besoin de tout comprendre mais c'est juste pour vous montrer parce que vu que maintenant type R va avoir des types vraiment scalaires natifs donc il va falloir aussi faire une différence entre l'implémentation sur un seul élément ou sur une liste d'éléments.

Un autre élément qui va être super puissant aussi qui nous aidera dans la gestion des données et tout,
c'est surtout la notion de ennum parfois on appelle ça des sum types etc mais mon frère m'avait montré en fait quelque chose d'encore plus puissant donc qui est basé sur ça mais qui marche vraiment très bien donc je vais pouvoir vous montrer ça donc je les appelle des tags donc pour utilisateurs de Rust ou autre je pense que vous savez que c'est peut-être des Enum ou peut-être des algebraic datatypes ici donc je vais vous montrer quelques éléments qui peuvent vraiment illustrer la puissance du truc c'est que là je vais définir un type comme par exemple je vais définir un type feu rouge et donc en fait je vais mettre un feu rouge je vais mettre un feu vert je mettre un feu orange pour les utilisateurs de air ce serait quelque chose de similaire à un factor ouais c'est ça c'est plutôt les factors ça donc de cette façon en fait je peux créer donc par exemple un élément feu qui va prendre justement le type feu et donc si je mets vert par exemple on va savoir que c'est un élément du lot.

Donc ici c'est noté là parce qu'on peut aussi mettre des éléments à l'intérieur donc c'est tout autant possible donc voilà vous avez en fait la capacité d'avoir ces types de données présents mais vous avez aussi tout autant la capacité la possibilité en fait d'utiliser un tag comme ça donc Donc c'est une valeur par défaut.

Donc vous n'avez pas besoin de la prédéfinir,
la préconstruire ou quoi que ce soit en fait.

Vous l'avez gratuit comme ça.

Donc si tout d'un coup vous avez envie de créer une valeur à la volée,
vous pouvez créer une valeur à la volée,
elle se créera à la volée.

Donc ça c'est quelque chose qui peut être très avantageux,
notamment si vous voulez comme par exemple définir,
parce que je vous ai parlé de mon objet point,
mais vous voulez peut-être avoir votre propre point qui ne soit pas d'une librairie en fait qui est autre,
mais en En fait,
vous pouvez faire quelque chose comme ça.

Point x num et y num.

Et donc là,
vous êtes sûr que vous n'avez pas un point qui est similaire à l'autre.

Donc comme ça,
vous pouvez très bien créer une fonction,
comme par exemple ce serait fn qui prend un p qui est de type point machin chose etc parce que en fait même si on peut les unir ensemble ces tags sont aussi indépendants et peuvent servir de type indépendant Donc l'un des trucs cool qui va être bien et intéressant,
c'est bien sûr qu'on va pouvoir faire du pattern matching.

donc on pourra faire match avec quelque chose qui nous permettrait de faire,
par exemple,
si c'est rouge,
si le feu est rouge,
alors on va faire telle chose,
si le feu est vert,
alors on va pouvoir faire telle chose,
etc.

donc ça va nous permettre de pouvoir éviter les erreurs ou d'oublier en fait des cases donc après vous aurez aussi la possibilité de pouvoir matcher un cas puis tout le reste des cas donc tout ça si vous connaissez le pattern matching ça marche tout autant.

Aussi ça va être pratique pour la gestion des erreurs donc parce que vous pouvez avoir un type option qui va pouvoir en fait prendre n'importe quel type dedans et donc en En fait,
vous avez soit un résultat qui contient le type,
donc ça pourrait être sum de T,
ou soit en faisant une union avec none.

Et donc après,
vous pouvez aussi créer d'autres types,
comme par exemple not number,
etc.

vous avez vraiment l'embarras du choix pour la construction de ces choses là de plus ce qu'il y a de cool avec les tags c'est que vous serez aussi capable de faire quelque chose dans le même genre donc si vous faites un if voulez et ensuite dans le système de type va pouvoir savoir ce qui est bon de faire.

De cette façon là en fait vous pourrez retourner des messages d'erreurs différents mais il s'occupera de tout mettre dedans parce que c'est vrai que les enum dans Rust et dans les agribes datatype dans Haskell comme par exemple sont un petit peu trop rigides,
ici on a beaucoup plus de liberté et ça correspond très bien à l'image liberté de R donc voilà,
nous sommes arrivés à la fin donc il y a encore plein d'autres choses que je pourrais aborder,
je pense que même dans la suite peut-être d'autres idées viendront parce que ma tête s'est remplie de plein de trucs mais c'est juste pour vous dire qu'on a abordé à peu près l'essentiel donc il y aura aussi quelques autres points que je trouve un petit peu moins intéressants notamment l'aspect de la mutabilité,
parce que ce n'est pas non plus un truc ultra révolutionnaire.

Mais voilà,
ce genre de choses peut aussi exister,
ce genre de choses peut aussi servir.

Je suis assez content d'avoir créé un système qui soit assez équilibré,
donc je dois encore le rééquilibrer parce que je pense qu'il y a quelques éléments qui n'entrent pas encore bien en compte,
c'est comme par exemple la syntaxe pour les tags union et bien évidemment la syntaxe pour les alias il va falloir aussi que j'arrive à trouver un juste milieu avec ça aussi plein d'autres choses que je ne vous ai pas montré parce que c'est encore buggy encore des trucs qui ne fonctionnent pas bien notamment avec les génériques qui ne sont pas très bien utilisés lorsque l'on crée des sortes de pseudo namespace avec le système donc il y a pas mal de choses que je peux encore améliorer et que je dois améliorer qui va peut-être prendre pas mal de temps et peut-être même le plus de temps ça je ne sais pas encore mais voilà en tout cas je vous ai présenté vraiment les fonctionnalités qui me tenaient à coeur je pense en tout cas que si ce langage sort,
donc quand il sortira bientôt qu'il abordera aussi pas mal de choses dans la communauté d'air en tout cas je l'espère je les souhaite et ouais c'est juste que voilà j'avais beaucoup apprécié ces idées beaucoup apprécié ce type de formatage donc bon pour l'instant comme je vous bleue,
à vous abonner si ce n'est pas le cas,
allez regarder aussi nos autres vidéos,
partagez aussi celle-ci,
donc vraiment,
vous avez encore plein de moyens de pouvoir interagir,
notamment dans la section commentaires,
si vous avez Sous-titrage Société Radio-Canada
