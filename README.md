# Site Web pour Echo'Z

Heyo la Zamal Team,


Voici le site de l'asso' Echo'Z !

Il est entièrement écrit en OCaml ([Dream](https://github.com/aantron/dream)) pour le backend, et gère des pages écrites en Markdown/HTML/CSS pour le contenu ([Omd](https://github.com/ocaml/omd)).

Le style c'est fait avec le Framework CSS [Bootstrap](https://getbootstrap.com/docs/5.0/examples/cheatsheet/), gratuit facile d'utilisation et tutti quanti !

# Usage

Pour lancer le site en local:
1. installer OCaml
2. installer les dependances comme Omd, Dream, Markup, Crunch, etcetc, se référer au fichier `dune-project`.

Ensuite:

```shell
  dune build @all && dune exec -- src/server.exe
```

Apparaîtra ensuite:

```shell
<date> <time> Running at http://localhost:8080
<date> <time> Type Ctrl+C to stop
```

Il faudra simplement cliquer sur le lien.

