# CLAUDE.md — règles de travail sur `scenarios_bn_pmsi`

Ces règles s'appliquent à toute session qui modifie ce dépôt, y compris celle qui les a écrites.
Elles complètent, sans les remplacer, `VISITE_GUIDEE.md` (l'orientation : où est chaque chose et
pourquoi) et `MODIFICATIONS_V8.md` (le journal : la référence exhaustive des décisions).

## 1. Avant de coder

- Lire `MODIFICATIONS_V8.md` (au minimum les dernières sections et toutes les questions ouvertes)
  et `VISITE_GUIDEE.md` : tout l'historique des décisions y est. Les doctrines de code (chaînes
  base copiées des scripts v7, écarts nommés et tracés) sont décrites dans `VISITE_GUIDEE.md` §7.
- Vérifier le commit courant (`git log --oneline -1`) et établir une base verte — les quatre passes
  du §4 — AVANT tout changement, pour que tout rouge ultérieur soit attribuable au chantier et non à
  l'environnement.

## 2. Le brief, rien que le brief

- AUCUNE action non autorisée, jamais d'initiative au-delà du brief. Un doute, une piste, une
  amélioration entrevue : une question consignée au journal, pas une action.
- Chaque chantier est journalisé dans `MODIFICATIONS_V8.md`, dans une section numérotée à la suite
  des précédentes : ce qui change, pourquoi, les écarts (par rapport au brief, aux scripts d'origine
  ou aux comportements antérieurs). Les questions vont dans la sous-section dédiée de la section
  (« Questions (aucune action non autorisée) »), numérotées Qn à la suite du journal.

## 3. Intangibles

- `tests/ancien_20260914/` est INTACT à jamais : ce sont les instantanés d'identité des anciens
  scripts d'entrée (fixtures figées, jamais exécutées hors test). Ni modification, ni suppression,
  ni ajout dans ce dossier.
- Les recettes d'identifiants (`id_v1` pour les longs, `id_courts_v1` pour les courts) et leurs
  valeurs de test en dur ne se modifient JAMAIS sans décision utilisateur explicite, consignée au
  journal. Les changer invaliderait toute la comptabilité des campagnes (registre, recyclage).

## 4. Avant toute livraison : les quatre passes, la démo, les notebooks

Toute livraison (compte rendu de fin de chantier) est précédée, dans l'ordre, de :

1. `Rscript tests/test_helpers.R` — sans arrow ;
2. `Rscript tests/test_chaines_sqlite.R` — sans arrow ;
3. les deux mêmes suites AVEC arrow ;
4. la démo (`Rscript demo/creer_base_demo.R` puis `Rscript demo/lancer_demo.R`) et les deux
   notebooks en mode démo (`Rscript demo/executer_notebook.R --raz RUN.Rmd` puis
   `Rscript demo/executer_notebook.R RUN_aval.Rmd`), avec et sans arrow, comme la CI
   (`.github/workflows/tests.yml`).

Les résultats chiffrés (assertions vertes par passe, volumes de la démo) figurent au compte rendu et
dans la sous-section « Vérifications » de la section du journal.

Avec / sans arrow : suites, démo et notebooks détectent arrow par `requireNamespace("arrow")` et
basculent sinon sur le repli mock RDS (`demo/mock_pratihque.R`). Le plus simple sur un poste est
d'installer arrow dans une bibliothèque SÉPARÉE et de la passer par `R_LIBS_TEST=<lib>` pour les
passes « avec arrow » ; sans la variable, arrow est absent et le repli est exercé.

## 5. Git

- Commits locaux libres (messages en français, préfixe `v8 : ` comme l'historique).
- `git push` sur `main` UNIQUEMENT sur accord explicite de l'utilisateur, donné APRÈS le compte
  rendu de fin de chantier. Jamais de push implicite, jamais de réécriture de l'historique distant.

## 6. Le compte rendu de fin de chantier

En français courant, dans cet ordre :

1. le tableau des passes (chaque suite avec / sans arrow et son nombre d'assertions ; démo et
   notebooks avec / sans arrow) ;
2. ce qui a été fait (renvoi à la section du journal) ;
3. les points à connaître (écarts, effets de bord, régénération de magasin imposée par un méta…) ;
4. les questions (Qn du journal — aucune action prise).

Le push attend l'accord donné sur ce compte rendu.
