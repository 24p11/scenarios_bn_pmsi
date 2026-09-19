# scenarios_bn_pmsi

**Génération de scénarios cliniques fictifs à partir de la base nationale PMSI**
— pipeline amont du projet PARTAGES (corpus synthétiques de comptes rendus
hospitaliers pour l'entraînement de modèles ouverts d'aide au codage CIM-10,
écosystème STREAM, projet national mené avec l'ATIH et l'INRIA).

> *English abstract — This repository contains the upstream pipeline of the
> PARTAGES project: it derives fictional but statistically grounded clinical
> case scenarios (diagnoses, patient profiles, stay characteristics) from
> aggregated counts of the French national hospital discharge database
> (PMSI). Only thresholded aggregate counts ever leave the secure platform;
> no patient-level data is stored or shared. The scenarios feed a downstream
> LLM pipeline that generates synthetic discharge summaries for training
> open ICD-10 coding models.*

## Ce que produit ce pipeline

Des **scénarios cliniques fictifs mais statistiquement fondés** : pour chaque
scénario, un profil patient (âge, sexe), un contexte de séjour (GHM, durée,
type d'unité, modes d'entrée/sortie), un diagnostic principal et un jeu de
diagnostics associés crédibles. Ces scénarios alimentent, en aval, la
génération de comptes rendus hospitaliers synthétiques (projet PARTAGES) —
chaque compte rendu étant rédigé pour justifier exactement les codes de son
scénario, jamais l'inverse.

Aucune ligne de la base nationale n'est copiée : les scénarios sont
**recomposés** à partir de comptes agrégés (fréquences de codes par strate
âge × sexe × contexte), avec un seuil de confidentialité systématique. La
seule composante issue du réel est la « graine » de chaque scénario long —
la paire des deux diagnostics associés les plus sévères d'un séjour, agrégée
sur ~10 ans et l'ensemble des établissements, sous seuil d'effectif.

## Architecture en deux mondes

```
BASE NATIONALE ──► EXTRACTION ──► comptes agrégés (parquet) ──► TIRAGE ──► scénarios
                (plateforme        seuillés, versionnés         (sans accès
                 sécurisée,        par des méta-fichiers          base)
                 accès pRatihque)
```

- **Extraction** (`extraction.R`) : seule partie qui
  requiert la base (accès via le paquet ATIH `pRatihque`, non distribué
  ici). Ne produit que des agrégats seuillés.
- **Tirage** (`tirage.R`) : n'ouvre jamais de connexion ; lit
  les parquets, tire les scénarios par campagnes itératives avec un registre
  de ce qui a déjà servi.

Le détail des mécanismes et de leurs justifications : **[VISITE_GUIDEE.md](VISITE_GUIDEE.md)**.
Le mode d'emploi opérationnel : **[RUN.md](RUN.md)** et les deux notebooks
**[RUN.Rmd](RUN.Rmd)** (extraction) / **[RUN_aval.Rmd](RUN_aval.Rmd)**
(exploitation). L'historique tracé de chaque modification :
**[MODIFICATIONS_V8.md](MODIFICATIONS_V8.md)**.

## Principes clés

- **Représentativité des diagnostics d'abord** : chaque code DP présent au
  catalogue est représenté dans chaque campagne de génération (quota par DP,
  plafonds pour les activités standardisées, recyclage contrôlé).
- **Corriger le codage, pas le reproduire** : le sous-codage des séjours
  courts n'est pas reproduit (les comorbidités chroniques sont tirées selon
  leur prévalence observée sur les séjours longs, mieux codés) ; les codes
  « sans précision » manifestement erronés (E669x) sont convertis selon la
  distribution réelle des codes précis.
- **Traçabilité totale** : chaque fichier produit porte un méta-fichier
  (paramètres, périmètre, version des doctrines) ; chaque scénario porte un
  identifiant stable recalculable ; un registre append-only mémorise ce qui
  a servi, campagne après campagne.
- **Reproductibilité** : graines aléatoires dérivées par paquet / population /
  campagne — une exécution interrompue puis reprise, ou parallélisée, donne
  un résultat strictement identique à une exécution d'une traite.
- **Règle d'or** : les requêtes vers la base sont des copies tracées des
  scripts v7 historiques (non testables hors plateforme) ; tout le code neuf
  vit dans des fonctions pures, testées hors base.

## Démarrage

**Hors plateforme** (n'importe quel poste) — exécuter les tests :

```sh
Rscript tests/test_helpers.R          # ~300 vérifications des fonctions pures
Rscript tests/test_chaines_sqlite.R   # les scripts réels sur une base SQLite
                                      # factice (mock de pRatihque inclus)
```

Dépendances : R ≥ 4.x, dplyr, dbplyr, purrr, tidyr, stringr, tibble, yaml,
DBI, RSQLite ; `arrow` recommandé (repli automatique en mock dans les tests
s'il est absent) ; `openssl` ou `digest` pour les identifiants.

## Mode démo

Le pipeline **complet**, hors plateforme, sur une base SQLite fictive (aucun accès à la base
nationale, faux paquet `pRatihque` installé à la volée) :

```sh
Rscript demo/creer_base_demo.R    # construit demo/base_demo.sqlite (graine fixe)
Rscript demo/lancer_demo.R        # extraction -> catalogue -> tirage -> finalisation, sous demo/resultats/
cat demo/resultats/exports_demo/echantillon_revue.csv
```

**Attention : données aléatoires, aucune validité épidémiologique.** La démo sert à voir
tourner les étapes, les garde-fous et les livrables ; le détail (profil « démo », tables,
sorties) est dans [demo/README.md](demo/README.md). Les deux notebooks `RUN.Rmd` et `RUN_aval.Rmd`
se déroulent aussi tels quels sur la base démo (chunk « Mode démo » en tête) : ils sont la
documentation exécutable du projet. L'intégration continue exécute la démo et les deux notebooks
à chaque push.

**Sur la plateforme sécurisée** — définir la racine du projet (variable d'environnement
`SCENARIOS_PMSI_PATH`, ou `config_locale.R` copié de `config_locale.exemple.R`, jamais versionné),
puis suivre [RUN.md](RUN.md) : profil
diagnostic (mesure de l'apport de chaque année/catégorie d'établissements),
choix du périmètre, puis profil production. Prérequis non distribués ici :
le paquet `pRatihque` (ATIH), et les référentiels externes attendus par
`referentiels.R` (table CIM-10 avec libellés, caractérisation
aigu/chronique des codes) ; `referentiels/codes_diabete.yaml` (doctrine diabète,
listes de codes) est versionné ici.

## Carte du dépôt

| Fichier / dossier | Rôle |
|---|---|
| `config.R` | Paramètres, profils diagnostic/production, surcharges ; `config_locale.exemple.R` = modèle de la configuration propre au poste (`config_locale.R`, non versionné) |
| `helpers.R` | Fonctions pures (sections A→G, une par chantier) |
| `etapes.R` | Requêtes base + fonctions d'étape + tableau de bord |
| `extraction.R` | Lanceur extraction (46 lignes) |
| `tirage.R` | Lanceur aval (32 lignes, sans base) |
| `referentiels/` | Doctrine versionnée (typologie des séjours, exclusions…) |
| `tests/` | Deux suites + instantanés de référence des versions antérieures |
| `utils.R`, `referentiels.R`, `exclusions.R` | Héritage v7 toujours utilisé (les scripts v7 historiques, sources des diffs de la règle d'or, sont relus par `git show e9f70c7:<fichier>`) |
| `demo/` | Mode démo hors plateforme : générateur de données fictives, mock `pRatihque`, base SQLite, lanceur, session démo pour les notebooks et lanceur de notebooks ([demo/README.md](demo/README.md)) |
| `.github/workflows/tests.yml` | Intégration continue : les deux suites puis la démo bout en bout |
| `VISITE_GUIDEE.md` | La visite guidée : où est chaque chose et pourquoi |
| `RUN.md`, `RUN.Rmd`, `RUN_aval.Rmd` | Séquences opérationnelles |
| `MODIFICATIONS_V8.md` | Journal exhaustif : provenance des blocs, écarts, questions Q1…Q40 |

## Données et confidentialité

Ce dépôt ne contient **aucune donnée** — ni individuelle, ni agrégée — et
n'en contiendra jamais : uniquement du code, de la doctrine (yaml) et de la
documentation. Les sorties du pipeline (comptes agrégés, catalogues,
scénarios) restent dans l'espace sécurisé de la plateforme des données de
santé ; les seuils d'effectifs (> 10 par profil, seuils dédiés par
référentiel) et la doctrine de divulgation sont documentés dans
[MODIFICATIONS_V8.md](MODIFICATIONS_V8.md) et [VISITE_GUIDEE.md](VISITE_GUIDEE.md).

## Statut, licence, citation

Projet en développement actif. [![tests](https://github.com/24p11/scenarios_bn_pmsi/actions/workflows/tests.yml/badge.svg)](https://github.com/24p11/scenarios_bn_pmsi/actions/workflows/tests.yml)
Licence : MIT, © 2026 Rémi Flicoteaux / AP-HP — voir [LICENSE](LICENSE).
Pour citer ce travail : voir [CITATION.cff](CITATION.cff) (auteur : Rémi Flicoteaux, AP-HP ;
code développé avec l'assistance de Claude Code comme outil d'aide au développement).

Contact : ouvrir une *issue* GitHub.
