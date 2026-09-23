# Mode démo — le pipeline complet hors plateforme, sur une base fictive

> **Avertissement : données aléatoires, aucune validité épidémiologique.** La base démo est
> tirée uniformément dans de petits pools de codes CIM-10 et de GHM ; les scénarios produits
> n'ont aucun sens clinique ni statistique. Le mode démo sert à **voir tourner** le pipeline
> (chaînes dbplyr, étapes, garde-fous, livrables) sans accès à la base nationale ni au paquet
> ATIH `pRatihque`.

## Les trois commandes

```sh
Rscript demo/creer_base_demo.R    # 1. construit demo/base_demo.sqlite (4000 séjours × 3 millésimes, graine fixe)
Rscript demo/lancer_demo.R        # 2. extraction -> repartitionnement -> tirage -> finalisation, tout sous demo/resultats/
cat demo/resultats/production/60_export_final/echantillon_revue_DEMO.csv   # 3. 50 scénarios de revue (chemin rappelé en fin de démo)
```

Depuis la racine du dépôt. Prérequis : R ≥ 4.x, dplyr, dbplyr, purrr, tidyr, stringr, tibble,
yaml, DBI, RSQLite, readr, `openssl` ou `digest` ; `arrow` recommandé (sans lui, repli mock
RDS : les fichiers `.parquet` produits ne sont alors lisibles que par ce mock). Ordre de
grandeur : quelques minutes, moins de 1 Go de RAM. `R_LIBS_TEST=<lib>` ajoute une bibliothèque
de paquets (comme pour les tests).

Options de `creer_base_demo.R` : `--n=<séjours par millésime>` (défaut 4000, même graine =
même base), `--annees=17,20,26`, `--graine=20260907`, `--sortie=<fichier>`. Le schéma des
tables produites est affiché.

## Ce que fait `lancer_demo.R`

1. source `demo/session_demo.R` (commun aux notebooks) : installe dans `tempdir()` le faux paquet
   `pRatihque` (SQLite) et, si besoin, le mock `arrow` (`demo/mock_pratihque.R`, source unique partagée
   avec les tests), crée la base si elle manque ;
2. copie le code du dépôt dans `demo/resultats/projet_demo/` avec des **stubs** `utils.R` /
   `referentiels.R` (les référentiels Excel externes ne sont pas distribués) ;
3. écrit le profil « démo » (`demo/resultats/surcharge_demo.R`, surcharge du profil production) :
   millésimes de la base, seuils abaissés (`SEUIL_PIVOT = 1`, refs à 5), `NB_CRH_CIBLE = 2000`,
   `MODE_SELECTION = quota_dp_fixe`, campagne `DEMO`, registre actif, sorties sous `demo/resultats/` ;
4. exécute `extraction.R` (prep_data → refs → partiels → catalogue), puis
   `etape_repartitionner_catalogue()` (typologie DPEC/TPEC), ferme la connexion et **interdit
   tout appel base** (`pmsi_mock_interdit`), puis `tirage.R` (sélection → courts de la campagne →
   DAS longs → habillage → finalisation) ;
5. affiche le résumé : nombre de scénarios longs par population, courts, répartition par TPEC,
   chemins des livrables.

`demo/resultats/` est **vidé à chaque lancement** (la démo repart de zéro) et ignoré par git,
comme `demo/base_demo*`.

## Dérouler les notebooks en mode démo

Les deux notebooks de parcours sont la documentation **exécutable** du projet et tournent tels quels sur la
base démo, **de haut en bas, sans rien sauter** (un notebook = un parcours utilisateur). Dans RStudio, ouvrir
`01_preparation_donnees.Rmd` puis `02_campagne.Rmd` et exécuter d'abord leur premier chunk « Mode démo »
(il source `demo/session_demo.R` : faux `pRatihque`, base créée si absente, projet démo, profil « démo »,
variable `SCENARIOS_PMSI_DEMO`) ; le chunk `session` affiche alors « MODE DÉMO » en évidence. Le chunk
`ouvrir_campagne` de 02 reconnaît le mode démo (la campagne y est pilotée par la surcharge démo,
`SCENARIOS_PMSI_DEMO_CAMPAGNE`, `campagne.R` n'est pas écrit) ; les `JE_CONFIRME…` restent à `FALSE`.
Ordre : `01` (session → prep_data → partiels → références (dont le tirable courts) → catalogue → repartitionnement →
vérifications) puis `02` (session → ouvrir_campagne → sélection → tirage longs → tirage courts → habillage →
finalisation → registre → rapport → revue). `demo/resultats/` n'est pas vidé entre les deux (sauf
`SCENARIOS_PMSI_DEMO_RAZ=1`). `03_outils_maintenance.Rmd` (outils d'exception, chunks en `eval=FALSE`) est hors démo.

Sans RStudio (et en CI) :

```sh
Rscript demo/executer_notebook.R --raz 01_preparation_donnees.Rmd   # exécute les chunks dans l'ordre, comme des clics « Run »
Rscript demo/executer_notebook.R 02_campagne.Rmd
```

La CI déroule suites, démo et notebooks dans deux jobs, **avec** et **sans** arrow (repli mock RDS) :
toute lecture des notebooks passe par des lecteurs à repli (`lire_catalogue`, `lire_registre`,
`lire_corpus_final`, `lire_si_present`), jamais par un dataset arrow direct. La promesse « tout s'exécute
dans l'ordre » est ainsi testée à chaque push.

`executer_notebook.R` n'utilise pas `rmarkdown::render` : les notebooks posent
`knitr::opts_chunk$set(eval = FALSE)` (un Knit ne doit jamais lancer le pipeline) ; le lanceur lit les
chunks, saute `opts`, exécute `mode_demo` puis tout le reste, et s'arrête à la première erreur (code de
sortie non nul) ; il refuse `03_outils_maintenance.Rmd`.

## Tables de la base démo (`creer_base_demo.R`)

| Table | Colonnes | Rôle |
|---|---|---|
| `PRD_VUE_MCOBL_20<an>.fixe` | anonyme, ident, dp, dr, age, sexe, provenance, modesortie, destination, duree, rumdudp, nbda, ghm2, passage_urg, nbrum, raac | un séjour par ligne |
| `PRD_VUE_MCOBL_20<an>.um` | ident, rum, finessgeo, type_hospum_1, type_rum_1 | unités médicales (1 à 2 RUM) |
| `PRD_VUE_MCOBL_20<an>.diag` | ident, rum, diag, typ_diag | DP (typ_diag 1) et DAS (typ_diag 5) |
| `PRD_VUE_MCOBL_20<an>.rgp` | ident, ghmv2023, ghmv2021 | GHM par version |
| `nomgen.finessgeo` | finessgeo, categ_pmsi | 3 établissements fictifs (CHR/U, CH) |
| `prd_vue_nompmsi.mco_diag_niveau` | code, v2021, v2023, v2025 | niveaux de CMA (aléatoires) |
| `prd_vue_nompmsi.all_cim10_caract_patient` | code, type_liste, caract | chronique / aigu |

Plus deux jeux de fixtures contrôlées sur le dernier millésime (écart B1-10 : `IDENT_B110` ;
fusion E669 → E660 : `IDENT_FUSION`, GHM `88M991`), utilisés par les tests.

## Sorties (`demo/resultats/`, arborescence par étapes)

| Chemin | Contenu |
|---|---|
| `00_partiels/catalogue_partiel_<etbs>_<an>.parquet` + `_meta.yaml` | comptes par (établissements × millésime) [partagé] |
| `10_references/ref_*.parquet` + `_meta.yaml` | les 10 références (préfixe unique `ref_`) [partagé] |
| `20_catalogue/catalogue_longs_seuil/part_<L>.parquet` + `_meta.yaml` ; `catalogue_longs_seuil_meta.yaml` ; `rapport_extraction.txt` | le catalogue par lettre de DP (+ lettre, DPEC, TPEC, id_profil) [partagé] |
| `30_courts/ref_pivots_courts.parquet` + `_meta.yaml` | le TIRABLE courts (pivots = catalogue des courts, `ANS_COURTS`) [partagé] ; les scénarios courts sont tirés PAR campagne sous `production/40_campagnes/DEMO/chunks_courts/` et `habille/courts/` (id_profil `k…`, id_scenario, hash_das) |
| `90_diagnostics/diagnostic_apports.csv`, `recouvrement.csv`, `diagnostic_memoire_production.csv` | diagnostics |
| `production/40_campagnes/DEMO/selection/`, `chunks/`, `habille/` | sélection, tirage des DAS par paquets, habillage (transitoires) |
| `production/50_registre/registre_tirages/registre_DEMO.parquet` | registre append-only de la campagne |
| `production/60_export_final/scenarios_DEMO.parquet` + `scenarios_DEMO_meta.yaml` | **le livrable unique** : longs (toutes populations) et courts de la campagne, colonne `branche` en tête |
| `production/60_export_final/echantillon_revue_DEMO.csv`, `top30_das_par_cmd_DEMO.csv`, `rapport_DEMO.txt` | livrables de validation |

Chaque parquet a son méta-fichier yaml à côté : commencer par le lire. Voir
[VISITE_GUIDEE.md](../VISITE_GUIDEE.md) §3 pour le flux, [RUN.md](../RUN.md) pour la
séquence réelle sur plateforme.
