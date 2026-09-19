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
cat demo/resultats/exports_demo/echantillon_revue.csv   # 3. 50 scénarios de revue (chemin rappelé en fin de démo)
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
   tout appel base** (`pmsi_mock_interdit`), puis `tirage.R` (courts → sélection →
   DAS longs → habillage → finalisation) ;
5. affiche le résumé : nombre de scénarios longs par population, courts, répartition par TPEC,
   chemins des livrables.

`demo/resultats/` est **vidé à chaque lancement** (la démo repart de zéro) et ignoré par git,
comme `demo/base_demo*`.

## Dérouler les notebooks en mode démo

Les deux notebooks sont la documentation **exécutable** du projet et tournent tels quels sur la base
démo. Dans RStudio, ouvrir `RUN.Rmd` puis `RUN_aval.Rmd` et exécuter d'abord leur premier chunk
« Mode démo (optionnel) » (il source `demo/session_demo.R` : faux `pRatihque`, base créée si absente,
projet démo, profil « démo », variable `SCENARIOS_PMSI_DEMO`) ; le chunk `session` affiche alors
« MODE DÉMO » en évidence. Les chunks non pertinents hors plateforme (migration inter-profils, palier
de mesure, vidages, tests) le disent dans leur en-tête et portent l'option `demo=FALSE` ; les chunks
`JE_CONFIRME…` restent à `FALSE`. Ordre : `RUN.Rmd` (amont : prep_data → refs → courts → partiels →
catalogue → repartitionnement) puis `RUN_aval.Rmd` (cycle de campagne : sélection → tirage →
habillage → finalisation → registre → revue). `demo/resultats/` n'est pas vidé entre les deux
(sauf `SCENARIOS_PMSI_DEMO_RAZ=1`).

Sans RStudio (et en CI) :

```sh
Rscript demo/executer_notebook.R --raz RUN.Rmd     # exécute les chunks dans l'ordre, comme des clics « Run »
Rscript demo/executer_notebook.R RUN_aval.Rmd
```

La CI déroule suites, démo et notebooks dans deux jobs, **avec** et **sans** arrow (repli mock RDS) :
toute lecture des notebooks passe par des lecteurs à repli (`lire_catalogue`, `lire_registre`,
`lire_corpus_final`, `lire_si_present`), jamais par un dataset arrow direct.

`executer_notebook.R` n'utilise pas `rmarkdown::render` : les notebooks posent
`knitr::opts_chunk$set(eval = FALSE)` (un Knit ne doit jamais lancer le pipeline) ; le lanceur lit les
chunks, saute `opts` et les `demo=FALSE`, exécute `mode_demo` puis tout le reste, et s'arrête à la
première erreur (code de sortie non nul).

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

## Sorties (`demo/resultats/`)

| Chemin | Contenu |
|---|---|
| `partiels/catalogue_partiel_<etbs>_<an>.parquet` + `partiels_meta.yaml` | comptes par (établissements × millésime) |
| `exports_demo/ref_*.parquet`, `pivots_courts.parquet`, `v_admin_*.parquet`, `distribution_e660.parquet`, `referentiel_*.parquet` | les 10 références |
| `exports_demo/catalogue_longs_seuil/part_<L>.parquet` + `_sidecar.yaml` | le catalogue par lettre de DP (+ lettre, DPEC, TPEC, id_profil) |
| `exports_demo/selection_longs/<population>/` (+ `meta_tirage.yaml`) | la sélection (quota par DP, plafonds de classe) |
| `exports_demo/chunks/<population>/longs_chunk_XXXX.parquet` | tirage des DAS par paquets (reprise) |
| `exports_demo/habille/<population>/lot_XXXX.parquet` | habillage admin |
| `exports_demo/scenarios_longs_tirage_v8_DEMO/<population>/part_*.parquet` + `_meta.yaml` | **les scénarios longs** (pivots, graine, diagnostic_associes, DPEC, TPEC, id_scenario, campagne…) — un dossier par campagne |
| `exports_demo/scenarios_courts_v8_<date>.parquet` | les scénarios courts (id_profil `c…`, id_scenario, hash_das) |
| `exports_demo/registre_tirages/registre_DEMO.parquet` | registre append-only de la campagne |
| `exports_demo/echantillon_revue.csv`, `top30_das_par_cmd.csv`, `rapport_v8_<date>.txt`, `rapport_extraction_v8_<date>.txt`, `diagnostic_apports.csv`, `diagnostic_memoire.csv`, `recouvrement.csv` | livrables de validation et diagnostics |

Chaque parquet a son méta-fichier yaml à côté : commencer par le lire. Voir
[VISITE_GUIDEE.md](../VISITE_GUIDEE.md) §3 pour le flux, [RUN.md](../RUN.md) pour la
séquence réelle sur plateforme.
