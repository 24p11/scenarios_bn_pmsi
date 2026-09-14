# RUN.md — pipeline scenarios_bn_pmsi v8, exécution par étapes

Fichiers : `config_v8.R` (config + profils), `helpers_v8.R` (helpers purs), `etapes_v8.R`
(chaînes base déplacées telles quelles + fonctions d'étape + `etat_pipeline()`),
`extraction_associations_codes_v8.R` (script d'entrée extraction : n'appelle que les étapes),
`tirage_scenarios_v8.R` (script d'entrée tirage, **sans base**). Version notebook : `RUN.Rmd`.
Spécification : `SPEC_V8.md` ; journal : `MODIFICATIONS_V8.md`.

Variables d'environnement : `SCENARIOS_PMSI_PATH` (racine du projet), `SCENARIOS_PMSI_PROFIL`
(`diagnostic` par défaut, ou `production`), `SCENARIOS_PMSI_SURCHARGE` (fichier R optionnel de
surcharges, évalué après le bloc profil), `SCENARIOS_PMSI_ETAPES_SEULEMENT=1` (charger la session
— config, sources, connexion pour l'extraction — sans exécuter aucune étape).

## Les étapes

Chaque fonction affiche une bannière début/fin avec sa durée, est **idempotente** (saute ce qui
existe déjà selon les règles de cache et le dit), prend ses décisions en arguments explicites
(config en défaut), retourne (invisible) les fichiers produits, et échoue avec un message
actionnable (« lancez etape_X d'abord », « fichier Y manquant ») si elle est appelée hors ordre.

| Étape | Famille | Produit | Relancer quand | Cache |
|---|---|---|---|---|
| `etape_prep_data(ans = NULL)` | extraction (base) | tables temporaires `prep_data_<an>` (et `prep_das_chro_<AN_REF>` si une ref chronique manque) ; `partiels_meta.yaml` | à chaque nouvelle session avant refs / partiels (les tables temporaires disparaissent à la déconnexion) ; `ans` force des années | plan = partiels et refs manquants ; garde-fou `partiels_meta.yaml` (K, NBDA_MAX, DUREE_LONGS, PIVOTS_LONGS) |
| `etape_refs(forcer = FORCER_REFS)` | extraction (base) | 10 refs parquet dans `EXPORTS_DIR` (dont `distribution_e660`, `pivots_courts`, `v_admin_*`) | après changement d'`AN_REF`, des seuils de refs, de `CONVERSION_E669` (`forcer = TRUE`) | ref sautée si son parquet existe |
| `etape_partiels_longs(iterations = NULL)` | extraction (base) | `PARTIELS_DIR/catalogue_partiel_<etbs>_<an>.parquet` manquants ; `diagnostic_apports.csv` ; `recouvrement.csv` | ajout d'années / de catégories au plan ; `iterations = data.frame(etbs, an)` pour une itération isolée (supprimer son partiel pour le recalculer) | partiel sauté s'il existe ; partiels en codes bruts, partagés entre profils |
| `etape_catalogue(ans = ANS_HISTORIQUE, etbs = TYPES_ETBS_LONGS)` | extraction (**sans base**) | `catalogue_longs_seuil.parquet` + `_meta.yaml` (trace du périmètre passé), `rapport_extraction_v8_<date>.txt`, `diagnostic_memoire.csv` | **décision de périmètre** : relancer avec les `ans`/`etbs` retenus | agrégation deux étages hors RAM depuis les partiels du périmètre ; conversion E669 puis seuil |
| `etape_tirage_courts()` | tirage | `chunks/courts_chunk_*.parquet`, `scenarios_courts_v8_<date>.parquet` | une fois par jeu de refs (AN_REF uniquement) | chunks présents sautés (reprise bit à bit) |
| `etape_selection_longs(budget = BUDGET_TOTAL_LONGS, mode = MODE_SELECTION)` | tirage | `selection_longs.parquet` (quota_dp), `selection_longs_effectifs.csv`, `meta_tirage.yaml` | changement de budget / mode : vider d'abord chunks + sélection + méta (garde-fou `meta_tirage.yaml`) | sélection relue si présente, jamais re-tirée |
| `etape_tirage_das_longs()` | tirage | `chunks/longs_chunk_*.parquet` (assemblé en mémoire de session) | reprise après plantage : relancer telle quelle | chunks présents sautés ; sélection relue si la session est neuve |
| `etape_habillage_longs()` | tirage | scénarios habillés en mémoire de session (jointure `v_admin_longs.parquet` relu, jamais `prep_data`) | après `etape_tirage_das_longs()` ; relit les chunks si la session est neuve | — |
| `etape_finalisation()` | tirage | `scenarios_longs_tirage_v8_<date>.parquet`, `rapport_v8_<date>.txt`, `echantillon_revue.csv`, `top30_das_par_cmd.csv` | après habillage ; reconstruit ce qui manque en session (chunks, stats des courts depuis leur parquet) | — |

`etat_pipeline()` : tableau de bord FAIT / PARTIEL / À FAIRE par étape pour le profil courant,
avec preuves (partiels présents / attendus, refs / 10, catalogue + date + périmètre, sélection +
budget, chunks n / attendus, exports finaux + dates). Fichiers seulement : appelable partout, sans
connexion (les tables temporaires affichent « inconnu hors connexion »).

Scripts bout-en-bout : `Rscript extraction_associations_codes_v8.R` = prep_data → refs →
partiels → catalogue ; `Rscript tirage_scenarios_v8.R` = courts → sélection → DAS longs →
habillage → finalisation. Comportement identique à l'ancien flux monolithique (identité bit à
bit prouvée par `tests/test_chaines_sqlite.R`).

Arborescence : `results/partiels/` (partagé entre profils), `results/exports_diagnostic/` ou
`results/exports/` (par profil, avec `chunks/`). Aucune table n'est persistée en base.

---

## Étape 1 — prep_data (à chaque session d'extraction)

```
SCENARIOS_PMSI_PROFIL=diagnostic ; conn <- pRatihque::connection_database() ; etape_prep_data()
```
Le plan imprimé dit ce qui manque (itérations, refs) et donc quelles années sont préparées.
Critère : aucune erreur SQL (`ROW_NUMBER()` / `COUNT() OVER` sont le dialecte du run), plan cohérent.

## Étape 2 — séjours courts (une année : AN_REF)

`etape_refs()` (si les refs manquent) puis `etape_tirage_courts()`. Produit `scenarios_courts_v8_<date>.parquet`.
Critères : chunks courts complets (`etat_pipeline()`), export présent. Les contrôles §8.2 des
courts sont repris dans le rapport de `etape_finalisation()`.

## Étape 3 — séjours longs

1. **Partiels** : `etape_partiels_longs()` (toutes les itérations du profil ; `iterations =` pour
   une itération isolée). Lire `diagnostic_apports.csv` (apport de chaque (etbs, an) : lignes,
   séjours, diag2 nouveaux) et `recouvrement.csv` (paires `PAIRES_RECOUVREMENT` : part des
   combinaisons / séjours de l'année B déjà vus en A, pivots déjà vus, diag2 nouveaux). Lire
   `diagnostic_memoire.csv` (pic gc() par morceau / partiel / ref / étage ; `alerte = TRUE` désigne
   l'étape à réduire ; `COLLECT_PAR_MORCEAUX <- TRUE` par défaut).
2. **Refs / intermédiaires** : `etape_refs()` (10 refs, dont `distribution_e660.parquet`).
3. **DÉCISION de périmètre** : `etape_catalogue(ans = 22:26, etbs = "CHR/U")` par exemple. Le
   méta enregistre les `ans`/`etbs` passés ; `rapport_extraction_v8_<date>.txt` mesure l'impact de
   la conversion E669 (fusions, profils entrés au catalogue : écart de volumétrie assumé par
   doctrine). C'est le « fichier parquet sans les DAS ».
4. **Sélection** : `etape_selection_longs(budget = 1000, mode = "quota_dp")` (diagnostic) ou
   `etape_selection_longs()` (production : `catalogue_complet`, 10 000 000 ; palier 100 000 conseillé
   d'abord). Changer de budget/mode impose de vider chunks + sélection + `meta_tirage.yaml`.
5. **Tirage DAS par chunks** : `etape_tirage_das_longs()` (reprise : relancer telle quelle).
6. **Habillage admin** : `etape_habillage_longs()`.
7. **Fichiers définitifs** : `etape_finalisation()` → `scenarios_longs_tirage_v8_<date>.parquet`,
   rapport (critère : « TOTAL anomalies = 0 », dont ^E669 résiduels), `echantillon_revue.csv`
   (50 scénarios, revue humaine DIM avant production), `top30_das_par_cmd.csv`.

Passage diagnostic → production : éditer le bloc `production` de `config_v8.R` (ANS_HISTORIQUE,
TYPES_ETBS_LONGS) d'après apports + recouvrement, puis `SCENARIOS_PMSI_PROFIL=production` et les
mêmes étapes ; les partiels sont réutilisés, seules les refs sont recalculées dans `exports/`
(copie possible depuis `exports_diagnostic/` si `AN_REF`, `SEUIL_REF_*`, `CONVERSION_E669`,
`BARE_E669_DEFAUT` sont identiques, sinon `etape_refs(forcer = TRUE)`).

---

## Règles de cache

- `results/partiels/` : dépend de `K_GRAINE_LONGS`, `NBDA_MAX`, `DUREE_LONGS`, `PIVOTS_LONGS` et de la
  logique amont (clés bloquantes de `partiels_meta.yaml` ; `VERSION_SCRIPT` en avertissement) ;
  **pas** de `SEUIL_PIVOT`, du périmètre d'années ni de `CONVERSION_E669` (partiels en codes bruts,
  conversion à la ré-agrégation). Vider après tout changement de ces clés ou de `prep_data` /
  `prep_scenarios2`. Partiels antérieurs au chantier mémoire : valides (équivalence prouvée).
- Refs (`exports*/`) : sautées si le parquet existe ; `etape_refs(forcer = TRUE)` après changement
  d'`AN_REF`, d'un `SEUIL_REF_*`, de `CONVERSION_E669` / `BARE_E669_DEFAUT` ou correction amont.
- Chunks / sélection / `meta_tirage.yaml` : reprise après plantage telle quelle (identité bit à bit par
  seed par chunk) ; à vider après changement de `MODE_SELECTION`, `BUDGET_TOTAL_LONGS`,
  `QUOTA_MIN_PAR_UNITE`, `CHUNK_SIZE`, `SEED` ou du catalogue (le garde-fou le demande).
- Sessions multiples : les tables temporaires (`prep_data_<an>`, `prep_das_chro_<an>`,
  `prep_topk_tmp`) disparaissent à la déconnexion ; `etape_prep_data()` recrée à la demande.
  Ne jamais persister de table en base.

## Tests hors base

```
Rscript tests/test_helpers.R            # helpers purs
Rscript tests/test_chaines_sqlite.R     # scripts réels sur SQLite fichier : sessions multiples, étapes, identité avec les anciens scripts
```
Prérequis du second : dbplyr, DBI, RSQLite, yaml (arrow réel ou mock RDS de repli) ;
`R_LIBS_TEST=<lib>` pour une bibliothèque additionnelle.
