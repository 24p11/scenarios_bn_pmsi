# RUN.md — séquence opérationnelle du pipeline scenarios_bn_pmsi v8

Fichiers : `config_v8.R` (config + profils), `helpers_v8.R` (helpers purs),
`extraction_associations_codes_v8.R` (base → agrégats parquet), `tirage_scenarios_v8.R`
(parquets → scénarios, **sans base**). Spécification : `SPEC_V8.md` ; journal : `MODIFICATIONS_V8.md`.

Variables d'environnement : `SCENARIOS_PMSI_PATH` (racine du projet, défaut = chemin commun),
`SCENARIOS_PMSI_PROFIL` (`diagnostic` par défaut, ou `production`),
`SCENARIOS_PMSI_SURCHARGE` (fichier R optionnel de surcharges, évalué après le bloc profil).

Arborescence des résultats (`PATH_RESULTS = <projet>/results/`) :

| Dossier | Contenu | Partagé entre profils |
|---------|---------|-----------------------|
| `results/partiels/` | `catalogue_partiel_<etbs>_<an>.parquet` (un par itération), `partiels_meta.yaml` | **oui** (cache inter-profils) |
| `results/exports_diagnostic/` | produits du profil diagnostic (refs, catalogue, tirage, livrables) | non |
| `results/exports/` | produits du profil production | non |
| `results/exports*/chunks/` | chunks de tirage (`courts_chunk_0001.parquet`, `longs_chunk_0001.parquet`, …) | non |

Aucune table n'est persistée en base : `prep_data_<an>` et `prep_das_chro_<an>` sont des
tables **temporaires**, recréées à la demande par la résolution des besoins.

Conversion E669 → E660 (`CONVERSION_E669 <- TRUE` dans les deux profils) : doctrine DIM, les
codes E669x « sans précision » sont convertis en E660x sur toutes les surfaces (DP/DR pivot,
graines, DAS de complétion, référentiels), entièrement **après** collecte, côté R, sans
toucher aux chaînes base. Suffixe conservé (E6692 → E6602) ; un E669 nu est réparti sur les
classes E660x observées (strate cage × sexe, sinon global, sinon `E660` + `BARE_E669_DEFAUT`).
Le rapport d'extraction (`rapport_extraction_v8_<date>.txt`) mesure l'impact (effectifs
convertis, lignes fusionnées, profils entrés au catalogue par fusion, changements de niveau CMA).

---

## Étape 1 — profil diagnostic

**But :** mesurer l'apport marginal de chaque (catégorie d'établissements, année) et valider
la complétion DAS sur petit volume (1 000 scénarios longs, mode `quota_dp`).

1. Extraction instrumentée complète (toutes les années 17:AN_REF, CHR/U et CH) :
   ```
   SCENARIOS_PMSI_PROFIL=diagnostic Rscript extraction_associations_codes_v8.R
   ```
   Le script imprime d'abord le **plan** (itérations à faire / sautées, refs à faire / sautées,
   années préparées), puis une ligne d'apport par itération.
   Critères de passage : plan cohérent ; `results/partiels/` contient un parquet par
   (etbs, an) ; `exports_diagnostic/` contient les 10 refs (dont `distribution_e660.parquet`),
   `catalogue_longs_seuil.parquet`, `catalogue_longs_seuil_meta.yaml`, `diagnostic_apports.csv`,
   `rapport_extraction_v8_<date>.txt` (section 3 : impact de la conversion E669, écart de
   volumétrie assumé par doctrine).
2. Lecture de `exports_diagnostic/diagnostic_apports.csv` : colonnes `etbs, an, statut,
   nb_lignes_partiel, nb_lignes_cumul, nb_diag2_cumul, nb_diag2_nouveaux` dans l'ordre
   d'exécution (CHR/U 17…AN_REF puis CH 17…AN_REF). Repérer à partir de quelle année / quelle
   catégorie l'apport en diag2 nouveaux et en lignes devient marginal.
3. Tirage 1 000 :
   ```
   SCENARIOS_PMSI_PROFIL=diagnostic Rscript tirage_scenarios_v8.R
   ```
   Produit dans `exports_diagnostic/` : `scenarios_courts_v8_<date>.parquet`,
   `scenarios_longs_tirage_v8_<date>.parquet`, `selection_longs.parquet` (+ `_effectifs.csv`),
   `meta_tirage.yaml`, `rapport_v8_<date>.txt`, `echantillon_revue.csv`, `top30_das_par_cmd.csv`.
   Critères de passage : rapport section 5 « TOTAL anomalies = 0 » (inclut les ^E669 résiduels,
   attendus à 0 quand `CONVERSION_E669: TRUE` dans le meta) ; distribution du nombre de DAS par
   classe d'âge conforme aux cibles ; taux « sans précision » acceptable.
4. Revue humaine de `echantillon_revue.csv` (50 scénarios : 25 courts + 25 longs, répartis
   sur les CMD, libellés CIM, graine marquée `[G]`). Critère : validation DIM de la
   vraisemblance des associations avant toute production.

## Étape 2 — choix du périmètre de production

D'après `diagnostic_apports.csv`, fixer dans le bloc `production` de `config_v8.R` :
`ANS_HISTORIQUE` (ex. `22:AN_REF`) et `TYPES_ETBS_LONGS` (ex. `"CHR/U"` seul).
Ne pas toucher à `K_GRAINE_LONGS` sans vider `results/partiels/` (voir règles de cache).

## Étape 3 — profil production

1. Ré-agrégation depuis les partiels :
   ```
   SCENARIOS_PMSI_PROFIL=production Rscript extraction_associations_codes_v8.R
   ```
   Si le diagnostic a déjà tout extrait, le plan indique « 0 itération à faire » ; seules les
   9 refs sont calculées dans `exports/` (elles sont propres au profil, donc `prep_data(AN_REF)`
   est recréée une fois). Pour ne pas les recalculer, copier les `ref_*.parquet`, `distribution_e660`,
   `pivots_courts`, `v_admin_*`, `referentiel_*` de `exports_diagnostic/` vers `exports/` : copie sûre
   **si et seulement si** `AN_REF`, `SEUIL_REF_DAS`, `SEUIL_REF_IMPRECIS`, `SEUIL_REF_PAIRES`,
   `CONVERSION_E669` et `BARE_E669_DEFAUT` sont identiques entre les deux profils ; sinon
   `FORCER_REFS <- TRUE` et recalcul.
   Critères : `catalogue_longs_seuil_meta.yaml` porte `PROFIL: production` et le périmètre choisi.
2. Tirage par paliers, en surchargeant `BUDGET_TOTAL_LONGS` sans éditer la config :
   ```
   echo 'BUDGET_TOTAL_LONGS <- 100000L' > /tmp/palier.R
   SCENARIOS_PMSI_PROFIL=production SCENARIOS_PMSI_SURCHARGE=/tmp/palier.R Rscript tirage_scenarios_v8.R
   ```
   Critères : rapport sans anomalie, volumétrie = nrow × NB_VARIANTES annoncé, temps
   d'exécution extrapolable. Puis budget complet (10 000 000) : **vider `exports/chunks/` et
   `exports/meta_tirage.yaml` avant** (le budget change NB_VARIANTES, le garde-fou
   `meta_tirage.yaml` refuse sinon de reprendre des chunks tirés avec d'autres paramètres).
   Le parquet final `scenarios_longs_tirage_v8_<date>.parquet` est le livrable.

---

## Règles de cache

- **`results/partiels/`** : dépend de `K_GRAINE_LONGS` et de la logique amont (`prep_data`,
  `prep_scenarios2`, `NBDA_MAX`, `DUREE_LONGS`, `PIVOTS_LONGS`), **pas** de `SEUIL_PIVOT` ni du
  périmètre d'années. `partiels_meta.yaml` mémorise ces valeurs : `K_GRAINE_LONGS`, `NBDA_MAX`,
  `DUREE_LONGS` ou `PIVOTS_LONGS` différent → `stop()` demandant de vider le dossier ;
  `VERSION_SCRIPT` différent → avertissement. Vider le dossier (`rm results/partiels/*`) après
  tout changement de `prep_data` / `prep_scenarios2` ou de l'une de ces clés.
- **Conversion E669** : les partiels sont stockés en **codes bruts** ; la conversion s'applique à
  la ré-agrégation. Basculer `CONVERSION_E669` ne nécessite donc PAS de vider `results/partiels/`
  (ce n'est pas une clé de `partiels_meta.yaml`), mais il faut recalculer les refs du profil
  (`FORCER_REFS <- TRUE`) et vider chunks / sélection / `meta_tirage.yaml` du tirage.
- **Refs (`exports*/`)** : sautées si le parquet existe. `distribution_e660.parquet` (distribution
  E660x de référence, calculée sur les comptes bruts de `ref_das_chronique`) est une ref comme les autres. Après un changement d'`AN_REF` ou une
  correction amont, passer `FORCER_REFS <- TRUE` (config ou surcharge) une fois, puis remettre
  `FALSE`.
- **Chunks et sélection (`exports*/chunks/`, `selection_longs.parquet`, `meta_tirage.yaml`)** :
  la reprise après plantage relance le tirage tel quel (chunks présents sautés, sélection relue,
  identité bit à bit garantie par un seed par chunk). Après changement de `MODE_SELECTION`,
  `BUDGET_TOTAL_LONGS`, `QUOTA_MIN_PAR_UNITE`, `CHUNK_SIZE`, `SEED` ou du catalogue, vider ces
  trois éléments (le garde-fou `meta_tirage.yaml` le demande).
- **Sessions multiples** : les tables temporaires disparaissent à la déconnexion. À chaque
  lancement, la résolution des besoins ne recrée `prep_data_<an>` que pour les années des
  itérations manquantes (et `AN_REF` si une ref manque) ; `prep_das_chronique` uniquement si une
  ref chronique manque. Ne jamais persister de table en base.
- **Relance après plantage de l'extraction** : relancer la même commande ; les partiels et refs
  déjà écrits sont sautés.

## Tests hors base

```
Rscript tests/test_helpers.R            # helpers purs (chunking, sélection, résolution des besoins, …)
Rscript tests/test_chaines_sqlite.R     # scripts réels sur SQLite fichier : sessions multiples, reprise, tirage sans base
```
Prérequis du second : dbplyr, DBI, RSQLite, arrow, yaml (`R_LIBS_TEST=<lib>` pour une bibliothèque additionnelle).
