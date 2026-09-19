# RUN.md — pipeline scenarios_bn_pmsi v8, exécution par étapes

Fichiers : `config.R` (config + profils), `helpers.R` (helpers purs), `etapes.R`
(chaînes base déplacées telles quelles + fonctions d'étape + `etat_pipeline()`),
`extraction.R` (script d'entrée extraction : n'appelle que les étapes),
`tirage.R` (script d'entrée tirage, **sans base**). Notebooks : `RUN.Rmd` (amont : extraction,
diagnostic, courts) et `RUN_aval.Rmd` (exploitation du catalogue parquet : repartitionnement, campagnes).
Spécification : `SPEC_V8.md` ; journal : `MODIFICATIONS_V8.md`.

Variables d'environnement : `SCENARIOS_PMSI_PATH` (racine du projet ; sinon `config_locale.R` à la racine du
dépôt, non versionné, copié de `config_locale.exemple.R` — aucun chemin personnel n'est versionné, le pipeline
s'arrête avec un message explicite si ni l'un ni l'autre n'est défini), `SCENARIOS_PMSI_PROFIL`
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
| `etape_catalogue(ans = ANS_HISTORIQUE, etbs = TYPES_ETBS_LONGS)` | extraction (**sans base**) | `catalogue_longs_seuil.parquet` + `_meta.yaml` (trace du périmètre passé), `rapport_extraction_v8_<date>.txt`, `diagnostic_memoire.csv` | **décision de périmètre** : relancer avec les `ans`/`etbs` retenus. **Le catalogue de production (21,6 M lignes) existe : ne JAMAIS le reconstruire sur ce périmètre** | agrégation deux étages hors RAM depuis les partiels du périmètre ; conversion E669 puis seuil |
| `etape_repartitionner_catalogue()` | aval (**sans base**) | `catalogue_longs_seuil/part_<L>.parquet` (par lettre de DP, + `lettre`, `DPEC`, `TPEC`) + `_sidecar.yaml` ; monofichier renommé `.ancien` | une fois par catalogue, et après changement de version de `typologie_sejours.yaml` (le garde-fou l'impose) | idempotente ; lecture par morceaux de lettres |
| `etape_tirage_courts(chunk_range = NULL)` | tirage | `chunks/courts_chunk_*.parquet` (+ sidecar `courts_chunks_meta.yaml`), `scenarios_courts_v8_<date>.parquet` | une fois par jeu de refs (AN_REF uniquement) | chunks présents sautés (reprise bit à bit, mêmes paramètres de découpage exigés) |
| `etape_selection_longs(budget = NB_CRH_CIBLE, mode = MODE_SELECTION, k = NB_LIGNES_PAR_DP)` | tirage | **quota_dp_fixe** (production) : `selection_longs/<population>/part_<L>.parquet`, `selection_longs_effectifs.csv`, `selection_longs_stats_dp.csv`, `meta_tirage.yaml` par population + global ; quota_dp (diagnostic) : `selection_longs.parquet` | changement de budget / mode / k : vider d'abord chunks + sélection + méta (garde-fou `meta_tirage.yaml`) | sélection relue si présente, jamais re-tirée ; `catalogue_complet` retiré (stop si budget < catalogue) |
| `etape_tirage_das_longs(chunk_range = NULL, populations = …)` | tirage | `chunks/<population>/longs_chunk_*.parquet` (+ sidecar) ; rien en RAM (fixe) | reprise : relancer telle quelle ; **parallélisme** : une session par plage `chunk_range = c(i, j)` disjointe, même dossier | chunks présents sautés ; écriture atomique (.tmp) ; `ref_das_aigu` indexé une fois ; débit imprimé par chunk |
| `etape_habillage_longs(populations = …)` | tirage | `habille/<population>/lot_*.parquet` (jointure `v_admin_longs.parquet` relu par lots de `LOT_CHUNKS_FINALISATION` chunks, DPEC/TPEC recalculés) | après un jeu de chunks complet (stop sinon) | réécrit les lots |
| `etape_finalisation(fusionner = NULL, populations = …)` | tirage | `scenarios_longs_tirage_v8_<CAMPAGNE>/<population>/part_*.parquet` + `_meta.yaml` (un dossier par campagne ; garde-fou : dossier d'une autre campagne ⇒ stop, même campagne ⇒ reprise) (+ monofichier fusionné si volume ≤ `SEUIL_EXPORT_MONOFICHIER` ou `fusionner = TRUE`), `rapport_v8_<date>.txt` (réalisé vs cible, doublons éliminés, manque à gagner), `echantillon_revue.csv`, `top30_das_par_cmd.csv` | après habillage ; relecture des lots en flux, contrôles agrégés par lot | — |

`memoire_session()` : objets par taille (Mo) dans globalenv, `ETAPES_ENV` et `CACHE_E669`, triés,
puis `gc()`. Discipline : **Restart R avant chaque étape lourde** (le RSS de R ne redescend pas
après `gc()` ; l'état utile est sur disque, `etat_pipeline()` réoriente).

`etat_pipeline()` : tableau de bord FAIT / PARTIEL / À FAIRE par étape pour le profil courant,
avec preuves (partiels présents / attendus, refs / 10, catalogue + date + périmètre, sélection +
budget, chunks n / attendus, exports finaux + dates). Fichiers seulement : appelable partout, sans
connexion (les tables temporaires affichent « inconnu hors connexion »).

Scripts bout-en-bout : `Rscript extraction.R` = prep_data → refs →
partiels → catalogue ; `Rscript tirage.R` = courts → sélection → DAS longs →
habillage → finalisation. Comportement identique à l'ancien flux monolithique (identité bit à
bit prouvée par `tests/test_chaines_sqlite.R`).

Arborescence : `results/partiels/` (partagé entre profils), `results/exports_diagnostic/` ou
`results/exports/` (par profil, avec `chunks/`). Aucune table n'est persistée en base.

## Architecture des fichiers d'une campagne

Sous `EXPORTS_DIR` (= `exports/` en production, `exports_diagnostic/` en diagnostic), deux familles :

| Famille | Fichiers | Écrit par | Lu par | Durée de vie |
|---|---|---|---|---|
| **PERMANENT** | `catalogue_longs_seuil/part_<L>.parquet` + `_sidecar.yaml` (+ `.ancien`) | `etape_repartitionner_catalogue()` | sélection, rétro-inscription, rapports | toute la vie du corpus (refait seulement si typologie ou recette d'id change) |
| PERMANENT | les 10 refs (`ref_*`, `pivots_courts`, `v_admin_*`, `distribution_e660`, `referentiel_*`) | `etape_refs()` | courts, tirage des DAS, habillage | tant que `AN_REF`, `SEUIL_REF_*`, `CONVERSION_E669` ne changent pas |
| PERMANENT | `registre_tirages/registre_<Cn>.parquet` | `etape_registre_campagne()` (fin de finalisation), `etape_retro_inscrire()` | sélection sous registre, rapports | **append-only, ne se vide JAMAIS** |
| PERMANENT | `scenarios_longs_tirage_v8_<Cn>/<population>/part_*.parquet` + `_meta.yaml` | `etape_finalisation()` | livraison | un dossier PAR CAMPAGNE, jamais écrasé par une autre (garde-fou) |
| PERMANENT | `scenarios_courts_v8_<date>.parquet` | `etape_tirage_courts()` | finalisation (fichier du jour, sinon le plus récent, annoncé) | tant que les refs courts ne changent pas |
| **PAR CAMPAGNE** | `selection_longs/<population>/`, `meta_tirage.yaml` | `etape_selection_longs()` | tirage, habillage, registre | vidés à l'ouverture de la campagne suivante |
| PAR CAMPAGNE | `chunks/<population>/longs_chunk_*.parquet` + sidecar | `etape_tirage_das_longs()` | habillage, registre | idem |
| PAR CAMPAGNE | `habille/<population>/lot_*.parquet` | `etape_habillage_longs()` | finalisation | idem (réécrits à chaque habillage) |
| PAR CAMPAGNE | `rapport_v8_<date>.txt`, `echantillon_revue.csv`, `top30_das_par_cmd.csv` | finalisation | revue | écrasés à chaque finalisation |

**Pourquoi deux répertoires d'exports** : `exports_diagnostic/` et `exports/` = un par profil, jamais
mélangés. Se migre d'un profil à l'autre : le catalogue et ses refs (condition Q13, chunk de migration
de `RUN_aval.Rmd`). Ne se migre PAS : les sorties de tirage (courts, sélection, chunks, habillé, corpus),
qui se **refont** sous le profil cible — étapes rapides et sans base ; le message « courts absent »
d'`etape_finalisation` le rappelle (trois branches : refaire, ne pas copier, refs).

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

## Séquence PRODUCTION (campagnes itératives, mode `quota_dp_fixe`)

Doctrine : représentativité des DIAGNOSTICS (DP) avant celle des situations cliniques ; la
diversité des contextes se reconstituera ENTRE les campagnes (registre, chantier suivant).
`NB_CRH_CIBLE` (500 000 par défaut) est un ordre de grandeur, pas un engagement : le réalisé est
inférieur (doublons éliminés, chiffrés au rapport).

1. **Repartitionner + typer, une fois** : `etape_repartitionner_catalogue()` (monofichier →
   parts par lettre + DPEC/TPEC, typologie `referentiels/typologie_sejours.yaml` versionnée ;
   monofichier renommé `.ancien`). Toute lecture du catalogue passe ensuite par `lire_catalogue()`.
1b. **Contrôle de couverture avant / après seuil** (chunk `couverture_dp` de RUN.Rmd) : DP distincts
   des partiels agrégés (`agreger_partiels` sur `nom_partiel(...)`, chemins absolus, conversion E669
   appliquée avant comparaison) vs DP du catalogue ; liste des DP perdus au seuil triée par effectif.
1c. **Migration inter-profils** (chunk `migration_catalogue` de RUN_aval.Rmd) : si le catalogue est
   absent d'`EXPORTS_DIR` mais présent dans un autre `exports*/` du même `PATH_RESULTS`, copie proposée
   (catalogue + méta + refs) derrière confirmation, condition Q13 affichée (`condition_q13`,
   `localiser_catalogue`, `fichiers_migration_catalogue` : helpers purs).
2. **Sélection de campagne** : `etape_selection_longs()` (`NB_CRH_CIBLE`, `NB_LIGNES_PAR_DP = 1`) :
   par population (`POPULATIONS`, budget au prorata des DP), k lignes distinctes par DP au poids
   sans remise, variantes déduites, plafonds `PLAFONDS_DPEC` par (DP × DPEC), planchers d'unités
   désactivés à k = 1 (mention au rapport). Lettre par lettre : pic RAM = une lettre.
   Ligne de log par population : `== Sélection <pop> : <nb_dp> DP, budget <b>, X = <x> par DP, k = <k> ==`
   (nb_dp = DP distincts du catalogue de la population ; budget = part de NB_CRH_CIBLE au prorata des DP ;
   X = ceiling(budget / nb_dp), minimum 1 par DP — si X × nb_dp > budget, le volume final dépasse le
   budget, la bannière le dit ; planchers d'unités inactifs à k = 1, la bannière le dit), puis
   `<pop> : <l> lignes sélectionnées, <v> variantes attendues ; plafonds appliqués = <p> ; manque à gagner = <m>`
   (l = lignes distinctes retenues ; v = Σ n_var = volume attendu ; p = groupes (DP × DPEC) plafonnés ;
   m = Σ max(0, X_dp − lignes disponibles)).
3. **Palier de mesure (OPTIONNEL)** : `palier.R` = `NB_CRH_CIBLE <- 100000L` + marqueur `PALIER_ACTIF <- TRUE`,
   `SCENARIOS_PMSI_SURCHARGE`, Restart R ; le chunk de palier de `RUN_aval.Rmd` §5 **refuse de tirer** si la
   surcharge palier n'est pas active (`palier_actif()`), et la bannière d'`etape_tirage_das_longs` affiche en
   première ligne `CAMPAGNE`, `NB_CRH_CIBLE` effectif et la surcharge active (« aucune » attendu en campagne).
   Le débit (scénarios/s) est imprimé par chunk. Extrapolation :
   temps campagne ≈ volume_attendu / débit ; sessions parallèles suggérées ≈ ceiling(temps / durée
   de session acceptable), plages `chunk_range` disjointes.
4. **Campagne parallèle** : une session par plage et par population, même dossier
   (`etape_tirage_das_longs(chunk_range = c(i, j), populations = "adulte")`), sidecar partagé,
   écriture atomique. Puis `etape_habillage_longs()` et `etape_finalisation()` (flux par lots).
   Restart R entre chaque étape. **Branche courts, même mode d'emploi** (parité) :
   `etape_tirage_courts(chunk_range = c(i, j))` par session sur des plages **disjointes** du même
   dossier `chunks/` (jamais deux sessions sur la même plage ; sidecar `courts_chunks_meta.yaml`
   partagé), puis un appel final `etape_tirage_courts()` sans plage qui saute les chunks présents,
   assemble, habille et exporte. Débit (scénarios/s) imprimé par chunk sur les deux branches ;
   extrapolation du restant en bannière tous les 10 chunks traités.

5. **Cycle de campagne** (registre des tirages, `RUN_aval.Rmd` §3 « ouvrir une campagne ») : (0) `etape_retro_inscrire(dossier_selection,
   dossier_chunks, campagne)` pour une campagne tirée avant le chantier campagnes ; (1) `CAMPAGNE <- "Cn"`,
   `REGISTRE_ACTIF <- TRUE`, Restart R, session (affiche `CAMPAGNE` et son statut au registre : « jamais inscrite » /
   « déjà N scénarios le <date> — changez d'identifiant »), puis vidage gardé (`JE_CONFIRME_NOUVELLE_CAMPAGNE`) ;
   (2) `etape_selection_longs()` sous registre (stop précoce, avant tout calcul, si `CAMPAGNE` est déjà inscrite) : chaque DP reçoit au moins 1 scénario
   (plancher automatique : X ≥ 1 et règle de classe), lignes VIERGES d'abord (id_profil absent du registre),
   sinon RECYCLAGE à variantes nouvelles (numérotation après variante_max, hash_das déjà enregistrés exclus,
   sans re-tirage), colonne `origine_profil` ; (3) tirage / habillage / finalisation ; (4)
   `etape_registre_campagne()` (automatique en fin de finalisation) ; (5) rapport de consommation.
   Plafonds DPEC = plafond du TOTAL de la classe par population, 1 représentant par DP prime (dépassement consigné).

Passage diagnostic → production : éditer le bloc `production` de `config.R` (ANS_HISTORIQUE,
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
- Catalogue absent du dossier d'exports effectif : message à trois branches (chemin effectif cherché ;
  s'il existe sous un autre profil, copiez-le via le chunk de migration de RUN_aval.Rmd — condition Q13 —,
  sinon `etape_catalogue()`, extraction coûteuse). `diagnostic_memoire.csv` est écrit en fin
  d'`etape_refs`, d'`etape_partiels_longs` et d'`etape_catalogue` (idempotent).
- Refs (`exports*/`) : sautées si le parquet existe ; `etape_refs(forcer = TRUE)` après changement
  d'`AN_REF`, d'un `SEUIL_REF_*`, de `CONVERSION_E669` / `BARE_E669_DEFAUT` ou correction amont.
- Chunks / sélection / `meta_tirage.yaml` : reprise après plantage telle quelle (identité bit à bit par
  seed par chunk) ; à vider après changement de `MODE_SELECTION`, `NB_CRH_CIBLE`, `NB_LIGNES_PAR_DP`,
  `QUOTA_MIN_PAR_UNITE`, `SEED`, de la version de typologie ou du catalogue (le garde-fou
  `meta_tirage.yaml` le demande). Chunks par population dans `chunks/<population>/`.
- Catalogue partitionné (`catalogue_longs_seuil/`) : sidecar avec version de typologie ET recette d'id
  (`id_v1`) ; changer l'une ou l'autre ⇒ supprimer le dossier, restaurer le `.ancien` en
  `catalogue_longs_seuil.parquet`, relancer `etape_repartitionner_catalogue()`, puis recalculer le registre
  par rétro-inscription de toutes les campagnes. À relancer une fois après le chantier campagnes (id_profil).
- Registre des tirages (`registre_tirages/`) : append-only, ne se vide JAMAIS (stop si réécriture divergente) ;
  nouvelle campagne = poser `CAMPAGNE`, Restart R, vider chunks + sélection + `meta_tirage.yaml` + `habille/`.
  Campagne déjà inscrite = **close** (Q49 actée) : si la sélection présente sur disque porte la même campagne,
  `etape_selection_longs()` la relit sans rien tirer (relancer `tirage.R` après finalisation reste
  un no-op sûr) ; sinon stop explicite (« campagne close, ouvrez une nouvelle campagne — section 3 »). Aucun
  re-tirage possible d'une campagne inscrite.
- Palier de mesure (Q53 actée) : `palier.R` impose `REGISTRE_ACTIF <- FALSE` et `etape_registre_campagne()`
  refuse de s'exécuter sous `PALIER_ACTIF` — une mesure n'écrit jamais dans la mémoire permanente.
- Lecteurs à repli (notebooks et scripts) : `lire_catalogue`, `lire_registre`, `lire_corpus_final` (corpus
  final d'une campagne) utilisent un dataset arrow si arrow est réel, sinon la relecture des parts ; aucun appel
  direct à un dataset arrow dans les notebooks (le repli mock ne l'exporte pas). Lectures de produits d'étape
  dans les notebooks : `lire_si_present(chemin, produit_par)` / `dernier_fichier(dossier, motif)` — fichier absent
  = message « produit par <étape>, pas encore exécutée », jamais d'erreur R brute.
- Corpus final : `scenarios_longs_tirage_v8_<CAMPAGNE>/` + `_meta.yaml` (campagne, date, populations, total) ; deux
  campagnes le même jour = deux dossiers ; un dossier portant le `_meta.yaml` d'une autre campagne ⇒ stop.
- Fichiers datés (`scenarios_courts_v8_<date>.parquet`, `rapport_v8_<date>.txt`, monofichier) : écrits au jour de la
  session ; en lecture, le fichier du jour sinon le plus récent (`chemin_export_lecture`, annoncé « relu depuis … »).
- Identifiants des séjours courts : recette `id_courts_v1` figée (sha256 des `PIVOTS_COURTS`, `id_profil` = `c` + 15 hex,
  `id_scenario` = `id_profil-variante`, `hash_das`) ; pas d'inscription au registre.
- **Chunking dynamique** : la taille des chunks est calculée par les données,
  `taille_chunk(n) = max(CHUNK_SIZE_MIN, ceiling(n / NB_CHUNKS_MAX))` — au plus `NB_CHUNKS_MAX` (50)
  chunks par tirage, plancher `CHUNK_SIZE_MIN` (500) ; `CHUNK_SIZE_FIXE` (NA par défaut) impose une
  taille manuelle. Chaque dossier de chunks porte un sidecar `<prefixe>_chunks_meta.yaml` (n,
  chunk_size, seed_base, nb_chunks, date) écrit avant le premier chunk : la reprise n'est acceptée
  qu'avec les MÊMES n / chunk_size / seed_base (les index de chunks sont des plages de lignes ;
  un découpage différent corromprait silencieusement le résultat). Changer `NB_CHUNKS_MAX`,
  `CHUNK_SIZE_MIN`, `CHUNK_SIZE_FIXE` ou le volume d'entrée ⇒ vider les dossiers de chunks (le
  garde-fou l'impose de toute façon, avec « attendu … / reçu … »). Dossiers de chunks antérieurs au
  chantier (sans sidecar) : à vider, le `stop()` l'explique.
- Sessions multiples : les tables temporaires (`prep_data_<an>`, `prep_das_chro_<an>`,
  `prep_topk_tmp`) disparaissent à la déconnexion ; `etape_prep_data()` recrée à la demande.
  Ne jamais persister de table en base.

## Tests hors base

```
Rscript tests/test_helpers.R            # helpers purs
Rscript tests/test_chaines_sqlite.R     # scripts réels sur SQLite fichier : sessions multiples, étapes, identité avec les anciens scripts
Rscript demo/creer_base_demo.R && Rscript demo/lancer_demo.R   # mode démo : pipeline complet sur base fictive (demo/README.md ; données aléatoires)
Rscript demo/executer_notebook.R --raz RUN.Rmd ; Rscript demo/executer_notebook.R RUN_aval.Rmd   # les notebooks déroulés en mode démo
```
Prérequis du second : dbplyr, DBI, RSQLite, yaml (arrow réel ou mock RDS de repli) ;
`R_LIBS_TEST=<lib>` pour une bibliothèque additionnelle. La CI exécute suites, démo et notebooks dans DEUX jobs,
avec et sans arrow, pour que le chemin de repli soit exercé à chaque push.
