# RUN.md — pipeline scenarios_bn_pmsi v8, exécution par étapes

Fichiers : `config.R` (config + profils), `helpers.R` (helpers purs), `etapes.R`
(chaînes base déplacées telles quelles + fonctions d'étape + `etat_pipeline()`),
`extraction.R` (script d'entrée extraction : n'appelle que les étapes),
`tirage.R` (script d'entrée tirage, **sans base**). Notebooks : `RUN.Rmd` (amont : extraction,
diagnostic, courts) et `RUN_aval.Rmd` (exploitation du catalogue parquet : repartitionnement, campagnes).
Spécification : `SPEC_V8.md` ; journal : `MODIFICATIONS_V8.md`.

**Trois niveaux de paramètres** (ordre de chargement, documenté en tête de `config.R`) :

| Fichier | Contenu | Change | Versionné |
|---|---|---|---|
| `config.R` | la DOCTRINE et les DÉFAUTS (seuils, profils, `NB_CRH_CIBLE`, `NB_LIGNES_PAR_DP`, `CAMPAGNE`, `REGISTRE_ACTIF`, `PLAFONDS_DPEC` par défaut) | à chaque chantier | oui |
| `config_locale.R` | le POSTE : racine du dépôt, `PATH_RESULTS`, `pschema` (modèle `config_locale.exemple.R`, une ligne par clé) | à l'installation | non (gitignoré) |
| `campagne.R` OU `palier.R` | la DÉCISION D'EXPLOITATION : identifiant de campagne, budget, k, registre — écrits depuis `RUN_aval.Rmd` (chunks `ouvrir_campagne` / `palier_surcharge`), activés par `SCENARIOS_PMSI_SURCHARGE` ; **exclusifs** (l'un refuse de s'écrire tant que l'autre est actif, le message dit lequel retirer et comment) | à chaque campagne / palier | non (gitignorés) |

Défauts → `config_locale.R` (poste) → surcharge (`SCENARIOS_PMSI_SURCHARGE` : `campagne.R` OU `palier.R` ; la
surcharge démo de `demo/session_demo.R` est le troisième cas légitime, hors exclusivité) → vérifications. Le chunk
`session` affiche, à côté de chaque valeur effective, sa SOURCE (`défaut config` / `surcharge campagne (campagne.R)` /
`surcharge palier (palier.R)`). Multi-utilisateurs : chacun son `config_locale.R`, magasins partagés communs, tables
temporaires disjointes par `pschema`.

Variables d'environnement : `SCENARIOS_PMSI_PATH` (racine du projet ; sinon `config_locale.R` à la racine du
dépôt, non versionné, copié de `config_locale.exemple.R` — aucun chemin personnel n'est versionné, le pipeline
s'arrête avec un message explicite si ni l'un ni l'autre n'est défini), `SCENARIOS_PMSI_PROFIL`
(`diagnostic` par défaut, ou `production`), `SCENARIOS_PMSI_SURCHARGE` (fichier R optionnel de
surcharges — `campagne.R` ou `palier.R` — évalué après le bloc profil), `SCENARIOS_PMSI_ETAPES_SEULEMENT=1` (charger la session
— config, sources, connexion pour l'extraction — sans exécuter aucune étape).

## Les étapes

Chaque fonction affiche une bannière début/fin avec sa durée, est **idempotente** (saute ce qui
existe déjà selon les règles de cache et le dit), prend ses décisions en arguments explicites
(config en défaut), retourne (invisible) les fichiers produits, et échoue avec un message
actionnable (« lancez etape_X d'abord », « fichier Y manquant ») si elle est appelée hors ordre.

| Étape | Famille | Produit | Relancer quand | Cache |
|---|---|---|---|---|
| `etape_prep_data(ans = NULL)` | extraction (base) | tables temporaires `prep_data_<an>` (et `prep_das_chro_<AN_REF>` si une ref chronique manque) ; `00_partiels/_meta.yaml` | à chaque nouvelle session avant refs / partiels (les tables temporaires disparaissent à la déconnexion) ; `ans` force des années | plan = partiels et refs manquants ; garde du magasin `00_partiels` (K, NBDA_MAX, DUREE_LONGS, PIVOTS_LONGS ; `FORCER_PARTIELS`) |
| `etape_refs(forcer = FORCER_REFS)` | extraction (base) | 9 `ref_*.parquet` + `_meta.yaml` dans `10_references/` **[partagé]** ; **le TIRABLE courts** `ref_pivots_courts.parquet` + `_meta.yaml` dans `30_courts/` (pivots = catalogue des courts, cumul `ANS_COURTS`) **[partagé]** | après changement d'`AN_REF` / `ANS_COURTS`, des seuils de refs, de `CONVERSION_E669`, ou magasin photographié AVANT le retrait de `nbda` de `ref_v_admin_longs` (le méta l'impose : `forcer = TRUE`, une requête distinct, quelques minutes) ; `FORCER_COURTS` régénère le seul tirable | ref sautée si son parquet existe ; gardes des magasins à chaque chargement |
| `etape_partiels_longs(iterations = NULL)` | extraction (base) | `00_partiels/catalogue_partiel_<etbs>_<an>.parquet` manquants **[partagé]** ; `90_diagnostics/diagnostic_apports.csv`, `recouvrement.csv` | ajout d'années / de catégories au plan ; `iterations = data.frame(etbs, an)` pour une itération isolée (supprimer son partiel pour le recalculer) | partiel sauté s'il existe ; partiels en codes bruts |
| `etape_catalogue(ans = ANS_HISTORIQUE, etbs = TYPES_ETBS_LONGS)` | extraction (**sans base**) | `20_catalogue/catalogue_longs_seuil.parquet` + `catalogue_longs_seuil_meta.yaml` (trace du périmètre passé), `rapport_extraction.txt`, `90_diagnostics/diagnostic_memoire_<profil>.csv` | **décision de périmètre** : relancer avec les `ans`/`etbs` retenus. Magasin existant avec les mêmes paramètres ⇒ **sauté** ; en écart ⇒ stop sauf `FORCER_CATALOGUE`. **Le catalogue de production (21,6 M lignes) existe : ne JAMAIS le reconstruire sur ce périmètre** | agrégation deux étages hors RAM depuis les partiels du périmètre ; conversion E669 puis seuil |
| `etape_repartitionner_catalogue()` | aval (**sans base**) | `20_catalogue/catalogue_longs_seuil/part_<L>.parquet` (par lettre de DP, + `lettre`, `DPEC`, `TPEC`, `id_profil`) + `_meta.yaml` (clés du magasin + typologie + recette d'id) **[partagé]** ; monofichier renommé `.ancien` | une fois par catalogue, et après changement de version de `typologie_sejours.yaml` (le garde-fou l'impose) | idempotente ; lecture par morceaux de lettres |
| `etape_tirage_courts(chunk_range = NULL, budget = NB_CRH_CIBLE_COURTS, ratio = RATIO_COURTS)` | tirage (**étape DE CAMPAGNE**, après la sélection) | `40_campagnes/<C>/chunks_courts/courts_chunk_*.parquet` (+ sidecar), `40_campagnes/<C>/habille/courts/scenarios_courts.parquet` + `_meta.yaml` (budget, source ratio / absolu, gardés / demandés) **[profil]** | à chaque campagne : budget = `NB_CRH_CIBLE_COURTS` (absolu) ou `RATIO_COURTS` × volume longs attendu (ratio provisoire 1.0), réparti sur les pivots au poids ; sous registre : variantes numérotées après la `variante_max` du pivot, `hash_das` enregistrés exclus (aucun re-tirage) ; campagne inscrite côté courts sans chunks ⇒ close | chunks présents sautés (reprise bit à bit) ; parallélisme par plages disjointes puis appel final sans plage |
| `etape_selection_longs(budget = NB_CRH_CIBLE, mode = MODE_SELECTION, k = NB_LIGNES_PAR_DP)` | tirage | `<profil>/40_campagnes/<C>/selection/` : **quota_dp_fixe** (production) `<population>/part_<L>.parquet`, `selection_longs_effectifs.csv`, `selection_longs_stats_dp.csv`, `_meta.yaml` par population + global ; quota_dp (diagnostic) : `selection_longs.parquet` | changement de budget / mode / k : ouvrir une nouvelle campagne ou vider `40_campagnes/<C>/` (garde-fou `selection/_meta.yaml`) | sélection relue si présente, jamais re-tirée ; `catalogue_complet` retiré (stop si budget < catalogue) |
| `etape_tirage_das_longs(chunk_range = NULL, populations = …)` | tirage | `40_campagnes/<C>/chunks/<population>/longs_chunk_*.parquet` (+ sidecar) ; rien en RAM (fixe) | reprise : relancer telle quelle ; **parallélisme** : une session par plage `chunk_range = c(i, j)` disjointe, même dossier | chunks présents sautés ; écriture atomique (.tmp) ; `ref_das_aigu` indexé une fois ; débit imprimé par chunk |
| `etape_habillage_longs(populations = …)` | tirage | `40_campagnes/<C>/habille/<population>/lot_*.parquet` (jointure `ref_v_admin_longs.parquet` relu par lots sur les **6 clés** `CLES_ADMIN_LONGS` — nbda retiré —, photographie filtrée sur `DUREE_LONGS`, **repli hiérarchique** âge → cage → mode_hospit × cage × racine, `NB_VARIANTES_ADMIN_LONGS` tenues admin par scénario (défaut 1 ; N > 1 : suffixe `-aN`) tirées au poids des effectifs `n`, colonne `repli_admin`, stop nominatif si aucun candidat ; DPEC/TPEC recalculés) | après un jeu de chunks complet (stop sinon) | réécrit les lots |
| `etape_finalisation(fusionner = NULL, populations = …)` | tirage | **UN livrable** `<profil>/60_export_final/scenarios_<C>.parquet` + `scenarios_<C>_meta.yaml` (longs de toutes les populations ET courts **de la campagne**, colonne `branche` en tête, union de schémas ; méta : volumes par branche, ratio courts réalisé ; parts `scenarios_<C>/` au-delà de `SEUIL_MONOFICHIER`), `rapport_<C>.txt` (§8.2 + « zéro NA d'habillage », distribution de `repli_admin`), `echantillon_revue_<C>.csv`, `top30_das_par_cmd_<C>.csv`, registre des deux branches ; garde-fou : méta d'une autre campagne ⇒ stop, même campagne ⇒ réécriture idempotente | après habillage ; relancée sans lots habillés / sans courts habillés, reconstruit depuis les chunks des deux branches | — |
| `etape_adopter_campagne(campagne = "C1", source_longs = NULL, fichier_courts = 30_courts/scenarios_courts.parquet)` | outil (**sans base**) | `scenarios_<C>.parquet` + méta (`origine = adoption`, sources, dates) reconstruits depuis l'ancien corpus longs daté (`scenarios_longs_tirage_v8_<AAAAMMJJ>/` avec `adulte/`, `pediatrie/`, population reconstituée si absente) et le corpus courts historique ; `registre_<C>.parquet` deux branches ; annexes datées renommées | une fois : adoption de C1 = longs + courts historiques SANS re-tirage | idempotente ; plusieurs corpus datés ⇒ `source_longs` explicite ; livrable présent et différent ⇒ stop ; volumes livrable == registre vérifiés |
| `etape_retro_inscrire_courts(campagne, fichier_courts)` | outil (**sans base**) | branche `court` de `registre_<C>.parquet` depuis un corpus courts (ids `k…` recalculés, variantes telles que tirées) | rétro-inscription seule (sans livrable) | idempotente ; extension append-only si les longs y sont déjà |
| `etape_reorganiser(dossier, mode = "plan" / "executer", migrer_registre = FALSE)` | outil (**sans base**) | mode plan : trois tables (reconnus → destination, ignorés, non reconnus), rien déplacé ; executer : copie de `_a_reorganiser/` vers l'arborescence par étapes, métas convertis, vérifications | une fois, au changement de répertoire de travail (procédure ci-dessous) | idempotente ; garde-fou : magasin différent déjà présent ⇒ stop |

`memoire_session()` : objets par taille (Mo) dans globalenv, `ETAPES_ENV` et `CACHE_E669`, triés,
puis `gc()`. Discipline : **Restart R avant chaque étape lourde** (le RSS de R ne redescend pas
après `gc()` ; l'état utile est sur disque, `etat_pipeline()` réoriente).

`etat_pipeline()` : tableau de bord FAIT / PARTIEL / À FAIRE par étape pour le profil courant,
avec preuves (partiels présents / attendus, refs / 10, catalogue + date + périmètre, sélection +
budget, chunks n / attendus, exports finaux + dates). Fichiers seulement : appelable partout, sans
connexion (les tables temporaires affichent « inconnu hors connexion »).

Scripts bout-en-bout : `Rscript extraction.R` = prep_data → refs (dont le tirable courts) →
partiels → catalogue ; `Rscript tirage.R` = sélection → courts de la campagne → DAS longs →
habillage → finalisation. Identité avec l'ancien flux monolithique prouvée par `tests/test_chaines_sqlite.R`
pour l'extraction, le tirable, la sélection et le tirage des DAS longs ; l'habillage (nbda retiré, repli) et les
courts (par campagne) divergent par décision (chantier « courts en campagnes + habillage robuste »).

Aucune table n'est persistée en base.

## Architecture des fichiers : arborescence par étapes, partage maximal

Tout vit sous `PATH_RESULTS` (posé dans `config_locale.R` ; défaut `<racine>/results/`). Doctrine du
partage : **est partagé tout objet qui ne dépend que de paramètres, pas du profil** ; chaque magasin
partagé porte un `_meta.yaml` avec les paramètres qui le définissent, comparé à la config **à chaque
chargement** (`verifier_magasin`) : divergence ⇒ stop nommant les clés en écart et les issues (régénérer
avec le drapeau `FORCER_*` — ATTENTION, il sert tous les profils — ou détourner le chemin de ce magasin
pour ce seul profil via `CHEMINS_SURCHARGES`, soupape non utilisée par défaut). Tous les chemins dérivent
d'un bloc unique de `config.R` (accesseurs `DIR_*()` / `FICHIER_*()` dans `etapes.R`).

```
<PATH_RESULTS>/
  00_partiels/                     cache d'extraction + _meta.yaml (clés bloquantes)           [PARTAGÉ]
  10_references/                   9 ref_*.parquet + _meta.yaml (clés ex-Q13 + ANS_COURTS,
                                   CLES_ADMIN_LONGS)                                            [PARTAGÉ]
  20_catalogue/                    catalogue_longs_seuil/ (parts + _meta.yaml : périmètre, seuil,
                                   conversion, clés amont, typologie, recette id) ; monofichier
                                   transitoire + méta ; rapport_extraction.txt                  [PARTAGÉ]
  30_courts/                       le TIRABLE courts : ref_pivots_courts.parquet + _meta.yaml
                                   (ANS_COURTS, seuils) ; scenarios_courts.parquet = corpus
                                   courts historique (adoption C1) + scenarios_courts_meta.yaml  [PARTAGÉ]
  90_diagnostics/                  diagnostic_apports.csv, recouvrement.csv [partagés] ;
                                   diagnostic_memoire_<profil>.csv
  <profil>/                        production/ ou diagnostic/
    40_campagnes/<CAMPAGNE>/       selection/ (+ _meta.yaml), chunks/<population>/, chunks_courts/,
                                   habille/<population>/, habille/courts/ (transitoires)
    50_registre/registre_tirages/  registre_<C>.parquet (deux branches, colonne branche ;
                                   permanent, JAMAIS vidé)
    60_export_final/               scenarios_<C>.parquet + scenarios_<C>_meta.yaml,
                                   rapport_<C>.txt, echantillon_revue_<C>.csv, top30_das_par_cmd_<C>.csv
```

Règle de nommage : **nom stable, date dans le méta** (aucun nom daté ; la date vit dans les `_meta.yaml`
ou en première ligne des `.txt`). Convention : `_meta.yaml` colocalisé dans chaque magasin / dossier,
`<objet>_meta.yaml` à côté d'un fichier. Deux profils sur le même `PATH_RESULTS` : le second réutilise
partiels, refs, catalogue et tirable courts sans recalcul. Le registre n'existe en pratique que côté production
(`REGISTRE_ACTIF <- FALSE` en diagnostic). Migration inter-profils : sans objet (retirée).

## Changement de répertoire de travail (réorganisation SUR PLACE d'un ancien `results/`)

1. `config_locale.R` : poser `PATH_RESULTS <- "<nouveau répertoire>/"` (vide au départ), Restart R.
2. À la main : `mkdir <PATH_RESULTS>/_a_reorganiser` puis `cp -r` des TROIS dossiers de l'ancien
   `results/` — `partiels/`, `exports/`, `exports_diagnostic/` — dedans (rien d'autre ; l'ancien
   répertoire n'est JAMAIS touché par le code).
3. `etape_reorganiser()` (mode `plan`, défaut) : NE DÉPLACE RIEN ; imprime trois tables — (1) RECONNUS →
   destination et nouveau nom (partiels → `00_partiels/` ; dix refs, anciens noms → `ref_*` →
   `10_references/` avec méta reconstruit ; parts + sidecar du catalogue → `20_catalogue/` ; cache
   courts → `30_courts/` dé-daté ; apports / recouvrement → `90_diagnostics/` ; registre →
   `production/50_registre/` seulement si `migrer_registre = TRUE`) ; doublons datés ou inter-profils
   du même export : le plus récent retenu, les autres listés ignorés ; (2) IGNORÉS volontairement
   (sélections, chunks longs, habillés, corpus, rapports, annexes) ; (3) NON RECONNUS — à arbitrer,
   jamais déplacés, jamais silencieux.
4. Lire les trois tables ; décider `migrer_registre` (TRUE = les campagnes passées comptent ; FALSE =
   registre vierge — décision liée au verdict de la revue clinique).
5. `etape_reorganiser(mode = "executer", migrer_registre = …)` : copie depuis `_a_reorganiser/`
   (intact), métas convertis, vérifications imprimées (lignes du catalogue == méta, partiels, dix refs,
   registre) ; idempotente ; magasin différent déjà présent ⇒ stop.
6. `etat_pipeline()` de contrôle, puis suppression manuelle de `_a_reorganiser/`.
Chunks dédiés dans `RUN_aval.Rmd` §1 (plan, puis executer derrière `JE_CONFIRME_REORGANISATION`).

---

## Étape 1 — prep_data (à chaque session d'extraction)

```
SCENARIOS_PMSI_PROFIL=diagnostic ; conn <- pRatihque::connection_database() ; etape_prep_data()
```
Le plan imprimé dit ce qui manque (itérations, refs) et donc quelles années sont préparées.
Critère : aucune erreur SQL (`ROW_NUMBER()` / `COUNT() OVER` sont le dialecte du run), plan cohérent.

## Étape 2 — références et TIRABLE courts (`30_courts/`)

`etape_refs()` produit les neuf `ref_*` de `10_references/` et le tirable courts `30_courts/ref_pivots_courts.parquet`
+ `_meta.yaml` (**les pivots courts sont le catalogue des courts** ; magasin partagé, clés `ANS_COURTS`, `SEUIL_PIVOT`,
`DUREE_COURTS`, `PIVOTS_COURTS`, conversion). `ANS_COURTS` (défaut `AN_REF`) étend le tirable ET les refs de saturation
courts (`ref_das_chronique`, `ref_nb_chroniques`, `ref_v_admin_courts`) sur le **cumul** des années (comptes additionnés,
seuils rejugés sur le cumul ; `prep_data` des années concernées assuré par la résolution des besoins) ; les ids des pivots
sont des hash de contenu : stables sous extension (pivots existants inchangés au registre, nouveaux pivots vierges).
Le **tirage** des courts est une étape **de campagne** (Étape 3, point 4b). Critère : ligne `tirable_courts` FAIT
(`etat_pipeline()`).

## Étape 3 — séjours longs

1. **Partiels** : `etape_partiels_longs()` (toutes les itérations du profil ; `iterations =` pour
   une itération isolée). Lire `diagnostic_apports.csv` (apport de chaque (etbs, an) : lignes,
   séjours, diag2 nouveaux) et `recouvrement.csv` (paires `PAIRES_RECOUVREMENT` : part des
   combinaisons / séjours de l'année B déjà vus en A, pivots déjà vus, diag2 nouveaux). Lire
   `diagnostic_memoire.csv` (pic gc() par morceau / partiel / ref / étage ; `alerte = TRUE` désigne
   l'étape à réduire ; `COLLECT_PAR_MORCEAUX <- TRUE` par défaut).
2. **Refs / intermédiaires** : `etape_refs()` (10 refs, dont `distribution_e660.parquet`).
3. **DÉCISION de périmètre** : `etape_catalogue(ans = 22:26, etbs = "CHR/U")` par exemple. Le
   méta enregistre les `ans`/`etbs` passés (clé du magasin : la config doit s'y aligner pour le charger) ; `20_catalogue/rapport_extraction.txt` mesure l'impact de
   la conversion E669 (fusions, profils entrés au catalogue : écart de volumétrie assumé par
   doctrine). C'est le « fichier parquet sans les DAS ».
4. **Sélection** : `etape_selection_longs(budget = 1000, mode = "quota_dp")` (diagnostic) ou
   `etape_selection_longs()` (production : `catalogue_complet`, 10 000 000 ; palier 100 000 conseillé
   d'abord). Changer de budget/mode impose d'ouvrir une autre campagne ou de vider `40_campagnes/<C>/`.
4b. **Séjours courts de la campagne** : `etape_tirage_courts()` — même statut que les longs dans le corpus ;
   budget = `NB_CRH_CIBLE_COURTS` (absolu) ou `RATIO_COURTS` × volume longs attendu (ratio **provisoire 1.0**,
   à calibrer avec l'équipe apprentissage), réparti sur les pivots au poids ; variantes numérotées par pivot
   après la `variante_max` du registre (recyclage = variantes nouvelles, aucun re-tirage).
5. **Tirage DAS par chunks** : `etape_tirage_das_longs()` (reprise : relancer telle quelle).
6. **Habillage admin** : `etape_habillage_longs()` (6 clés, repli hiérarchique, `repli_admin` tracé).
7. **Livrable** : `etape_finalisation()` → `<profil>/60_export_final/scenarios_<C>.parquet` (longs + courts,
   `branche` en tête) + méta, `rapport_<C>.txt` (critère : « TOTAL anomalies = 0 », dont ^E669 résiduels),
   `echantillon_revue_<C>.csv` (`NB_REVUE` scénarios des deux branches, revue humaine DIM avant production),
   `top30_das_par_cmd_<C>.csv`.

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
1c. (La migration inter-profils est retirée : les magasins `10_references/` et `20_catalogue/` sont
   partagés entre profils et gardés par leur `_meta.yaml`.)
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
3. **Palier de mesure (OPTIONNEL)** : chunk `palier_surcharge` (`ecrire_surcharge_palier(100000L)`) écrit `palier.R` =
   `NB_CRH_CIBLE <- 100000L` + marqueur `PALIER_ACTIF <- TRUE` + `REGISTRE_ACTIF <- FALSE`, pose
   `SCENARIOS_PMSI_SURCHARGE`, Restart R (refusé tant que `campagne.R` est actif) ; le chunk de palier de `RUN_aval.Rmd` §5 **refuse de tirer** si la
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
   dossier `40_campagnes/<C>/chunks_courts/` (jamais deux sessions sur la même plage ; sidecar
   `courts_chunks_meta.yaml` partagé), puis un appel final `etape_tirage_courts()` sans plage qui saute
   les chunks présents, assemble, type, habille et écrit `habille/courts/scenarios_courts.parquet`. Débit
   (scénarios/s) imprimé par chunk sur les deux branches ; extrapolation du restant en bannière tous les
   10 chunks traités.

5. **Cycle de campagne** (registre des tirages, `RUN_aval.Rmd` §3 « ouvrir une campagne ») : (0) `etape_retro_inscrire(dossier_selection,
   dossier_chunks, campagne)` pour une campagne tirée avant le chantier campagnes ; (1) chunk `ouvrir_campagne` :
   paramètres en clair (`CAMPAGNE_A_OUVRIR`, `NB_CRH_CIBLE_CAMP`, `NB_LIGNES_PAR_DP_CAMP`, `REGISTRE_ACTIF_CAMP`),
   `ecrire_surcharge_campagne()` écrit `campagne.R` (entiers `L`, marqueur `SURCHARGE_CAMPAGNE_ACTIVE`) et pose
   `SCENARIOS_PMSI_SURCHARGE` (refusé tant qu'un palier est actif : `vider_palier` d'abord) ; Restart R, session
   (affiche `CAMPAGNE`, son statut au registre : « jamais inscrite » / « déjà N scénarios le <date> — changez
   d'identifiant », et la source de chaque paramètre), puis vidage gardé (`JE_CONFIRME_NOUVELLE_CAMPAGNE`) ;
   (2) `etape_selection_longs()` sous registre (stop précoce, avant tout calcul, si `CAMPAGNE` est déjà inscrite) : chaque DP reçoit au moins 1 scénario
   (plancher automatique : X ≥ 1 et règle de classe), lignes VIERGES d'abord (id_profil absent du registre),
   sinon RECYCLAGE à variantes nouvelles (numérotation après variante_max, hash_das déjà enregistrés exclus,
   sans re-tirage), colonne `origine_profil` ; (2b) `etape_tirage_courts()` (courts de la campagne, recyclage
   des pivots à variantes nouvelles) ; (3) tirage / habillage / finalisation ; (4)
   `etape_registre_campagne()` (automatique en fin de finalisation : les DEUX branches) ; (5) rapport de
   consommation. **Adoption de C1** (longs + courts historiques, sans re-tirage) : `etape_adopter_campagne("C1",
   source_longs = …)` une fois, après la réorganisation ; les campagnes suivantes tirent leurs propres courts.
   Plafonds DPEC = plafond du TOTAL de la classe par population, 1 représentant par DP prime (dépassement consigné).

Passage diagnostic → production : éditer le bloc `production` de `config.R` (ANS_HISTORIQUE,
TYPES_ETBS_LONGS) d'après apports + recouvrement — aligné sur le périmètre du catalogue —, puis
`SCENARIOS_PMSI_PROFIL=production` et les mêmes étapes sur le MÊME `PATH_RESULTS` : partiels, refs,
catalogue et tirable courts sont relus sans recalcul (magasins partagés) ; seuls `40_campagnes/`, `50_registre/`
et `60_export_final/` sont propres au profil.

---

## Règles de cache

- Magasins partagés (`00_partiels/`, `10_references/`, `20_catalogue/`, `30_courts/`) : `_meta.yaml` vérifié
  à chaque chargement ; écart ⇒ stop nommant les clés, le drapeau `FORCER_*` (régénération : il sert TOUS
  les profils) et la soupape `CHEMINS_SURCHARGES`.
- `00_partiels/` : dépend de `K_GRAINE_LONGS`, `NBDA_MAX`, `DUREE_LONGS`, `PIVOTS_LONGS` et de la
  logique amont (clés bloquantes ; `VERSION_SCRIPT` en avertissement) ; **pas** de `SEUIL_PIVOT`, du
  périmètre d'années ni de `CONVERSION_E669` (partiels en codes bruts, conversion à la ré-agrégation).
  `FORCER_PARTIELS` après tout changement de ces clés ou de `prep_data` / `prep_scenarios2`.
- Catalogue absent du magasin `20_catalogue/` : message (lancez `etape_catalogue()` puis
  `etape_repartitionner_catalogue()`). `diagnostic_memoire_<profil>.csv` est écrit en fin
  d'`etape_refs`, d'`etape_partiels_longs` et d'`etape_catalogue` (idempotent).
- `10_references/` : ref sautée si son parquet existe ; `etape_refs(forcer = TRUE)` après changement
  d'`AN_REF` / `ANS_COURTS`, d'un `SEUIL_REF_*`, de `CONVERSION_E669` / `BARE_E669_DEFAUT` ou correction amont.
  `ref_v_admin_longs` est photographié SANS `nbda` (clé du magasin `CLES_ADMIN_LONGS`) ; les deux photographies
  admin sont filtrées sur le périmètre de durée de leur branche (`DUREE_LONGS` / `DUREE_COURTS`, clés du magasin) et
  portent un effectif `n` par combinaison (tirage pondéré) : un magasin antérieur est en écart, le méta l'impose de
  lui-même ⇒ `FORCER_REFS` (une requête `count` par branche, quelques minutes).
- `20_catalogue/` : mêmes paramètres ⇒ `etape_catalogue()` sautée ; en écart (périmètre, seuil, conversion,
  clés amont) ⇒ `FORCER_CATALOGUE` (parts, monofichier et `.ancien` supprimés, repartitionnement à relancer).
  `30_courts/` (le tirable : `ANS_COURTS`, `SEUIL_PIVOT`, `DUREE_COURTS`, `PIVOTS_COURTS`, conversion) : `FORCER_COURTS`
  régénère les pivots ; le corpus courts historique `scenarios_courts.parquet` n'est jamais re-tiré (adoption).
- `40_campagnes/<C>/` (sélection + `selection/_meta.yaml`, chunks, habillé) : reprise après plantage telle
  quelle (identité bit à bit par seed par chunk) ; un dossier par campagne ; à vider après changement de
  `MODE_SELECTION`, `NB_CRH_CIBLE`, `NB_LIGNES_PAR_DP`, `QUOTA_MIN_PAR_UNITE`, `SEED`, de la version de
  typologie ou du catalogue (le garde-fou `selection/_meta.yaml` le demande).
- Catalogue partitionné (`20_catalogue/catalogue_longs_seuil/`) : `_meta.yaml` avec version de typologie ET recette d'id
  (`id_v1`) ; changer l'une ou l'autre ⇒ supprimer le dossier, restaurer le `.ancien` en
  `catalogue_longs_seuil.parquet`, relancer `etape_repartitionner_catalogue()`, puis recalculer le registre
  par rétro-inscription de toutes les campagnes. À relancer une fois après le chantier campagnes (id_profil).
- Registre des tirages (`<profil>/50_registre/registre_tirages/`) : append-only, ne se vide JAMAIS (stop si
  réécriture divergente) ; nouvelle campagne = poser `CAMPAGNE`, Restart R (son dossier `40_campagnes/<C>/`
  est neuf ; les dossiers des campagnes précédentes se vident par le chunk gardé).
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
- Livrable : `<profil>/60_export_final/scenarios_<C>.parquet` + `scenarios_<C>_meta.yaml` (longs de toutes les
  populations ET courts de la campagne, `branche` en tête, union de schémas typée, familles de colonnes au méta ;
  parts `scenarios_<C>/` au-delà de `SEUIL_MONOFICHIER`, lues par `lire_corpus_final`) ; un livrable par campagne,
  jamais écrasé par une autre (méta d'une autre campagne ⇒ stop) ; même campagne ⇒ réécriture idempotente.
- Nommage : aucun fichier daté (règle « nom stable, date dans le méta ») ; la résolution de fichiers datés
  inter-sessions est retirée (sans objet).
- Identifiants des séjours courts : recette `id_courts_v1` figée (sha256 des `PIVOTS_COURTS`, `id_profil` = `k` + 15 hex —
  `k` hors alphabet hexadécimal, aucun identifiant long ne peut commencer par `k` (Q63 résolue, corrigé avant toute circulation),
  `id_scenario` = `id_profil-variante`, `hash_das`) ; **inscrits au registre** comme les longs (colonne `branche`,
  registres antérieurs relus avec `long` implicite ; append-only, extension acceptée seulement pour une branche absente).
- Courts par campagne : `40_campagnes/<C>/chunks_courts/` et `habille/courts/` sont transitoires (vidés à l'ouverture de la
  campagne suivante comme le reste) ; le registre, jamais. Campagne inscrite côté courts sans chunks ⇒ close (stop).
- Habillage admin des longs : 6 clés (`CLES_ADMIN_LONGS`, nbda retiré), repli hiérarchique, `NB_VARIANTES_ADMIN_LONGS`
  tenues admin par scénario (défaut 1 ; paramètre de campagne, `ouvrir_campagne` ; N > 1 : N tenues sans remise, `id_scenario`
  suffixé `-a2`..`-aN`, le registre compte toujours les jeux de DAS ; NA = toutes, v7.2) tirées au poids des effectifs `n`
  (aux replis, `n` sommés), colonne `repli_admin` (0 / 1 / 2) jusqu'au corpus, contrôles « zéro NA », « durée dans le
  périmètre de la branche » et « lignes = scénarios × N » au rapport, stop nominatif sinon — jamais de NA silencieux.
- Courts (Q76) : `NB_VARIANTES_ADMIN_COURTS` tenues admin par scénario court, même mécanique (défaut 1 ; N > 1 : suffixe `-aN` ;
  2 = convention v7.1.2) ; le budget courts compte des scénarios ; contrôle « lignes = scénarios × N » par branche et
  « id_scenario unique par ligne, toutes branches » (campagnes nouvelles ; un livrable adopté garde ses courts historiques
  à 2 tenues sans suffixe, le méta le note).
- `poids` (famille audit du livrable, les deux branches, numérique, jamais NA) = effectif réel du profil sur le périmètre
  du catalogue (longs) ou du pivot (courts) : la colonne de ré-échantillonnage — corpus à couverture équitable, entraînement
  ∝ `poids` ou `poids^alpha` (curseur réalisme / couverture, décision équipe apprentissage) ; note au méta (`notes_familles`).
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
