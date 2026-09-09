# MODIFICATIONS_V8.md — journal des écarts de `extraction_associations_codes_v8.R`

Journal exigé par SPEC_V8.md §0.4. Pour chaque bloc dbplyr repris : provenance (fichier +
lignes), lignes du v8, et liste exhaustive des caractères modifiés avec la référence §5 /
config qui l'autorise. Les blocs ont été comparés par `diff` avec leur source (commandes
en fin de document). Tout écart non listé ici est une violation du spec.

Conventions : `v7.1.2` = `extraction_associations_codes_v7.1.2.R`, `v7.2` =
`extraction_associations_codes_v7.2.R`, `v8` = `extraction_associations_codes_v8.R`.
Les numéros de lignes v8 sont ceux du fichier livré.

---

## 1. Blocs dbplyr repris (copies + écarts autorisés)

### B1 — `prep_data(an)` — v8 l.148-402
**Source :** v7.2 l.24-275 (version riche : `type_unite`, `prep_sc`, flags `diabete`/`hta`,
branches `an>22` / `18-22` / `<=17`). Copie caractère par caractère, sauf :

| # | v7.2 | v8 | Autorisation |
|---|------|----|--------------|
| 1 | `prep_data<-function(an,type_etbs){` | `prep_data<-function(an){` | §3.2 (`prep_data(an)`) ; `type_etbs` était inutilisé |
| 2 | `dplyr::distinct(ident,type_unite,.keep_all = TRUE)` (×3, l.42, 117, 196) | `dplyr::group_by(ident,type_unite) \|> dplyr::filter(dplyr::row_number(mode_hospit) == 1L) \|> dplyr::ungroup()` | §5.9a (règle d'ordre explicite ; voir Q3) |
| 3 | `dplyr::distinct(anonyme,ghm2,.keep_all = TRUE)` (×3, l.49, 122, 201) | `dplyr::group_by(anonyme,ghm2) \|> dplyr::filter(dplyr::row_number(ident) == 1L) \|> dplyr::ungroup()` | §5.9a (séjour de plus petit `ident` par patient × GHM ; voir Q3) |
| 4 | `dplyr::select(ident,diabete) \|> dplyr::distinct(ident,.keep_all = TRUE)` (×3, l.92-93, 171-172, 250-251) | `... \|> dplyr::group_by(ident) \|> dplyr::filter(dplyr::row_number(diabete) == 1L) \|> dplyr::ungroup()` | §5.9a (néo-code le plus petit : E10 < E11i < E11ni) |
| 5 | `dplyr::distinct(ident,.keep_all = TRUE)` du sous-bloc HTA (×3, l.98, 177, 256) | **inchangé**, commentaire ajouté | §8.3 : `hta` est une constante `"I10"` → la ligne est déjà déterministe |
| 6 | l.48 `...ghm2,passage_urg,nbrum)` | `...ghm2,passage_urg,nbrum,raac)` | §3.2 + §6.1 : `raac` est un pivot de la chirurgie ambulatoire (v7.1.2 l.30) |
| 7 | (absent, branches 18-22 et <=17) | `dplyr::mutate(raac = NA) \|>` inséré après le `inner_join(.rgp)` (v8 l.288, 368) | idem 6 : colonne requise par le `select` final ; `raac` n'existe pas dans les millésimes anciens (Q4) |
| 8 | (absent) | `dplyr::mutate(cage2 = ifelse(cage3=="lt_18" & substr(ghm2,3,3)=="C" & age>14,"ge_18",cage3)) \|>` avant le `select` final (v8 l.390) | §4 (règle `cage2` v7.1.2 l.68 conservée) + §6.1 |
| 9 | `dplyr::select(anonyme,...,cage3,cage,...,type_unite,prep_sc)` (l.264-265) | `... cage3,cage2,cage, ... ,type_unite,prep_sc,raac)` | idem 6, 8 |

Le `dplyr::rename(age = cage3)` de v7.2 l.267 est conservé : la colonne `age` de
`prep_data` reste la classe `ge_18`/`lt_18` (pivot des séjours longs), `cage2` est la
même classe avec la règle « mineur >14 ans en GHM C ».

### B2 — `ref_das_aigu(an)` — v8 l.408-420 (ex `df_das_ref`)
**Source :** v7.2 l.444-453. Écarts :
- `dplyr::filter(duree>3)` → `dplyr::filter(duree>DUREE_MIN_REF)` (config, `DUREE_MIN_REF = 3`).
- `all_of(` → `dplyr::all_of(` (§1, appels namespacés).
- `dplyr::collect()-> df_das_ref` → `dplyr::collect()` (valeur de retour de la fonction ; §0.2 « nom de la variable de sortie »). Affectation `df_das_ref <- ref_das_aigu(AN_REF)` en v8 l.537.
- `comp_sat_diab` (non défini dans le dépôt) : la chaîne est **inchangée** ; un alias
  `comp_sat_diab <- codes_comp_sat_diab` est ajouté dans `referentiels.R` (Q2).

### B3 — `prep_das_chronique(an)` + `ref_das_chronique(an)` — v8 l.426-459
**Source :** v7.1.2 l.88-112 (`prep_das`). Écarts :
- nom de fonction `prep_das` → `prep_das_chronique` ; nom de table `"prep_das" %+% an` → `"prep_das_chro_" %+% an`.
- `anseqta = anseqta_de(an)` ajouté en tête ; `dplyr::filter(v2025>1)` → `dplyr::filter(!!dplyr::sym("v20"%+% anseqta)>1)` ; `"v20"%+% an` (×2) → `"v20"%+% anseqta` (§5.8 : même millésime dynamique partout ; pour `an = 26`, `v2026` n'existe pas, `anseqta` donne `"25"`).
- `dplyr::filter(duree>DUREE_MIN_REF) |>` inséré après `atihble(...)` (§6.2 : prévalence estimée sur les séjours longs).
- `all_of(` → `dplyr::all_of(`.
- La ligne `dplyr::summarise(nb_das = dplyr::n(),.by= c(diag2,das,sexe,cage,niveau,type_liste,caract))` (v7.1.2 l.108) est **déplacée à l'identique** dans `ref_das_chronique()` (v8 l.455-459), appliquée sur la table calculée ; le `compute` garde le niveau séjour (`ident`) pour alimenter B4 (§3.3d) et N3 (§7.6). Résultat identique à v7.1.2 l.223 (`collect()` de la table).
- `|> invisible()` après le `compute` (pas d'impression au niveau supérieur).

### B4 — `ref_comp_diabete(an)` — v8 l.465-477 (ex `df_res_epi_diabete_chu`)
**Source :** v7.1.2 l.195-205 (chaîne valide). La version v7.2 l.456-467, au pipe cassé
(`mutate(sc = max(prep_sc), .by = ident)` sans `|>`), est abandonnée ; le `mutate(sc=...)`
orphelin, vestigial, est supprimé (§5.3). Écarts :
- `categ_pmsi=="CHR/U"` → `categ_pmsi==TYPE_ETBS_REF_DIABETE` (config).
- `dplyr::collect() ->df_res_epi_diabete_chu` → `dplyr::collect()`.
- Post-collect (R, v7.1.2 l.207-214), v8 l.542-547 : `cage_ped`/`cage_ages` → `CAGE_PED`/`CAGE_AGES`, `0.2`/`0.5` → `PENALITE_9_AGES`/`PENALITE_9_AUTRES` (config).

### B5 — Sélection chirurgie ambulatoire — v8 l.922-925
**Source :** v7.1.2 l.261-264. Écarts : `"prep_data_" %+% an` → `%+% AN_REF` (§5.12) ;
`%in%c("14","15")` → `%in%CMD_OBSTETRIQUE` ; `duree==0` → `duree==DUREE_CHIR_AMBU` ;
liste GHM littérale → `GHM_CHIR_AMBU_LISTE` (config) ; `df_scenarios<-` → `df_scenarios_ambu <-`.
Post-collect (v7.1.2 l.265-269, R) : `all_of` → `dplyr::all_of(PIVOTS_CHIR_AMBU)`,
`nb>9` → `nb>SEUIL_PIVOT` (§2.2 : >10), `sample_age(cage)` → `sample_age(cage, AGE_MAX_OUVERT)`
(§5.6, tirage par ligne), `dplyr::rename(poids = nb)` ajouté (§8.2 : contrôle `poids`).

### B6 — Pivots séjours courts — v8 l.941-943
**Source :** v7.1.2 l.219-221. Écarts : `an` → `AN_REF` ; `duree<3` → `duree%in%DUREE_COURTS`
(config `0:2`, équivalent pour une durée entière) ; `all_of(pivots)` → `dplyr::all_of(PIVOTS_COURTS)` ;
`nb>10` → `nb>SEUIL_PIVOT` (même valeur) ; `-> df_cases` → `df_cases_courts <-`.

### B7 — `df_v_admin_courts` — v8 l.945-947
**Source :** v7.1.2 l.232-234. Écarts : `an` (25 en dur) → `AN_REF` ; nom `df_v_admin` → `df_v_admin_courts`.

### B8 — `prep_scenarios2(...)` — v8 l.973-1022
**Source :** v7.2 l.278-327. Écarts :
- `anseqta = dplyr::case_when(...)` (3 lignes) → `anseqta = anseqta_de(an)` (même table de correspondance, déplacée en config §3.0).
- `dplyr::filter(v2025>1)` → `dplyr::filter(!!dplyr::sym("v20"%+% anseqta)>1)` (§5.8).
- `all_of(` → `dplyr::all_of(` (×3).
- Post-collect (R) : `dplyr::arrange(ident,desc(niveau),desc(nb_das))` → `dplyr::arrange(ident,dplyr::desc(niveau),dplyr::desc(nb_das),das)` (§5.9 : ordre total, les ex æquo de niveau/fréquence étaient tranchés par l'ordre de collecte).

### B9 — Catalogue séjours longs — v8 l.1025-1049
**Source :** v7.2 l.483-534. Le code de boucle (R, post-collect) est encapsulé dans
`construire_catalogue_longs()` : deux boucles `TYPES_ETBS_LONGS × ANS_HISTORIQUE` reproduisent
CHR/U (26 puis 17-25) puis CH (17-26) ; la somme étant commutative, l'ordre des années est
sans effet. `an` n'est plus réassigné au niveau supérieur (§5.12, variable de boucle `an_`).
Constantes → config (`DUREE_LONGS`, `NBDA_MAX`, `K_GRAINE_LONGS`, `PIVOTS_LONGS`).
Seuil (v7.2 l.526-530) : `.by = c("mode_hospit",...,"diag2")` → `.by = dplyr::all_of(PIVOTS_LONGS_SEUIL)`
(= `PIVOTS_LONGS` sans `nbda`, même liste + `type_unite`, `prep_sc`, cf. Q5) ; `nb>9` → `nb>SEUIL_PIVOT`
(§2.2) ; `select(-n)` conservé (§2.2) ; `dplyr::rename(poids = nb)` ajouté (§6.3).
Supprimés : `print("- Noombre ...")` (§5.14), l.537-540 (`sample_n(3000)`, `nb>5000` : §2.5).

### B10 — `df_v_admin_longs` — v8 l.1064-1066
**Source :** v7.2 l.551-553. Écarts : `an` (25 en dur) → `AN_REF` ; nom → `df_v_admin_longs`.

### B11 — `cma` dans `referentiels.R` l.32
`dplyr::filter(v2025>1)` → `dplyr::filter(!!dplyr::sym("v20" %+% ANSEQTA_REF)>1)` (§5.8) avec
`if(!exists("ANSEQTA_REF")) ANSEQTA_REF <- "25"` pour la compatibilité des scripts v7.

---

## 2. Blocs dbplyr nouveaux (exigés par le spec, pas de source v7)

Écrits dans le style des chaînes existantes, sur les tables produites par B1/B3.

| Bloc | v8 | Exigence | Description |
|------|----|----------|-------------|
| N1 `ref_nb_chroniques(an)` | l.482-492 | §3.3d, §6.2 | `prep_data_<an>` (durée > `DUREE_MIN_REF`), `distinct(ident,cage,sexe)`, left_join du nb de DAS chroniques distincts par séjour (`prep_das_chro_<an>`), NA → 0, comptage par `(cage, sexe, nb_chro)`. |
| N2 `ref_substitution_imprecis(an, codes_imprecis)` | l.498-516 | §7.5 | effectifs de **tous** les codes de `.diag` par `(cat, code, cage, sexe)`, seuil `nb >= SEUIL_REF_IMPRECIS`, jointure niveau (`anseqta`), puis **après collect** filtre sur les catégories contenant un code « sans précision » (évite une liste `IN` de plusieurs centaines de valeurs côté base) et colonne `imprecis`. |
| N3 `ref_paires_chroniques(an)` | l.519-528 | §7.6 | auto-jointure de `prep_das_chro_<an>` distinct `(ident,cage,sexe,das)` sur `(ident,cage,sexe)`, `das_a < das_b`, comptage, seuil `nb >= SEUIL_REF_PAIRES`. |

---

## 3. Correspondance des corrections §5

| §5 | Correction | Où (v8) |
|----|-----------|---------|
| 1 | `sexe_ ==sexe_` → `sexe == sexe_` | `sample_das_long` l.819 ; tests « sexe respecté » (helpers + SQLite) |
| 2 | `age_` non défini → argument `age` ; aucune globale implicite (`refs`, tables en argument) ; `df_tmp_sav<<-` supprimé | `sample_das_long`, `sample_das_court`, `construire_refs` |
| 3 | bloc `df_res_epi_diabete_chu` au pipe cassé | B4 (version v7.1.2 valide, `mutate(sc=...)` supprimé) |
| 4 | `filter_chap` (1 caractère) → `dedup_categorie` (3 caractères + YAML) | l.594-614 ; `filter_cat` (2 caractères, v7.1.2) également remplacé |
| 5 | `complications_diab` → `codes_diab` en argument | `get_codes_diabete_from_neo(…, codes_diab, …)` |
| 6 | `sample_age` par ligne, bornes semi-ouvertes (`[1-5[` → 1:4, `[80-[` → 80:`AGE_MAX_OUVERT`) | `sample_age_ligne`, `sample_age` (vapply) ; tests 1000 tirages/classe + régression vectorisation |
| 7 | bloc v7.1.2 l.224-228 mort et cassé | **supprimé** (`niveau` ne servait qu'au filtre commenté l.134-136) |
| 8 | `filter(v2025 > 1)` en dur | B3, B8, N2 (`!!dplyr::sym("v20"%+% anseqta)`), B11 (`ANSEQTA_REF`) ; grep `v2025` : plus aucune occurrence hors commentaires |
| 9 | (a) `distinct(.keep_all=TRUE)` côté base → window `row_number` (B1) ; (b) `slice(1:2)` → `dplyr::slice_sample(n = NB_VARIANTES_ADMIN_COURTS)` sous le seed global (l.959-963) ; tiebreak `das` dans B8 | B1, B8, section 6 |
| 10 | `neo_codes_diabete` défini dans `sample_das` | `referentiels.R` (définition unique) ; passé en argument `refs$neo_codes` |
| 11 | extension `.parquet` | `chemin_export()` l.1081 |
| 12 | `an` réassigné | plus aucune affectation de `an` ; `AN_REF`, `ANS_HISTORIQUE`, boucles `an_` |
| 13 | `PATH_PROJET` via `SCENARIOS_PMSI_PATH` avec défaut | l.24-26 ; alias `path_projet` pour `referentiels.R` |
| 14 | `Noombre`, code commenté mort | supprimés ; v7.1.2 l.289-315 (liste de libellés) non repris |

---

## 4. Écarts hors liste §5 (à valider par le relecteur — chacun est réversible en une ligne)

Tous dans les **helpers purs** (§0.3 : « là où tu peux écrire du code neuf »), aucun dans une chaîne base.

- **H1 — `retro_code_diabete` : 5e caractère inversé dans utils.R l.338-340.** utils.R produisait
  `E11i → "E11"+comp+"8"` et `E11ni → "E11"+comp+"0"`, alors que `code_dnid_ins` (insulinotraité)
  = `E1120, E1130…` (5e caractère **0**) et `code_dnid` = `E1128…` (**8**) — cf. aussi les libellés
  v7.1.2 l.293-294. v8 l.628-633 : `E11i → …0`, `E11ni → …8`. Test : « rétro-codes E11 appartiennent
  aux listes code_dnid_ins / code_dnid ». Pour revenir à l'ancien comportement : échanger `"0"` et `"8"`.
- **H2 — filtre GHM en C (v7.1.2 l.132) : `substr(das,1,2)!="F10"`** compare 2 caractères à
  une chaîne de 3 → toujours vrai, F1x jamais exclu. Le spec (§6.2 « F10 sauf F17 ») décrit
  l'intention. v8 `filtre_das_ghm_c` l.733-738 : `substr(das,1,2)!="F1"`. Pour revenir : remettre `"F10"`.
- **H3 — `get_codes_diabete_from_neo` (utils.R l.361-377)** : pour une complication hors 2:6
  (ex. E10 comp "1", acidocétose) le `case_when` renvoyait `NA` qui était ajouté aux DAS. v8 :
  astérisque uniquement pour comp ∈ {2,…,6} (`CHEMINS_ASTERISQUES_DIABETE`). Strate `(diabete, cage)`
  vide dans `ref_comp_diabete` (ex. `[0-1[` × E11) : v7 échouait sur `sample(character(0))` ;
  v8 replie sur toutes les classes d'âge, puis sur comp "9".
- **H4 — ordre final des codes** : inchangé par rapport à v7.2 (astérisques, code diabète, I10,
  graine, tirés). Un `dedup_categorie` supplémentaire est appliqué **après** l'insertion des codes
  diabète (graine en tête) pour garantir §8.2 « aucun scénario avec deux codes de même catégorie »
  (v7.2 ne dédoublonnait pas après insertion, ex. astérisque N083 + N089 tiré).
- **H5 — `relationship = "many-to-many"`** ajouté aux deux `left_join(df_v_admin_*)` (post-collect,
  R) : dplyr ≥ 1.1 émet sinon un avertissement ; le comportement est identique.
- **H6 — colonnes de sortie enrichies** (R, post-tirage) : `poids`, `variante`, `nb_das`,
  `diabete_scenario` (néo-code effectif, DP compris), `hta_scenario` (courts), `graine` (longs),
  `source_ref` / `nb_cible` (courts). La colonne `diabete` des longs reste la valeur du pivot.
- **H7 — `dplyr::all_of` / `dplyr::desc`** namespacés (§1) : `all_of` est réexporté par dplyr, sans effet.

---

## 5. Code v7 volontairement non repris

| Source | Contenu | Raison |
|--------|---------|--------|
| v7.1.2 l.224-228 | forçage `niveau` cassé | §5.7 |
| v7.1.2 l.278-287 | second bloc chir ambu `duree<3` (`age2`), sortie `scenarios_chir_ambu_` | doublon hors §6.1 |
| v7.1.2 l.289-315 | libellés E11x en commentaires | §5.14 |
| v7.1.2 l.166-176, v7.2 l.424-434 | `filter_cat`, `filter_chap` | §5.4 |
| v7.2 l.13-14, 16-19 | `cma.csv` commenté ; `TYPEAUT_SC`/`TYPEAUT_SC_NEONAT` écrasés l.21 | mort |
| v7.2 l.512-516, 532, 557 | commentaires de volumétrie, `Noombre` | §5.14 (remplacés par des `print` et le rapport §9) |
| v7.2 l.537-540 | `sample_n(3000)` obstétrique + `nb>5000` | §2.5 |
| v7.2 l.546-547 | contrôle commenté | mort |
| v7.2 l.561-580 | « Séjours courts » v7.2 (appelle `prep_scenarios` inexistant, `nbda_sup=5`) | cassé ; la branche courts suit §6.2 / v7.1.2 |
| v7.2 l.585-757 | second `prep_data` (sans type_unite) | écrasé par le premier dans v7.2 lui-même |
| v7.2 l.763-843 | « Gériatrie, réan » : `df_das_ref` à `nb_das_sej`, `distinct_das` | expérimental, hors spec |
| v7.2 l.847-885 | « Tirage et randomness » (`df_stat`, `sample_vars`) | hors spec (§6.3 : habillage par `df_v_admin`) |
| v7.2 l.891-925 | « Autres variables » (`View()`, `hist()`) | exploration interactive |
| v7.2 l.930-934 | lecture parquet commentée | mort |
| utils.R l.21-191 | `prep_data(an,type_etbs)` ancien | **laissé en place** (masqué par le `prep_data` v8, toujours utilisé potentiellement par des scripts tiers) ; contient 6 `distinct(.keep_all)` hors périmètre v8 |
| utils.R l.336-382 | `retro_code_diabete`, `get_codes_diabete_from_neo` (globales) | laissés en place, **masqués** par les versions v8 (section 4) |

---

## 6. Fichiers annexes modifiés / créés

- `referentiels.R` : B11 ; `comp_sat_diab <- codes_comp_sat_diab` ; `neo_codes_diabete` (§5.10). Rien d'autre.
- `referentiels/exclusions_paires.yaml` : créé (§6.4), 4 paires évidentes, structure `- [A, B]`.
- `tests/test_helpers.R` : §8.1, 103 assertions (`stopifnot`, sans testthat). Charge uniquement la section 4 du v8.
- `tests/test_chaines_sqlite.R` : simulation **hors base** des chaînes dbplyr (sections 2, 3, 5, 6, 7)
  sur SQLite en mémoire avec un faux paquet `pRatihque` et des tables factices ; 35 assertions.
  Valide l'enchaînement R/dbplyr (dont les fenêtres §5.9a, `.by`, `!!sym`), **pas** le dialecte
  ni les colonnes réelles de la base de production.
- `utils.R`, `exclusions.R`, scripts v7 : inchangés.

---

## 7. Questions (doutes consignés, aucune action prise — §0.5)

- **Q1 — `df_dp_das` n'est défini nulle part** (v7.1.2 l.268 le joint ; ni utils.R, ni referentiels.R,
  ni exclusions.R ne le créent). v8 l.920 : `stop()` explicite s'il est absent de l'environnement.
  À fournir avant la section 5 (chargement d'un parquet/xlsx ?). Le rapport §9 gère l'absence
  éventuelle de colonne `diagnostic_associes` dans cette branche.
- **Q2 — `comp_sat_diab`** (v7.2 l.448) n'existe pas ; `referentiels.R` définit `codes_comp_sat_diab`
  (codes « satellites » du yaml). Alias ajouté sans toucher à la chaîne. Si une autre liste était visée,
  redéfinir l'alias.
- **Q3 — règle d'ordre §5.9a.** Le spec propose « garder le RUM du DP, sinon min(rum) ; pour .fixe :
  garder la ligne de rumdudp ». (a) Le RUM du DP (`rumdudp`) n'est connu qu'après la jointure `.fixe`,
  postérieure au `distinct` de `.um` : l'appliquer imposerait de réordonner les jointures (interdit §0.1).
  (b) L'existence d'une colonne `rum` dans `.um` n'est pas vérifiable hors base. v8 ordonne donc sur des
  colonnes **présentes dans le `select`** : `.um` → `mode_hospit` (HC avant HP ; les lignes restantes
  d'un même `(ident, type_unite)` ne diffèrent que par `finessgeo`, cas multi-sites), `.fixe` →
  `ident` (plus petit séjour par patient × GHM ; `.fixe` étant à une ligne par séjour, « ligne de
  rumdudp » n'a pas d'objet), diabète → `diabete`. Si la base n'accepte pas les fonctions fenêtre
  (`ROW_NUMBER() OVER`), remplacer par l'ancien `distinct` (3 lignes × 3 branches, repérées `# §5.9a`).
- **Q4 — `raac`** : sélectionné dans `.fixe` pour `an > 22` (comme v7.1.2 sur 2025) ; `NA` pour
  `an <= 22`. Si la colonne existe aussi avant 2023, on peut la sélectionner dans ces branches.
- **Q5 — `type_unite`/`prep_sc` ajoutés à `PIVOTS_LONGS`** (§2.8 « conservés dans les sorties »).
  Conséquence : un séjour multi-unités (ex. HC + GERIATRIE, hors SC) compte une fois par `type_unite`
  dans le catalogue et dans le seuil `PIVOTS_LONGS_SEUIL`. Alternative : les retirer des pivots et
  les rattacher après coup (perte de la cohérence type_unite × DAS).
- **Q6 — graine à deux codes de même catégorie** (ex. `J440 J449`, `N185 N189`) : `prep_scenarios2`
  (v7.2, conservé) peut produire une telle graine ; `dedup_categorie` élimine alors le second code
  (§6.4 et §8.2 priment sur « graine jamais éliminée »). Si l'on préfère une graine de deux
  catégories distinctes, modifier le `slice(1:nb_assoc_das)` de B8 (R, post-collect).
- **Q7 — taille de la complétion longs** : `min(nbda, candidats)` DAS tirés **en plus** de la graine
  (v7.2 et §6.3 littéral) ; `nbda - K_GRAINE_LONGS` serait plus cohérent avec le nombre réel de DAS.
- **Q8 — DP diabète** : comme en v7.2, quand le DP est un code diabète, un code diabète rétro-codé est
  aussi inséré dans les DAS (`diabete_` recalculé depuis `diag`). Conservé tel quel.
- **Q9 — millésime unique `AN_REF = 26`** : v7.2 utilisait 26 pour le catalogue mais **25** pour
  `df_das_ref`, `df_res_epi_diabete_chu` et `df_v_admin` ; v7.1.2 utilisait 25 partout. v8 utilise
  `AN_REF` partout (§5.12). Si 2026 est incomplet à l'exécution, mettre `AN_REF <- 25L`.
- **Q10 — codes du yaml avec point** (`lire_codes_diabete` accepte `E11.20`) : `codes_astrisques_diabete`
  est utilisé tel quel dans les filtres base (v7 idem). Si le yaml contient des points, ces codes ne
  matchent pas les codes PMSI. Non modifié (hors §5). N2 normalise les codes de `cim_2024.xlsx`
  (`gsub(".", "")`), et suppose des colonnes `code` et `libelle` (`stopifnot`).
- **Q11 — jointure `.diag` sur `(ident, rum = rumdudp)`** (B2, B3, B4, v7 idem) : seuls les DAS du
  RUM du DP sont vus par les tables de référence. Comportement v7 conservé, signalé.
- **Q12 — `duree > 3`** (B2, B3 : `DUREE_MIN_REF = 3`, strict comme v7.2 l.445) alors que
  `DUREE_LONGS` commence à 3. Conservé tel quel.

---

## 8. Vérifications effectuées hors base

```
Rscript -e 'parse("extraction_associations_codes_v8.R")'        # syntaxe OK
Rscript tests/test_helpers.R                                     # 103 assertions vertes
R_LIBS_TEST=<lib avec dbplyr/DBI/RSQLite> Rscript tests/test_chaines_sqlite.R   # 35 assertions vertes
grep -n 'filter_chap\|sexe_ ==sexe_\|v2025\|slice(1:2)\|<<-\|distinct(.*\.keep_all' extraction_associations_codes_v8.R
#  -> uniquement des commentaires, plus l'unique distinct(.keep_all) HTA commenté « déterministe » (B1 #5)
```

Diff des blocs (à rejouer par le relecteur) :
```
sed -n '24,275p'  extraction_associations_codes_v7.2.R   > /tmp/b1_src.R
awk '/^prep_data<-function/{f=1} f{print} /^## ---- 3\. Tables/{exit}' extraction_associations_codes_v8.R | sed '$d' > /tmp/b1_v8.R
diff /tmp/b1_src.R /tmp/b1_v8.R          # attendu : exactement les 9 écarts de B1
sed -n '278,327p' extraction_associations_codes_v7.2.R   > /tmp/b8_src.R
awk '/^prep_scenarios2<-function/{f=1} f{print} f&&/^}/{exit}' extraction_associations_codes_v8.R > /tmp/b8_v8.R
diff /tmp/b8_src.R /tmp/b8_v8.R          # attendu : anseqta_de, §5.8, dplyr::all_of ×3, tiebreak das
sed -n '88,112p'  extraction_associations_codes_v7.1.2.R > /tmp/b3_src.R
awk '/^prep_das_chronique<-function/{f=1} f{print} f&&/^}/{exit}' extraction_associations_codes_v8.R > /tmp/b3_v8.R
diff /tmp/b3_src.R /tmp/b3_v8.R          # attendu : les écarts de B3
```

## 9. Points à surveiller à la première exécution en espace sécurisé

1. Q1 (`df_dp_das`) — bloquant à la section 5.
2. Q3 — support des fonctions fenêtre par la base (B1, 9 lignes marquées `# §5.9a`).
3. `raac` et `nbrum` dans `.fixe` (an > 22) : `raac` vient de v7.1.2, `nbrum` de v7.2, tous deux sur 2025.
4. Volume de la section 7 : `pmap` sur tout le catalogue éligible (plus de filtre `nb>5000`) ;
   si trop long, renseigner `MAX_SCENARIOS_LONGS` (tirage au poids, §6.3), et éventuellement
   `NB_VARIANTES_ADMIN_LONGS` pour limiter l'habillage admin (v7.2 gardait toutes les variantes).
5. Colonnes `code`/`libelle` de `cim_2024.xlsx` (N2, `stopifnot`).
