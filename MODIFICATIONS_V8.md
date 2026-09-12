# MODIFICATIONS_V8.md — journal des écarts de `extraction_associations_codes_v8.R`

Journal exigé par SPEC_V8.md §0.4. Pour chaque bloc dbplyr repris : provenance (fichier +
lignes), lignes du v8, et liste exhaustive des caractères modifiés avec la référence §5 /
config qui l'autorise. Les blocs ont été comparés par `diff` avec leur source (commandes
en fin de document). Tout écart non listé ici est une violation du spec.

Conventions : `v7.1.2` = `extraction_associations_codes_v7.1.2.R`, `v7.2` =
`extraction_associations_codes_v7.2.R`. Depuis le chantier « industrialisation » (section 10),
le v8 est scindé en quatre fichiers : `extraction` = `extraction_associations_codes_v8.R`,
`tirage` = `tirage_scenarios_v8.R`, `helpers` = `helpers_v8.R`, `config` = `config_v8.R`.
Les numéros de lignes sont ceux des fichiers livrés (recalés à chaque évolution).

---

## 1. Blocs dbplyr repris (copies + écarts autorisés)

### B1 — `prep_data(an)` — extraction l.70-348
**Source :** v7.2 l.24-275 (version riche : `type_unite`, `prep_sc`, flags `diabete`/`hta`,
branches `an>22` / `18-22` / `<=17`). Copie caractère par caractère, sauf :

| # | v7.2 | v8 | Autorisation |
|---|------|----|--------------|
| 1 | `prep_data<-function(an,type_etbs){` | `prep_data<-function(an){` | §3.2 (`prep_data(an)`) ; `type_etbs` était inutilisé |
| 2 | `dplyr::distinct(ident,type_unite,.keep_all = TRUE)` (×3, l.42, 117, 196) | `dplyr::group_by(ident,type_unite) \|> dplyr::filter(dplyr::row_number(mode_hospit) == 1L) \|> dplyr::ungroup()` | §5.9a (règle d'ordre explicite ; voir Q3) |
| 3 | `dplyr::distinct(anonyme,ghm2,.keep_all = TRUE)` (×3, l.49, 122, 201) | `dplyr::group_by(anonyme,ghm2) \|> dplyr::filter(dplyr::row_number(ident) == 1L) \|> dplyr::ungroup()` | §5.9a (séjour de plus petit `ident` par patient × GHM ; voir Q3) |
| 4 | `dplyr::select(ident,diabete) \|> dplyr::distinct(ident,.keep_all = TRUE)` (×3, l.92-93, 171-172, 250-251) | `... \|> dplyr::group_by(ident) \|> dplyr::filter(dplyr::row_number(diabete) == 1L) \|> dplyr::ungroup()` | §5.9a (néo-code le plus petit : E10 < E11i < E11ni) |
| 5 | `dplyr::distinct(ident,.keep_all = TRUE)` du sous-bloc HTA (×3, l.98, 177, 256) | **inchangé**, commentaire ajouté | §8.3 : `hta` est une constante `"I10"` → la ligne est déjà déterministe |
| 6 | l.48 `...ghm2,passage_urg,nbrum)` | `...ghm2,passage_urg,nbrum,raac)` | §3.2 + §6.1 : `raac` était un pivot de la chirurgie ambulatoire (v7.1.2 l.30) — branche supprimée depuis, colonne **inerte conservée** (voir note sous le tableau) |
| 7 | (absent, branches 18-22 et <=17) | `dplyr::mutate(raac = NA) \|>` inséré après le `inner_join(.rgp)` (extraction l.226, 314) | idem 6 : colonne requise par le `select` final ; `raac` n'existe pas dans les millésimes anciens (Q4) |
| 8 | (absent) | `dplyr::mutate(cage2 = ifelse(cage3=="lt_18" & substr(ghm2,3,3)=="C" & age>14,"ge_18",cage3)) \|>` avant le `select` final (extraction l.336) | §4 (règle `cage2` v7.1.2 l.68 conservée) + §6.1 — colonne **inerte conservée** |
| 9 | `dplyr::select(anonyme,...,cage3,cage,...,type_unite,prep_sc)` (l.264-265) | `... cage3,cage2,cage, ... ,type_unite,prep_sc,raac)` | idem 6, 8 |
| 10 | (absent ; grain `(ident, type_unite)`) | Bloc de 8 lignes inséré dans les **trois** branches immédiatement après le dédoublonnage `(ident, type_unite)` de l'écart 2 (extraction l.89-96, 172-179, 260-267), marqué `# écart B1-10` : `dplyr::mutate(prep_sc = max(prep_sc), .by = ident)` puis `rang_unite` par `case_when` littéral (SC 1, SC-NEONAT 2, NEONAT 3, GERIATRIE 4, HC 5, HP 6, autres/UHCD 7), `group_by(ident) \|> filter(row_number(rang_unite) == 1L) \|> ungroup() \|> select(-rang_unite)` | **Relecture, 2e évolution** : grain réduit à **une ligne par séjour** (`ident`), unité la plus prioritaire, `prep_sc` = max par séjour. Motivation : intention d'origine du v7.2 (cf. la ligne orpheline `mutate(sc = max(prep_sc), .by = ident)` v7.2 l.458) ; résout Q5. L'ordre est documenté par `PRIORITE_TYPE_UNITE` (config l.79-80) et reste littéral dans la chaîne. UHCD dernier : sinon un séjour multi-RUM passé par l'UHCD serait réduit à sa ligne UHCD puis supprimé par le filtre `(nbrum == 1 & type_unite == "UHCD") \| type_unite != "UHCD"`. |

**Conséquences de l'écart 10 en aval (aucun changement de code) :** le mécanisme `sc` de
`prep_scenarios2` (B8 : `full_join` + `filter(!(prep_sc==0 & sc==1))`, conservé verbatim) devient
sans effet, `prep_sc` étant désormais constant par séjour ; `distinct(ident,...)` dans N1 et N2
est sans effet ; un séjour compte une seule fois dans le catalogue longs et dans le seuil
`PIVOTS_LONGS_SEUIL` (test SQLite : `sum(n)` = nombre de séjours éligibles). `PIVOTS_LONGS` inchangés.

**Relecture (branche chirurgie ambulatoire supprimée) :** les écarts 6 à 9 (`raac`, `cage2`) avaient
été faits pour servir les pivots ambulatoires. Ils n'ont plus de consommateur mais sont **conservés
volontairement** : les retirer reviendrait à rééditer la plus grosse chaîne base pour un gain nul,
exactement le type de retouche interdit par §0.1. Colonnes inertes dans `prep_data_<an>`.

Le `dplyr::rename(age = cage3)` de v7.2 l.267 est conservé : la colonne `age` de
`prep_data` reste la classe `ge_18`/`lt_18` (pivot des séjours longs), `cage2` est la
même classe avec la règle « mineur >14 ans en GHM C ».

### B2 — `ref_das_aigu(an)` — extraction l.354-366 (ex `df_das_ref`)
**Source :** v7.2 l.444-453. Écarts :
- `dplyr::filter(duree>3)` → `dplyr::filter(duree>DUREE_MIN_REF)` (config, `DUREE_MIN_REF = 3`).
- `all_of(` → `dplyr::all_of(` (§1, appels namespacés).
- `dplyr::collect()-> df_das_ref` → `dplyr::collect()` (valeur de retour de la fonction ; §0.2 « nom de la variable de sortie »). Appel via `FABRIQUES_REFS$ref_das_aigu` (extraction l.561-571), export `ref_das_aigu.parquet`.
- `comp_sat_diab` (non défini dans le dépôt) : la chaîne est **inchangée** ; un alias
  `comp_sat_diab <- codes_comp_sat_diab` est ajouté dans `referentiels.R` (Q2).

### B3 — `prep_das_chronique(an)` + `ref_das_chronique(an)` — extraction l.372-405
**Source :** v7.1.2 l.88-112 (`prep_das`). Écarts :
- nom de fonction `prep_das` → `prep_das_chronique` ; nom de table `"prep_das" %+% an` → `"prep_das_chro_" %+% an`.
- `anseqta = anseqta_de(an)` ajouté en tête ; `dplyr::filter(v2025>1)` → `dplyr::filter(!!dplyr::sym("v20"%+% anseqta)>1)` ; `"v20"%+% an` (×2) → `"v20"%+% anseqta` (§5.8 : même millésime dynamique partout ; pour `an = 26`, `v2026` n'existe pas, `anseqta` donne `"25"`).
- `dplyr::filter(duree>DUREE_MIN_REF) |>` inséré après `atihble(...)` (§6.2 : prévalence estimée sur les séjours longs).
- `all_of(` → `dplyr::all_of(`.
- La ligne `dplyr::summarise(nb_das = dplyr::n(),.by= c(diag2,das,sexe,cage,niveau,type_liste,caract))` (v7.1.2 l.108) est **déplacée à l'identique** dans `ref_das_chronique()` (extraction l.401-405), appliquée sur la table calculée ; le `compute` garde le niveau séjour (`ident`) pour alimenter B4 (§3.3d) et N3 (§7.6). Résultat identique à v7.1.2 l.223 (`collect()` de la table).
- `|> invisible()` après le `compute` (pas d'impression au niveau supérieur).

### B4 — `ref_comp_diabete(an)` — extraction l.411-423 (ex `df_res_epi_diabete_chu`)
**Source :** v7.1.2 l.195-205 (chaîne valide). La version v7.2 l.456-467, au pipe cassé
(`mutate(sc = max(prep_sc), .by = ident)` sans `|>`), est abandonnée ; le `mutate(sc=...)`
orphelin, vestigial, est supprimé (§5.3). Écarts :
- `categ_pmsi=="CHR/U"` → `categ_pmsi==TYPE_ETBS_REF_DIABETE` (config).
- `dplyr::collect() ->df_res_epi_diabete_chu` → `dplyr::collect()`.
- Post-collect (R, v7.1.2 l.207-214), désormais côté tirage dans `penaliser_comp_diabete()` (helpers l.554-562 ; l'export `ref_comp_diabete.parquet` contient les effectifs bruts) : `cage_ped`/`cage_ages` → `CAGE_PED`/`CAGE_AGES`, `0.2`/`0.5` → `PENALITE_9_AGES`/`PENALITE_9_AUTRES` (config).

### B5 — Sélection chirurgie ambulatoire — **supprimé à la relecture**
Bloc v7.1.2 l.261-269 initialement repris en v8 (section 5). Retiré avec toute la branche :
partie obsolète et seul consommateur de `df_dp_das`, référentiel absent du dépôt. Détail en
section 5 (« non repris ») ; Q1 résolue. La construction de `REFS` (commune aux deux branches) est désormais dans tirage l.54-57.

### B6 — Pivots séjours courts — extraction l.543-547 (`fabrique_pivots_courts`)
**Source :** v7.1.2 l.219-221. Écarts : `an` → `AN_REF` ; `duree<3` → `duree%in%DUREE_COURTS`
(config `0:2`, équivalent pour une durée entière) ; `all_of(pivots)` → `dplyr::all_of(PIVOTS_COURTS)` ;
`nb>10` → `nb>SEUIL_PIVOT` (même valeur) ; `-> df_cases` → `df_cases_courts <-`.

### B7 — `df_v_admin_courts` — extraction l.549-553 (`fabrique_v_admin_courts`)
**Source :** v7.1.2 l.232-234. Écarts : `an` (25 en dur) → `AN_REF` ; nom `df_v_admin` → `df_v_admin_courts`.

### B8 — `prep_scenarios2(...)` — extraction l.480-527
**Source :** v7.2 l.278-327. Écarts :
- `anseqta = dplyr::case_when(...)` (3 lignes) → `anseqta = anseqta_de(an)` (même table de correspondance, déplacée en config §3.0).
- `dplyr::filter(v2025>1)` → `dplyr::filter(!!dplyr::sym("v20"%+% anseqta)>1)` (§5.8).
- `all_of(` → `dplyr::all_of(` (×3).
- Post-collect (R) : `dplyr::arrange(ident,desc(niveau),desc(nb_das))` → `dplyr::arrange(ident,dplyr::desc(niveau),dplyr::desc(nb_das),das)` (§5.9 : ordre total, les ex æquo de niveau/fréquence étaient tranchés par l'ordre de collecte).

### B9 — Catalogue séjours longs — extraction l.588-631 (`construire_catalogue_longs` + seuil)
**Source :** v7.2 l.483-534. Le code de boucle (R, post-collect) est encapsulé dans
`construire_catalogue_longs()` : deux boucles `TYPES_ETBS_LONGS × ANS_HISTORIQUE` reproduisent
CHR/U (26 puis 17-25) puis CH (17-26) ; la somme étant commutative, l'ordre des années est
sans effet. `an` n'est plus réassigné au niveau supérieur (§5.12, variable de boucle `an_`).
Constantes → config (`DUREE_LONGS`, `NBDA_MAX`, `K_GRAINE_LONGS`, `PIVOTS_LONGS`).
Seuil (v7.2 l.526-530) : `.by = c("mode_hospit",...,"diag2")` → `.by = dplyr::all_of(PIVOTS_LONGS_SEUIL)`
(= `PIVOTS_LONGS` sans `nbda`, même liste + `type_unite`, `prep_sc`, cf. Q5) ; `nb>9` → `nb>SEUIL_PIVOT`
(§2.2) ; `select(-n)` conservé (§2.2) ; `dplyr::rename(poids = nb)` ajouté (§6.3).
Supprimés : `print("- Noombre ...")` (§5.14), l.537-540 (`sample_n(3000)`, `nb>5000` : §2.5).

### B10 — `df_v_admin_longs` — extraction l.555-559 (`fabrique_v_admin_longs`)
**Source :** v7.2 l.551-553. Écarts : `an` (25 en dur) → `AN_REF` ; nom → `df_v_admin_longs`.

### B11 — `cma` dans `referentiels.R` l.32
`dplyr::filter(v2025>1)` → `dplyr::filter(!!dplyr::sym("v20" %+% ANSEQTA_REF)>1)` (§5.8) avec
`if(!exists("ANSEQTA_REF")) ANSEQTA_REF <- "25"` pour la compatibilité des scripts v7.

---

## 2. Blocs dbplyr nouveaux (exigés par le spec, pas de source v7)

Écrits dans le style des chaînes existantes, sur les tables produites par B1/B3.

| Bloc | v8 | Exigence | Description |
|------|----|----------|-------------|
| N1 `ref_nb_chroniques(an)` | extraction l.428-438 | §3.3d, §6.2 | `prep_data_<an>` (durée > `DUREE_MIN_REF`), `distinct(ident,cage,sexe)`, left_join du nb de DAS chroniques distincts par séjour (`prep_das_chro_<an>`), NA → 0, comptage par `(cage, sexe, nb_chro)`. |
| N2 `ref_substitution_imprecis(an, codes_imprecis)` | extraction l.444-462 | §7.5 | effectifs de **tous** les codes de `.diag` par `(cat, code, cage, sexe)`, seuil `nb >= SEUIL_REF_IMPRECIS`, jointure niveau (`anseqta`), puis **après collect** filtre sur les catégories contenant un code « sans précision » (évite une liste `IN` de plusieurs centaines de valeurs côté base) et colonne `imprecis`. |
| N3 `ref_paires_chroniques(an)` | extraction l.465-474 | §7.6 | auto-jointure de `prep_das_chro_<an>` distinct `(ident,cage,sexe,das)` sur `(ident,cage,sexe)`, `das_a < das_b`, comptage, seuil `nb >= SEUIL_REF_PAIRES`. |

---

## 3. Correspondance des corrections §5

| §5 | Correction | Où (v8) |
|----|-----------|---------|
| 1 | `sexe_ ==sexe_` → `sexe == sexe_` | `sample_das_long`, helpers l.280 ; tests « sexe respecté » (helpers + SQLite) |
| 2 | `age_` non défini → argument `age` ; aucune globale implicite (`refs`, tables en argument) ; `df_tmp_sav<<-` supprimé | `sample_das_long`, `sample_das_court`, `construire_refs` |
| 3 | bloc `df_res_epi_diabete_chu` au pipe cassé | B4 (version v7.1.2 valide, `mutate(sc=...)` supprimé) |
| 4 | `filter_chap` (1 caractère) → `dedup_categorie` (3 caractères + YAML) | helpers l.55-75 ; `filter_cat` (2 caractères, v7.1.2) également remplacé |
| 5 | `complications_diab` → `codes_diab` en argument | `get_codes_diabete_from_neo(…, codes_diab, …)` |
| 6 | `sample_age` par ligne, bornes semi-ouvertes (`[1-5[` → 1:4, `[80-[` → 80:`AGE_MAX_OUVERT`) | `sample_age_ligne`, `sample_age` (vapply, plus de consommateur dans le v8 depuis la suppression de la branche chir ambu, conservé et testé) ; tests 1000 tirages/classe + régression vectorisation |
| 7 | bloc v7.1.2 l.224-228 mort et cassé | **supprimé** (`niveau` ne servait qu'au filtre commenté l.134-136) |
| 8 | `filter(v2025 > 1)` en dur | B3, B8, N2 (`!!dplyr::sym("v20"%+% anseqta)`), B11 (`ANSEQTA_REF`) ; grep `v2025` : plus aucune occurrence hors commentaires |
| 9 | (a) `distinct(.keep_all=TRUE)` côté base → window `row_number` (B1) ; (b) `slice(1:2)` → `dplyr::slice_sample(n = NB_VARIANTES_ADMIN_COURTS)` sous le seed global (tirage l.79-83) ; tiebreak `das` dans B8 | B1, B8, section 6 |
| 10 | `neo_codes_diabete` défini dans `sample_das` | `referentiels.R` (définition unique) ; passé en argument `refs$neo_codes` |
| 11 | extension `.parquet` | `chemin_export()` tirage l.62 ; exports d'extraction nommés `<produit>.parquet` |
| 12 | `an` réassigné | plus aucune affectation de `an` ; `AN_REF`, `ANS_HISTORIQUE`, boucles `an_` |
| 13 | `PATH_PROJET` via `SCENARIOS_PMSI_PATH` avec défaut | config l.11-13 (et bootstrap des deux scripts) ; alias `path_projet` pour `referentiels.R` |
| 14 | `Noombre`, code commenté mort | supprimés ; v7.1.2 l.289-315 (liste de libellés) non repris |

---

## 4. Écarts hors liste §5 (à valider par le relecteur — chacun est réversible en une ligne)

Tous dans les **helpers purs** (§0.3 : « là où tu peux écrire du code neuf »), aucun dans une chaîne base.

- **H1 — `retro_code_diabete` : 5e caractère inversé dans utils.R l.338-340.** utils.R produisait
  `E11i → "E11"+comp+"8"` et `E11ni → "E11"+comp+"0"`, alors que `code_dnid_ins` (insulinotraité)
  = `E1120, E1130…` (5e caractère **0**) et `code_dnid` = `E1128…` (**8**) — cf. aussi les libellés
  v7.1.2 l.293-294. helpers l.89-94 : `E11i → …0`, `E11ni → …8`. Test : « rétro-codes E11 appartiennent
  aux listes code_dnid_ins / code_dnid ». Pour revenir à l'ancien comportement : échanger `"0"` et `"8"`.
- **H2 — filtre GHM en C (v7.1.2 l.132) : `substr(das,1,2)!="F10"`** compare 2 caractères à
  une chaîne de 3 → toujours vrai, F1x jamais exclu. Le spec (§6.2 « F10 sauf F17 ») décrit
  l'intention. helpers `filtre_das_ghm_c` l.194-199 (utilisé par la branche courts) : `substr(das,1,2)!="F1"`. Pour revenir : remettre `"F10"`.
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
| v7.1.2 l.259-272 | **branche chirurgie ambulatoire** (durée 0, pivots avec `raac`/`cage2`, jointures `df_dp_das` et `df_ref_specialite`, sortie `scenarios_chir_ambu_v3_`) | **supprimée à la relecture** : partie obsolète, seul consommateur de `df_dp_das` (référentiel absent du dépôt). Retirés avec elle : le `stop()` sur `df_dp_das`, l'export `scenarios_chir_ambu_*`, ses lignes du rapport §9, et les constantes orphelines `GHM_CHIR_AMBU_LISTE`, `PIVOTS_CHIR_AMBU`, `DUREE_CHIR_AMBU`, `CMD_OBSTETRIQUE`, `FILE_VERSION_SPE`. Non touchés : `prep_data` (B1, colonnes `raac`/`cage2` inertes), `filtre_das_ghm_c` (utilisé par les courts), `sample_age` (helper testé). |
| v7.1.2 l.12-13 | chargement `df_ref_specialite` (`referentiel_spe_racine_30_2.xlsx`) | plus de consommateur : non chargé |
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
| utils.R l.336-382 | `retro_code_diabete`, `get_codes_diabete_from_neo` (globales) | laissés en place, **masqués** par les versions de helpers_v8.R (partie A) |

---

## 6. Fichiers annexes modifiés / créés

- `referentiels.R` : B11 ; `comp_sat_diab <- codes_comp_sat_diab` ; `neo_codes_diabete` (§5.10). Rien d'autre.
- `referentiels/exclusions_paires.yaml` : créé (§6.4), 4 paires évidentes, structure `- [A, B]`.
- `tests/test_helpers.R` : §8.1 + brief industrialisation §8, 146 assertions (`stopifnot`, sans testthat). Source `config_v8.R` puis `helpers_v8.R`.
- `tests/test_chaines_sqlite.R` : les **scripts réels** (extraction puis tirage) sur SQLite **fichier**
  avec un faux paquet `pRatihque` (mock interdit pendant le tirage), 55 assertions : chaînes dbplyr
  (§5.9a, B1-10, refs, §7.5/§7.6), sessions multiples et résolution des besoins, cache des partiels,
  reprise, FORCER_REFS, garde-fou `partiels_meta`, tirage sans base, reprise des chunks, identité
  parquet, livrables, mode `catalogue_complet`. Ne valide PAS le dialecte ni les colonnes réelles.
- `RUN.md` : séquence opérationnelle et règles de cache (section 10).
- `utils.R`, `exclusions.R`, scripts v7 : inchangés.

---

## 7. Questions (doutes consignés, aucune action prise — §0.5)

- **Q1 — `df_dp_das` (RÉSOLUE à la relecture)** : il n'était défini nulle part et n'avait qu'un
  consommateur, la branche chirurgie ambulatoire. La branche est supprimée (section 5) ; plus aucune
  dépendance à `df_dp_das` ni à `df_ref_specialite`.
- **Q2 — `comp_sat_diab`** (v7.2 l.448) n'existe pas ; `referentiels.R` définit `codes_comp_sat_diab`
  (codes « satellites » du yaml). Alias ajouté sans toucher à la chaîne. Si une autre liste était visée,
  redéfinir l'alias.
- **Q3 — règle d'ordre §5.9a.** Le spec propose « garder le RUM du DP, sinon min(rum) ; pour .fixe :
  garder la ligne de rumdudp ». (a) Le RUM du DP (`rumdudp`) n'est connu qu'après la jointure `.fixe`,
  postérieure au `distinct` de `.um` : l'appliquer imposerait de réordonner les jointures (interdit §0.1).
  (b) L'existence d'une colonne `rum` dans `.um` n'est pas vérifiable hors base. v8 ordonne donc sur des
  colonnes **présentes dans le `select`** : `.um` → `mode_hospit` (HC avant HP ; les lignes restantes
  d'un même `(ident, type_unite)` ne diffèrent que par `finessgeo`, cas multi-sites ; depuis l'écart
  B1-10 cette étape est suivie de la réduction à une ligne par `ident`), `.fixe` →
  `ident` (plus petit séjour par patient × GHM ; `.fixe` étant à une ligne par séjour, « ligne de
  rumdudp » n'a pas d'objet), diabète → `diabete`. Si la base n'accepte pas les fonctions fenêtre
  (`ROW_NUMBER() OVER`), remplacer par l'ancien `distinct` (3 lignes × 3 branches, repérées `# §5.9a`).
- **Q4 — `raac`** (colonne désormais inerte, B1) : sélectionné dans `.fixe` pour `an > 22` (comme v7.1.2 sur 2025) ; `NA` pour
  `an <= 22`. Si la colonne existe aussi avant 2023, on peut la sélectionner dans ces branches.
- **Q5 — `type_unite`/`prep_sc` dans `PIVOTS_LONGS` (RÉSOLUE, 2e évolution)** : le grain de
  `prep_data` est réduit à une ligne par séjour (écart B1-10, unité la plus prioritaire selon
  `PRIORITE_TYPE_UNITE`). Un séjour multi-unités n'est plus compté qu'une fois dans le catalogue et
  le seuil ; `PIVOTS_LONGS` inchangés.
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
Rscript -e 'for(f in c("config_v8.R","helpers_v8.R","extraction_associations_codes_v8.R","tirage_scenarios_v8.R")) parse(f)'
Rscript tests/test_helpers.R                                     # 146 assertions vertes
R_LIBS_TEST=<lib avec dbplyr/DBI/RSQLite/arrow> Rscript tests/test_chaines_sqlite.R   # 55 assertions vertes
grep -n 'filter_chap\|sexe_ ==sexe_\|v2025\|slice(1:2)\|<<-\|distinct(.*\.keep_all' config_v8.R helpers_v8.R extraction_associations_codes_v8.R tirage_scenarios_v8.R
grep -c 'pRatihque::' tirage_scenarios_v8.R                       # 0 attendu
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

1. (Q1 résolue : plus de dépendance à `df_dp_das`.)
2. Q3 — support des fonctions fenêtre par la base (B1 : 9 lignes marquées `# §5.9a`, plus les blocs `# écart B1-10` qui utilisent `max() OVER (PARTITION BY ident)` et `ROW_NUMBER()`).
3. `raac` et `nbrum` dans `.fixe` (an > 22) : `raac` vient de v7.1.2, `nbrum` de v7.2, tous deux sur 2025. `raac` est inerte mais toujours sélectionné : si la colonne manquait, retirer `,raac` du `select` (B1 #6) et le `mutate(raac = NA)` (B1 #7).
4. Volume du tirage longs : piloté par `MODE_SELECTION` / `BUDGET_TOTAL_LONGS` (section 10) ;
   `NB_VARIANTES_ADMIN_LONGS` limite éventuellement l'habillage admin (v7.2 gardait toutes les variantes).
5. Colonnes `code`/`libelle` de `cim_2024.xlsx` (N2, `stopifnot`).

---

## 10. Chantier « industrialisation » (scission en quatre fichiers)

Point de départ : v8 mono-fichier avec l'écart B1-10 (commit `c4bd020`). Règle d'or §0
inchangée : les chaînes dbplyr sont **déplacées en blocs entiers, jamais modifiées** (diff
vide bloc à bloc, commandes en 10.3) ; les `compute(temporary = TRUE)` restent temporaires.

### 10.1 Mapping ancien -> nouveau

| v8 mono-fichier (c4bd020) | Nouveau | Notes |
|---|---|---|
| section 0 Config (l.21-118) | `config_v8.R` | + bloc PROFIL, surcharges, dérivés, `valeurs_effectives_config()` |
| section 1 Sources et connexion (l.119-136) | extraction section 0 (l.21-40) ; tirage section 0 (l.13-29) | tirage : sans `conn`, `referentiels.R` sourcé avec ses deux appels base gardés par `exists("conn")` |
| (nouveau) | extraction section 1 « Résolution des besoins » (l.42-59) | `resoudre_besoins()`, `imprimer_plan()`, garde-fou `partiels_meta.yaml` |
| section 2 prep_data (l.137-425) | extraction section 2 (l.61-348) | bloc déplacé, diff vide |
| section 3 défs 3a-3e (l.426-552) | extraction section 3 (l.350-475) | bloc déplacé, diff vide |
| section 3f exécution (l.553-570) | extraction section 5a-5b (l.529-585) | `prep_data` pour `plan$annees_a_preparer` seulement ; refs via `FABRIQUES_REFS`, chacune sautée si son parquet existe ; pénalisation .9 déplacée côté tirage |
| section 4 Helpers purs (l.571-928) | `helpers_v8.R` partie A (l.14-366) | bloc déplacé, diff vide (hors ligne vide finale) |
| (nouveau) | `helpers_v8.R` partie B (l.368-fin) | `pmap_chunks`, sélection (`selection_catalogue_complet`, `selection_quota_dp`, `repartir_equitable`, `tirer_avec_remise`, `effectifs_selection`), résolution des besoins, méta/apports des partiels, `penaliser_comp_diabete`, `fichiers_manquants`, `verifier_meta_tirage`, livrables (`libelles_cim`, `echantillonner_revue`, `formater_revue`, `top_das_par_cmd`) |
| section 5 (REFS) (l.929-939) | tirage l.54-57 | |
| section 6 Séjours courts (l.941-969) | chaînes `df_cases_courts`, `df_v_admin_courts` -> extraction `fabrique_pivots_courts`, `fabrique_v_admin_courts` (l.543-553, diff vide, indentation +2) ; tirage section 2 (l.66-94) | tirage par `pmap_chunks`, habillage sous seed dédié |
| section 7 prep_scenarios2 (l.976-1025) | extraction section 4 (l.477-527) | bloc déplacé, diff vide |
| section 7 `construire_catalogue_longs` (l.1029-1041) | extraction 5c (l.588-618) | enrobage R : partiels (checkpoint), instrumentation ; la chaîne `prep_scenarios2` et la summarise d'agrégation inchangées |
| section 7 seuil (l.1046-1050) | extraction section 6 (l.625-629) | identique, renommé |
| section 7 tirage longs + `df_v_admin_longs` (l.1054-1077) | chaîne -> extraction `fabrique_v_admin_longs` (l.555-559, diff vide) ; tirage section 3 (l.96-173) | sélection figée + chunks |
| section 8 Exports (l.1083-1098) | extraction section 6 ; tirage sections 2-3 | voir 10.4 |
| section 9 Rapport (l.1100-fin) | tirage section 4 (l.175-fin) | + meta.yaml en tête, top 30 DAS par CMD, table diag2 × type_unite, `echantillon_revue.csv` |

### 10.2 Renommages et noms uniques
- `df_catalogue_brut` -> `df_prep_scenarios` ; `df_catalogue_longs` -> `df_prep_scenarios_seuil` (extraction).
- Tirage : un seul nom `df_scenarios` par branche (courts puis longs), écrasé ; `df_tirage` pour le résultat brut des chunks ;
  `suppressWarnings(rm(df_scenarios, df_tirage))` en tête de chaque branche ; `write_parquet` immédiatement après création ;
  `rm()` + `gc()` après le calcul des petits agrégats du rapport (jamais deux branches vivantes).
- Noms de fichiers de tirage inchangés : `scenarios_courts_v8_<date>.parquet`, `scenarios_longs_tirage_v8_<date>.parquet`,
  `rapport_v8_<date>.txt`. `scenarios_longs_catalogue_v8_<date>.parquet` devient `catalogue_longs_seuil.parquet`
  (+ `_meta.yaml`) ; les référentiels §7.5/§7.6 deviennent `referentiel_substitution_imprecis.parquet` /
  `referentiel_paires_chroniques.parquet` (sans date : ce sont des produits cachés par existence).

### 10.3 Blocs base déplacés — diff vide attendu
```
OLD=$(mktemp); git show c4bd020:extraction_associations_codes_v8.R > $OLD ; NEW=extraction_associations_codes_v8.R
diff <(awk '/^## ---- 2\. prep_data/{f=1} /^## ---- 3\. Tables/{f=0} f' $OLD) <(awk '/^## ---- 2\. prep_data/{f=1} /^## ---- 3\. Tables/{f=0} f' $NEW)
diff <(awk '/^## ---- 3\. Tables/{f=1} /^# 3f\. Exécution/{f=0} f' $OLD) <(awk '/^## ---- 3\. Tables/{f=1} /^## ---- 4\. prep_scenarios2/{f=0} f' $NEW)
diff <(awk '/^#----------------------------- Prépa DAS/{f=1} /^# Catalogue : agrégation/{f=0} f' $OLD) <(awk '/^#----------------------------- Prépa DAS/{f=1} /^## ---- 5\. Exécution/{f=0} f' $NEW)
for v in df_cases_courts df_v_admin_courts df_v_admin_longs; do
  diff <(awk -v v="^$v <- " '$0 ~ v {f=1} f{print} f&&/dplyr::collect\(\)/{exit}' $OLD) <(awk -v v="^  $v <- " '$0 ~ v {f=1} f{sub(/^  /,""); print} f&&/dplyr::collect\(\)/{exit}' $NEW)
done
diff <(awk '/^## ---- 4\. Helpers purs/{f=1;c=0} f{c++; if(c>5) print} /^## ---- 5\. Branche/{exit}' $OLD | sed '$d') <(awk '/^## ---- A\. Helpers de tirage/{f=1;next} /^## ---- B\. Industrialisation/{exit} f' helpers_v8.R | sed '$d')
```
Résultat au moment de la livraison : tous vides (helpers : une ligne vide finale près).
Seules retouches hors chaînes : `prep_das_chronique` inchangé ; `referentiels.R` : `if(exists("conn"))`
devant `copy_to(...)` et devant `cma <- ...` (les chaînes elles-mêmes ne changent pas).

### 10.4 Config nouvelle (config_v8.R)
| Constante | Rôle |
|---|---|
| `PROFIL` (`SCENARIOS_PMSI_PROFIL`, défaut `diagnostic`) | sélectionne le bloc PROFIL : `ANS_HISTORIQUE`, `TYPES_ETBS_LONGS`, `MODE_SELECTION`, `BUDGET_TOTAL_LONGS`, `CHUNK_SIZE`, `EXPORTS_DIR` ; chaque valeur surchargeable après le bloc |
| `MODE_SELECTION` | `quota_dp` (diagnostic) / `catalogue_complet` (production) |
| `BUDGET_TOTAL_LONGS` | 1 000 (diagnostic) / 10 000 000 (production) |
| `QUOTA_MIN_PAR_UNITE` = 5 | plancher par `type_unite` en mode `quota_dp` |
| `CHUNK_SIZE` | 200 / 2 000 ; `GARDER_CHUNKS` = TRUE |
| `EXPORTS_DIR` | `results/exports_diagnostic/` ou `results/exports/` ; `CHUNKS_DIR` = `EXPORTS_DIR/chunks/` |
| `PARTIELS_DIR` = `results/partiels/` | partagé entre profils |
| `FORCER_REFS` = FALSE | TRUE : recalcul des refs malgré leur existence |
| `SCENARIOS_PMSI_SURCHARGE` | fichier R optionnel évalué après le bloc PROFIL (paliers, tests) |
| `NOMS_REFS`, `REFS_CHRONIQUES`, `VERSION_SCRIPT`, `NOMS_CONFIG_META`, `valeurs_effectives_config()` | produits de référence, méta |
Supprimés : `NB_TIRAGES_LONGS` (remplacé par `NB_VARIANTES` calculé en mode `catalogue_complet`,
1 en mode `quota_dp`) et `MAX_SCENARIOS_LONGS` (remplacé par `MODE_SELECTION`/`BUDGET_TOTAL_LONGS`).
`MODE_REPRISE` n'existait pas.

### 10.5 Résolution des besoins et cache
`resoudre_besoins()` (pure, testée) calcule : `iterations_manquantes` = (etbs, an) sans
`PARTIELS_DIR/catalogue_partiel_<etbs>_<an>.parquet` (`<etbs>` = `etbs_label()`, ex. `CHRU`) ;
`refs_manquantes` = `NOMS_REFS` sans parquet dans `EXPORTS_DIR` (ou toutes si `FORCER_REFS`) ;
`annees_a_preparer` = années des itérations manquantes ∪ {`AN_REF`} si une ref manque ;
`prep_das_chronique` requis si une `REFS_CHRONIQUES` manque. `prep_data(an)` n'est appelée que
pour `annees_a_preparer` ; tout-à-jour -> message et passage direct à l'agrégation. Les tables
temporaires ne sont consommées que par des produits dont l'absence a déclenché leur création
(vérifié par le test : session 2 ne crée que `prep_data_20`, session 3 aucune table).
`partiels_meta.yaml` : K différent -> `stop()` ; `NBDA_MAX`/`DUREE_LONGS`/`PIVOTS_LONGS`/
`VERSION_SCRIPT` différents -> avertissement (le brief n'exige que K ; le reste est signalé, pas bloqué).
`diagnostic_apports.csv` : une ligne par itération dans l'ordre d'exécution (statut `calculé`/`relu`).

### 10.6 Tirage
Sélection figée sous seed avant le premier chunk : `selection_longs.parquet` (quota_dp) ou
`NB_VARIANTES` dans `meta_tirage.yaml` (catalogue_complet) ; `verifier_meta_tirage()` refuse de
reprendre des chunks tirés avec d'autres paramètres (`MODE_SELECTION`, `BUDGET_TOTAL_LONGS`,
`QUOTA_MIN_PAR_UNITE`, `CHUNK_SIZE`, `SEED`, `nrow_catalogue`). `pmap_chunks` : seed par chunk
(`seed_base + i`, `seed_base` = `SEED` pour les courts, `SEED + 1e5` pour les longs), chunk présent
sauté. Habillage admin et échantillons de revue sous seeds dédiés (`SEED + 1e6 …`), pour que la
reprise reste bit à bit identique. Ordre v7.2 conservé dans `sample_das_*`.

### 10.7 Questions (industrialisation)
- **Q13 — refs propres au profil** : `EXPORTS_DIR` distincts ⇒ les 9 refs sont recalculées une fois par
  profil (elles dépendent d'`AN_REF`, pas du profil). Copier les parquets `ref_*`, `pivots_courts`,
  `v_admin_*`, `referentiel_*` de `exports_diagnostic/` vers `exports/` évite la requête (RUN.md).
- **Q14 — `quota_dp` avec remise** : le brief impose le tirage avec remise ; pour des DP à gros catalogue
  cela produit des doublons de pivots (complétions différentes par le seed). Sans remise quand
  `nrow >= quota` serait plus divers ; non fait.
- **Q15 — `partiels_meta`** : avertissement (non bloquant) pour `NBDA_MAX`, `DUREE_LONGS`, `PIVOTS_LONGS`,
  `VERSION_SCRIPT`. Si l'on veut bloquer aussi, passer ces clés en erreur dans `verifier_partiels_meta()`.
- **Q16 — `top_das_par_cmd` et `echantillon_revue`** portent sur les scénarios habillés (après
  habillage admin), donc pondérés par le nombre de variantes admin. Mesure indicative.
