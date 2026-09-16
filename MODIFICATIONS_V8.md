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
- Post-collect (R, v7.1.2 l.207-214), désormais côté tirage dans `penaliser_comp_diabete()` (helpers l.559-567 ; l'export `ref_comp_diabete.parquet` contient les effectifs bruts) : `cage_ped`/`cage_ages` → `CAGE_PED`/`CAGE_AGES`, `0.2`/`0.5` → `PENALITE_9_AGES`/`PENALITE_9_AUTRES` (config).

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

### B8 — `prep_scenarios2(...)` — extraction l.488-547 (écart P1 depuis le chantier mémoire, section 13)
**Source :** v7.2 l.278-327. Écarts :
- `anseqta = dplyr::case_when(...)` (3 lignes) → `anseqta = anseqta_de(an)` (même table de correspondance, déplacée en config §3.0).
- `dplyr::filter(v2025>1)` → `dplyr::filter(!!dplyr::sym("v20"%+% anseqta)>1)` (§5.8).
- `all_of(` → `dplyr::all_of(` (×3).
- Post-collect (R) : `dplyr::arrange(ident,desc(niveau),desc(nb_das))` → `dplyr::arrange(ident,dplyr::desc(niveau),dplyr::desc(nb_das),das)` (§5.9 : ordre total, les ex æquo de niveau/fréquence étaient tranchés par l'ordre de collecte).

### B9 — Catalogue séjours longs — extraction l.696-727 (`construire_catalogue_longs`) et l.752-801 (catalogue deux étages + seuil ; section 13)
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
- `tests/test_helpers.R` : §8.1 + briefs industrialisation §8, conversion §7, mémoire, orchestration, chunking dynamique et aval production et finitions exploitation, 278 assertions (`stopifnot`, sans testthat). Source `config_v8.R` puis `helpers_v8.R`. Repli arrow par paquet mock (section 11).
- `tests/test_chaines_sqlite.R` : les **scripts réels** (extraction puis tirage) sur SQLite **fichier**
  avec un faux paquet `pRatihque` (mock interdit pendant le tirage), 119 assertions : chaînes dbplyr
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
Rscript tests/test_helpers.R                                     # 278 assertions vertes (275 sans arrow : chemin (a) non testé)
R_LIBS_TEST=<lib avec dbplyr/DBI/RSQLite[/arrow]> Rscript tests/test_chaines_sqlite.R   # 119 assertions vertes (avec ou sans arrow)
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
`VERSION_SCRIPT` différents -> avertissement (rendus bloquants sauf `VERSION_SCRIPT` par les finitions, section 11).
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
- **Q13 — refs propres au profil (TRAITÉE, finitions)** : `EXPORTS_DIR` distincts ⇒ les 9 refs sont
  recalculées une fois par profil. Copier les parquets `ref_*`, `pivots_courts`, `v_admin_*`, `referentiel_*`
  de `exports_diagnostic/` vers `exports/` évite la requête, **si et seulement si** `AN_REF`, `SEUIL_REF_DAS`,
  `SEUIL_REF_IMPRECIS` et `SEUIL_REF_PAIRES` sont identiques entre les deux profils ; sinon
  `FORCER_REFS <- TRUE` et recalcul. Règle reportée dans RUN.md (étape 3.1).
- **Q14 — `quota_dp` avec remise** : le brief impose le tirage avec remise ; pour des DP à gros catalogue
  cela produit des doublons de pivots (complétions différentes par le seed). Sans remise quand
  `nrow >= quota` serait plus divers ; non fait.
- **Q15 — `partiels_meta` (SOLDÉE, finitions)** : `NBDA_MAX`, `DUREE_LONGS`, `PIVOTS_LONGS` sont désormais
  bloquants au même titre que `K_GRAINE_LONGS` (`CLES_PARTIELS_BLOQUANTES`, helpers l.522) ; seule
  `VERSION_SCRIPT` reste en avertissement (`CLES_PARTIELS_AVERTISSEMENT`, l.523).
- **Q16 — `top_das_par_cmd` et `echantillon_revue`** portent sur les scénarios habillés (après
  habillage admin), donc pondérés par le nombre de variantes admin. Mesure indicative.

---

## 11. Finitions avant espace sécurisé (relecture)

Aucun changement fonctionnel des scripts de production ; aucune chaîne base touchée.

1. **Repli arrow dans les tests** (`tests/test_chaines_sqlite.R`, `tests/test_helpers.R`) : si
   `requireNamespace("arrow")` échoue, un paquet mock `arrow` est construit et installé à la volée
   dans `tempdir()` (même mécanique que le mock `pRatihque`), exposant `write_parquet = saveRDS`
   et `read_parquet = readRDS`, puis placé en tête de `.libPaths()`. Limite documentée en
   commentaire : les fichiers du mock sont des RDS nommés `.parquet`, valables parce que relus par
   le même mock. Les scripts de production continuent d'exiger le vrai arrow. L'ancien
   `stop("Paquet manquant : arrow")` et le repli `ecrire/lire` de test_helpers.R sont retirés.
   Les deux suites ont été lancées AVEC arrow (`R_LIBS_TEST` = bibliothèque contenant arrow) et
   SANS arrow (`R_LIBS_TEST` = bibliothèque ne contenant que dbplyr/DBI/RSQLite et leurs
   dépendances) : 150 et 55 assertions vertes dans les quatre cas ; la ligne finale du test SQLite
   indique `arrow = mock RDS` ou `arrow = réel`.
2. **`verifier_partiels_meta` bloquant** (helpers l.517-547) : `NBDA_MAX`, `DUREE_LONGS`,
   `PIVOTS_LONGS` promus bloquants (`stop()` demandant de vider `PARTIELS_DIR`, toutes les clés
   divergentes listées dans le message) au même titre que `K_GRAINE_LONGS` ; `VERSION_SCRIPT` reste
   en avertissement. Motif : ces paramètres agissent en amont de l'écriture des partiels (filtres
   des séjours, grain des comptes). Tests : un cas bloquant par clé promue, un cas multi-clés, le
   cas `VERSION_SCRIPT` non bloquant. RUN.md (règles de cache) mis à jour. Q15 soldée.
3. **RUN.md, étape 3.1** : condition de validité de la copie des refs diagnostic → production
   (`AN_REF`, `SEUIL_REF_DAS`, `SEUIL_REF_IMPRECIS`, `SEUIL_REF_PAIRES` identiques, sinon
   `FORCER_REFS <- TRUE`). Q13 traitée.

---

## 12. Chantier « conversion E669 -> E660 »

**Doctrine et périmètre.** Les codes E669x (obésité/surpoids « sans précision ») de la base
nationale sont tenus pour des erreurs de codage et convertis en E660x (« dus à un excès
calorique ») sur toutes les surfaces : diag2 (DP/DR pivot), graines, DAS de complétion,
référentiels. Périmètre STRICT `^E669` ; E661, E662, E668 intacts. Toggle `CONVERSION_E669`
(config l.102, TRUE dans les deux profils), `BARE_E669_DEFAUT = "0"` (l.103) ; les deux sont
écrits dans les meta.yaml (`NOMS_CONFIG_META`).

**Zéro écart chaîne base.** Conversion entièrement post-collect, côté R : dans les fabriques de
refs (extraction l.580-642, chaque fabrique = chaîne verbatim puis `if(!CONVERSION_E669) return`)
et sur le catalogue agrégé (extraction 5d, l.692-709). Les diffs bloc à bloc de la section 10.3
restent vides.

**Règles (helpers section C, l.635-834, tous purs et testés).**
- `convertir_e669` : E669 à suffixe → E660 + suffixe conservé (E6692 → E6602, E66920 → E66020),
  jamais re-tiré ; E669 nu inchangé à ce niveau ; NA-sûre ; tout autre code intact.
- `distribution_e660` : table (cage, sexe, code, n, part) par strate + lignes globales
  (cage = sexe = NA), calculée UNE fois par exécution sur les comptes BRUTS de `ref_das_chronique`
  (avant sa propre conversion) et exportée comme ref `distribution_e660.parquet`
  (ajoutée à `NOMS_REFS`, en 2e position après `ref_das_chronique`, et à `REFS_CHRONIQUES`).
- `repartir_e669_nu` : cascade strate (cage, sexe) → globale → `"E660" + BARE_E669_DEFAUT`,
  répartition d'effectifs aux plus forts restes (`repartir_proportionnel`, sum(n) conservé
  exactement, aucun aléa).
- `convertir_e669_comptes` : conversion → répartition du nu → ré-agrégation sum(col_n) par
  (cols_strate, col_code), schéma préservé.
- `convertir_e669_combo` (graines) : conversion de chaque code, nu réparti par la même cascade
  (ligne éclatée), codes re-triés en ordre C (comme `arrange(das)` de prep_scenarios2) et
  dédoublonnés, ré-agrégation.
- `convertir_e669_distinct` (v_admin, sans effectif) : nu remplacé par chaque classe de la cascade.
- Mesure : `compter_e669`, `effectifs_e660`, `impact_conversion_catalogue`, `impact_niveau_cma`.

**Ordre impératif partout : conversion → ré-agrégation → seuil.**

| Surface | Application | Note |
|---|---|---|
| catalogue longs | 5d : diag2 (`comptes`, strate = autres pivots + graine) puis graines (`combo`, strate = pivots), sum(n), PUIS seuil (section 6) | partiels relus en codes BRUTS |
| pivots_courts | conversion diag2 + ré-agrégation sum(nb) sur le collecté | **seuil déjà appliqué EN BASE** par la chaîne v7.1.2 (non modifiée) : pas de re-seuil possible ; perte conservatrice, les classes E669 sous le seuil individuellement ne sont jamais vues |
| ref_das_chronique | diag2 puis das (`comptes`, strate incl. niveau/type_liste/caract) ; `distribution_e660` calculée AVANT | niveau/type_liste/caract conservés comme attributs du code brut (Q18) |
| ref_das_aigu | diag2 puis das (`comptes`, strate mode_hospit, sexe, cage, racine, ghm2) | |
| ref_nb_chroniques, ref_comp_diabete | intacts (aucun code E66) | |
| referentiel_paires_chroniques | das_a puis das_b, `das_a < das_b` ré-imposé (`pmin`/`pmax`), paires identiques retirées, ré-agrégation, seuil `>= SEUIL_REF_PAIRES` | |
| referentiel_substitution_imprecis | conversion sur `code` (strate cat, cage, sexe, niveau), `imprecis` recalculé ; E669 retiré de la détection des codes imprécis (`codes_imprecis_extraction`), aussi côté tirage quand le meta porte `CONVERSION_E669: TRUE` | |
| v_admin_courts / v_admin_longs | `convertir_e669_distinct` sur diag2, `distinct()` | |
| tirage | aucune conversion ; contrôle : `^E669` résiduels comptés (diag2, graine, DAS) → anomalie si > 0 quand le meta porte TRUE ; effectifs E660x par classe au rapport | |

**Cache des partiels.** Les partiels restent en codes bruts ; la conversion n'est PAS une clé
de `verifier_partiels_meta` (commentaire helpers section C, config, RUN.md). Basculer le toggle
n'invalide pas `PARTIELS_DIR` mais impose `FORCER_REFS` et le vidage des chunks du tirage.

**Mesure d'impact (rapport d'extraction `rapport_extraction_v8_<date>.txt`, section 3).**
Distribution E660x de référence ; effectif E669 converti (diag2 / graine, suffixé / nu) ;
sum(n) avant/après (conservé) ; lignes fusionnées ; profils (pivots) > seuil avant / après,
ENTRÉS par fusion (clés > seuil après sans clé > seuil avant, clés avant exprimées avec diag2
converti par suffixe) et sortis — **écart de volumétrie assumé par doctrine** ; conversions
changeant le niveau CMA (niveau observé par code dans `ref_das_chronique` brute ; « non mesuré »
si la ref a été relue déjà convertie). Résumé (`conversion_e669_lignes_fusionnees`,
`conversion_e669_profils_entres`) dans `catalogue_longs_seuil_meta.yaml`.

**Tests.** test_helpers.R : 31 assertions (convertir_e669, repartir_proportionnel,
distribution/classes, repartir_e669_nu avec proportionnalité exacte et cascade, comptes, combo
avec tri/éclatement/totaux, distinct, compteurs, impact). test_chaines_sqlite.R : pool enrichi
(E6690, E6602, E6600, E669 nu, DP E6690), aucun ^E669 dans catalogue/refs/sorties tirage, sum(n)
conservée, fixture de fusion sous-seuil → au-dessus (GHM 88M991, DP E6690 + E6600, n = 1 + 1 > 1),
toggle FALSE dans un projet dédié (E669 présents, fusion absente), partiels bruts identiques dans
les deux cas.

### 12.1 Questions
- **Q17 — pivots courts** : le seuil `nb > SEUIL_PIVOT` est dans la chaîne v7.1.2 (en base).
  Pour un seuil après conversion, il faudrait déplacer le filtre post-collect (modification de
  chaîne, interdite). Perte conservatrice consignée ; non fait.
- **Q18 — attributs de code après conversion** : dans `ref_das_chronique` et
  `referentiel_substitution_imprecis`, `niveau` (et type_liste/caract) restent ceux du code brut
  E669x, portés par la strate ; une même clé (diag2, das, sexe, cage) peut donc apparaître avec
  deux niveaux. Sans effet sur le tirage (`prep_ref_chronique` somme par diag2/das/sexe/cage).
  Alternative : réassigner les attributs de la cible E660x observée.
- **Q19 — distribution unique** : la cascade utilise la distribution E660x de `ref_das_chronique`
  (DAS chroniques des séjours longs, AN_REF) pour toutes les surfaces, y compris diag2 et le
  catalogue multi-années. Choix de simplicité (une distribution par exécution, §2c du brief).
- **Q20 — partiels d'un run antérieur** : un `distribution_e660.parquet` absent d'un `EXPORTS_DIR`
  ancien force le recalcul de `ref_das_chronique` (résolution des besoins) : une requête base
  supplémentaire au premier lancement après ce chantier.

---

## 13. Chantier « mémoire 15 GiB »

Plateforme sécurisée limitée à 15 GiB. Doctrine : ne collecter que des agrégats, le plus tard
possible, libérer immédiatement ; le niveau séjour ne quitte JAMAIS la base (tables temporaires
autorisées, parquet interdit pour ce grain). Un run est en cours avec des partiels déjà écrits :
les correctifs ne changent pas le contenu des partiels (preuve d'équivalence P1.5), qui restent
valides et mélangeables.

### 13.1 Écart P1 — `prep_scenarios2` : top-k en base, table temporaire unique, collect minimal
Premier écart de chaîne base depuis B1-10, autorisé nominativement. La chaîne jusqu'au
`left_join(mco_diag_niveau)` inclus est **inchangée**. Avant (v7.2 l.305-322, v8 jusqu'au
commit `4d07aaf`) / après (extraction l.513-520) :

```
AVANT
    dplyr::collect() -> df_das
  df_das |> 
    dplyr::mutate(niveau = ifelse(is.na(niveau),"0",niveau)) |> 
    dplyr::mutate(nb_das = dplyr::n(),.by= dplyr::all_of(c(pivots,"das"))) -> df_das
  df_das |> 
    dplyr::arrange(ident,dplyr::desc(niveau),dplyr::desc(nb_das),das) |>
    dplyr::group_by(ident) |> 
    dplyr::slice(1:nb_assoc_das) -> df_das
  df_das |> 
    dplyr::group_by_at(c("ident",pivots)) |> 
    dplyr::arrange(das) |> 
    dplyr::summarise(diagnostic_associes = paste0(das,collapse = " "),.groups="drop") |> 
    dplyr::ungroup() |> 
    dplyr::summarise(n = dplyr::n(),.by=dplyr::all_of(c(pivots,"diagnostic_associes"))) -> df_cases

APRÈS (en base)
    dplyr::mutate(niveau = ifelse(is.na(niveau), "0", niveau)) |>
    dplyr::mutate(nb_das = dplyr::n(), .by = dplyr::all_of(c(pivots, "das"))) |>      # COUNT() OVER (PARTITION BY pivots, das)
    dplyr::group_by(ident) |>
    dbplyr::window_order(desc(niveau), desc(nb_das), das) |>                           # = arrange(ident, desc(niveau), desc(nb_das), das)
    dplyr::filter(dplyr::row_number() <= nb_assoc_das) |>                             # = slice(1:k) ; ROW_NUMBER() OVER
    dplyr::ungroup() |>
    dplyr::select(dplyr::all_of(c("ident", pivots, "das"))) |>
    dplyr::compute("prep_topk_tmp", temporary = TRUE, overwrite = TRUE)
APRÈS (en R, extraction l.526-545) : collect depuis prep_topk_tmp — par morceaux de cage
  (COLLECT_PAR_MORCEAUX, config l.107, défaut TRUE : un collect par modalité, filtre en lecture
  seule sur la table figée, chaque morceau collapsé puis libéré, df_cases partiels concaténés) ou
  collect unique (FALSE) — puis collapse_graine() (helpers l.846-872) : k = 2 vectorisé
  (arrange(ident, das), !duplicated, match, paste), sinon repli générique summarise + paste0.
```
- Les fenêtres (`COUNT OVER`, `ROW_NUMBER OVER`) sont celles déjà validées par `prep_data` sur la
  base de production. `desc` non namespacé dans `window_order` : dbplyr 2.5 échoue à traduire
  `dplyr::desc(...)` (test SQLite) ; `desc` est la forme documentée.
- `prep_topk_tmp` : UN SEUL nom, écrasé à chaque itération (`overwrite = TRUE`), jamais
  d'empilement ; temporaire de session, pas de DROP nécessaire ; jamais de parquet pour ce grain.
- Signature : `+ collect_par_morceaux = TRUE, noter = NULL` (fonction d'instrumentation optionnelle).
- Collapse par séjour sain en mode morceaux : un ident a une seule cage (cage est un pivot), le
  morcelage ne coupe jamais un séjour (assertion « invariant morceaux » du test SQLite).
- `gc()` après chaque morceau et chaque itération ; intermédiaires libérés sitôt df_cases construit.
- **Preuve d'équivalence (test_chaines_sqlite.R)** : ancienne version conservée en fonction privée
  datée `prep_scenarios2_ancien_20260912` ; sur les fixtures, `nouvelle == ancienne` (mêmes lignes,
  ordre indifférent) pour (CHR/U, 26, k=2), (CH, 17, k=2), (CHR/U, 26, k=3), morceaux TRUE et FALSE,
  et `partiel écrit par le run == ancienne`. Les partiels antérieurs au chantier restent valides.

### 13.2 P2 — boucle sans accumulateur, recouvrement, catalogue final en deux étages
- `construire_catalogue_longs` (extraction l.696-727) ne maintient plus de `df_cases` cumulé :
  calcule/relit, écrit, imprime les stats DU PARTIEL SEUL (`apports_partiel`, helpers l.550 :
  nb_lignes_partiel, sum_n_partiel = séjours éligibles, nb_diag2_partiel, nb_diag2_nouveaux —
  seul cumul conservé : le set des diag2 vus), libère. `diagnostic_apports.csv` perd les colonnes
  `nb_lignes_cumul` / `nb_diag2_cumul` (sans sens sans accumulateur). `apports_iteration` supprimé.
- **Recouvrement** (`PAIRES_RECOUVREMENT`, config l.111, défaut `list(c("CHR/U", 24, 25))` ;
  `mesurer_recouvrement` extraction l.730-749 ; `recouvrement_partiels` helpers l.959) : pour
  chaque paire (etbs, anA, anB) dont les deux partiels existent, relecture des DEUX partiels
  seulement : combinaisons (nb_A, nb_B, nb_communes, part des combinaisons de B déjà vues en A,
  part des séjours de B), même chose au niveau pivots, diag2 nouveaux, séjours uniques nouveaux ;
  `recouvrement.csv` + impression lisible ; partiel manquant -> ligne « non calculable ».
- **Catalogue final** (extraction l.752-801), construit UNE FOIS après la boucle, hors RAM R :
  Étage 1 : `agreger_partiels(fichiers, PIVOTS_LONGS_SEUIL, "n")` -> conversion E669 des comptes
  pivots -> seuil > SEUIL_PIVOT -> pivots retenus (convertis). Étage 2 : `cles_brutes_retenues`
  (helpers l.917 : identité ; E669 suffixé -> sa cible ; E669 nu -> retenu si AU MOINS une cible
  de la cascade est retenue) -> `agreger_partiels(..., filtre_cles = clés brutes)` (semi-jointure)
  -> pipeline existant `convertir_e669_comptes` + `convertir_e669_combo` -> ré-agrégation ->
  **seuil re-appliqué exactement** (l'étage 2 sur-matérialise les autres cibles des E669 nus ;
  le seuil final fait foi ; ordre conversion -> ré-agrégation -> seuil et fusions sous-seuil
  préservés). Preuve : test SQLite « catalogue deux étages == ancien flux » (conversion TRUE avec
  la fixture de fusion E669, et FALSE).
- `agreger_partiels` (helpers l.902) : (a) `arrow::open_dataset` + dplyr (production, mémoire
  bornée par arrow ; repli automatique sur (b) en cas d'échec avec avertissement) ; (b) pur R
  incrémental, un partiel à la fois, fusion successive (référence sémantique ; utilisé quand arrow
  est le mock des tests). Sélection auto : (a) si `open_dataset` est exporté par arrow. Tests :
  (b) == bind_rows + summarise global, avec et sans filtre ; (a) == (b) quand arrow est réel.
- Mesure d'impact E669 recalculée sur les tables PIVOT des deux étages (`impact_conversion_pivots`,
  helpers l.944 ; `effectif_e669_combos` sur l'étage 2 borné) ; plus jamais de copie `avant` du
  catalogue complet. `df_prep_scenarios` (brut intégral) disparaît du flux.

### 13.3 P3 — libération et instrumentation
- Boucle des refs : `rm(df_ref); gc()` après chaque écriture (déjà) + mesure mémoire ; aucune ref
  n'est liée à une variable globale après sa fabrique (test « P3 : aucun objet ref ni cache brut
  vivant »).
- `CACHE_E669` : `brute` purgé dans la fabrique `distribution_e660` sitôt la distribution et
  l'impact niveau CMA calculés (l'impact, petit, est conservé dans le cache) ; purge de sécurité
  après la boucle des refs (l.689).
- `mesurer_memoire` (helpers l.978) : étiquette, horodatage, taille de l'objet (Mo), mémoire
  utilisée et pic gc() depuis la mesure précédente (Go, `gc(reset = TRUE)`, dernière colonne
  « max used (Mb) » — la colonne « limit » présente sur certains R décale les indices), alerte si
  pic > SEUIL_ALERTE_GO (config l.108, défaut 10) : avertissement visible + colonne `alerte`.
  Journal accumulé dans `MEMOIRE_ENV` (extraction l.570-574, `noter_memoire`, pas de `<<-`) après
  chaque morceau, partiel, ref, étage du catalogue ; exporté en `diagnostic_memoire.csv` et repris
  en section 6 du rapport d'extraction. Livrable de calibration de la passe diagnostic.
- `v_admin_longs` : inchangé sur le fond (grain en arbitrage), écrit puis libéré, taille mesurée.

### 13.4 Questions
- **Q21 — dialecte `window_order`** : validé sur SQLite ; en production, `ROW_NUMBER() OVER
  (PARTITION BY ident ORDER BY ...)` et `COUNT(*) OVER (PARTITION BY ...)` sont ceux de
  `prep_data`. Si la base refuse `desc` sur `niveau` (type texte), le tri reste celui de
  l'ancienne version R (desc sur caractère).
- **Q22 — NULLs dans `ORDER BY das`** : un séjour sans DAS a une seule ligne (das NULL) : l'ordre
  des NULL n'a pas d'incidence ; la graine vaut "NA" comme avant (paste). Conservé tel quel.
- **Q23 — morcelage par cage** : 12 morceaux par itération ; si un morceau reste trop gros, la
  clé de morcelage pourrait être (cage, sexe) — même invariant (un ident, une strate).
- **Q24 — `PAIRES_RECOUVREMENT`** par défaut (CHR/U 24 -> 25) : à étendre selon les partiels
  disponibles ; le recouvrement pivots ignore `nbda` ? Non : il utilise `PIVOTS_LONGS` complets
  (nbda inclus), comme les combinaisons.

---

## 14. Chantier « orchestration par étapes »

Pur ré-enrobage : mêmes calculs, mêmes fichiers, mêmes noms d'exports ; aucune chaîne base
modifiée (diffs vides, commandes en 14.3). Les deux scripts d'entrée n'appellent plus que des
étapes ; `RUN.Rmd` appelle une étape par chunk.

### 14.1 Nouveau fichier `etapes_v8.R` — mapping ancien -> nouveau

| Ancien (extraction c2a7e0a / tirage c2a7e0a) | Nouveau (`etapes_v8.R`) | Contenu |
|---|---|---|
| extraction section 2 (`prep_data`, l.61-348) | l.23-310, bloc déplacé | diff vide |
| extraction section 3 (refs 3a-3e, l.350-475) | l.312-437, bloc déplacé | diff vide |
| extraction section 4 (`prep_scenarios2`, l.477-547) | l.439-509, bloc déplacé | diff vide (ligne vide finale près) |
| extraction 5b : `CACHE_E669`, `MEMOIRE_ENV`, `noter_memoire`, `purger_cache_e669`, `charger_dist_e660`, `ref_das_chronique_brute`, `codes_imprecis_extraction`, fabriques pivots/v_admin, `FABRIQUES_REFS` (l.567-663) | section 0b, l.511-623, bloc déplacé | diff vide |
| extraction 5a (prep_data des années du plan, garde-fou partiels_meta, plan) | `etape_prep_data(ans = NULL)` l.672 | plan stocké dans `ETAPES_ENV$plan` |
| extraction 5b (boucle des refs) | `etape_refs(forcer = FORCER_REFS)` l.699 | boucle déplacée ; prérequis `prep_data_<AN_REF>` (crée `prep_das_chro` si une ref chronique manque) |
| extraction 5c (`construire_catalogue_longs`) + 5d (`mesurer_recouvrement`) | `etape_partiels_longs(iterations = NULL)` l.729 + `mesurer_recouvrement` l.772 | sans agrégation finale |
| extraction section 6 (catalogue deux étages, meta, rapport d'extraction) | `etape_catalogue(ans, etbs)` l.796 | périmètre en ARGUMENT, tracé dans le meta (`ANS_HISTORIQUE`/`TYPES_ETBS_LONGS` = valeurs passées, + `perimetre_ans`/`perimetre_etbs`) ; ne touche pas la base |
| tirage section 1 (lecture des produits, REFS, codes imprécis, libellés) | `charger_contexte_tirage(requis, etape)` l.909 | chargé une fois par session (`ETAPES_ENV$ctx`), prérequis par étape |
| tirage section 2 (courts) | `etape_tirage_courts()` l.948 | bannière « AN_REF uniquement » |
| tirage 3a (sélection figée, meta_tirage) | `etape_selection_longs(budget, mode)` l.985 + `relire_selection_longs` l.1042 | reprise de session : sélection relue depuis les fichiers |
| tirage 3b (pmap_chunks longs) | `etape_tirage_das_longs()` l.1062 | assemblé en mémoire de session |
| tirage 3c (habillage) | `etape_habillage_longs()` l.1082 | jointure `v_admin_longs.parquet` relu (frontière tirage / base réaffirmée : jamais `prep_data` en direct) |
| tirage 3c export + section 4 (livrables, rapport) | `etape_finalisation()` l.1108 | reconstruit ce qui manque en session (chunks via habillage, stats des courts depuis leur parquet) |
| (nouveau) | `etat_pipeline()` l.1195 | FAIT / PARTIEL / À FAIRE avec preuves, fichiers seulement |

Infrastructure (l.624-665) : `ETAPES_ENV` (état de session : plan, impact_e669, rapport, revue,
sélection, df longs), `banniere_debut/fin` (durée, fichiers produits), `exiger_conn`,
`exiger_fichiers` (« lancez etape_X d'abord »), `table_temporaire_existe` / `exiger_table`,
`plan_courant`, `fmt_df`, `chemin_export`. Signatures : voir tableau de RUN.md.

### 14.2 Scripts d'entrée et notebook
- `extraction_associations_codes_v8.R` (46 lignes) : bootstrap puis `etape_prep_data(); etape_refs();
  etape_partiels_longs(); etape_catalogue()`. `tirage_scenarios_v8.R` (32 lignes) : bootstrap puis
  `etape_tirage_courts(); etape_selection_longs(); etape_tirage_das_longs(); etape_habillage_longs();
  etape_finalisation()`. Variable d'environnement `SCENARIOS_PMSI_ETAPES_SEULEMENT=1` : charge la
  session (config, sources, connexion pour l'extraction) sans exécuter d'étape — utilisée par
  RUN.Rmd (chunk « session ») et les tests.
- Ordre des seeds inchangé : chaque tirage est précédé de son `set.seed` explicite (`SEED + i` par
  chunk, `SEED + 1e6 … 5e6`), donc l'ordre d'appel des étapes est sans effet sur les résultats.
- `RUN.md` réécrit autour des étapes (tableau nom / produit / quand relancer / cache ; séquence
  utilisateur 1 prep_data, 2 courts, 3 longs avec DÉCISION de périmètre -> `etape_catalogue(ans=, etbs=)`) ;
  `RUN.Rmd` synchronisé (un chunk par étape, chunk `etat_pipeline()` réutilisable, chunk de décision
  avec `ANS_CHOISIES` / `ETBS_CHOISIS` en clair, protections `eval = FALSE` et `JE_CONFIRME` conservées).

### 14.3 Aucune chaîne base touchée — commandes de vérification
```
OLD=tests/ancien_20260914/extraction_associations_codes_v8.R ; NEW=etapes_v8.R
diff <(awk '/^## ---- 2\. prep_data/{f=1} /^## ---- 3\. Tables/{f=0} f' $OLD) <(awk '/^## ---- 2\. prep_data/{f=1} /^## ---- 3\. Tables/{f=0} f' $NEW)
diff <(awk '/^## ---- 3\. Tables/{f=1} /^## ---- 4\. prep_scenarios2/{f=0} f' $OLD) <(awk '/^## ---- 3\. Tables/{f=1} /^## ---- 4\. prep_scenarios2/{f=0} f' $NEW)
diff <(awk '/^## ---- 4\. prep_scenarios2/{f=1} /^## ---- 5\. Exécution/{f=0} f' $OLD) <(awk '/^## ---- 4\. prep_scenarios2/{f=1} /^## ---- 0b\. Infrastructure/{f=0} f' $NEW | sed '$d')
diff <(awk '/^CACHE_E669 <- new\.env\(\)/{f=1} /^stopifnot\(setequal/{f=0} f' $OLD) <(awk '/^CACHE_E669 <- new\.env\(\)/{f=1} /^stopifnot\(setequal/{f=0} f' $NEW)
```
Tous vides à la livraison. Les instantanés `tests/ancien_20260914/` (deux scripts d'entrée avant
le chantier) servent de référence d'identité aux tests.

### 14.4 Tests
- test_chaines_sqlite.R (96 assertions) : (a) bout-en-bout par les scripts d'entrée == résultats
  antérieurs (assertions existantes inchangées ; état de session lu dans `ETAPES_ENV`) ET identité
  bit à bit avec les anciens scripts d'entrée (catalogue, 10 refs, sélection, scenarios_courts,
  scenarios_longs_tirage, echantillon_revue, top30) ; (b) étape par étape avec déconnexion /
  reconnexion entre `etape_refs` et `etape_partiels_longs`, puis tirage par étapes en sessions
  séparées (sélection et chunks relus) == bout-en-bout ; (c) `etape_catalogue(ans = c(17, 26),
  etbs = "CHR/U")` : catalogue restreint == ancien flux sur ces partiels, méta cohérent ; (d) hors
  ordre : `etape_refs()` sans prep_data, `etape_tirage_das_longs()` sans sélection,
  `etape_partiels_longs()` après reconnexion sans prep_data -> erreurs actionnables ;
  (e) `etat_pipeline()` avant / après chaque phase, y compris sans connexion.
- test_helpers.R (207) : inchangé sauf le chemin arrow d'`agreger_partiels`, désormais testé
  SANS repli (voir 14.5).

### 14.5 Correctif révélé par le chantier
`agreger_partiels_arrow` échouait silencieusement (avertissement puis repli incrémental) : arrow
ne traduit ni le pronom `.data[[col]]` ni `across(all_of(cols))`. Remplacés par `!!dplyr::sym(col)`
et `!!!dplyr::syms(cols)` ; le test du chemin (a) exige maintenant l'absence de repli. Sans ce
correctif, la production aurait utilisé le chemin incrémental (résultats identiques, mémoire non
bornée par arrow).

### 14.6 Questions
- **Q25 — `etape_refs` et `prep_das_chronique`** : si une ref chronique est à calculer et que la
  table `prep_das_chro_<AN_REF>` manque, l'étape la crée elle-même (au lieu d'échouer), pour rester
  idempotente en session neuve ; `prep_data_<AN_REF>` reste un prérequis strict.
- **Q26 — bannière « AN_REF uniquement » des courts** : les pivots courts sont extraits sur
  l'année de référence (chaîne v7.1.2) ; une extension multi-années des courts changerait la
  chaîne (hors périmètre).
- **Q27 — `etape_catalogue` sans plan de session** : appelée seule (session neuve), le meta porte
  `plan_* = NA` (le plan n'est connu que d'`etape_prep_data`) ; le périmètre passé reste tracé.
- **Q28 — `etat_pipeline` et tables temporaires** : avec une connexion ouverte, teste
  `prep_data_<AN_REF>` / `prep_das_chro_<AN_REF>` par `atihble()` dans un `tryCatch` ; « inconnu
  hors connexion » sinon.

---

## 15. Chantier « chunking dynamique »

**Motivation.** 390 chunks constatés sur les séjours courts en diagnostic avec `CHUNK_SIZE = 200` ;
en production, les longs en produiraient des milliers. Cible : au plus `NB_CHUNKS_MAX` runs par
tirage. Aucune chaîne base concernée : helpers, config, points d'appel, documentation.

**Config (config_v8.R).** `CHUNK_SIZE` supprimé (des deux blocs profil : le calcul s'adapte au
volume) ; remplacé par `NB_CHUNKS_MAX = 50L` (borne haute du nombre de chunks), `CHUNK_SIZE_MIN = 500L`
(plancher), `CHUNK_SIZE_FIXE = NA_integer_` (surcharge manuelle qui court-circuite le calcul). Les
trois entrent dans `NOMS_CONFIG_META`, dans `meta_tirage.yaml` et dans ses clés de garde-fou
(`CLES_META_TIRAGE`, à la place de `CHUNK_SIZE`).

**Formule (helpers `taille_chunk`, pure, testée).** `CHUNK_SIZE_FIXE` si non-NA, sinon
`max(CHUNK_SIZE_MIN, ceiling(n / NB_CHUNKS_MAX))` : jamais plus de `NB_CHUNKS_MAX` chunks, jamais de
chunks minuscules, n petit -> un seul chunk. `pmap_chunks(chunk_size = NULL)` (nouveau défaut) calcule
`taille_chunk(nrow(df))` et imprime n, chunk_size retenu et nombre de chunks. `etape_tirage_courts`
et `etape_tirage_das_longs` passent `chunk_size = NULL` ; `etat_pipeline` calcule les chunks attendus
avec la même formule.

**Garde-fou de reprise (sidecar).** `pmap_chunks` écrit `<dossier>/<prefixe>_chunks_meta.yaml`
(n, chunk_size, seed_base, nb_chunks, date) AVANT le premier chunk (un sidecar par préfixe : les
branches courts et longs partagent `CHUNKS_DIR`). À la reprise (chunks du préfixe présents), si n,
chunk_size ou seed_base diffèrent -> `stop()` explicite « découpage incompatible avec les chunks
existants ; videz <dossier> ou restaurez les paramètres : attendu … ; reçu … ». Pourquoi : les index
de chunks désignent des PLAGES DE LIGNES de df ; reprendre sur un autre découpage ferait sauter des
lignes ou en tirerait deux fois, sans erreur (corruption silencieuse). Chunks présents sans sidecar
(dossiers antérieurs au chantier) -> même stop, en l'expliquant (`verifier_chunks_meta`).

**Migration.** Les dossiers de chunks existants (sans sidecar) doivent être vidés ; le stop l'explique.
`meta_tirage.yaml` antérieurs : la clé `CHUNK_SIZE` n'est plus vérifiée, les nouvelles clés le sont ->
un `meta_tirage.yaml` antérieur diffère (`NB_CHUNKS_MAX` absent) et déclenche le garde-fou existant :
vider chunks + sélection + méta du tirage avant de rejouer.

**Tests.** test_helpers.R (220 assertions, 217 sans arrow) : `taille_chunk` (n petit -> 1 chunk, plancher actif,
n grand -> exactement NB_CHUNKS_MAX chunks, `CHUNK_SIZE_FIXE` prioritaire, défauts config) ;
`pmap_chunks` auto (<= NB_CHUNKS_MAX), sidecar écrit avant le premier chunk (présent même si le
chunk 1 échoue) et complet, reprise mêmes paramètres == run complet bit à bit, n / chunk_size /
seed_base modifiés -> stop, chunks sans sidecar -> stop, deux préfixes dans un dossier -> sidecars
distincts, déterminisme inchangé. test_chaines_sqlite.R (97) : fixtures petites -> cas multi-chunks
forcés par `CHUNK_SIZE_FIXE <- 40L` en surcharge (reprise et garde-fou réellement exercés) ;
assertions de résultat inchangées ; sidecars des deux branches présents et cohérents ; identité
avec les anciens scripts conservée (`CHUNK_SIZE <- 40L` ajouté à la surcharge de test pour eux seuls).

### 15.1 Questions
- **Q29 — un sidecar par préfixe** : le brief nomme `chunks_meta.yaml` ; les deux branches
  partageant `CHUNKS_DIR`, le sidecar est `courts_chunks_meta.yaml` / `longs_chunks_meta.yaml`.
- **Q30 — `garder_chunks = FALSE`** : les chunks sont supprimés à la fin mais le sidecar reste ;
  sans chunk présent, il est simplement réécrit au run suivant (pas de garde-fou déclenché).
- **Q31 — `CHUNK_SIZE_MIN = 500` en diagnostic** : les courts (~78 000 pivots) donnent 50 chunks
  de ~1 560 ; le tirage longs de 1 000 lignes donne 2 chunks de 500 (au lieu de 5 de 200).

---

## 16. Chantier « aval production »

Contexte : le catalogue longs de production existe (21 607 117 lignes, `catalogue_longs_seuil.parquet`,
produit en dépassement RAM toléré) et ne doit **jamais** être reconstruit sur ce périmètre. Goulots
aval : RAM de tout ce qui lit le catalogue, temps du tirage. Production par campagnes itératives ;
`NB_CRH_CIBLE` est un ordre de grandeur. Doctrine : représentativité des DIAGNOSTICS (DP) avant celle
des situations cliniques, la diversité des contextes se reconstituant ENTRE les campagnes. Aucune
chaîne base concernée : tout est post-parquets (helpers section E, étapes, config, doc).

### 16.1 Typologie DPEC / TPEC
- `referentiels/typologie_sejours.yaml` (version `2026-09-16-b`) : listes et libellés DPEC / TPEC recopiés
  TELS QUELS du code STREAM `with_typologie` (fourni après la première livraison ; la version `-a`,
  incomplète, est remplacée), y compris le chevauchement BB_MED / BB_CHIR sur 15M10/11/13/14
  (inoffensif : l'ordre prime) et le commentaire « critère à confirmer (CMD 22) ». `typologie_sejour()`
  (helpers) est une traduction fidèle de l'ordre des `.when()` ; `racine` = substr(ghm2, 1, 5) (identique à la
  colonne de prep_data) ; `col_age` accepte l'âge numérique (`agean >= 18`) ou la classe `ge_18`/`lt_18`
  (pivot age des longs) ; `duree_defaut = 3` pour les longs (périmètre 3-100 : classes < 3 nuits / HDJ /
  séances inaccessibles, attendu) ; les courts seront typés avec leur vraie durée. Vérifications contre le
  STREAM : un cas par classe, précédences (15M10, 14Z13T, 14Z13A, 14Z10 IMG_FC avant ACC_PATHO, Z511 âge 15,
  CMD 28 avant M/Z), défaut TPEC « Autre ».
### 16.2 Repartitionnement one-shot du catalogue
`etape_repartitionner_catalogue()` : lecture par morceaux de lettres (jamais tout en RAM), ajout de
`lettre`, `DPEC`, `TPEC`, écriture `EXPORTS_DIR/catalogue_longs_seuil/part_<L>.parquet` + `_sidecar.yaml`
(nb lignes par part, sum(poids), effectifs DPEC, version de typologie, date, provenance), vérification
nb_lignes == méta, monofichier renommé `.ancien` (jamais supprimé). Idempotente ; version de typologie
différente du sidecar → stop proposant de re-repartitionner. Lecteur unique `lire_catalogue(lettres,
colonnes)` (dataset arrow filtré ; mock : rbind des parts ; monofichier : message de dépréciation, schéma
inchangé) ; `lettres_catalogue()`. Toutes les lectures (sélection, tirage, `etat_pipeline`) passent par lui.
### 16.3 Sélection `quota_dp_fixe` (production) — `catalogue_complet` retiré
Config : `NB_CRH_CIBLE` (remplace `BUDGET_TOTAL_LONGS`, alias de compatibilité conservé pour les anciens
scripts / surcharges), `NB_LIGNES_PAR_DP = 1`, `POPULATIONS` (partition exacte des cages, vérifiée),
`PLAFONDS_DPEC`, `MODE_SELECTION = "quota_dp_fixe"` en production (`quota_dp` conservé pour le diagnostic).
Politique, par population (budget au prorata du nb de DP, `repartir_budget_populations`) : X =
ceiling(budget_pop / nb_dp) ; par DP, plafond par (DP × DPEC plafonné) — X_dp = min(X, plafond), le reste
du DP suit X ; k lignes DISTINCTES au poids sans remise (`choisir_lignes_dp`) ; n_var = ceiling(X_dp / k)
variantes par ligne, dernière tronquée pour totaliser X_dp (`variantes_par_ligne`) ; planchers par type
d'unité seulement si k >= nb de types (sinon désactivés, comptés au méta / rapport). Exécution lettre par
lettre (pic RAM = une lettre), seed stable `SEED + 7e6 + 1e4 × index(population) + utf8ToInt(lettre)`.
Sorties `selection_longs/<population>/part_<L>.parquet`, `selection_longs_effectifs.csv`,
`selection_longs_stats_dp.csv`, `meta_tirage.yaml` par population + global (garde-fou sur MODE, NB_CRH_CIBLE,
k, chunking, SEED, version typologie). Manque à gagner (sélection) = Σ max(0, X_dp − lignes disponibles)
(slots servis par des variantes de lignes déjà utilisées : risque de doublons) ; 30 DP les plus pauvres au
rapport. `catalogue_complet` : stop() si budget < nb lignes du catalogue, renvoyant vers quota_dp_fixe.
### 16.4 Tirage : index, unicité souple, plages, atomicité, populations
- `indexer_ref_das` (split par la clé exacte du filtre de `sample_das_long`) construit une fois à l'entrée
  d'`etape_tirage_das_longs` ; `sample_das_long` accepte l'index (accès direct) ; identité avec le filtre
  prouvée sous seed. Idem `indexer_ref_chronique` / `candidats_chroniques` pour les courts (identité prouvée ;
  non branché dans `etape_tirage_courts`, Q34). Débit imprimé par chunk.
- Unicité souple : `sample_das_long(dedoublonner = TRUE)` — les n_var variantes d'une ligne sont tirées
  groupées puis dédoublonnées sur le jeu complet de DAS (ordre indifférent), AUCUN re-tirage, colonne
  `nb_variantes_demandees` ; doublons éliminés chiffrés par DP au rapport ; le réalisé peut être < cible.
- `pmap_chunks(chunk_range = c(i, j), assembler)` : plages disjointes pour des sessions parallèles sur le
  même dossier (sidecar partagé, vérifié, pas réécrit s'il concorde) ; écriture atomique `.tmp` +
  `file.rename` (un `.tmp` orphelin est ignoré et recalculé) ; le découpage est par LIGNES de sélection,
  une ligne et ses variantes vivent dans le même chunk. Mode fixe : chunks par population
  (`chunks/<population>/`, seed `SEED + 1e5 + 1e4 × index(population)`), rien d'assemblé en RAM.
- `etape_habillage_longs` (fixe) : relecture des chunks par lots (`lire_chunks_par_lots`,
  `LOT_CHUNKS_FINALISATION`), jointure `v_admin_longs.parquet`, slice_sample par lot (seed dérivé), DPEC/TPEC
  recalculés (duree = 3, comme le catalogue), lots `habille/<population>/`. Complétude des chunks exigée.
- `etape_finalisation` (fixe) : relecture des lots en flux, contrôles §8.2 et statistiques agrégés par lot
  (`acc_stats_*`, égalité avec les statistiques globales prouvée), export
  `scenarios_longs_tirage_v8_<date>/<population>/part_*.parquet` + monofichier fusionné si volume ≤
  `SEUIL_EXPORT_MONOFICHIER` ou `fusionner = TRUE` ; rapport : réalisé vs cible par population, lignes
  plafonnées par DPEC, 30 DP au plus fort manque à gagner, doublons éliminés par DP ; DPEC/TPEC jusqu'aux
  sorties finales. Modes historiques (quota_dp, catalogue_complet autorisé) : flux inchangé.
### 16.4b Notebook dédié
`RUN_aval.Rmd` : notebook de la phase aval (session sans base, paramètres effectifs, repartitionnement,
sélection, palier 100 k avec extrapolation du débit, campagne parallèle par plages, habillage, finalisation,
lecture du corpus en flux, mémo de cache) ; `RUN.Rmd` reste le notebook de l'amont et le référence.

### 16.5 Hygiène mémoire
`memoire_session()` (objets par taille dans globalenv / ETAPES_ENV / CACHE_E669, puis gc()) ; discipline
« Restart R avant chaque étape lourde » dans RUN.md / RUN.Rmd.
### 16.6 Tests
test_helpers.R : typologie (un cas par classe, ordres 15M10 / 14Z13T / 14Z13A / Z511 âge 15 / CMD 28 avant
M-Z, défaut TPEC, classe d'âge + durée constante), `lire_catalogue` (parts, colonnes, monofichier déprécié,
dataset arrow si réel), sélection (partition, prorata, `variantes_par_ligne`, k = 1, plafond 14Z13A vs
14Z13B, sans remise, planchers désactivés, manque à gagner, k = 3 avec planchers actifs, DP pauvre,
déterminisme, seeds), `dedoublonner_variantes`, index (long et court : identité), unicité souple (strate
riche / pauvre, déterminisme), `pmap_chunks` (variantes dans le même chunk, plages disjointes == complet,
assemblage refusé si chunk manquant, sidecar partagé, atomicité), `lire_chunks_par_lots`, `acc_stats`,
`memoire_session`. test_chaines_sqlite.R : projet de production (quota_dp_fixe, `CHUNK_SIZE_FIXE` petit,
lots de 2) : repartitionnement (recomposition == monofichier + colonnes, sidecar, idempotence, garde-fou
version), catalogue_complet refusé, sélection (prorata, volume annoncé, k = 1, sans remise, idempotence),
tirage par plages puis complet == bit à bit, unicité, chunks par population, habillage et finalisation en
flux (== statistiques globales), rapport, `etat_pipeline`, revue par population. Identité avec les anciens
scripts conservée (mode quota_dp).
### 16.7 Questions
- **Q32 — listes STREAM (RÉSOLUE)** : le code STREAM a été fourni après la première livraison ; listes et
  libellés recopiés tels quels (version `2026-09-16-b`). Un catalogue repartitionné avec la version `-a`
  doit être re-repartitionné (le garde-fou de version l'impose).
- **Q33 — plafond par (DP × DPEC plafonné)** : interprété comme un groupe séparé du DP avec X_dp =
  min(X, plafond), le reste du DP (DPEC non plafonnés) gardant X ; un DP mixte peut donc porter jusqu'à
  X + min(X, plafond).
- **Q34 — index des courts** : `indexer_ref_chronique` est disponible et prouvé mais `etape_tirage_courts`
  garde le filtre (identité bit à bit avec les anciens scripts conservée par le test) ; à brancher si le
  temps des courts devient un goulot.
- **Q35 — manque à gagner** : défini à la sélection comme Σ max(0, X_dp − lignes disponibles) ; le vrai
  écart réalisé est le compteur de doublons éliminés (finalisation).
- **Q36 — revue longs** : un échantillon par population (`longs_<population>`), tiré par lots puis
  ré-échantillonné (25 par population).

---

## 17. Lot « finitions exploitation » (premières exécutions réelles en espace sécurisé)

Aucune chaîne base, aucune logique de calcul modifiée.

| Item | Statut | Où |
|---|---|---|
| 1. Chunk de MIGRATION inter-profils en tête de RUN_aval.Rmd (après le setup, avant le repartitionnement) : présence du catalogue dans `EXPORTS_DIR`, sinon `localiser_catalogue()` dans les autres `exports*/` du même `PATH_RESULTS`, copie (catalogue + méta + 10 refs) derrière `JE_CONFIRME_COPIE`, condition Q13 affichée (`condition_q13`) — refs non copiées si Q13 non satisfaite ; chemins absolus uniquement | fait | RUN_aval.Rmd chunk `migration_catalogue` ; helpers section F (`localiser_catalogue`, `condition_q13`, `fichiers_migration_catalogue`, `CLES_Q13`) |
| 2. Message à trois branches quand le catalogue est absent (chemin effectif cherché ; copie inter-profils avec les dossiers où il a été trouvé et la condition Q13 ; sinon `etape_catalogue()` coûteux) | fait | `message_catalogue_absent()` (helpers F), utilisé par `etape_repartitionner_catalogue`, `charger_contexte_tirage` (toute étape aval exigeant le catalogue) et `lire_catalogue` |
| 3. En-tête de RUN_aval.Rmd : session sans connexion base par conception, extraction dans RUN.Rmd, chemins absolus | fait | RUN_aval.Rmd |
| 4a. `diagnostic_memoire.csv` écrit en fin d'`etape_refs` ET d'`etape_partiels_longs` (mêmes lignes qu'`etape_catalogue`, idempotent : `ecrire_diagnostic_memoire()`), garde `file.exists()` dans RUN.Rmd, listé par `etat_pipeline()` | fait (non traité antérieurement : le lot « finitions avant espace sécurisé », section 11, ne le couvrait pas) | etapes_v8.R, RUN.Rmd chunk `apports`, `etat_pipeline` (preuve de la ligne partiels) |
| 4b. Bannière d'`etape_selection_longs` : phrase explicite quand X × nb_DP > budget (« minimum X par DP … volume final = … ») et quand les planchers d'unités sont inactifs au quota courant (k < 2) | fait | etapes_v8.R (mode quota_dp_fixe) |
| 4c. RUN.md + RUN.Rmd : documentation de chaque valeur des lignes de log de la sélection ; chunk `couverture_dp` (DP distincts des partiels agrégés via `nom_partiel` / `agreger_partiels`, chemins absolus, conversion E669 appliquée avant comparaison, vs catalogue, DP perdus triés par effectif) | fait | RUN.md (séquence production 1b, sélection), RUN.Rmd chunk `couverture_dp` |
| 5. Tests : message à trois branches (contenu, chemin effectif, dossier trouvé) ; helpers de migration purs (`localiser_catalogue` avec dossier courant exclu et dataset partitionné, `condition_q13` satisfaite / non satisfaite, `fichiers_migration_catalogue`) ; `diagnostic_memoire.csv` après `etape_refs` et `etape_partiels_longs` + `etat_pipeline` (projet SQLite dédié) | fait | test_helpers.R (10 assertions), test_chaines_sqlite.R (1 assertion composite) |

Tests : 278 (helpers, 275 sans arrow) + 119 (SQLite) assertions vertes.
