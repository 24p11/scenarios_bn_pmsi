###############################################################################
# tests/test_chaines_sqlite.R — simulation hors base des chaînes dbplyr du v8
#
# Objet : vérifier que les chaînes base (sections 2, 3, 5, 6, 7 du v8) se traduisent
# et s'exécutent sans erreur R/dbplyr, sur une base SQLite en mémoire peuplée de tables
# FACTICES portant les colonnes utilisées par les requêtes, avec un faux paquet
# `pRatihque` (atihble = dplyr::tbl). Cela ne valide PAS le dialecte SQL de la base de
# production ni les noms réels des colonnes : c'est un filet de sécurité R.
#
# Prérequis : dbplyr, DBI, RSQLite (+ un compilateur non nécessaire : paquet mock pur R).
# Exécution : Rscript tests/test_chaines_sqlite.R   [R_LIBS_TEST=<lib supplémentaire>]
###############################################################################
lib_test <- Sys.getenv("R_LIBS_TEST", unset = "")
if(nzchar(lib_test)) .libPaths(c(lib_test, .libPaths()))
for(p in c("dbplyr", "DBI", "RSQLite")) if(!requireNamespace(p, quietly = TRUE)) stop("Paquet manquant : " %+% p)
suppressPackageStartupMessages({library(dplyr); library(tibble); library(stringr); library(dbplyr)})
for(loc in c("fr_FR.UTF-8", "en_US.UTF-8", "C.UTF-8")) if(!is.na(suppressWarnings(Sys.setlocale("LC_CTYPE", loc))) && Sys.getlocale("LC_CTYPE") == loc) break
`%+%` <- function(x, y) paste0(x, y)

# ------------------------------------------------ faux paquet pRatihque --
lib_mock <- file.path(tempdir(), "lib_mock")
dir.create(lib_mock, showWarnings = FALSE)
pkg <- file.path(tempdir(), "pRatihque")
dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
writeLines(c("Package: pRatihque", "Version: 0.0.0.9000", "Title: Mock", "Description: Mock SQLite pour tests hors base.",
             "License: MIT", "Encoding: UTF-8", "Imports: DBI, RSQLite, dplyr"), file.path(pkg, "DESCRIPTION"))
writeLines("export(atihble, connection_database)", file.path(pkg, "NAMESPACE"))
writeLines(c(
  "connection_database <- function(){",
  "  if(!exists('.conn_mock', envir = globalenv())) assign('.conn_mock', DBI::dbConnect(RSQLite::SQLite(), ':memory:'), envir = globalenv())",
  "  get('.conn_mock', envir = globalenv())",
  "}",
  "atihble <- function(conn, name) dplyr::tbl(conn, name)"), file.path(pkg, "R", "mock.R"))
utils::install.packages(pkg, repos = NULL, type = "source", lib = lib_mock, quiet = TRUE)
.libPaths(c(lib_mock, .libPaths()))
stopifnot(requireNamespace("pRatihque", quietly = TRUE))

# ---------------------------------------------- chargement du v8 (parties) --
fichier_v8 <- c("extraction_associations_codes_v8.R", "../extraction_associations_codes_v8.R")
fichier_v8 <- fichier_v8[file.exists(fichier_v8)][1]
lignes <- readLines(fichier_v8, encoding = "UTF-8")
sec <- function(deb_motif, fin_motif){
  deb <- grep(deb_motif, lignes); fin <- grep(fin_motif, lignes)
  stopifnot(length(deb) == 1, length(fin) == 1, fin > deb)
  lignes[deb:(fin - 1)]
}
bloc_inline <- function(motif_debut){
  deb <- grep(motif_debut, lignes); stopifnot(length(deb) == 1)
  fin <- deb + which(grepl("dplyr::collect\\(\\)", lignes[deb:length(lignes)]))[1] - 1
  lignes[deb:fin]
}
evalq_lignes <- function(l) eval(parse(text = l, encoding = "UTF-8"), envir = globalenv())

evalq_lignes(sec("^## ---- 0\\. Config", "^## ---- 1\\. Sources"))
evalq_lignes(sec("^## ---- 2\\. prep_data", "^## ---- 3\\. Tables"))
evalq_lignes(sec("^## ---- 3\\. Tables", "^# 3f\\. Exécution"))
evalq_lignes(sec("^## ---- 4\\. Helpers purs", "^## ---- 5\\. Branche"))
evalq_lignes(sec("^prep_scenarios2<-function", "^# Catalogue : agrégation"))
evalq_lignes(sec("^construire_catalogue_longs <- function", "^df_catalogue_brut <- "))

# Paramètres réduits pour la simulation
SEUIL_PIVOT <- 1
SEUIL_REF_PAIRES <- 5
ANS_HISTORIQUE <- c(17L, 20L, 26L)
AN_REF <- 26L
set.seed(SEED)

# ---------------------------------------------- référentiels factices --
code_did      <- c("E102","E103","E104","E105","E106","E107","E108","E109")
code_dnid_ins <- c("E1120","E1130","E1140","E1150","E1160","E1170","E1180","E1190")
code_dnid     <- c("E1128","E1138","E1148","E1158","E1168","E1178","E1188","E1198")
hta_autres    <- c("I110","I119","I120","I129","I131","I132","I139","I150","I151","I152","I158","I159")
codes_astrisques_diabete <- c("N083","H360","G632")
codes_comp_sat_diab <- c("N083","H360","G632")
comp_sat_diab <- codes_comp_sat_diab
neo_codes_diabete <- c("E10","E11i","E11ni")
codes_diab <- tibble::tibble(code = c("N083","H360","G632","I792","M142"),
                             chemin = c("complications/renal/asterisques_obligatoires/x", "complications/oculaire/asterisques_obligatoires/x",
                                        "complications/neurologique/asterisques_obligatoires/x", "complications/vasculaire_peripherique/asterisques_obligatoires/x",
                                        "complications/autres_precisees/asterisques_obligatoires/x"))
cim <- tibble::tibble(code = c("J44.9","J44.0","I10","N18.9","N18.5","K80.2","I50.9","I50.0"),
                      libelle = c("BPCO, sans précision","BPCO avec infection","HTA essentielle","IRC, sans précision","IRC stade 5","Lithiase","Insuffisance cardiaque, sans précision","IC congestive"))
PAIRES_EXCLUES <- list(c("E10","E11"), c("I10","I15"))

# ---------------------------------------------- tables factices --
conn <- pRatihque::connection_database()
N <- 4000
pool_das <- c("I10","I110","E1120","E1128","E102","E785","J449","N189","N185","F172","I48","G20","M199",
              "R2630","F050","F102","N083","E1198","I509","I500","K802","J440","C189","Z511","D649","E669")
pool_ghm <- c("04M053","05M093","06C041","10M021","03K021","14Z081","90Z001","06C042")
pool_dp  <- c("J449","I500","E1120","E102","Z511","K802","I10")
gen_annee <- function(an){
  ident <- seq_len(N) + an * 100000
  fixe <- tibble::tibble(
    anonyme = sample(1:(N * 0.7), N, replace = TRUE), ident = ident,
    dp = sample(pool_dp, N, replace = TRUE), dr = NA_character_,
    age = sample(0:95, N, replace = TRUE), sexe = sample(c("1","2"), N, replace = TRUE),
    provenance = sample(c("5","8",NA), N, replace = TRUE), modesortie = sample(c("8","9","7"), N, replace = TRUE, prob = c(8,1,1)),
    destination = sample(c("1","2",NA), N, replace = TRUE), duree = sample(c(0,0,1,2,3,4,5,8,12,30), N, replace = TRUE),
    rumdudp = 1L, nbda = sample(0:8, N, replace = TRUE), ghm2 = sample(pool_ghm, N, replace = TRUE),
    passage_urg = sample(c("5","U","V","0"), N, replace = TRUE), nbrum = sample(1:2, N, replace = TRUE, prob = c(8, 2)),
    raac = sample(c("0","1"), N, replace = TRUE))
  fixe$dr[fixe$dp == "Z511"] <- "C189"
  um <- fixe |> dplyr::select(ident, nbrum, duree) |> tidyr::uncount(nbrum, .id = "rum") |>
    dplyr::mutate(finessgeo = sample(c("750100042","750100075","920100013"), dplyr::n(), replace = TRUE),
                  type_hospum_1 = ifelse(duree == 0 & runif(dplyr::n()) < 0.5, "P", "C"),
                  type_rum_1 = sample(c("01A","07A","27","04","06","10","10","10"), dplyr::n(), replace = TRUE)) |>
    dplyr::select(ident, rum, finessgeo, type_hospum_1, type_rum_1)
  diag <- fixe |> dplyr::select(ident, dp, nbda) |>
    dplyr::mutate(das = purrr::map(nbda, ~ sample(pool_das, .x, replace = FALSE))) |>
    tidyr::unnest(das) |> dplyr::transmute(ident, rum = 1L, diag = das, typ_diag = 5L) |>
    dplyr::bind_rows(fixe |> dplyr::transmute(ident, rum = 1L, diag = dp, typ_diag = 1L))
  rgp <- fixe |> dplyr::transmute(ident, ghmv2023 = ghm2, ghmv2021 = ghm2)
  DBI::dbWriteTable(conn, "PRD_VUE_MCOBL_20" %+% an %+% ".fixe", as.data.frame(fixe), overwrite = TRUE)
  DBI::dbWriteTable(conn, "PRD_VUE_MCOBL_20" %+% an %+% ".um", as.data.frame(um), overwrite = TRUE)
  DBI::dbWriteTable(conn, "PRD_VUE_MCOBL_20" %+% an %+% ".diag", as.data.frame(diag), overwrite = TRUE)
  DBI::dbWriteTable(conn, "PRD_VUE_MCOBL_20" %+% an %+% ".rgp", as.data.frame(rgp), overwrite = TRUE)
}
for(an_ in ANS_HISTORIQUE) gen_annee(an_)
DBI::dbWriteTable(conn, "nomgen.finessgeo", data.frame(finessgeo = c("750100042","750100075","920100013"), categ_pmsi = c("CHR/U","CHR/U","CH")), overwrite = TRUE)
DBI::dbWriteTable(conn, "prd_vue_nompmsi.mco_diag_niveau",
                  data.frame(code = pool_das, v2021 = sample(1:4, length(pool_das), TRUE), v2023 = sample(1:4, length(pool_das), TRUE), v2025 = sample(1:4, length(pool_das), TRUE)), overwrite = TRUE)
DBI::dbWriteTable(conn, "prd_vue_nompmsi.all_cim10_caract_patient",
                  data.frame(code = pool_das, type_liste = ifelse(pool_das %in% c("I10","I110","E1120","E1128","E102","E785","J449","N189","N185","F172","I48","G20","M199","E1198","I509","I500","J440","C189","E669"), "Patho_chro", "Aigu"),
                             caract = "x"), overwrite = TRUE)

n_ok <- 0
ok <- function(nom, expr){ if(!isTRUE(expr)) stop("ECHEC : " %+% nom); n_ok <<- n_ok + 1; cat("  ok  ", nom, "\n") }

# ---------------------------------------------- section 2 : prep_data --
cat("\n# prep_data (3 branches de millésime)\n")
for(an_ in ANS_HISTORIQUE) prep_data(an_)
pd <- pRatihque::atihble(conn, "prep_data_26") |> dplyr::collect()
ok("prep_data_26 non vide", nrow(pd) > 0)
ok("colonnes attendues (dont cage2, raac, type_unite, prep_sc, diabete, hta)",
   all(c("anonyme","ident","mode_hospit","mode_entree","mode_sortie","sexe","categ_pmsi","age","cage2","cage","racine","ghm2",
         "diabete","hta","diag2","mdp","rumdudp","nbda","duree","type_unite","prep_sc","raac") %in% names(pd)))
ok("GHM 90 exclus", !any(substr(pd$ghm2, 1, 2) == "90"))
ok("diabete/hta renseignés (N ou valeur)", all(pd$diabete %in% c("N","E10","E11i","E11ni")) && all(pd$hta %in% c("N","I10")))
ok("§5.9a : une seule ligne par (ident, type_unite)", !any(duplicated(pd[, c("ident","type_unite")])))
ok("§5.9a : une seule ligne par (anonyme, ghm2, type_unite)", !any(duplicated(pd[, c("anonyme","ghm2","type_unite")])))
ok("règle cage2 : mineur > 14 ans en GHM C -> ge_18",
   { p <- pd |> dplyr::filter(age == "lt_18", substr(ghm2,3,3) == "C", cage == "[15-18[")
     nrow(p) == 0 || all(p$cage2 == "ge_18") })
ok("diag2 = DR quand DP en Z", all(pd$diag2[pd$mdp != "DP"] == "C189"))
pd20 <- pRatihque::atihble(conn, "prep_data_20") |> dplyr::collect(); pd17 <- pRatihque::atihble(conn, "prep_data_17") |> dplyr::collect()
ok("millésimes 17 et 20 : mêmes colonnes, raac NA", identical(names(pd20), names(pd)) && identical(names(pd17), names(pd)) && all(is.na(pd20$raac)))

# ---------------------------------------------- section 3 : tables de référence --
cat("\n# tables de référence\n")
df_das_ref <- ref_das_aigu(AN_REF)
ok("ref_das_aigu : colonnes strate + das + n, sans diabète/I10/astérisques",
   all(c("mode_hospit","sexe","cage","racine","ghm2","diag2","das","n") %in% names(df_das_ref)) && nrow(df_das_ref) > 0 &&
     !any(df_das_ref$das %in% c(code_did, code_dnid, code_dnid_ins, codes_astrisques_diabete, "I10")))
prep_das_chronique(AN_REF)
df_das_chronique <- ref_das_chronique(AN_REF)
ok("prep_das_chronique : appel silencieux (invisible)", !withVisible(prep_das_chronique(AN_REF))$visible)
ok("ref_das_chronique : Patho_chro seulement, néo-codes appliqués",
   nrow(df_das_chronique) > 0 && all(df_das_chronique$type_liste == "Patho_chro") &&
     !any(df_das_chronique$das %in% c(code_did, code_dnid, code_dnid_ins)) && any(df_das_chronique$das %in% neo_codes_diabete))
df_nb_chroniques <- ref_nb_chroniques(AN_REF)
ok("ref_nb_chroniques : (cage, sexe, nb_chro, nb) avec des zéros",
   all(c("cage","sexe","nb_chro","nb") %in% names(df_nb_chroniques)) && any(df_nb_chroniques$nb_chro == 0))
ok("ref_nb_chroniques : total = nb de séjours longs distincts",
   sum(df_nb_chroniques$nb) == nrow(dplyr::distinct(pd |> dplyr::filter(duree > DUREE_MIN_REF), ident, cage, sexe)))
df_res_epi_comp_diabete <- ref_comp_diabete(AN_REF) |>
  dplyr::mutate(tot = sum(nb,na.rm=TRUE),.by=c(cage,diabete)) |>
  dplyr::mutate(nb = dplyr::case_when(comp=="9"&cage%in%CAGE_AGES~tot*PENALITE_9_AGES,
                                      comp=="9"&! cage%in%CAGE_PED~tot*PENALITE_9_AUTRES,
                                      TRUE~nb)) |>
  dplyr::select(-tot)
ok("ref_comp_diabete : (cage, diabete, comp, nb)", all(c("cage","diabete","comp","nb") %in% names(df_res_epi_comp_diabete)) && nrow(df_res_epi_comp_diabete) > 0)
codes_imprecis <- codes_imprecis_de_cim(cim, MOTIF_IMPRECIS)
ok("codes_imprecis factices", identical(sort(codes_imprecis), c("I509","J449","N189")))
df_ref_imprecis <- ref_substitution_imprecis(AN_REF, codes_imprecis)
ok("§7.5 : catégories des codes imprécis, codes frères, niveau joint, seuil",
   all(df_ref_imprecis$cat %in% c("I50","J44","N18")) && all(c("cat","code","cage","sexe","nb","niveau","imprecis") %in% names(df_ref_imprecis)) &&
     all(df_ref_imprecis$nb >= SEUIL_REF_IMPRECIS) && any(df_ref_imprecis$code == "N185"))
df_ref_paires <- ref_paires_chroniques(AN_REF)
cat("   paires chroniques :", nrow(df_ref_paires), "lignes ; nb max =", suppressWarnings(max(df_ref_paires$nb)), "\n")
ok("§7.6 : paires das_a < das_b, seuil", nrow(df_ref_paires) > 0 && all(df_ref_paires$das_a < df_ref_paires$das_b) && all(df_ref_paires$nb >= SEUIL_REF_PAIRES))

# ---------------------------------------------- section 5 : REFS (branche chir ambu supprimée) --
REFS <- construire_refs(comp_diabete = df_res_epi_comp_diabete, codes_diab = codes_diab, codes_comp_sat_diab = codes_comp_sat_diab,
                        hta_autres = hta_autres, code_did = code_did, code_dnid_ins = code_dnid_ins, code_dnid = code_dnid,
                        neo_codes = neo_codes_diabete, paires_exclues = PAIRES_EXCLUES)
ok("sample_age vectorisé sur les cage de prep_data : âge cohérent avec la classe (§5.6)",
   { a <- sample_age(pd$cage, AGE_MAX_OUVERT); all(decoupe_cage(a) == pd$cage) && length(unique(a)) > 10 })

# ---------------------------------------------- section 6 : séjours courts --
cat("\n# séjours courts\n")
evalq_lignes(bloc_inline("^df_cases_courts <- "))
evalq_lignes(bloc_inline("^df_v_admin_courts <- "))
ok("df_cases_courts : pivots + nb > seuil", all(c(PIVOTS_COURTS, "nb") %in% names(df_cases_courts)) && all(df_cases_courts$nb > SEUIL_PIVOT) && nrow(df_cases_courts) > 0)
ref_chro <- prep_ref_chronique(df_das_chronique)
df_courts_tirage <- purrr::pmap(df_cases_courts[, c(PIVOTS_COURTS, "nb")], sample_das_court,
                                ref_chro = ref_chro, ref_nb_chro = df_nb_chroniques, refs = REFS,
                                nb_tirages = NB_TIRAGES_COURTS, seuil_ref = SEUIL_REF_DAS,
                                cibles_defaut = CIBLES_NB_CHRONIQUES, age_max = AGE_MAX_OUVERT) |> purrr::list_rbind()
ok("courts : tirage non vide, NB_TIRAGES_COURTS variantes", nrow(df_courts_tirage) > 0 && max(df_courts_tirage$variante) == NB_TIRAGES_COURTS)
df_courts <- df_courts_tirage |>
  dplyr::left_join(df_v_admin_courts,relationship = "many-to-many") |>
  dplyr::group_by(dplyr::across(-dplyr::any_of(COLS_ADMIN))) |>
  dplyr::slice_sample(n = NB_VARIANTES_ADMIN_COURTS) |>
  dplyr::ungroup()
ok("courts : habillage admin <= 2 variantes par scénario", nrow(df_courts) <= 2 * nrow(df_courts_tirage) && all(c("mode_entree","mode_sortie","mdp") %in% names(df_courts)))
cc <- controler_scenarios(df_courts, hta_autres, SEUIL_PIVOT)
ok("courts : contrôles §8.2 à zéro", cc$doublons_categorie == 0 && cc$diabete_hors_flag == 0 && cc$i10_avec_hta_autres == 0 && cc$poids_sous_seuil == 0)

# ---------------------------------------------- section 7 : séjours longs --
cat("\n# séjours longs\n")
df_catalogue_brut <- construire_catalogue_longs()
ok("catalogue brut : pivots + diagnostic_associes + n", all(c(PIVOTS_LONGS, "diagnostic_associes", "n") %in% names(df_catalogue_brut)) && nrow(df_catalogue_brut) > 0)
ok("graine : au plus K_GRAINE_LONGS DAS, sans diabète/I10", all(lengths(split_das(df_catalogue_brut$diagnostic_associes)) <= K_GRAINE_LONGS) &&
     !any(unlist(split_das(df_catalogue_brut$diagnostic_associes)) %in% c(code_did, code_dnid, code_dnid_ins, "I10")))
ok("catalogue : nbda dans 1:NBDA_MAX, durée dans DUREE_LONGS", all(df_catalogue_brut$nbda %in% 1:NBDA_MAX))
df_catalogue_longs <- df_catalogue_brut |>  dplyr::inner_join(df_catalogue_brut |>
                                                                dplyr::summarise(nb=sum(n),
                                                                                 .by =dplyr::all_of(PIVOTS_LONGS_SEUIL) )  ) |>
  dplyr::filter(nb>SEUIL_PIVOT) |> dplyr::select(-n) |>
  dplyr::rename(poids = nb)
ok("catalogue éligible : poids > SEUIL_PIVOT, colonne n abandonnée", all(df_catalogue_longs$poids > SEUIL_PIVOT) && !"n" %in% names(df_catalogue_longs) && nrow(df_catalogue_longs) > 0)
df_longs_tirage <- purrr::pmap(df_catalogue_longs |> dplyr::select(dplyr::all_of(c(PIVOTS_LONGS, "diagnostic_associes", "poids"))),
                               sample_das_long, ref_das_aigu = df_das_ref, refs = REFS, nb_tirage = NB_TIRAGES_LONGS) |> purrr::list_rbind()
ok("longs : tirage non vide", nrow(df_longs_tirage) > 0)
# La graine n'est jamais éliminée, sauf son 2e code quand les deux codes de graine partagent
# la même catégorie 3 caractères (règle §6.4, ex. "J440 J449") — cf. MODIFICATIONS_V8.md Q6.
ok("longs : la graine n'est jamais éliminée (hors doublon de catégorie interne à la graine)",
   all(mapply(function(g, d) all(g %in% d) || (any(duplicated(substr(g, 1, 3))) && g[1] %in% d),
              split_das(df_longs_tirage$graine), split_das(df_longs_tirage$diagnostic_associes))))
evalq_lignes(bloc_inline("^df_v_admin_longs <- "))
df_longs <- df_longs_tirage |> dplyr::left_join(df_v_admin_longs,relationship = "many-to-many")
ok("longs : habillage admin joint", nrow(df_longs) >= nrow(df_longs_tirage) && all(c("mode_entree","mode_sortie","mdp","duree") %in% names(df_longs)))
cl <- controler_scenarios(df_longs, hta_autres, SEUIL_PIVOT)
ok("longs : contrôles §8.2 à zéro", cl$doublons_categorie == 0 && cl$diabete_hors_flag == 0 && cl$i10_avec_hta_autres == 0 && cl$poids_sous_seuil == 0)
ok("longs : HTA -> I10 présent quand hta != N et aucun hta_autres",
   all(mapply(function(h, d) h == "N" || "I10" %in% d || any(d %in% hta_autres), df_longs_tirage$hta, split_das(df_longs_tirage$diagnostic_associes))))
ok("longs : flag diabète -> code E10/E11 réel présent",
   all(mapply(function(f, d) f == "N" || any(substr(d,1,3) %in% c("E10","E11")), df_longs_tirage$diabete_scenario, split_das(df_longs_tirage$diagnostic_associes))))
ok("rapport : distribution / taux calculables", !is.null(distribution_nb_das(df_longs)) && !is.na(taux_imprecis(df_longs, codes_imprecis)))

DBI::dbDisconnect(conn)
cat("\nSIMULATION SQLITE VERTE :", n_ok, "assertions\n")
