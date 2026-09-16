###############################################################################
# tests/test_chaines_sqlite.R — les SCRIPTS RÉELS du v8 sur une base SQLite FICHIER
#
# Objet : exécuter extraction_associations_codes_v8.R puis tirage_scenarios_v8.R tels
# quels, dans un projet temporaire (config/helpers/scripts copiés, utils.R et referentiels.R
# remplacés par des stubs sans Excel ni base), avec un faux paquet `pRatihque` dont
# atihble = dplyr::tbl et connection_database = nouvelle connexion SQLite sur un fichier :
# les tables sources persistent entre « sessions », les tables temporaires disparaissent
# à la déconnexion. Vérifie : traduction/exécution des chaînes dbplyr (dont §5.9a, B1-10),
# résolution des besoins en sessions multiples, cache des partiels, reprise, FORCER_REFS,
# garde-fou partiels_meta, phase tirage sans base (mock interdit), reprise des chunks,
# identité parquet, livrables. Ne valide PAS le dialecte ni les colonnes de la base réelle.
#
# Prérequis : dbplyr, DBI, RSQLite, yaml (arrow : mock RDS de repli si absent). Exécution : Rscript tests/test_chaines_sqlite.R
# [R_LIBS_TEST=<lib supplémentaire>]
###############################################################################
lib_test <- Sys.getenv("R_LIBS_TEST", unset = "")
if(nzchar(lib_test)) .libPaths(c(lib_test, .libPaths()))
for(p in c("dbplyr", "DBI", "RSQLite", "yaml", "tidyr", "readr")) if(!requireNamespace(p, quietly = TRUE)) stop("Paquet manquant : ", p)
# Repli arrow (tests UNIQUEMENT) : si arrow est absent, un paquet mock `arrow` est installé dans
# tempdir (même mécanique que le mock pRatihque), dont write_parquet/read_parquet sont
# saveRDS/readRDS. Limite : les fichiers produits sont des RDS nommés .parquet, valables
# seulement parce qu'ils sont relus par le même mock dans cette session. Les scripts de
# production continuent d'exiger le vrai arrow.
ARROW_MOCK <- !requireNamespace("arrow", quietly = TRUE)
if(ARROW_MOCK){
  lib_mock_arrow <- file.path(tempdir(), "lib_mock_arrow"); dir.create(lib_mock_arrow, showWarnings = FALSE)
  pkg_arrow <- file.path(tempdir(), "arrow"); dir.create(file.path(pkg_arrow, "R"), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("Package: arrow", "Version: 0.0.0.9000", "Title: Mock", "Description: Mock arrow (RDS) pour tests hors base.",
               "License: MIT", "Encoding: UTF-8"), file.path(pkg_arrow, "DESCRIPTION"))
  writeLines("export(write_parquet, read_parquet)", file.path(pkg_arrow, "NAMESPACE"))
  writeLines(c("write_parquet <- function(x, sink, ...) saveRDS(x, sink)",
               "read_parquet  <- function(file, ...)  readRDS(file)"), file.path(pkg_arrow, "R", "mock.R"))
  utils::install.packages(pkg_arrow, repos = NULL, type = "source", lib = lib_mock_arrow, quiet = TRUE)
  .libPaths(c(lib_mock_arrow, .libPaths()))
  stopifnot(requireNamespace("arrow", quietly = TRUE))
}
suppressPackageStartupMessages({library(dplyr); library(tibble); library(stringr); library(dbplyr)})
for(loc in c("fr_FR.UTF-8", "en_US.UTF-8", "C.UTF-8")) if(!is.na(suppressWarnings(Sys.setlocale("LC_CTYPE", loc))) && Sys.getlocale("LC_CTYPE") == loc) break
`%+%` <- function(x, y) paste0(x, y)
n_ok <- 0
ok <- function(nom, expr){ if(!isTRUE(expr)) stop("ECHEC : " %+% nom); n_ok <<- n_ok + 1; cat("  ok  ", nom, "\n") }

# ------------------------------------------------ faux paquet pRatihque (fichier) --
lib_mock <- file.path(tempdir(), "lib_mock"); dir.create(lib_mock, showWarnings = FALSE)
pkg <- file.path(tempdir(), "pRatihque"); dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
writeLines(c("Package: pRatihque", "Version: 0.0.0.9000", "Title: Mock", "Description: Mock SQLite fichier pour tests hors base.",
             "License: MIT", "Encoding: UTF-8", "Imports: DBI, RSQLite, dplyr"), file.path(pkg, "DESCRIPTION"))
writeLines("export(atihble, connection_database)", file.path(pkg, "NAMESPACE"))
writeLines(c(
  "connection_database <- function(){",
  "  if(isTRUE(getOption('pmsi_mock_interdit'))) stop('APPEL BASE INTERDIT : connection_database() pendant la phase tirage')",
  "  DBI::dbConnect(RSQLite::SQLite(), getOption('pmsi_mock_db', ':memory:'))",
  "}",
  "atihble <- function(conn, name){",
  "  if(isTRUE(getOption('pmsi_mock_interdit'))) stop('APPEL BASE INTERDIT : atihble() pendant la phase tirage')",
  "  dplyr::tbl(conn, name)",
  "}"), file.path(pkg, "R", "mock.R"))
utils::install.packages(pkg, repos = NULL, type = "source", lib = lib_mock, quiet = TRUE)
.libPaths(c(lib_mock, .libPaths()))
stopifnot(requireNamespace("pRatihque", quietly = TRUE))

# ------------------------------------------------ projet temporaire --
racine <- normalizePath(c(".", "..")[file.exists(c("config_v8.R", "../config_v8.R"))][1])
stub_utils <- c("`%+%` <- function(x,y){paste0(x,y)}", "prep_grep <- function(x) paste(x, collapse = \"|\")")
stub_referentiels <- c(
  'code_did      <- c("E102","E103","E104","E105","E106","E107","E108","E109")',
  'code_dnid_ins <- c("E1120","E1130","E1140","E1150","E1160","E1170","E1180","E1190")',
  'code_dnid     <- c("E1128","E1138","E1148","E1158","E1168","E1178","E1188","E1198")',
  'hta_autres    <- c("I110","I119","I120","I129","I131","I132","I139","I150","I151","I152","I158","I159")',
  'codes_astrisques_diabete <- c("N083","H360","G632")',
  'codes_comp_sat_diab <- c("N083","H360","G632")',
  'comp_sat_diab <- codes_comp_sat_diab',
  'neo_codes_diabete <- c("E10","E11i","E11ni")',
  'codes_diab <- tibble::tibble(code = c("N083","H360","G632","I792","M142"),',
  '  chemin = c("complications/renal/asterisques_obligatoires/x", "complications/oculaire/asterisques_obligatoires/x",',
  '             "complications/neurologique/asterisques_obligatoires/x", "complications/vasculaire_peripherique/asterisques_obligatoires/x",',
  '             "complications/autres_precisees/asterisques_obligatoires/x"))',
  'cim <- tibble::tibble(code = c("J44.9","J44.0","I10","N18.9","N18.5","K80.2","I50.9","I50.0","E78.5","F17.2","I48"),',
  '  libelle = c("BPCO, sans précision","BPCO avec infection","HTA essentielle","IRC, sans précision","IRC stade 5","Lithiase",',
  '              "Insuffisance cardiaque, sans précision","IC congestive","Hyperlipidémie","Tabagisme","Fibrillation auriculaire"))')
creer_projet <- function(nom){
  proj <- file.path(tempdir(), nom); unlink(proj, recursive = TRUE); dir.create(file.path(proj, "referentiels"), recursive = TRUE)
  for(f in c("config_v8.R", "helpers_v8.R", "etapes_v8.R", "extraction_associations_codes_v8.R", "tirage_scenarios_v8.R", "exclusions.R"))
    file.copy(file.path(racine, f), file.path(proj, f))
  file.copy(file.path(racine, "referentiels", "exclusions_paires.yaml"), file.path(proj, "referentiels", "exclusions_paires.yaml"))
  file.copy(file.path(racine, "referentiels", "typologie_sejours.yaml"), file.path(proj, "referentiels", "typologie_sejours.yaml"))
  writeLines(stub_utils, file.path(proj, "utils.R"))
  writeLines(stub_referentiels, file.path(proj, "referentiels.R"))
  proj
}
proj <- creer_projet("projet_v8")
db_file <- file.path(tempdir(), "mock_v8.sqlite"); unlink(db_file)
options(pmsi_mock_db = db_file, pmsi_mock_interdit = FALSE)
Sys.setenv(SCENARIOS_PMSI_PATH = proj, SCENARIOS_PMSI_PROFIL = "diagnostic")
# CHUNK_SIZE_FIXE = 40 : force des cas multi-chunks sur les petites fixtures (reprise et garde-fou
# réellement exercés) ; CHUNK_SIZE <- 40L n'est lu que par les anciens scripts d'entrée (référence d'identité).
# NB_CRH_CIBLE = 120 (nouveau nom) ; BUDGET_TOTAL_LONGS = 120 n'est lu que par les anciens scripts d'entrée.
SURCHARGE_BASE <- c("SEUIL_PIVOT <- 1", "SEUIL_REF_PAIRES <- 5", "NB_CRH_CIBLE <- 120L", "BUDGET_TOTAL_LONGS <- 120L", "CHUNK_SIZE_FIXE <- 40L", "CHUNK_SIZE <- 40L",
                    'PAIRES_RECOUVREMENT <- list(c("CHR/U", 17, 26), c("CH", 24, 25))')
surcharger <- function(...){
  f <- file.path(tempdir(), "surcharge.R"); writeLines(c(SURCHARGE_BASE, ...), f); Sys.setenv(SCENARIOS_PMSI_SURCHARGE = f)
}
temp_tables <- function(conn){ x <- DBI::dbGetQuery(conn, "SELECT name FROM sqlite_temp_master WHERE type = 'table'")$name; x[!grepl("^sqlite_", x)] }
fermer <- function(){ if(exists("conn", envir = globalenv()) && DBI::dbIsValid(get("conn", envir = globalenv()))) DBI::dbDisconnect(get("conn", envir = globalenv())) }
lancer <- function(script, ...){ source(file.path(Sys.getenv("SCENARIOS_PMSI_PATH"), script), local = FALSE, ...) }
lire_cat <- function(dir) arrange(as_tibble(arrow::read_parquet(file.path(dir, "catalogue_longs_seuil.parquet"))), across(everything()))

# ------------------------------------------------ tables factices (base fichier) --
set.seed(20260907)
N <- 4000
pool_das <- c("I10","I110","E1120","E1128","E102","E785","J449","N189","N185","F172","I48","G20","M199",
              "R2630","F050","F102","N083","E1198","I509","I500","K802","J440","C189","Z511","D649","E669","E6690","E6602","E6600")
pool_ghm <- c("04M053","05M093","06C041","10M021","03K021","14Z081","90Z001","06C042")
pool_dp  <- c("J449","I500","E1120","E102","Z511","K802","I10","E6690")
conn0 <- DBI::dbConnect(RSQLite::SQLite(), db_file)
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
  DBI::dbWriteTable(conn0, "PRD_VUE_MCOBL_20" %+% an %+% ".fixe", as.data.frame(fixe), overwrite = TRUE)
  DBI::dbWriteTable(conn0, "PRD_VUE_MCOBL_20" %+% an %+% ".um", as.data.frame(um), overwrite = TRUE)
  DBI::dbWriteTable(conn0, "PRD_VUE_MCOBL_20" %+% an %+% ".diag", as.data.frame(diag), overwrite = TRUE)
  DBI::dbWriteTable(conn0, "PRD_VUE_MCOBL_20" %+% an %+% ".rgp", as.data.frame(rgp), overwrite = TRUE)
}
for(an_ in c(17L, 20L, 26L)) gen_annee(an_)
# Fixtures contrôlées (écart B1-10), millésime 26
IDENT_B110 <- c(hc_sc = 2699001, hc_uhcd = 2699002, uhcd_seul = 2699003, ger_sc = 2699004)
invisible(DBI::dbAppendTable(conn0, "PRD_VUE_MCOBL_2026.fixe", as.data.frame(tibble::tibble(
  anonyme = 999001:999004, ident = unname(IDENT_B110), dp = "J449", dr = NA_character_, age = 70, sexe = "1",
  provenance = "8", modesortie = "8", destination = "1", duree = 5, rumdudp = 1L, nbda = 2L, ghm2 = "04M053",
  passage_urg = "0", nbrum = c(2L, 2L, 1L, 2L), raac = "0"))))
invisible(DBI::dbAppendTable(conn0, "PRD_VUE_MCOBL_2026.um", as.data.frame(tibble::tibble(
  ident = c(rep(IDENT_B110[["hc_sc"]], 2), rep(IDENT_B110[["hc_uhcd"]], 2), IDENT_B110[["uhcd_seul"]], rep(IDENT_B110[["ger_sc"]], 2)),
  rum = c(1L, 2L, 1L, 2L, 1L, 1L, 2L), finessgeo = "750100042", type_hospum_1 = "C",
  type_rum_1 = c("10", "01A", "10", "07A", "07A", "27", "01A")))))
invisible(DBI::dbAppendTable(conn0, "PRD_VUE_MCOBL_2026.diag", as.data.frame(tibble::tibble(ident = rep(unname(IDENT_B110), each = 2), rum = 1L, diag = rep(c("I10", "E785"), 4), typ_diag = 5L))))
# Fixture fusion E669 (chantier conversion) : deux séjours identiques sauf DP E6690 / E6600 sur un
# GHM dédié 88M991 -> deux profils n = 1 (<= SEUIL_PIVOT = 1) qui fusionnent (n = 2 > seuil) après conversion.
IDENT_FUSION <- c(2699101, 2699102)
invisible(DBI::dbAppendTable(conn0, "PRD_VUE_MCOBL_2026.fixe", as.data.frame(tibble::tibble(
  anonyme = 999101:999102, ident = IDENT_FUSION, dp = c("E6690", "E6600"), dr = NA_character_, age = 72, sexe = "2",
  provenance = "8", modesortie = "8", destination = "1", duree = 6, rumdudp = 1L, nbda = 1L, ghm2 = "88M991",
  passage_urg = "0", nbrum = 1L, raac = "0"))))
invisible(DBI::dbAppendTable(conn0, "PRD_VUE_MCOBL_2026.um", as.data.frame(tibble::tibble(
  ident = IDENT_FUSION, rum = 1L, finessgeo = "750100042", type_hospum_1 = "C", type_rum_1 = "10"))))
invisible(DBI::dbAppendTable(conn0, "PRD_VUE_MCOBL_2026.diag", as.data.frame(tibble::tibble(ident = IDENT_FUSION, rum = 1L, diag = "I48", typ_diag = 5L))))
DBI::dbWriteTable(conn0, "nomgen.finessgeo", data.frame(finessgeo = c("750100042","750100075","920100013"), categ_pmsi = c("CHR/U","CHR/U","CH")), overwrite = TRUE)
DBI::dbWriteTable(conn0, "prd_vue_nompmsi.mco_diag_niveau",
                  data.frame(code = pool_das, v2021 = as.character(sample(1:4, length(pool_das), TRUE)), v2023 = as.character(sample(1:4, length(pool_das), TRUE)), v2025 = as.character(sample(1:4, length(pool_das), TRUE))), overwrite = TRUE)
DBI::dbWriteTable(conn0, "prd_vue_nompmsi.all_cim10_caract_patient",
                  data.frame(code = pool_das, type_liste = ifelse(pool_das %in% c("I10","I110","E1120","E1128","E102","E785","J449","N189","N185","F172","I48","G20","M199","E1198","I509","I500","J440","C189","E669","E6690","E6602","E6600"), "Patho_chro", "Aigu"),
                             caract = "x"), overwrite = TRUE)
DBI::dbDisconnect(conn0)
sortie <- function(expr) utils::capture.output(expr, type = "output")

# =============================================================== SESSION 1 ==
cat("\n# session 1 : extraction partielle (années 17 et 26, CHR/U et CH)\n")
surcharger("ANS_HISTORIQUE <- c(17L, 26L)")
log1 <- sortie(lancer("extraction_associations_codes_v8.R"))
ok("plan session 1 : 4 itérations, 10 refs, années 17 et 26, prep_das_chronique",
   sum(ETAPES_ENV$plan$iterations$a_faire) == 4 && all(ETAPES_ENV$plan$refs$a_faire) && identical(ETAPES_ENV$plan$annees_a_preparer, c(17L, 26L)) && ETAPES_ENV$plan$prep_das_chronique)
tt <- temp_tables(conn)
ok("tables temporaires créées : prep_data_17, prep_data_26, prep_das_chro_26 ; pas de prep_data_20",
   all(c("prep_data_17", "prep_data_26", "prep_das_chro_26") %in% tt) && !"prep_data_20" %in% tt)
pd <- pRatihque::atihble(conn, "prep_data_26") |> dplyr::collect()
ok("prep_data_26 : colonnes attendues, GHM 90 exclus, flags", all(c("anonyme","ident","mode_hospit","mode_entree","mode_sortie","sexe","categ_pmsi","age","cage2","cage","racine","ghm2",
   "diabete","hta","diag2","mdp","rumdudp","nbda","duree","type_unite","prep_sc","raac") %in% names(pd)) && !any(substr(pd$ghm2,1,2) == "90") &&
   all(pd$diabete %in% c("N","E10","E11i","E11ni")) && all(pd$hta %in% c("N","I10")))
ok("B1-10 : un ident = une ligne ; §5.9a : une ligne par (anonyme, ghm2)", !any(duplicated(pd$ident)) && !any(duplicated(pd[, c("anonyme","ghm2")])))
b110 <- pd |> dplyr::filter(ident %in% IDENT_B110) |> dplyr::arrange(ident)
ok("B1-10 : HC+SC -> SC/prep_sc 1 ; HC+UHCD -> HC ; UHCD seul -> UHCD ; GERIATRIE+SC -> SC",
   identical(b110$ident, unname(IDENT_B110)) && identical(b110$type_unite, c("SC", "HC", "UHCD", "SC")) && identical(b110$prep_sc, c(1, 0, 0, 1)))
ok("règle cage2 : mineur > 14 ans en GHM C -> ge_18", { p <- pd |> dplyr::filter(age == "lt_18", substr(ghm2,3,3) == "C", cage == "[15-18["); nrow(p) == 0 || all(p$cage2 == "ge_18") })
ok("diag2 = DR quand DP en Z", all(pd$diag2[pd$mdp != "DP"] == "C189"))
pd17 <- pRatihque::atihble(conn, "prep_data_17") |> dplyr::collect()
ok("millésime 17 : mêmes colonnes, raac NA, une ligne par ident", identical(names(pd17), names(pd)) && all(is.na(pd17$raac)) && !any(duplicated(pd17$ident)))
ok("partiels écrits : 4 parquet + partiels_meta.yaml",
   setequal(list.files(PARTIELS_DIR), c("catalogue_partiel_CHRU_17.parquet", "catalogue_partiel_CHRU_26.parquet", "catalogue_partiel_CH_17.parquet", "catalogue_partiel_CH_26.parquet", "partiels_meta.yaml")))
ok("exports : 10 refs + catalogue + meta + diagnostic_apports.csv",
   all(c(nom_ref(NOMS_REFS), "catalogue_longs_seuil.parquet", "catalogue_longs_seuil_meta.yaml", "diagnostic_apports.csv") %in% list.files(EXPORTS_DIR)))
ap1 <- utils::read.csv(file.path(EXPORTS_DIR, "diagnostic_apports.csv"))
ok("diagnostic_apports : 4 lignes calculées, stats du partiel seul (sans colonnes cumul), ordre types × années",
   nrow(ap1) == 4 && all(ap1$statut == "calculé") && identical(names(ap1), c("etbs", "an", "statut", "nb_lignes_partiel", "sum_n_partiel", "nb_diag2_partiel", "nb_diag2_nouveaux")) &&
     identical(ap1$etbs, c("CHR/U", "CHR/U", "CH", "CH")) && identical(ap1$an, c(17L, 26L, 17L, 26L)) && ap1$nb_diag2_nouveaux[1] == ap1$nb_diag2_partiel[1] && all(ap1$sum_n_partiel >= ap1$nb_lignes_partiel))
# refs : contrôles de contenu
df_das_ref <- arrow::read_parquet(file.path(EXPORTS_DIR, "ref_das_aigu.parquet"))
ok("ref_das_aigu : strate + das + n, sans diabète/I10/astérisques", all(c("mode_hospit","sexe","cage","racine","ghm2","diag2","das","n") %in% names(df_das_ref)) && nrow(df_das_ref) > 0 &&
     !any(df_das_ref$das %in% c(code_did, code_dnid, code_dnid_ins, codes_astrisques_diabete, "I10")))
df_chro <- arrow::read_parquet(file.path(EXPORTS_DIR, "ref_das_chronique.parquet"))
ok("ref_das_chronique : Patho_chro, néo-codes appliqués", all(df_chro$type_liste == "Patho_chro") && !any(df_chro$das %in% c(code_did, code_dnid, code_dnid_ins)) && any(df_chro$das %in% neo_codes_diabete))
df_nbc <- arrow::read_parquet(file.path(EXPORTS_DIR, "ref_nb_chroniques.parquet"))
ok("ref_nb_chroniques : zéros inclus, total = séjours longs distincts", any(df_nbc$nb_chro == 0) && sum(df_nbc$nb) == nrow(dplyr::distinct(pd |> dplyr::filter(duree > DUREE_MIN_REF), ident, cage, sexe)))
df_imp <- arrow::read_parquet(file.path(EXPORTS_DIR, "referentiel_substitution_imprecis.parquet"))
ok("§7.5 : catégories imprécises, niveau joint, seuil", all(df_imp$cat %in% c("I50","J44","N18")) && all(c("cat","code","cage","sexe","nb","niveau","imprecis") %in% names(df_imp)) && all(df_imp$nb >= SEUIL_REF_IMPRECIS) && any(df_imp$code == "N185"))
df_pair <- arrow::read_parquet(file.path(EXPORTS_DIR, "referentiel_paires_chroniques.parquet"))
ok("§7.6 : paires das_a < das_b, seuil", nrow(df_pair) > 0 && all(df_pair$das_a < df_pair$das_b) && all(df_pair$nb >= SEUIL_REF_PAIRES))
df_pc <- arrow::read_parquet(file.path(EXPORTS_DIR, "pivots_courts.parquet"))
ok("pivots_courts : pivots + nb > seuil", all(c(PIVOTS_COURTS, "nb") %in% names(df_pc)) && all(df_pc$nb > SEUIL_PIVOT) && nrow(df_pc) > 0)
ok("ref_comp_diabete : effectifs bruts (pas de pénalisation côté extraction)", { r <- arrow::read_parquet(file.path(EXPORTS_DIR, "ref_comp_diabete.parquet")); all(r$nb == round(r$nb)) && all(c("cage","diabete","comp","nb") %in% names(r)) })
cat1 <- lire_cat(EXPORTS_DIR)
ok("catalogue seuil : poids > SEUIL_PIVOT, pas de colonne n, graine <= K sans diabète/I10",
   all(cat1$poids > SEUIL_PIVOT) && !"n" %in% names(cat1) && all(lengths(split_das(cat1$diagnostic_associes)) <= K_GRAINE_LONGS) &&
     !any(unlist(split_das(cat1$diagnostic_associes)) %in% c(code_did, code_dnid, code_dnid_ins, "I10")))
meta1 <- yaml::read_yaml(file.path(EXPORTS_DIR, "catalogue_longs_seuil_meta.yaml"))
ok("meta.yaml cohérent avec le profil et la surcharge", meta1$PROFIL == "diagnostic" && identical(unlist(meta1$ANS_HISTORIQUE), c(17L, 26L)) && meta1$SEUIL_PIVOT == 1 &&
     meta1$nb_lignes == nrow(cat1) && meta1$MODE_SELECTION == "quota_dp" && meta1$K_GRAINE_LONGS == K_GRAINE_LONGS && meta1$plan_iterations_calculees == 4)
# --- conversion E669 -> E660 (CONVERSION_E669 = TRUE par défaut)
sans_e669 <- function(df, cols) compter_e669(df, cols) == 0
ok("conversion : aucun ^E669 dans le catalogue (diag2, graines)", CONVERSION_E669 && sans_e669(cat1, c("diag2", "diagnostic_associes")) && any(grepl("^E660", cat1$diag2)))
ok("conversion : aucun ^E669 dans les refs (aigu, chronique, paires, imprécis, pivots, v_admin)",
   sans_e669(df_das_ref, c("diag2", "das")) && sans_e669(df_chro, c("diag2", "das")) && sans_e669(df_pair, c("das_a", "das_b")) &&
     sans_e669(df_imp, "code") && sans_e669(df_pc, "diag2") &&
     sans_e669(arrow::read_parquet(file.path(EXPORTS_DIR, "v_admin_courts.parquet")), "diag2") && sans_e669(arrow::read_parquet(file.path(EXPORTS_DIR, "v_admin_longs.parquet")), "diag2"))
ok("conversion : somme des n inchangée sur le catalogue agrégé, distribution E660x exportée",
   ETAPES_ENV$impact_e669$n_total_avant == ETAPES_ENV$impact_e669$n_total_apres && ETAPES_ENV$impact_e669$e669_diag2_suffixe + ETAPES_ENV$impact_e669$e669_graine_suffixe > 0 &&
     file.exists(file.path(EXPORTS_DIR, "distribution_e660.parquet")) && nrow(arrow::read_parquet(file.path(EXPORTS_DIR, "distribution_e660.parquet"))) > 0)
ok("conversion : les partiels restent en codes BRUTS (E669 présents)",
   any(vapply(list.files(PARTIELS_DIR, pattern = "_26\\.parquet$", full.names = TRUE), function(f){ d <- arrow::read_parquet(f); compter_e669(d, c("diag2", "diagnostic_associes")) > 0 }, logical(1))))
ok("conversion : cas de fusion sous-seuil -> au-dessus (GHM 88M991, DP E6690 + E6600 -> E6600, n = 2 > 1)",
   { f <- cat1[cat1$ghm2 == "88M991", ]; nrow(f) == 1 && f$diag2 == "E6600" && f$poids == 2 && ETAPES_ENV$impact_e669$profils_entres >= 1 })
ok("conversion : paires das_a < das_b, aucune paire identique", all(df_pair$das_a < df_pair$das_b))
ok("conversion : meta.yaml porte CONVERSION_E669 et BARE_E669_DEFAUT, rapport d'extraction écrit",
   isTRUE(meta1$CONVERSION_E669) && meta1$BARE_E669_DEFAUT == "0" && file.exists(file.path(EXPORTS_DIR, "rapport_extraction_v8_" %+% DATE_TAG %+% ".txt")) &&
     any(grepl("ENTRÉS par fusion", readLines(file.path(EXPORTS_DIR, "rapport_extraction_v8_" %+% DATE_TAG %+% ".txt")))))
ok("aucune ligne niveau séjour exportée (pas de colonne ident dans les parquets d'exports)",
   !any(vapply(list.files(EXPORTS_DIR, pattern = "\\.parquet$", full.names = TRUE), function(f) "ident" %in% names(arrow::read_parquet(f, as_data_frame = FALSE)), logical(1))))
# --- chantier mémoire : P1 équivalence, P2 catalogue deux étages, P3 libération
cat("\n# mémoire : équivalence P1, catalogue deux étages, recouvrement, libération\n")
prep_scenarios2_ancien_20260912<-function(an,type_etbs,nb_journees_aut,nbda_aut,nb_assoc_das,pivots){
  
  anseqta = anseqta_de(an)
  
  
  pRatihque::atihble(conn, 'prep_data_' %+% an ) |>
    dplyr::filter(prep_sc==1) |> 
    dplyr::distinct(ident,prep_sc) |> dplyr::rename(sc= prep_sc) |> 
    dplyr::full_join(pRatihque::atihble(conn, 'prep_data_' %+% an )) |>
    dplyr::mutate(sc = ifelse(is.na(sc),0,1)) |> 
    dplyr::filter(! (prep_sc==0 & sc==1)) |>
    dplyr::filter(categ_pmsi %in% type_etbs,nbda%in%1:nbda_aut,duree %in%nb_journees_aut) |> 
    dplyr::rename(rum =  rumdudp) |> 
    dplyr::left_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.diag') |> 
                       dplyr::filter(typ_diag==5,!diag%in%c(code_dnid_ins,code_dnid,code_did,
                                                            codes_astrisques_diabete,"I10")) |> 
                       dplyr::rename(das = diag) ) |> 
    dplyr::distinct_at(c("ident",pivots,"das")) |>
    
                    
    dplyr::left_join( pRatihque::atihble(conn, 'prd_vue_nompmsi.mco_diag_niveau') |> dplyr::filter(!!dplyr::sym("v20"%+% anseqta)>1) |>   # §5.8 (ex v2025>1)
                        dplyr::select(dplyr::all_of(c("code","v20"%+% anseqta))) |> 
                        dplyr::rename(das = code,niveau = !!dplyr::sym("v20"%+% anseqta))
    ) |> 
    
    dplyr::collect() -> df_das
  
  
  df_das |> 
    dplyr::mutate(niveau = ifelse(is.na(niveau),"0",niveau)) |> 
    dplyr::mutate(nb_das = dplyr::n(),.by= dplyr::all_of(c(pivots,"das"))) -> df_das
  
  df_das |> 
    dplyr::arrange(ident,dplyr::desc(niveau),dplyr::desc(nb_das),das) |>   # §5.9 : `das` en dernier critère (ordre total)
    dplyr::group_by(ident) |> 
    dplyr::slice(1:nb_assoc_das) -> df_das
  
  df_das |> 
    dplyr::group_by_at(c("ident",pivots)) |> 
    dplyr::arrange(das) |> 
    dplyr::summarise(diagnostic_associes = paste0(das,collapse = " "),.groups="drop") |> 
    dplyr::ungroup() |> 
    dplyr::summarise(n = dplyr::n(),.by=dplyr::all_of(c(pivots,"diagnostic_associes"))) -> df_cases
  
  return(df_cases)
  
  
}

comparer <- function(a, b) identical(as.data.frame(dplyr::arrange(tibble::as_tibble(a), dplyr::across(dplyr::everything()))),
                                     as.data.frame(dplyr::arrange(tibble::as_tibble(b), dplyr::across(dplyr::everything()))))
for(cas in list(list("CHR/U", 26L, 2L), list("CH", 17L, 2L), list("CHR/U", 26L, 3L))){
  anc <- prep_scenarios2_ancien_20260912(cas[[2]], cas[[1]], DUREE_LONGS, NBDA_MAX, cas[[3]], PIVOTS_LONGS)
  nv_m <- prep_scenarios2(cas[[2]], cas[[1]], DUREE_LONGS, NBDA_MAX, cas[[3]], PIVOTS_LONGS, TRUE)
  nv_u <- prep_scenarios2(cas[[2]], cas[[1]], DUREE_LONGS, NBDA_MAX, cas[[3]], PIVOTS_LONGS, FALSE)
  ok(sprintf("P1 équivalence %s %s k=%d : nouvelle chaîne (morceaux) == ancienne", cas[[1]], cas[[2]], cas[[3]]), comparer(anc, nv_m) && nrow(anc) > 0)
  ok(sprintf("P1 équivalence %s %s k=%d : collect unique == ancienne", cas[[1]], cas[[2]], cas[[3]]), comparer(anc, nv_u))
}
ok("partiel écrit par le run == ancienne chaîne (partiels antérieurs valides)",
   comparer(arrow::read_parquet(file.path(PARTIELS_DIR, "catalogue_partiel_CH_17.parquet")), prep_scenarios2_ancien_20260912(17L, "CH", DUREE_LONGS, NBDA_MAX, K_GRAINE_LONGS, PIVOTS_LONGS)))
tk <- pRatihque::atihble(conn, "prep_topk_tmp") |> dplyr::collect()
ok("prep_topk_tmp : au plus k lignes par ident, colonnes étroites (ident, pivots, das)",
   max(table(tk$ident)) <= 3 && identical(names(tk), c("ident", PIVOTS_LONGS, "das")))
ok("invariant morceaux : un ident n'apparaît que dans une seule cage", all((tk |> dplyr::distinct(ident, cage) |> dplyr::count(ident))$n == 1))
ok("prep_topk_tmp : une seule table (pas d'empilement), écrasée par la dernière itération (CHR/U 26 k=3 ci-dessus)",
   sum(temp_tables(conn) == "prep_topk_tmp") == 1 && all(tk$ident %in% (pRatihque::atihble(conn, "prep_data_26") |> dplyr::filter(categ_pmsi == "CHR/U") |> dplyr::distinct(ident) |> dplyr::collect())$ident))
# catalogue deux étages == ancien flux (bind_rows global -> conversion -> ré-agrégation -> seuil)
ancien_flux <- function(fichiers, conversion){
  d <- dplyr::bind_rows(lapply(fichiers, arrow::read_parquet)) |> dplyr::summarise(n = sum(n), .by = dplyr::all_of(c(PIVOTS_LONGS, "diagnostic_associes")))
  if(conversion){
    dist <- arrow::read_parquet(file.path(EXPORTS_DIR, "distribution_e660.parquet"))
    d <- d |> convertir_e669_comptes("diag2", c(setdiff(PIVOTS_LONGS, "diag2"), "diagnostic_associes"), "n", dist, BARE_E669_DEFAUT) |>
      convertir_e669_combo("diagnostic_associes", PIVOTS_LONGS, "n", dist, BARE_E669_DEFAUT)
  }
  d |> dplyr::inner_join(d |> dplyr::summarise(nb = sum(n), .by = dplyr::all_of(PIVOTS_LONGS_SEUIL)), by = PIVOTS_LONGS_SEUIL) |>
    dplyr::filter(nb > SEUIL_PIVOT) |> dplyr::select(-n) |> dplyr::rename(poids = nb)
}
ok("P2 catalogue deux étages == ancien flux (conversion TRUE, fixture de fusion E669 incluse)",
   comparer(cat1, ancien_flux(file.path(PARTIELS_DIR, ETAPES_ENV$plan$iterations$fichier), TRUE)) && any(cat1$ghm2 == "88M991"))
ok("recouvrement.csv : (CHR/U 17->26) ok avec parts dans [0,1], (CH 24->25) non calculable",
   { r <- utils::read.csv(file.path(EXPORTS_DIR, "recouvrement.csv")); nrow(r) == 2 && r$statut[1] == "ok" && r$part_combos_B_vues[1] >= 0 && r$part_combos_B_vues[1] <= 1 &&
     r$nb_B[1] == ap1$nb_lignes_partiel[2] && grepl("non calculable", r$statut[2]) })
ok("diagnostic_memoire.csv : schéma, mesures par morceau / partiel / ref / étage",
   { m <- utils::read.csv(file.path(EXPORTS_DIR, "diagnostic_memoire.csv")); all(c("etiquette", "horodatage", "taille_objet_mo", "memoire_utilisee_go", "pic_go", "alerte") %in% names(m)) &&
     any(grepl("morceau", m$etiquette)) && any(grepl("^partiel ", m$etiquette)) && any(grepl("^ref v_admin_longs", m$etiquette)) && any(grepl("étage 1", m$etiquette)) && any(grepl("catalogue final", m$etiquette)) })
ok("P3 : aucun objet ref ni cache brut vivant après l'extraction", !exists("df_ref") && !exists("brute", envir = CACHE_E669) && !exists("pivots_bruts") && !exists("combos") && !exists("df_prep_scenarios") && !exists("df_prep_scenarios_seuil"))
fermer()

# =============================================================== SESSION 2 ==
cat("\n# session 2 : reconnexion, année 20 ajoutée -> seules les itérations manquantes\n")
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)")
log2 <- sortie(lancer("extraction_associations_codes_v8.R"))
ok("plan session 2 : 2 itérations (CHR/U 20, CH 20), 0 ref, année 20 seule, pas de prep_das_chronique",
   sum(ETAPES_ENV$plan$iterations$a_faire) == 2 && all(ETAPES_ENV$plan$iterations$an[ETAPES_ENV$plan$iterations$a_faire] == 20) && !any(ETAPES_ENV$plan$refs$a_faire) &&
     identical(ETAPES_ENV$plan$annees_a_preparer, 20L) && !ETAPES_ENV$plan$prep_das_chronique)
ok("seule prep_data_20 recréée (plus la table top-k unique de l'itération)", setequal(temp_tables(conn), c("prep_data_20", "prep_topk_tmp")))
ok("refs sautées (message)", any(grepl("ref ref_das_aigu : présente, sautée", log2)))
ap2 <- utils::read.csv(file.path(EXPORTS_DIR, "diagnostic_apports.csv"))
ok("diagnostic_apports : 6 lignes, relu pour 17/26 et calculé pour 20",
   nrow(ap2) == 6 && identical(ap2$statut[ap2$an == 20], c("calculé", "calculé")) && all(ap2$statut[ap2$an != 20] == "relu"))
cat_multi <- lire_cat(EXPORTS_DIR)
ok("catalogue étendu (3 années > 2 années)", nrow(cat_multi) > nrow(cat1))
fermer()

# ------------------------------------------------ run mono-session (projet 2) --
cat("\n# run mono-session (3 années d'un coup) : identité du catalogue\n")
proj2 <- creer_projet("projet_v8_mono"); Sys.setenv(SCENARIOS_PMSI_PATH = proj2)
log_mono <- sortie(lancer("extraction_associations_codes_v8.R"))
ok("mono-session : 6 itérations calculées", sum(ETAPES_ENV$plan$iterations$a_faire) == 6)
cat_mono <- lire_cat(EXPORTS_DIR)
ok("identité du catalogue final multi-sessions == mono-session", identical(cat_multi, cat_mono))
ok("identité des partiels (CH, 20)", identical(arrow::read_parquet(file.path(PARTIELS_DIR, "catalogue_partiel_CH_20.parquet")),
                                                arrow::read_parquet(file.path(dirname(dirname(PARTIELS_DIR)), "..", "projet_v8", "results", "partiels", "catalogue_partiel_CH_20.parquet"))))
fermer()
Sys.setenv(SCENARIOS_PMSI_PATH = proj)

# =============================================================== SESSION 3 ==
cat("\n# session 3 : tout présent -> rien à faire\n")
log3 <- sortie(lancer("extraction_associations_codes_v8.R"))
ok("plan : rien à faire, message explicite", ETAPES_ENV$plan$rien_a_faire && any(grepl("TOUT EST A JOUR", log3)))
ok("aucune table temporaire créée", length(temp_tables(conn)) == 0)
ok("catalogue ré-agrégé identique", identical(lire_cat(EXPORTS_DIR), cat_multi))
fermer()

# ------------------------------------------------ FORCER_REFS --
cat("\n# FORCER_REFS\n")
mt_avant <- file.info(file.path(EXPORTS_DIR, nom_ref(NOMS_REFS)))$mtime; Sys.sleep(1.1)
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)", "FORCER_REFS <- TRUE")
invisible(sortie(lancer("extraction_associations_codes_v8.R")))
ok("FORCER_REFS : 9 refs recalculées, 0 itération, année AN_REF seule, prep_das_chronique",
   all(ETAPES_ENV$plan$refs$a_faire) && !any(ETAPES_ENV$plan$iterations$a_faire) && identical(ETAPES_ENV$plan$annees_a_preparer, 26L) && ETAPES_ENV$plan$prep_das_chronique)
ok("tables temporaires : prep_data_26 et prep_das_chro_26 uniquement", setequal(temp_tables(conn), c("prep_data_26", "prep_das_chro_26")))
ok("refs réécrites (mtime)", all(file.info(file.path(EXPORTS_DIR, nom_ref(NOMS_REFS)))$mtime > mt_avant))
fermer()

# ------------------------------------------------ garde-fou partiels_meta --
cat("\n# garde-fou partiels_meta\n")
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)", "K_GRAINE_LONGS <- 3L")
err <- tryCatch({ invisible(sortie(lancer("extraction_associations_codes_v8.R"))); NULL }, error = function(e) conditionMessage(e))
ok("K_GRAINE_LONGS différent -> stop() demandant de vider PARTIELS_DIR", !is.null(err) && grepl("K_GRAINE_LONGS", err) && grepl("PARTIELS_DIR", err))
fermer()

# ------------------------------------------------ reprise des partiels --
cat("\n# reprise des partiels\n")
unlink(file.path(PARTIELS_DIR, "catalogue_partiel_CH_20.parquet"))
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)")
invisible(sortie(lancer("extraction_associations_codes_v8.R")))
ok("un partiel supprimé -> 1 itération, année 20", sum(ETAPES_ENV$plan$iterations$a_faire) == 1 && identical(ETAPES_ENV$plan$annees_a_preparer, 20L))
ok("catalogue identique après reprise", identical(lire_cat(EXPORTS_DIR), cat_multi))
fermer()

# ------------------------------------------------ CONVERSION_E669 = FALSE (toggle effectif) --
cat("\n# CONVERSION_E669 = FALSE : E669 présents, partiels bruts identiques\n")
proj3 <- creer_projet("projet_v8_noconv"); Sys.setenv(SCENARIOS_PMSI_PATH = proj3)
surcharger("ANS_HISTORIQUE <- c(17L, 26L)", "CONVERSION_E669 <- FALSE")
invisible(sortie(lancer("extraction_associations_codes_v8.R")))
cat_nc <- lire_cat(EXPORTS_DIR)
ok("toggle FALSE : ^E669 présents dans le catalogue et les refs", compter_e669(cat_nc, c("diag2", "diagnostic_associes")) > 0 &&
     compter_e669(arrow::read_parquet(file.path(EXPORTS_DIR, "ref_das_chronique.parquet")), "das") > 0 && !isTRUE(yaml::read_yaml(file.path(EXPORTS_DIR, "catalogue_longs_seuil_meta.yaml"))$CONVERSION_E669))
ok("toggle FALSE : le cas de fusion n'entre pas au catalogue (deux profils n = 1 <= seuil)", !any(cat_nc$ghm2 == "88M991"))
ok("P2 catalogue deux étages == ancien flux (conversion FALSE)", comparer(cat_nc, ancien_flux(file.path(PARTIELS_DIR, ETAPES_ENV$plan$iterations$fichier), FALSE)))
ok("toggle FALSE : partiels bruts identiques à ceux du projet converti (cache indépendant du toggle)",
   identical(arrow::read_parquet(file.path(PARTIELS_DIR, "catalogue_partiel_CHRU_26.parquet")),
             arrow::read_parquet(file.path(proj, "results", "partiels", "catalogue_partiel_CHRU_26.parquet"))) &&
     compter_e669(arrow::read_parquet(file.path(PARTIELS_DIR, "catalogue_partiel_CHRU_26.parquet")), c("diag2", "diagnostic_associes")) > 0)
fermer()
Sys.setenv(SCENARIOS_PMSI_PATH = proj)
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)")
invisible(sortie(lancer("extraction_associations_codes_v8.R")))   # replace la config du projet principal (plan : rien à faire)
fermer()

# =============================================================== TIRAGE (sans base) ==
cat("\n# tirage : phase sans base (mock interdit)\n")
options(pmsi_mock_interdit = TRUE)
ok("aucun appel pRatihque:: dans tirage_scenarios_v8.R", !any(grepl("pRatihque::", readLines(file.path(proj, "tirage_scenarios_v8.R")))))
log_t <- sortie(lancer("tirage_scenarios_v8.R"))
f_courts <- file.path(EXPORTS_DIR, "scenarios_courts_v8_" %+% DATE_TAG %+% ".parquet")
f_longs  <- file.path(EXPORTS_DIR, "scenarios_longs_tirage_v8_" %+% DATE_TAG %+% ".parquet")
ok("exports du tirage présents", all(file.exists(c(f_courts, f_longs, file.path(EXPORTS_DIR, c("selection_longs.parquet", "selection_longs_effectifs.csv", "meta_tirage.yaml",
   "echantillon_revue.csv", "top30_das_par_cmd.csv", "rapport_v8_" %+% DATE_TAG %+% ".txt"))))))
ok("chunks courts et longs écrits", length(list.files(CHUNKS_DIR, pattern = "^courts_chunk_")) >= 1 && length(list.files(CHUNKS_DIR, pattern = "^longs_chunk_")) >= 1)
sc_courts <- arrow::read_parquet(f_courts); sc_longs <- arrow::read_parquet(f_longs)
ok("identité parquet relu / objet mémoire (volumétrie du rapport)", nrow(sc_courts) == ETAPES_ENV$rapport$courts$n && nrow(sc_longs) == ETAPES_ENV$rapport$longs$n && nrow(sc_courts) > 0 && nrow(sc_longs) > 0)
ok("aucun objet df_scenarios vivant en fin de script", !exists("df_scenarios"))
sel <- arrow::read_parquet(file.path(EXPORTS_DIR, "selection_longs.parquet"))
mt <- yaml::read_yaml(file.path(EXPORTS_DIR, "meta_tirage.yaml"))
ok("quota_dp : quota exact par diag2, origine renseignée", all(table(sel$diag2) == mt$quota_par_dp) && all(grepl("^plancher_|^libre$", sel$origine)) && nrow(sel) == mt$volume_attendu)
ok("meta_tirage.yaml cohérent avec le profil", mt$PROFIL == "diagnostic" && mt$MODE_SELECTION == "quota_dp" && mt$BUDGET_TOTAL_LONGS == 120 && mt$CHUNK_SIZE_FIXE == 40 && mt$NB_CHUNKS_MAX == NB_CHUNKS_MAX && mt$nrow_catalogue == nrow(cat_multi))
ok("tirage longs indexé (quota_dp) : identique au tirage historique (comparé plus bas aux anciens scripts)", file.exists(f_longs))
ok("sidecars de chunking présents pour les deux branches, cohérents (chunk_size = 40, nb_chunks = fichiers)",
   { sc_c <- yaml::read_yaml(file.path(CHUNKS_DIR, "courts_chunks_meta.yaml")); sc_l <- yaml::read_yaml(file.path(CHUNKS_DIR, "longs_chunks_meta.yaml"))
     sc_c$chunk_size == 40 && sc_l$chunk_size == 40 && sc_c$nb_chunks == length(list.files(CHUNKS_DIR, pattern = "^courts_chunk_")) &&
       sc_l$nb_chunks == length(list.files(CHUNKS_DIR, pattern = "^longs_chunk_")) && sc_l$n == nrow(sel) && sc_c$nb_chunks > 1 && sc_l$nb_chunks > 1 })
ok("tirage : aucun ^E669 dans les sorties (diag2, graine, DAS), effectifs E660x au rapport",
   ETAPES_ENV$rapport$courts$e669_residuels == 0 && ETAPES_ENV$rapport$longs$e669_residuels == 0 && sans_e669(sc_courts, c("diag2", "diagnostic_associes")) &&
     sans_e669(sc_longs, c("diag2", "graine", "diagnostic_associes")) && any(grepl("effectifs E660x par classe", rap <- readLines(file.path(EXPORTS_DIR, "rapport_v8_" %+% DATE_TAG %+% ".txt")))))
ok("contrôles §8.2 à zéro sur les deux branches", { cc <- ETAPES_ENV$rapport$courts$controles; cl <- ETAPES_ENV$rapport$longs$controles
   cc$doublons_categorie == 0 && cc$diabete_hors_flag == 0 && cc$i10_avec_hta_autres == 0 && cc$poids_sous_seuil == 0 &&
     cl$doublons_categorie == 0 && cl$diabete_hors_flag == 0 && cl$i10_avec_hta_autres == 0 && cl$poids_sous_seuil == 0 })
ok("longs : graine conservée (hors doublon de catégorie interne), HTA et diabète cohérents",
   all(mapply(function(g, d) all(g %in% d) || (any(duplicated(substr(g, 1, 3))) && g[1] %in% d), split_das(sc_longs$graine), split_das(sc_longs$diagnostic_associes))) &&
     all(mapply(function(h, d) h == "N" || "I10" %in% d || any(d %in% hta_autres), sc_longs$hta, split_das(sc_longs$diagnostic_associes))) &&
     all(mapply(function(f, d) f == "N" || any(substr(d,1,3) %in% c("E10","E11")), sc_longs$diabete_scenario, split_das(sc_longs$diagnostic_associes))))
rev <- readr::read_csv2(file.path(EXPORTS_DIR, "echantillon_revue.csv"), show_col_types = FALSE)
ok("echantillon_revue.csv : <= 50 lignes, deux branches, libellés et [G] chez les longs",
   nrow(rev) <= 50 && setequal(unique(rev$branche), c("courts", "longs")) && any(grepl("\\[G\\]", rev$das_libelles[rev$branche == "longs"])) &&
     all(c("dp_libelle", "das_libelles", "cmd", "type_unite") %in% names(rev)) && any(grepl("BPCO", rev$dp_libelle)))
rap <- readLines(file.path(EXPORTS_DIR, "rapport_v8_" %+% DATE_TAG %+% ".txt"))
ok("rapport : meta en tête, table diag2 × type_unite, top 30, anomalies = 0", any(grepl("^== 0\\. Meta du catalogue", rap)) && any(grepl("PROFIL: diagnostic", rap)) &&
     any(grepl("effectifs sélectionnés diag2", rap)) && any(grepl("== 3\\. Top 30 DAS par CMD", rap)) && any(grepl("TOTAL anomalies = 0", rap)))
# reprise des chunks : identité bit à bit
ch_longs <- sort(list.files(CHUNKS_DIR, pattern = "^longs_chunk_", full.names = TRUE))
unlink(ch_longs[min(2, length(ch_longs))]); unlink(sort(list.files(CHUNKS_DIR, pattern = "^courts_chunk_", full.names = TRUE))[1])
log_t2 <- sortie(lancer("tirage_scenarios_v8.R"))
ok("reprise : sélection relue, chunks présents sautés", any(grepl("relue depuis", log_t2)) && any(grepl("déjà présent, sauté", log_t2)))
ok("reprise après suppression d'un chunk : parquets identiques bit à bit", identical(arrow::read_parquet(f_courts), sc_courts) && identical(arrow::read_parquet(f_longs), sc_longs))
# =============================================================== ORCHESTRATION ==
# (a) identité bit à bit avec les ANCIENS scripts d'entrée (instantanés tests/ancien_20260914),
#     mêmes fixtures, même seed, même surcharge : extraction puis tirage dans un projet dédié.
cat("\n# orchestration (a) : nouveau flux == anciens scripts d'entrée (bit à bit)\n")
options(pmsi_mock_interdit = FALSE)
proj_anc <- creer_projet("projet_v8_ancien")
for(f in c("extraction_associations_codes_v8.R", "tirage_scenarios_v8.R")) file.copy(file.path(racine, "tests", "ancien_20260914", f), file.path(proj_anc, f), overwrite = TRUE)
Sys.setenv(SCENARIOS_PMSI_PATH = proj_anc); surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)")
invisible(sortie(lancer("extraction_associations_codes_v8.R"))); fermer()
options(pmsi_mock_interdit = TRUE)
invisible(sortie(lancer("tirage_scenarios_v8.R")))
EXPORTS_ANC <- EXPORTS_DIR
Sys.setenv(SCENARIOS_PMSI_PATH = proj); surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)"); source(file.path(proj, "config_v8.R"))
meme_parquet <- function(a, b) identical(as.data.frame(arrow::read_parquet(a)), as.data.frame(arrow::read_parquet(b)))
ok("(a) catalogue et les 10 refs identiques aux anciens scripts",
   all(vapply(c("catalogue_longs_seuil.parquet", nom_ref(NOMS_REFS)), function(f) meme_parquet(file.path(EXPORTS_ANC, f), file.path(EXPORTS_DIR, f)), logical(1))))
ok("(a) scenarios_courts et scenarios_longs_tirage identiques bit à bit aux anciens scripts",
   meme_parquet(file.path(EXPORTS_ANC, basename(f_courts)), f_courts) && meme_parquet(file.path(EXPORTS_ANC, basename(f_longs)), f_longs) &&
     meme_parquet(file.path(EXPORTS_ANC, "selection_longs.parquet"), file.path(EXPORTS_DIR, "selection_longs.parquet")))
ok("(a) echantillon_revue.csv et top30 identiques", identical(readLines(file.path(EXPORTS_ANC, "echantillon_revue.csv")), readLines(file.path(EXPORTS_DIR, "echantillon_revue.csv"))) &&
     identical(readLines(file.path(EXPORTS_ANC, "top30_das_par_cmd.csv")), readLines(file.path(EXPORTS_DIR, "top30_das_par_cmd.csv"))))

# (d) étapes hors ordre -> erreur actionnable ; (e) etat_pipeline avant ; (b) étape par étape
cat("\n# orchestration (b)(d)(e) : étape par étape, hors ordre, tableau de bord\n")
proj_et <- creer_projet("projet_v8_etapes"); Sys.setenv(SCENARIOS_PMSI_PATH = proj_et, SCENARIOS_PMSI_ETAPES_SEULEMENT = "1")
options(pmsi_mock_interdit = FALSE)
invisible(sortie(lancer("extraction_associations_codes_v8.R")))   # session chargée sans exécution
etat0 <- etat_pipeline()
ok("(e) avant toute étape : refs, partiels, catalogue, courts, sélection, finalisation À FAIRE",
   all(etat0$statut[etat0$etape %in% c("etape_refs", "etape_partiels_longs", "etape_catalogue", "etape_tirage_courts", "etape_selection_longs", "etape_tirage_das_longs", "etape_finalisation")] == "À FAIRE"))
err <- tryCatch({ invisible(sortie(etape_refs())); NULL }, error = function(e) conditionMessage(e))
ok("(d) etape_refs() sans prep_data -> erreur actionnable", !is.null(err) && grepl("etape_prep_data", err))
err <- tryCatch({ invisible(sortie(etape_tirage_das_longs())); NULL }, error = function(e) conditionMessage(e))
ok("(d) etape_tirage_das_longs() sans sélection -> erreur actionnable", !is.null(err) && grepl("etape_selection_longs|manquant", err))
invisible(sortie(etape_prep_data())); invisible(sortie(etape_refs()))
ok("(b) après etape_refs : refs FAIT, partiels À FAIRE", { e <- etat_pipeline(); e$statut[e$etape == "etape_refs"] == "FAIT" && e$statut[e$etape == "etape_partiels_longs"] == "À FAIRE" })
fermer()   # déconnexion entre etape_refs et etape_partiels_longs
invisible(sortie(lancer("extraction_associations_codes_v8.R")))   # reconnexion (session chargée sans exécution)
err <- tryCatch({ invisible(sortie(etape_partiels_longs())); NULL }, error = function(e) conditionMessage(e))
ok("(b) etape_partiels_longs() après reconnexion sans prep_data -> erreur actionnable", !is.null(err) && grepl("etape_prep_data", err))
invisible(sortie(etape_prep_data()))
ok("(b) reconnexion : seules les années des partiels sont préparées, refs sautées", identical(ETAPES_ENV$plan$annees_a_preparer, c(17L, 20L, 26L)) && !any(ETAPES_ENV$plan$refs$a_faire))
invisible(sortie(etape_partiels_longs())); invisible(sortie(etape_catalogue()))
ok("(b) catalogue et refs par étapes == bout-en-bout",
   all(vapply(c("catalogue_longs_seuil.parquet", nom_ref(NOMS_REFS)), function(f) meme_parquet(file.path(EXPORTS_DIR, f), file.path(proj, "results", "exports_diagnostic", f)), logical(1))))
fermer(); rm(conn)
ok("(e) sans connexion : tables temporaires « inconnu hors connexion », catalogue FAIT", { e <- etat_pipeline(); grepl("inconnu hors connexion", e$preuve[e$etape == "etape_prep_data"]) && e$statut[e$etape == "etape_catalogue"] == "FAIT" })
options(pmsi_mock_interdit = TRUE)
invisible(sortie(lancer("tirage_scenarios_v8.R")))   # session tirage chargée sans exécution
invisible(sortie(etape_tirage_courts())); invisible(sortie(etape_selection_longs()))
invisible(sortie(lancer("tirage_scenarios_v8.R")))   # nouvelle session : la sélection et les chunks doivent être relus
ok("(b) nouvelle session : état de tirage vide", is.null(etat_tirage("selection")) && is.null(etat_tirage("df_tirage_longs")))
invisible(sortie(etape_tirage_das_longs())); invisible(sortie(etape_habillage_longs())); invisible(sortie(etape_finalisation()))
ok("(b) sorties du tirage par étapes (sessions séparées) == bout-en-bout, bit à bit",
   meme_parquet(file.path(EXPORTS_DIR, basename(f_courts)), f_courts) && meme_parquet(file.path(EXPORTS_DIR, basename(f_longs)), f_longs) &&
     identical(readLines(file.path(EXPORTS_DIR, "echantillon_revue.csv")), readLines(file.path(proj, "results", "exports_diagnostic", "echantillon_revue.csv"))))
ok("(e) après finalisation : tout FAIT", { e <- etat_pipeline(); all(e$statut[e$etape %in% c("etape_refs", "etape_partiels_longs", "etape_catalogue", "etape_tirage_courts", "etape_selection_longs", "etape_tirage_das_longs", "etape_finalisation")] == "FAIT") })
# (c) etape_catalogue à périmètre restreint
invisible(sortie(etape_catalogue(ans = c(17L, 26L), etbs = "CHR/U")))
mres <- yaml::read_yaml(file.path(EXPORTS_DIR, "catalogue_longs_seuil_meta.yaml"))
ok("(c) etape_catalogue(ans, etbs) : catalogue restreint au périmètre passé, méta cohérent",
   identical(unlist(mres$ANS_HISTORIQUE), c(17L, 26L)) && identical(unlist(mres$TYPES_ETBS_LONGS), "CHR/U") && identical(unlist(mres$perimetre_ans), c(17L, 26L)) &&
     comparer(lire_cat(EXPORTS_DIR), ancien_flux(file.path(PARTIELS_DIR, nom_partiel("CHR/U", c(17L, 26L))), TRUE)))
Sys.unsetenv("SCENARIOS_PMSI_ETAPES_SEULEMENT")
Sys.setenv(SCENARIOS_PMSI_PATH = proj); surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)"); source(file.path(proj, "config_v8.R"))
invisible(sortie(lancer("tirage_scenarios_v8.R")))   # rétablit l'état de session du projet principal (chunks présents : reprise)

# =============================================================== AVAL PRODUCTION ==
cat("\n# aval production : repartitionnement, quota_dp_fixe, tirage indexé par population, flux\n")
proj_pr <- creer_projet("projet_v8_prod"); Sys.setenv(SCENARIOS_PMSI_PATH = proj_pr, SCENARIOS_PMSI_ETAPES_SEULEMENT = "1")
options(pmsi_mock_interdit = FALSE)
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)", "MODE_SELECTION <- 'quota_dp_fixe'", "NB_CRH_CIBLE <- 200L", "NB_LIGNES_PAR_DP <- 1L", "CHUNK_SIZE_FIXE <- 30L", "LOT_CHUNKS_FINALISATION <- 2L")
invisible(sortie(lancer("extraction_associations_codes_v8.R")))
invisible(sortie(etape_prep_data())); invisible(sortie(etape_refs())); invisible(sortie(etape_partiels_longs())); invisible(sortie(etape_catalogue()))
fermer(); rm(conn); options(pmsi_mock_interdit = TRUE)
invisible(sortie(lancer("tirage_scenarios_v8.R")))
ok("diagnostic_memoire.csv écrit en fin d'etape_refs / etape_partiels_longs (avant etape_catalogue) et listé par etat_pipeline",
   { proj_m <- creer_projet("projet_v8_mem"); Sys.setenv(SCENARIOS_PMSI_PATH = proj_m); options(pmsi_mock_interdit = FALSE)
     invisible(sortie(lancer("extraction_associations_codes_v8.R"))); invisible(sortie(etape_prep_data())); invisible(sortie(etape_refs()))
     a <- file.exists(file.path(EXPORTS_DIR, "diagnostic_memoire.csv")); n1 <- nrow(utils::read.csv(file.path(EXPORTS_DIR, "diagnostic_memoire.csv")))
     invisible(sortie(etape_partiels_longs())); n2 <- nrow(utils::read.csv(file.path(EXPORTS_DIR, "diagnostic_memoire.csv")))
     e <- etat_pipeline(); fermer(); rm(conn); options(pmsi_mock_interdit = TRUE)
     # message à trois branches quand le catalogue est absent (etape_catalogue non lancée dans ce projet)
     invisible(sortie(lancer("tirage_scenarios_v8.R")))
     msg <- tryCatch({ invisible(sortie(etape_repartitionner_catalogue())); "" }, error = function(e) conditionMessage(e))
     msg2 <- tryCatch({ invisible(sortie(etape_selection_longs())); "" }, error = function(e) conditionMessage(e))
     Sys.setenv(SCENARIOS_PMSI_PATH = proj_pr); invisible(sortie(lancer("tirage_scenarios_v8.R")))
     a && n2 > n1 && grepl("diagnostic_memoire.csv : présent", e$preuve[e$etape == "etape_partiels_longs"]) &&
       grepl(sub("/$", "", file.path(proj_m, "results", "exports_diagnostic")), msg, fixed = TRUE) && grepl("copiez-le", msg) && grepl("Q13", msg) && grepl("etape_catalogue\\(\\) \\(extraction, coûteux\\)", msg) &&
       grepl("copiez-le", msg2) })
mono_avant <- as.data.frame(arrow::read_parquet(MONO_CATALOGUE()))
invisible(sortie(etape_repartitionner_catalogue()))
side <- yaml::read_yaml(file.path(DIR_CATALOGUE(), "_sidecar.yaml"))
parts <- lire_catalogue(DIR_CATALOGUE())
ok("repartitionnement : parts recomposées == monofichier d'origine + colonnes lettre/DPEC/TPEC ; monofichier renommé .ancien",
   nrow(parts) == nrow(mono_avant) && identical(as.data.frame(dplyr::arrange(parts[, names(mono_avant)], dplyr::across(dplyr::everything()))), as.data.frame(dplyr::arrange(mono_avant, dplyr::across(dplyr::everything())))) &&
     all(c("lettre", "DPEC", "TPEC") %in% names(parts)) && all(parts$lettre == substr(parts$diag2, 1, 1)) && !file.exists(MONO_CATALOGUE()) && file.exists(MONO_CATALOGUE() %+% ".ancien"))
ok("repartitionnement : sidecar (nb lignes par part == méta, sum poids, effectifs DPEC, version typologie)",
   side$nb_lignes_total == yaml::read_yaml(file.path(EXPORTS_DIR, "catalogue_longs_seuil_meta.yaml"))$nb_lignes && sum(unlist(side$sum_poids_par_part)) == sum(parts$poids) &&
     side$version_typologie == charger_typo()$version && sum(unlist(side$effectifs_dpec)) == nrow(parts) && all(parts$DPEC[substr(parts$ghm2, 3, 3) == "C"] == "Chirurgie adultes > 3 nuits"))
ok("repartitionnement : idempotent (sauté)", { o <- sortie(etape_repartitionner_catalogue()); any(grepl("sauté", o)) })
ok("repartitionnement : garde-fou version typologie", { assign("typo", modifyList(charger_typo(), list(version = "autre")), envir = ETAPES_ENV)
   e <- tryCatch({ invisible(sortie(etape_repartitionner_catalogue())); NULL }, error = function(e) conditionMessage(e)); rm("typo", envir = ETAPES_ENV); !is.null(e) && grepl("re-repartitionner", e) })
ok("etat_pipeline : repartitionnement FAIT", { e <- etat_pipeline(); e$statut[e$etape == "etape_repartitionner_catalogue"] == "FAIT" })
ok("catalogue_complet retiré : stop renvoyant vers quota_dp_fixe", grepl("quota_dp_fixe", tryCatch({ invisible(sortie(etape_selection_longs(budget = 10L, mode = "catalogue_complet"))); "" }, error = function(e) conditionMessage(e))))
invisible(sortie(etape_tirage_courts()))
invisible(sortie(etape_selection_longs()))
mt_f <- yaml::read_yaml(file.path(EXPORTS_DIR, "meta_tirage.yaml"))
sel_pops <- lapply(names(POPULATIONS), function(pp) lire_catalogue(DIR_SELECTION(pp))); names(sel_pops) <- names(POPULATIONS)
ok("quota_dp_fixe : sélection par population, budget au prorata des DP, volume total == annoncé",
   mt_f$MODE_SELECTION == "quota_dp_fixe" && sum(vapply(mt_f$par_population, function(m) m$budget_population, numeric(1))) == 200 &&
     sum(vapply(sel_pops, function(d) if(is.null(d)) 0L else sum(d$n_var), integer(1))) == mt_f$volume_attendu && mt_f$volume_attendu > 0)
ok("quota_dp_fixe : k = 1 -> une ligne par (DP × groupe) et X_dp variantes ; sans remise ; DPEC/TPEC/id_selection présents",
   all(vapply(sel_pops, function(d){ if(is.null(d)) return(TRUE); st <- utils::read.csv(file.path(DIR_SELECTION(d$population[1]), "selection_longs_stats_dp.csv"))
     all(st$k_eff <= 1) && all(st$variantes == st$X_dp) && !any(duplicated(d[, c(PIVOTS_LONGS, "diagnostic_associes")])) && all(c("DPEC", "TPEC", "id_selection", "n_var") %in% names(d)) }, logical(1))))
ok("quota_dp_fixe : X cohérent avec nb_dp et budget de la population", all(vapply(mt_f$par_population, function(m) m$X == ceiling(m$budget_population / max(m$nb_dp, 1)), logical(1))))
ok("quota_dp_fixe : sélection relue à l'identique (idempotence)", { o <- sortie(etape_selection_longs()); any(grepl("relue", o)) })
# tirage par plages disjointes (parallélisme simulé) puis run complet : identité bit à bit
invisible(sortie(etape_tirage_das_longs(chunk_range = c(1, 1))))
invisible(sortie(etape_tirage_das_longs()))
ch_pops <- lapply(names(POPULATIONS), function(pp) sort(list.files(DIR_CHUNKS_POP(pp), pattern = "^longs_chunk_.*\\.parquet$", full.names = TRUE)))
ok("tirage fixe : chunks par population avec sidecar, nb complet", all(vapply(seq_along(ch_pops), function(i){ pp <- names(POPULATIONS)[i]; f <- file.path(DIR_CHUNKS_POP(pp), "longs_chunks_meta.yaml")
   !file.exists(f) || length(ch_pops[[i]]) == yaml::read_yaml(f)$nb_chunks }, logical(1))))
lu <- function(fs) purrr::map(fs, function(f) as.data.frame(arrow::read_parquet(f))) |> purrr::list_rbind()
tir_A <- lapply(ch_pops, lu)
unlink(unlist(ch_pops)); invisible(sortie(etape_tirage_das_longs()))
ok("tirage fixe : run complet après suppression des chunks == run par plages, bit à bit", identical(tir_A, lapply(ch_pops, lu)))
tir_all <- purrr::list_rbind(tir_A)
ok("unicité souple : variantes dédoublonnées, colonne nb_variantes_demandees, aucun ^E669", !any(duplicated(tir_all[, c(PIVOTS_LONGS, "graine", "diagnostic_associes")])) && all(tir_all$nb_variantes_demandees >= 1) && sans_e669(tir_all, c("diag2", "graine", "diagnostic_associes")))
ok("une ligne et ses variantes dans le même chunk (aucune clé pivots × graine dans deux fichiers)",
   { cles_par_fichier <- lapply(unlist(ch_pops), function(f){ d <- arrow::read_parquet(f); unique(do.call(paste, c(lapply(c(PIVOTS_LONGS, "graine"), function(cc) as.character(d[[cc]])), sep = "\r"))) })
     toutes <- unlist(cles_par_fichier); length(cles_par_fichier) >= 2 && !any(duplicated(toutes)) && length(toutes) > 0 })
invisible(sortie(etape_habillage_longs()))
ok("habillage fixe : lots habillés par population avec DPEC/TPEC", all(vapply(names(POPULATIONS), function(pp){ fs <- list.files(DIR_HABILLE(pp), pattern = "^lot_", full.names = TRUE); length(fs) == 0 || all(c("DPEC", "TPEC", "mode_entree", "population") %in% names(arrow::read_parquet(fs[1]))) }, logical(1))) && sum(vapply(names(POPULATIONS), function(pp) length(list.files(DIR_HABILLE(pp), pattern = "^lot_")), integer(1))) > 0)
invisible(sortie(lancer("tirage_scenarios_v8.R")))   # session neuve : finalisation en flux depuis les fichiers
invisible(sortie(etape_finalisation()))
rap_f <- readLines(file.path(EXPORTS_DIR, "rapport_v8_" %+% DATE_TAG %+% ".txt"))
finaux <- lu(list.files(DIR_FINAL(), pattern = "^part_", recursive = TRUE, full.names = TRUE))
ok("finalisation en flux : parts finales par population == lots habillés ; DPEC/TPEC en sortie ; monofichier fusionné (volume <= seuil)",
   nrow(finaux) == sum(vapply(names(POPULATIONS), function(pp) sum(vapply(list.files(DIR_HABILLE(pp), full.names = TRUE), function(f) nrow(arrow::read_parquet(f)), integer(1))), integer(1))) &&
     all(c("DPEC", "TPEC", "population") %in% names(finaux)) && file.exists(chemin_export("scenarios_longs_tirage")) && nrow(arrow::read_parquet(chemin_export("scenarios_longs_tirage"))) == nrow(finaux))
ok("rapport fixe : réalisé vs cible, doublons éliminés, manque à gagner, anomalies = 0",
   any(grepl("réalisé vs cible", rap_f)) && any(grepl("Doublons éliminés par DP", rap_f)) && any(grepl("manque à gagner", rap_f)) && any(grepl("TOTAL anomalies = 0", rap_f)))
ok("finalisation en flux == statistiques globales (contrôles, taux) sur les mêmes lignes",
   { st <- stats_branche(finaux, PIVOTS_LONGS, ETAPES_ENV$ctx$codes_imprecis, c("diag2", "graine", "diagnostic_associes")); r <- ETAPES_ENV$rapport$longs_fixe
     cles <- c("doublons_categorie", "diabete_hors_flag", "i10_avec_hta_autres", "poids_sous_seuil")
     tot <- Reduce(`+`, lapply(r, function(x) unlist(x$stats$controles[cles]))); identical(unname(as.integer(tot)), unname(as.integer(unlist(st$controles[cles])))) && sum(vapply(r, function(x) x$stats$n, integer(1))) == st$n })
ok("etat_pipeline (fixe) : sélection, chunks par population, habillage, finalisation FAIT", { e <- etat_pipeline(); all(e$statut[e$etape %in% c("etape_selection_longs", "etape_tirage_das_longs", "etape_habillage_longs", "etape_finalisation")] == "FAIT") })
ok("echantillon_revue : courts + longs par population", { rv <- readr::read_csv2(file.path(EXPORTS_DIR, "echantillon_revue.csv"), show_col_types = FALSE); "courts" %in% rv$branche && any(grepl("^longs_", rv$branche)) })
Sys.unsetenv("SCENARIOS_PMSI_ETAPES_SEULEMENT")
Sys.setenv(SCENARIOS_PMSI_PATH = proj); surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)"); source(file.path(proj, "config_v8.R"))
options(pmsi_mock_interdit = TRUE)
invisible(sortie(lancer("tirage_scenarios_v8.R")))   # rétablit l'état de session du projet principal

# changement de paramètres -> garde-fou meta_tirage, puis mode catalogue_complet
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)", "MODE_SELECTION <- 'catalogue_complet'", "NB_CRH_CIBLE <- " %+% (3 * nrow(cat_multi)) %+% "L", "BUDGET_TOTAL_LONGS <- " %+% (3 * nrow(cat_multi)) %+% "L")
err <- tryCatch({ invisible(sortie(lancer("tirage_scenarios_v8.R"))); NULL }, error = function(e) conditionMessage(e))
ok("meta_tirage : paramètres différents -> stop() demandant de vider les chunks", !is.null(err) && grepl("meta_tirage.yaml", err))
unlink(CHUNKS_DIR, recursive = TRUE); unlink(file.path(EXPORTS_DIR, c("meta_tirage.yaml", "selection_longs.parquet")))
log_t3 <- sortie(lancer("tirage_scenarios_v8.R"))
mt3 <- yaml::read_yaml(file.path(EXPORTS_DIR, "meta_tirage.yaml"))
ok("catalogue_complet : NB_VARIANTES = 3, volume attendu = nrow × 3, variante max = 3",
   mt3$NB_VARIANTES == 3 && mt3$volume_attendu == 3 * nrow(cat_multi) && max(arrow::read_parquet(f_longs)$variante) == 3 && ETAPES_ENV$rapport$longs_tirage_n <= mt3$volume_attendu)
ok("rapport catalogue_complet : ligne nrow / NB_VARIANTES / volume", any(grepl("mode catalogue_complet : nrow catalogue", readLines(file.path(EXPORTS_DIR, "rapport_v8_" %+% DATE_TAG %+% ".txt")))))
ok("la phase tirage n'a jamais touché la base (mock interdit resté silencieux)", isTRUE(getOption("pmsi_mock_interdit")))
options(pmsi_mock_interdit = FALSE)

cat("\nSIMULATION SQLITE (scripts réels, sessions multiples) VERTE :", n_ok, "assertions ; arrow =", if(ARROW_MOCK) "mock RDS" else "réel", "\n")
