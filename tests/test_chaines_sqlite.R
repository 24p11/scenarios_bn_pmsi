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
# Prérequis : dbplyr, DBI, RSQLite, arrow, yaml. Exécution : Rscript tests/test_chaines_sqlite.R
# [R_LIBS_TEST=<lib supplémentaire>]
###############################################################################
lib_test <- Sys.getenv("R_LIBS_TEST", unset = "")
if(nzchar(lib_test)) .libPaths(c(lib_test, .libPaths()))
for(p in c("dbplyr", "DBI", "RSQLite", "arrow", "yaml", "tidyr", "readr")) if(!requireNamespace(p, quietly = TRUE)) stop("Paquet manquant : ", p)
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
  for(f in c("config_v8.R", "helpers_v8.R", "extraction_associations_codes_v8.R", "tirage_scenarios_v8.R", "exclusions.R"))
    file.copy(file.path(racine, f), file.path(proj, f))
  file.copy(file.path(racine, "referentiels", "exclusions_paires.yaml"), file.path(proj, "referentiels", "exclusions_paires.yaml"))
  writeLines(stub_utils, file.path(proj, "utils.R"))
  writeLines(stub_referentiels, file.path(proj, "referentiels.R"))
  proj
}
proj <- creer_projet("projet_v8")
db_file <- file.path(tempdir(), "mock_v8.sqlite"); unlink(db_file)
options(pmsi_mock_db = db_file, pmsi_mock_interdit = FALSE)
Sys.setenv(SCENARIOS_PMSI_PATH = proj, SCENARIOS_PMSI_PROFIL = "diagnostic")
SURCHARGE_BASE <- c("SEUIL_PIVOT <- 1", "SEUIL_REF_PAIRES <- 5", "BUDGET_TOTAL_LONGS <- 120L", "CHUNK_SIZE <- 40L")
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
              "R2630","F050","F102","N083","E1198","I509","I500","K802","J440","C189","Z511","D649","E669")
pool_ghm <- c("04M053","05M093","06C041","10M021","03K021","14Z081","90Z001","06C042")
pool_dp  <- c("J449","I500","E1120","E102","Z511","K802","I10")
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
DBI::dbWriteTable(conn0, "nomgen.finessgeo", data.frame(finessgeo = c("750100042","750100075","920100013"), categ_pmsi = c("CHR/U","CHR/U","CH")), overwrite = TRUE)
DBI::dbWriteTable(conn0, "prd_vue_nompmsi.mco_diag_niveau",
                  data.frame(code = pool_das, v2021 = sample(1:4, length(pool_das), TRUE), v2023 = sample(1:4, length(pool_das), TRUE), v2025 = sample(1:4, length(pool_das), TRUE)), overwrite = TRUE)
DBI::dbWriteTable(conn0, "prd_vue_nompmsi.all_cim10_caract_patient",
                  data.frame(code = pool_das, type_liste = ifelse(pool_das %in% c("I10","I110","E1120","E1128","E102","E785","J449","N189","N185","F172","I48","G20","M199","E1198","I509","I500","J440","C189","E669"), "Patho_chro", "Aigu"),
                             caract = "x"), overwrite = TRUE)
DBI::dbDisconnect(conn0)
sortie <- function(expr) utils::capture.output(expr, type = "output")

# =============================================================== SESSION 1 ==
cat("\n# session 1 : extraction partielle (années 17 et 26, CHR/U et CH)\n")
surcharger("ANS_HISTORIQUE <- c(17L, 26L)")
log1 <- sortie(lancer("extraction_associations_codes_v8.R"))
ok("plan session 1 : 4 itérations, 9 refs, années 17 et 26, prep_das_chronique",
   sum(plan$iterations$a_faire) == 4 && all(plan$refs$a_faire) && identical(plan$annees_a_preparer, c(17L, 26L)) && plan$prep_das_chronique)
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
ok("exports : 9 refs + catalogue + meta + diagnostic_apports.csv",
   all(c(nom_ref(NOMS_REFS), "catalogue_longs_seuil.parquet", "catalogue_longs_seuil_meta.yaml", "diagnostic_apports.csv") %in% list.files(EXPORTS_DIR)))
ap1 <- utils::read.csv(file.path(EXPORTS_DIR, "diagnostic_apports.csv"))
ok("diagnostic_apports : 4 lignes calculées, cumuls croissants, ordre types × années",
   nrow(ap1) == 4 && all(ap1$statut == "calculé") && !is.unsorted(ap1$nb_lignes_cumul) && !is.unsorted(ap1$nb_diag2_cumul) &&
     identical(ap1$etbs, c("CHR/U", "CHR/U", "CH", "CH")) && identical(ap1$an, c(17L, 26L, 17L, 26L)) && ap1$nb_diag2_nouveaux[1] == ap1$nb_diag2_cumul[1])
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
ok("aucune ligne niveau séjour exportée (pas de colonne ident dans les parquets d'exports)",
   !any(vapply(list.files(EXPORTS_DIR, pattern = "\\.parquet$", full.names = TRUE), function(f) "ident" %in% names(arrow::read_parquet(f, as_data_frame = FALSE)), logical(1))))
fermer()

# =============================================================== SESSION 2 ==
cat("\n# session 2 : reconnexion, année 20 ajoutée -> seules les itérations manquantes\n")
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)")
log2 <- sortie(lancer("extraction_associations_codes_v8.R"))
ok("plan session 2 : 2 itérations (CHR/U 20, CH 20), 0 ref, année 20 seule, pas de prep_das_chronique",
   sum(plan$iterations$a_faire) == 2 && all(plan$iterations$an[plan$iterations$a_faire] == 20) && !any(plan$refs$a_faire) &&
     identical(plan$annees_a_preparer, 20L) && !plan$prep_das_chronique)
ok("seule prep_data_20 recréée (aucune autre table temporaire)", identical(temp_tables(conn), "prep_data_20"))
ok("refs sautées (message)", any(grepl("ref ref_das_aigu : présente, sautée", log2)))
ap2 <- utils::read.csv(file.path(EXPORTS_DIR, "diagnostic_apports.csv"))
ok("diagnostic_apports : 6 lignes, relu pour 17/26 et calculé pour 20, cumuls croissants",
   nrow(ap2) == 6 && identical(ap2$statut[ap2$an == 20], c("calculé", "calculé")) && all(ap2$statut[ap2$an != 20] == "relu") && !is.unsorted(ap2$nb_lignes_cumul))
cat_multi <- lire_cat(EXPORTS_DIR)
ok("catalogue étendu (3 années > 2 années)", nrow(cat_multi) > nrow(cat1))
fermer()

# ------------------------------------------------ run mono-session (projet 2) --
cat("\n# run mono-session (3 années d'un coup) : identité du catalogue\n")
proj2 <- creer_projet("projet_v8_mono"); Sys.setenv(SCENARIOS_PMSI_PATH = proj2)
log_mono <- sortie(lancer("extraction_associations_codes_v8.R"))
ok("mono-session : 6 itérations calculées", sum(plan$iterations$a_faire) == 6)
cat_mono <- lire_cat(EXPORTS_DIR)
ok("identité du catalogue final multi-sessions == mono-session", identical(cat_multi, cat_mono))
ok("identité des partiels (CH, 20)", identical(arrow::read_parquet(file.path(PARTIELS_DIR, "catalogue_partiel_CH_20.parquet")),
                                                arrow::read_parquet(file.path(dirname(dirname(PARTIELS_DIR)), "..", "projet_v8", "results", "partiels", "catalogue_partiel_CH_20.parquet"))))
fermer()
Sys.setenv(SCENARIOS_PMSI_PATH = proj)

# =============================================================== SESSION 3 ==
cat("\n# session 3 : tout présent -> rien à faire\n")
log3 <- sortie(lancer("extraction_associations_codes_v8.R"))
ok("plan : rien à faire, message explicite", plan$rien_a_faire && any(grepl("TOUT EST A JOUR", log3)))
ok("aucune table temporaire créée", length(temp_tables(conn)) == 0)
ok("catalogue ré-agrégé identique", identical(lire_cat(EXPORTS_DIR), cat_multi))
fermer()

# ------------------------------------------------ FORCER_REFS --
cat("\n# FORCER_REFS\n")
mt_avant <- file.info(file.path(EXPORTS_DIR, nom_ref(NOMS_REFS)))$mtime; Sys.sleep(1.1)
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)", "FORCER_REFS <- TRUE")
invisible(sortie(lancer("extraction_associations_codes_v8.R")))
ok("FORCER_REFS : 9 refs recalculées, 0 itération, année AN_REF seule, prep_das_chronique",
   all(plan$refs$a_faire) && !any(plan$iterations$a_faire) && identical(plan$annees_a_preparer, 26L) && plan$prep_das_chronique)
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
ok("un partiel supprimé -> 1 itération, année 20", sum(plan$iterations$a_faire) == 1 && identical(plan$annees_a_preparer, 20L))
ok("catalogue identique après reprise", identical(lire_cat(EXPORTS_DIR), cat_multi))
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
ok("identité parquet relu / objet mémoire (volumétrie du rapport)", nrow(sc_courts) == rapport$courts$n && nrow(sc_longs) == rapport$longs$n && nrow(sc_courts) > 0 && nrow(sc_longs) > 0)
ok("aucun objet df_scenarios vivant en fin de script", !exists("df_scenarios"))
sel <- arrow::read_parquet(file.path(EXPORTS_DIR, "selection_longs.parquet"))
mt <- yaml::read_yaml(file.path(EXPORTS_DIR, "meta_tirage.yaml"))
ok("quota_dp : quota exact par diag2, origine renseignée", all(table(sel$diag2) == mt$quota_par_dp) && all(grepl("^plancher_|^libre$", sel$origine)) && nrow(sel) == mt$volume_attendu)
ok("meta_tirage.yaml cohérent avec le profil", mt$PROFIL == "diagnostic" && mt$MODE_SELECTION == "quota_dp" && mt$BUDGET_TOTAL_LONGS == 120 && mt$CHUNK_SIZE == 40 && mt$nrow_catalogue == nrow(cat_multi))
ok("contrôles §8.2 à zéro sur les deux branches", { cc <- rapport$courts$controles; cl <- rapport$longs$controles
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
# changement de paramètres -> garde-fou meta_tirage, puis mode catalogue_complet
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)", "MODE_SELECTION <- 'catalogue_complet'", "BUDGET_TOTAL_LONGS <- " %+% (3 * nrow(cat_multi)) %+% "L")
err <- tryCatch({ invisible(sortie(lancer("tirage_scenarios_v8.R"))); NULL }, error = function(e) conditionMessage(e))
ok("meta_tirage : paramètres différents -> stop() demandant de vider les chunks", !is.null(err) && grepl("meta_tirage.yaml", err))
unlink(CHUNKS_DIR, recursive = TRUE); unlink(file.path(EXPORTS_DIR, c("meta_tirage.yaml", "selection_longs.parquet")))
log_t3 <- sortie(lancer("tirage_scenarios_v8.R"))
mt3 <- yaml::read_yaml(file.path(EXPORTS_DIR, "meta_tirage.yaml"))
ok("catalogue_complet : NB_VARIANTES = 3, volume attendu = nrow × 3, variante max = 3",
   mt3$NB_VARIANTES == 3 && mt3$volume_attendu == 3 * nrow(cat_multi) && max(arrow::read_parquet(f_longs)$variante) == 3 && rapport$longs_tirage_n <= mt3$volume_attendu)
ok("rapport catalogue_complet : ligne nrow / NB_VARIANTES / volume", any(grepl("mode catalogue_complet : nrow catalogue", readLines(file.path(EXPORTS_DIR, "rapport_v8_" %+% DATE_TAG %+% ".txt")))))
ok("la phase tirage n'a jamais touché la base (mock interdit resté silencieux)", isTRUE(getOption("pmsi_mock_interdit")))
options(pmsi_mock_interdit = FALSE)

cat("\nSIMULATION SQLITE (scripts réels, sessions multiples) VERTE :", n_ok, "assertions\n")
