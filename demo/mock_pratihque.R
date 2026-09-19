###############################################################################
# demo/mock_pratihque.R — environnement HORS PLATEFORME (source unique, extrait des tests)
#
# Trois mécaniques, extraites de tests/test_chaines_sqlite.R et tests/test_helpers.R (chantier
# « packaging + démo ») ; les tests et le mode démo les sourcent depuis ici. Ce fichier ne
# définit que des fonctions (aucun effet de bord au source).
#
# 1. installer_mock_pratihque(lib) : construit et installe dans `lib` (tempdir par défaut) un
#    faux paquet `pRatihque` dont connection_database() ouvre une connexion RSQLite sur le
#    fichier getOption("pmsi_mock_db", ":memory:") et atihble(conn, nom) = dplyr::tbl(conn, nom).
#    option pmsi_mock_interdit = TRUE : tout appel stoppe (preuve que la phase tirage ne touche
#    jamais la base). Le vrai paquet pRatihque (ATIH) n'est pas distribué ici.
# 2. installer_mock_arrow(lib) : faux paquet `arrow` de repli (write_parquet/read_parquet =
#    saveRDS/readRDS). Limite : fichiers RDS nommés .parquet, valables seulement relus par le même
#    mock dans la même session. Les scripts de production exigent le vrai arrow.
# 3. creer_projet_stub(nom, racine, dossier) : copie du projet (config, helpers, étapes, scripts
#    d'entrée, exclusions, referentiels/*.yaml) dans dossier/nom, avec utils.R et referentiels.R
#    remplacés par des STUBS (pas d'Excel, pas de tidyverse, pas de base) : STUB_UTILS,
#    STUB_REFERENTIELS (listes de codes de la doctrine diabète/HTA, mini table CIM avec libellés).
###############################################################################
installer_paquet_mock <- function(nom, description, namespace, code, lib){
  dir.create(lib, showWarnings = FALSE, recursive = TRUE)
  pkg <- file.path(tempdir(), "src_mock_" %+% nom, nom); unlink(dirname(pkg), recursive = TRUE)
  dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
  writeLines(description, file.path(pkg, "DESCRIPTION")); writeLines(namespace, file.path(pkg, "NAMESPACE"))
  writeLines(code, file.path(pkg, "R", "mock.R"))
  utils::install.packages(pkg, repos = NULL, type = "source", lib = lib, quiet = TRUE)
  .libPaths(c(lib, .libPaths()))
  if(!requireNamespace(nom, quietly = TRUE)) stop("Installation du paquet mock " %+% nom %+% " échouée (lib = " %+% lib %+% ")")
  invisible(lib)
}
if(!exists("%+%")) `%+%` <- function(x, y) paste0(x, y)

installer_mock_pratihque <- function(lib = file.path(tempdir(), "lib_mock")){
  for(p in c("DBI", "RSQLite", "dplyr")) if(!requireNamespace(p, quietly = TRUE)) stop("Paquet manquant : ", p)
  installer_paquet_mock("pRatihque",
    c("Package: pRatihque", "Version: 0.0.0.9000", "Title: Mock", "Description: Mock SQLite fichier pour tests et demo hors base.",
      "License: MIT", "Encoding: UTF-8", "Imports: DBI, RSQLite, dplyr"),
    "export(atihble, connection_database)",
    c("connection_database <- function(){",
      "  if(isTRUE(getOption('pmsi_mock_interdit'))) stop('APPEL BASE INTERDIT : connection_database() pendant la phase tirage')",
      "  DBI::dbConnect(RSQLite::SQLite(), getOption('pmsi_mock_db', ':memory:'))",
      "}",
      "atihble <- function(conn, name){",
      "  if(isTRUE(getOption('pmsi_mock_interdit'))) stop('APPEL BASE INTERDIT : atihble() pendant la phase tirage')",
      "  dplyr::tbl(conn, name)",
      "}"), lib)
}

installer_mock_arrow <- function(lib = file.path(tempdir(), "lib_mock_arrow")){
  installer_paquet_mock("arrow",
    c("Package: arrow", "Version: 0.0.0.9000", "Title: Mock", "Description: Mock arrow (RDS) pour tests et demo hors base.",
      "License: MIT", "Encoding: UTF-8"),
    "export(write_parquet, read_parquet)",
    c("write_parquet <- function(x, sink, ...) saveRDS(x, sink)",
      "read_parquet  <- function(file, ...)  readRDS(file)"), lib)
}

STUB_UTILS <- c("`%+%` <- function(x,y){paste0(x,y)}", "prep_grep <- function(x) paste(x, collapse = \"|\")")
STUB_REFERENTIELS <- c(
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
FICHIERS_PROJET <- c("config_v8.R", "helpers_v8.R", "etapes_v8.R", "extraction_associations_codes_v8.R", "tirage_scenarios_v8.R", "exclusions.R")
creer_projet_stub <- function(nom, racine, dossier = tempdir()){
  proj <- file.path(dossier, nom); unlink(proj, recursive = TRUE); dir.create(file.path(proj, "referentiels"), recursive = TRUE)
  for(f in FICHIERS_PROJET){
    if(!file.exists(file.path(racine, f))) stop("Fichier introuvable dans la racine du dépôt (" %+% racine %+% ") : " %+% f)
    file.copy(file.path(racine, f), file.path(proj, f))
  }
  for(f in c("exclusions_paires.yaml", "typologie_sejours.yaml"))
    file.copy(file.path(racine, "referentiels", f), file.path(proj, "referentiels", f))
  writeLines(STUB_UTILS, file.path(proj, "utils.R"))
  writeLines(STUB_REFERENTIELS, file.path(proj, "referentiels.R"))
  proj
}
