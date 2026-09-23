###############################################################################
# demo/session_demo.R — prépare une SESSION en mode démo (notebooks 01_preparation_donnees.Rmd / 02_campagne.Rmd, lancer_demo.R ;
# 03_outils_maintenance.Rmd est hors démo)
#
# À sourcer AVANT le chunk `session` d'un notebook (chunk « Mode démo (optionnel) »), depuis la racine
# du dépôt. Données FICTIVES et ALÉATOIRES : aucune validité épidémiologique. Sur la plateforme : ne
# pas l'exécuter. Effets (tous hors du dépôt versionné) :
#   - installe dans tempdir() le faux paquet pRatihque (SQLite) et, si arrow manque, le mock arrow ;
#   - pose la base demo/base_demo.sqlite (créée si absente, générateur demo/generateur_donnees_fictives.R) ;
#   - copie le code dans demo/resultats/projet_demo/ avec les stubs utils/referentiels ;
#   - écrit la surcharge du profil « démo » (demo/resultats/surcharge_demo.R) et pose les variables
#     d'environnement SCENARIOS_PMSI_PATH / _PROFIL = production / _SURCHARGE / _DEMO = 1.
# Ne vide PAS demo/resultats/ (les notebooks s'enchaînent) sauf SCENARIOS_PMSI_DEMO_RAZ=1 (lancer_demo.R).
# Campagne : SCENARIOS_PMSI_DEMO_CAMPAGNE (défaut "DEMO"). Résultat : liste DEMO (racine, base, dossier, projet).
###############################################################################
local({
  racine_depot <- function(){
    a <- grep("^--file=", commandArgs(), value = TRUE)
    d <- if(length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else getwd()
    for(cand in c(d, file.path(d, ".."), getwd(), file.path(getwd(), ".."))) if(file.exists(file.path(cand, "config.R"))) return(normalizePath(cand))
    stop("Racine du dépôt introuvable (config.R) : lancer depuis la racine du dépôt (source(\"demo/session_demo.R\"))")
  }
  `%+%` <- function(x, y) paste0(x, y)
  racine <- racine_depot()
  lib_test <- Sys.getenv("R_LIBS_TEST", unset = ""); if(nzchar(lib_test)) .libPaths(c(lib_test, .libPaths()))
  for(p in c("dplyr", "dbplyr", "purrr", "tidyr", "stringr", "tibble", "yaml", "DBI", "RSQLite", "readr")) if(!requireNamespace(p, quietly = TRUE)) stop("Paquet manquant : ", p)
  if(!requireNamespace("openssl", quietly = TRUE) && !requireNamespace("digest", quietly = TRUE)) stop("Paquet manquant : openssl ou digest (identifiants sha256)")
  for(loc in c("fr_FR.UTF-8", "en_US.UTF-8", "C.UTF-8")) if(!is.na(suppressWarnings(Sys.setlocale("LC_CTYPE", loc))) && Sys.getlocale("LC_CTYPE") == loc) break
  source(file.path(racine, "demo", "mock_pratihque.R"), local = TRUE)
  base_demo <- file.path(racine, "demo", "base_demo.sqlite")
  if(!file.exists(base_demo)){
    cat("Base démo absente : création de ", base_demo, " (4000 séjours × 3 millésimes, graine fixe)\n", sep = "")
    source(file.path(racine, "demo", "generateur_donnees_fictives.R"), local = TRUE)
    generer_donnees_fictives(base_demo)
  }
  if(!requireNamespace("arrow", quietly = TRUE)){ cat("arrow absent : repli mock RDS (fichiers .parquet = RDS, lisibles seulement par ce mock)\n"); installer_mock_arrow() }
  installer_mock_pratihque()
  options(pmsi_mock_db = base_demo, pmsi_mock_interdit = FALSE)
  conn_b <- DBI::dbConnect(RSQLite::SQLite(), base_demo)
  annees_base <- sort(as.integer(sub("^PRD_VUE_MCOBL_20([0-9]{2})\\.fixe$", "\\1", grep("^PRD_VUE_MCOBL_20[0-9]{2}\\.fixe$", DBI::dbListTables(conn_b), value = TRUE))))
  DBI::dbDisconnect(conn_b)
  if(length(annees_base) == 0) stop("Aucune vue PRD_VUE_MCOBL_20xx.fixe dans " %+% base_demo %+% " : recréer la base (Rscript demo/creer_base_demo.R)")
  dossier_res <- file.path(racine, "demo", "resultats")
  if(nzchar(Sys.getenv("SCENARIOS_PMSI_DEMO_RAZ")) && dir.exists(dossier_res)){ cat("demo/resultats/ existant vidé (SCENARIOS_PMSI_DEMO_RAZ)\n"); unlink(dossier_res, recursive = TRUE) }
  if(!dir.exists(dossier_res)) dir.create(dossier_res, recursive = TRUE)
  proj <- creer_projet_stub("projet_demo", racine, dossier = dossier_res)
  campagne <- Sys.getenv("SCENARIOS_PMSI_DEMO_CAMPAGNE", unset = "DEMO")
  f_surcharge <- file.path(dossier_res, "surcharge_demo.R")
  writeLines(c(
    "# Profil « démo » : surcharge du profil production (SCENARIOS_PMSI_SURCHARGE), écrite par demo/session_demo.R",
    "ANS_HISTORIQUE <- c(" %+% paste0(annees_base, "L", collapse = ", ") %+% ")   # millésimes présents dans la base démo",
    "AN_REF <- " %+% max(annees_base) %+% "L",
    "SEUIL_PIVOT <- 1 ; SEUIL_REF_DAS <- 5 ; SEUIL_REF_IMPRECIS <- 5 ; SEUIL_REF_PAIRES <- 5   # seuils abaissés : base minuscule",
    "NB_CRH_CIBLE <- 2000L ; NB_LIGNES_PAR_DP <- 1L ; MODE_SELECTION <- 'quota_dp_fixe'",
    "CAMPAGNE <- '" %+% campagne %+% "' ; REGISTRE_ACTIF <- TRUE",
    "PAIRES_RECOUVREMENT <- list(c('CHR/U', " %+% min(annees_base) %+% ", " %+% max(annees_base) %+% "))",
    "PATH_RESULTS <- '" %+% dossier_res %+% "/'   # arborescence par étapes sous demo/resultats/ (00_partiels ... production/60_export_final)"),
    f_surcharge)
  Sys.setenv(SCENARIOS_PMSI_PATH = proj, SCENARIOS_PMSI_PROFIL = "production", SCENARIOS_PMSI_SURCHARGE = f_surcharge, SCENARIOS_PMSI_DEMO = "1")
  cat("\n#### MODE DÉMO scenarios_bn_pmsi — base ", base_demo, " ; millésimes 20", paste(annees_base, collapse = ", 20"), " ; campagne ", campagne,
      "\n#### sorties : ", dossier_res, " ; projet (copie + stubs) : ", proj, "\n#### Données FICTIVES et ALÉATOIRES : aucune validité épidémiologique.\n\n", sep = "")
  assign("DEMO", list(racine = racine, base = base_demo, dossier = dossier_res, projet = proj, surcharge = f_surcharge, campagne = campagne, annees = annees_base), envir = globalenv())
})
