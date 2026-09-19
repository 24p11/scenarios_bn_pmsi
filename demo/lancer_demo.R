###############################################################################
# demo/lancer_demo.R — le pipeline COMPLET sur la base démo, hors plateforme (sans pRatihque)
#
# Usage (depuis la racine du dépôt), après Rscript demo/creer_base_demo.R :
#   Rscript demo/lancer_demo.R
# Enchaîne : mock pRatihque (SQLite) -> projet démo (copie du code, stubs utils/referentiels)
# -> profil « démo » (surcharge : millésimes de la base, seuils abaissés, NB_CRH_CIBLE = 2000)
# -> extraction (prep_data, refs, partiels, catalogue) -> repartitionnement (typologie DPEC/TPEC)
# -> tirage SANS base (courts, sélection, DAS longs, habillage, finalisation) -> résumé.
# Toutes les sorties sous demo/resultats/ (vidé au départ : la démo repart toujours de zéro).
# Les scénarios produits sont ALÉATOIRES : aucune validité épidémiologique.
###############################################################################
racine_depot <- function(){   # dupliqué de creer_base_demo.R (les deux scripts doivent se suffire)
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if(length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else getwd()
  for(cand in c(d, file.path(d, ".."), getwd(), file.path(getwd(), ".."))) if(file.exists(file.path(cand, "config_v8.R"))) return(normalizePath(cand))
  stop("Racine du dépôt introuvable (config_v8.R) : lancer depuis la racine du dépôt, ex. Rscript demo/lancer_demo.R")
}
`%+%` <- function(x, y) paste0(x, y)
racine <- racine_depot()
lib_test <- Sys.getenv("R_LIBS_TEST", unset = ""); if(nzchar(lib_test)) .libPaths(c(lib_test, .libPaths()))
for(p in c("dplyr", "dbplyr", "purrr", "tidyr", "stringr", "tibble", "yaml", "DBI", "RSQLite", "readr")) if(!requireNamespace(p, quietly = TRUE)) stop("Paquet manquant : ", p)
if(!requireNamespace("openssl", quietly = TRUE) && !requireNamespace("digest", quietly = TRUE)) stop("Paquet manquant : openssl ou digest (identifiants sha256)")
for(loc in c("fr_FR.UTF-8", "en_US.UTF-8", "C.UTF-8")) if(!is.na(suppressWarnings(Sys.setlocale("LC_CTYPE", loc))) && Sys.getlocale("LC_CTYPE") == loc) break
suppressPackageStartupMessages({library(dplyr); library(tibble); library(stringr); library(dbplyr)})
source(file.path(racine, "demo", "mock_pratihque.R"))

## ---- 1. Base démo, faux paquets ----
base_demo <- file.path(racine, "demo", "base_demo.sqlite")
if(!file.exists(base_demo)) stop("Base démo absente : " %+% base_demo %+% "\nLa créer d'abord : Rscript demo/creer_base_demo.R")
ARROW_MOCK <- !requireNamespace("arrow", quietly = TRUE)
if(ARROW_MOCK){ cat("arrow absent : repli mock RDS (fichiers .parquet = RDS, lisibles seulement par ce mock)\n"); installer_mock_arrow() }
installer_mock_pratihque()
options(pmsi_mock_db = base_demo, pmsi_mock_interdit = FALSE)
conn_b <- DBI::dbConnect(RSQLite::SQLite(), base_demo)
annees_base <- sort(as.integer(sub("^PRD_VUE_MCOBL_20([0-9]{2})\\.fixe$", "\\1", grep("^PRD_VUE_MCOBL_20[0-9]{2}\\.fixe$", DBI::dbListTables(conn_b), value = TRUE))))
DBI::dbDisconnect(conn_b)
if(length(annees_base) == 0) stop("Aucune vue PRD_VUE_MCOBL_20xx.fixe dans " %+% base_demo %+% " : recréer la base (Rscript demo/creer_base_demo.R)")

## ---- 2. Projet démo et profil « démo » (surcharge) ----
dossier_res <- file.path(racine, "demo", "resultats")
if(dir.exists(dossier_res)){ cat("demo/resultats/ existant vidé (la démo repart de zéro)\n"); unlink(dossier_res, recursive = TRUE) }
dir.create(dossier_res, recursive = TRUE)
proj <- creer_projet_stub("projet_demo", racine, dossier = dossier_res)
f_surcharge <- file.path(dossier_res, "surcharge_demo.R")
writeLines(c(
  "# Profil « démo » : surcharge du profil production (SCENARIOS_PMSI_SURCHARGE), écrite par demo/lancer_demo.R",
  "ANS_HISTORIQUE <- c(" %+% paste0(annees_base, "L", collapse = ", ") %+% ")   # millésimes présents dans la base démo",
  "AN_REF <- " %+% max(annees_base) %+% "L",
  "SEUIL_PIVOT <- 1 ; SEUIL_REF_DAS <- 5 ; SEUIL_REF_IMPRECIS <- 5 ; SEUIL_REF_PAIRES <- 5   # seuils abaissés : base minuscule",
  "NB_CRH_CIBLE <- 2000L ; NB_LIGNES_PAR_DP <- 1L ; MODE_SELECTION <- 'quota_dp_fixe'",
  "CAMPAGNE <- 'DEMO' ; REGISTRE_ACTIF <- TRUE",
  "PAIRES_RECOUVREMENT <- list(c('CHR/U', " %+% min(annees_base) %+% ", " %+% max(annees_base) %+% "))",
  "PATH_RESULTS <- '" %+% dossier_res %+% "/' ; EXPORTS_DIR <- PATH_RESULTS %+% 'exports_demo/' ; PARTIELS_DIR <- PATH_RESULTS %+% 'partiels/'"),
  f_surcharge)
Sys.setenv(SCENARIOS_PMSI_PATH = proj, SCENARIOS_PMSI_PROFIL = "production", SCENARIOS_PMSI_SURCHARGE = f_surcharge)
Sys.unsetenv("SCENARIOS_PMSI_ETAPES_SEULEMENT")
t0 <- Sys.time()
cat("\n#### DÉMO scenarios_bn_pmsi — base ", base_demo, " ; millésimes 20", paste(annees_base, collapse = ", 20"), " ; sorties ", dossier_res, "\n",
    "#### Données FICTIVES et ALÉATOIRES : aucune validité épidémiologique.\n\n", sep = "")

## ---- 3. Extraction (base mock) puis repartitionnement ----
source(file.path(proj, "extraction_associations_codes_v8.R"))   # etape_prep_data, etape_refs, etape_partiels_longs, etape_catalogue
etape_repartitionner_catalogue()
DBI::dbDisconnect(conn); rm(conn)
options(pmsi_mock_interdit = TRUE)   # à partir d'ici, tout appel base stoppe

## ---- 4. Tirage (sans base) ----
source(file.path(proj, "tirage_scenarios_v8.R"))                # courts, sélection, DAS longs, habillage, finalisation

## ---- 5. Résumé ----
lire_parts <- function(d) if(dir.exists(d)) dplyr::bind_rows(lapply(list.files(d, pattern = "^part_.*\\.parquet$", full.names = TRUE), function(f) as_tibble(arrow::read_parquet(f)))) else tibble()
longs <- dplyr::bind_rows(lapply(names(POPULATIONS), function(pp){ d <- lire_parts(DIR_FINAL(pp)); if(nrow(d)) d$population <- pp; d }))
chemin_export <- function(x) sub("/+$", "", EXPORTS_DIR) %+% "/" %+% x   # EXPORTS_DIR se termine par "/"
f_courts <- chemin_export("scenarios_courts_v8_" %+% DATE_TAG %+% ".parquet")
courts <- if(file.exists(f_courts)) as_tibble(arrow::read_parquet(f_courts)) else tibble()
cat("\n==== RÉSUMÉ DÉMO (", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min) ====\n",
    "Scénarios longs : ", nrow(longs), " (", paste(sprintf("%s = %d", names(POPULATIONS), vapply(names(POPULATIONS), function(pp) sum(longs$population == pp), integer(1))), collapse = ", "), ")\n",
    "Scénarios courts : ", nrow(courts), "\n", sep = "")
if(nrow(longs) && "TPEC" %in% names(longs)){
  cat("Répartition des longs par TPEC :\n"); print(longs |> count(TPEC, sort = TRUE) |> mutate(part = sprintf("%.1f %%", 100 * n / sum(n))), n = 50)
}
cat("\nÉchantillon de revue : ", chemin_export("echantillon_revue.csv"), "\n",
    "Livrables : ", DIR_FINAL(), "/<population>/part_*.parquet ; registre : ", chemin_export("registre_tirages"), "\n",
    "Rapports : ", chemin_export("rapport_v8_" %+% DATE_TAG %+% ".txt"), " ; tableau de bord : etat_pipeline()\n",
    "Rappel : scénarios ALÉATOIRES issus d'une base fictive, aucune validité épidémiologique.\n", sep = "")
if(nrow(longs) == 0) stop("Démo : aucun scénario long produit (échec)")
