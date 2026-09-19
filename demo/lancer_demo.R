###############################################################################
# demo/lancer_demo.R — le pipeline COMPLET sur la base démo, hors plateforme (sans pRatihque)
#
# Usage (depuis la racine du dépôt), après Rscript demo/creer_base_demo.R (sinon la base est créée) :
#   Rscript demo/lancer_demo.R
# Enchaîne : demo/session_demo.R (mock pRatihque, projet démo, profil « démo ») -> extraction
# (prep_data, refs, partiels, catalogue) -> repartitionnement (typologie DPEC/TPEC) -> tirage SANS base
# (courts, sélection, DAS longs, habillage, finalisation) -> résumé. Toutes les sorties sous
# demo/resultats/ (vidé au départ : la démo repart toujours de zéro). Pour dérouler les mêmes étapes
# chunk par chunk : notebooks RUN.Rmd / RUN_aval.Rmd, chunk « Mode démo » (demo/README.md).
# Les scénarios produits sont ALÉATOIRES : aucune validité épidémiologique.
###############################################################################
racine_depot <- function(){   # dupliqué de creer_base_demo.R (les scripts doivent se suffire)
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if(length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else getwd()
  for(cand in c(d, file.path(d, ".."), getwd(), file.path(getwd(), ".."))) if(file.exists(file.path(cand, "config_v8.R"))) return(normalizePath(cand))
  stop("Racine du dépôt introuvable (config_v8.R) : lancer depuis la racine du dépôt, ex. Rscript demo/lancer_demo.R")
}
`%+%` <- function(x, y) paste0(x, y)
Sys.setenv(SCENARIOS_PMSI_DEMO_RAZ = "1"); Sys.unsetenv("SCENARIOS_PMSI_ETAPES_SEULEMENT")
source(file.path(racine_depot(), "demo", "session_demo.R"))
suppressPackageStartupMessages({library(dplyr); library(tibble); library(stringr); library(dbplyr)})
t0 <- Sys.time()

## ---- 1. Extraction (base mock) puis repartitionnement ----
source(file.path(DEMO$projet, "extraction_associations_codes_v8.R"))   # etape_prep_data, etape_refs, etape_partiels_longs, etape_catalogue
etape_repartitionner_catalogue()
DBI::dbDisconnect(conn); rm(conn)
options(pmsi_mock_interdit = TRUE)   # à partir d'ici, tout appel base stoppe

## ---- 2. Tirage (sans base) ----
source(file.path(DEMO$projet, "tirage_scenarios_v8.R"))                # courts, sélection, DAS longs, habillage, finalisation

## ---- 3. Résumé ----
chemin_export_dir <- function(x) sub("/+$", "", EXPORTS_DIR) %+% "/" %+% x   # EXPORTS_DIR se termine par "/"
lire_parts <- function(d) if(dir.exists(d)) dplyr::bind_rows(lapply(list.files(d, pattern = "^part_.*\\.parquet$", full.names = TRUE), function(f) as_tibble(arrow::read_parquet(f)))) else tibble()
longs <- dplyr::bind_rows(lapply(names(POPULATIONS), function(pp){ d <- lire_parts(DIR_FINAL(pp)); if(nrow(d)) d$population <- pp; d }))
f_courts <- chemin_export("scenarios_courts")
courts <- if(file.exists(f_courts)) as_tibble(arrow::read_parquet(f_courts)) else tibble()
cat("\n==== RÉSUMÉ DÉMO (", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min) ====\n",
    "Scénarios longs : ", nrow(longs), " (", paste(sprintf("%s = %d", names(POPULATIONS), vapply(names(POPULATIONS), function(pp) sum(longs$population == pp), integer(1))), collapse = ", "), ")\n",
    "Scénarios courts : ", nrow(courts), "\n", sep = "")
if(nrow(longs) && "TPEC" %in% names(longs)){
  cat("Répartition des longs par TPEC :\n"); print(longs |> count(TPEC, sort = TRUE) |> mutate(part = sprintf("%.1f %%", 100 * n / sum(n))), n = 50)
}
cat("\nÉchantillon de revue : ", chemin_export_dir("echantillon_revue.csv"), "\n",
    "Livrables : ", DIR_FINAL(), "/<population>/part_*.parquet (+ _meta.yaml) ; registre : ", chemin_export_dir("registre_tirages"), "\n",
    "Rapports : ", chemin_export_dir("rapport_v8_" %+% DATE_TAG %+% ".txt"), " ; tableau de bord : etat_pipeline()\n",
    "Rappel : scénarios ALÉATOIRES issus d'une base fictive, aucune validité épidémiologique.\n", sep = "")
if(nrow(longs) == 0) stop("Démo : aucun scénario long produit (échec)")
