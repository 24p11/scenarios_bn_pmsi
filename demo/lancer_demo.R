###############################################################################
# demo/lancer_demo.R — le pipeline COMPLET sur la base démo, hors plateforme (sans pRatihque)
#
# Usage (depuis la racine du dépôt), après Rscript demo/creer_base_demo.R (sinon la base est créée) :
#   Rscript demo/lancer_demo.R
# Enchaîne : demo/session_demo.R (mock pRatihque, projet démo, profil « démo ») -> extraction
# (prep_data, refs dont le tirable courts, partiels, catalogue) -> repartitionnement (typologie DPEC/TPEC) -> tirage SANS base
# (sélection, courts de la campagne, DAS longs, habillage, finalisation) -> résumé. Toutes les sorties sous
# demo/resultats/ (vidé au départ : la démo repart toujours de zéro). Pour dérouler les mêmes étapes
# chunk par chunk : notebooks RUN.Rmd / RUN_aval.Rmd, chunk « Mode démo » (demo/README.md).
# Les scénarios produits sont ALÉATOIRES : aucune validité épidémiologique.
###############################################################################
racine_depot <- function(){   # dupliqué de creer_base_demo.R (les scripts doivent se suffire)
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if(length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else getwd()
  for(cand in c(d, file.path(d, ".."), getwd(), file.path(getwd(), ".."))) if(file.exists(file.path(cand, "config.R"))) return(normalizePath(cand))
  stop("Racine du dépôt introuvable (config.R) : lancer depuis la racine du dépôt, ex. Rscript demo/lancer_demo.R")
}
`%+%` <- function(x, y) paste0(x, y)
Sys.setenv(SCENARIOS_PMSI_DEMO_RAZ = "1"); Sys.unsetenv("SCENARIOS_PMSI_ETAPES_SEULEMENT")
source(file.path(racine_depot(), "demo", "session_demo.R"))
suppressPackageStartupMessages({library(dplyr); library(tibble); library(stringr); library(dbplyr)})
t0 <- Sys.time()

## ---- 1. Extraction (base mock) puis repartitionnement ----
source(file.path(DEMO$projet, "extraction.R"))   # etape_prep_data, etape_refs, etape_partiels_longs, etape_catalogue
etape_repartitionner_catalogue()
DBI::dbDisconnect(conn); rm(conn)
options(pmsi_mock_interdit = TRUE)   # à partir d'ici, tout appel base stoppe

## ---- 2. Tirage (sans base) ----
source(file.path(DEMO$projet, "tirage.R"))                # sélection, courts de la campagne, DAS longs, habillage, finalisation

## ---- 3. Résumé ----
livrable <- lire_corpus_final(CAMPAGNE)
longs <- livrable[livrable$branche == "long", , drop = FALSE]; courts <- livrable[livrable$branche == "court", , drop = FALSE]
cat("\n==== RÉSUMÉ DÉMO (", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min) ====\n",
    "Livrable unique : ", FICHIER_LIVRABLE(), " (", nrow(livrable), " lignes)\n",
    "Scénarios longs : ", nrow(longs), " (", paste(sprintf("%s = %d", names(POPULATIONS), vapply(names(POPULATIONS), function(pp) sum(longs$population == pp, na.rm = TRUE), integer(1))), collapse = ", "), ")\n",
    "Scénarios courts : ", nrow(courts), " lignes, ", dplyr::n_distinct(courts$id_scenario), " scénarios tirés POUR la campagne (", FICHIER_COURTS_CAMPAGNE(), " ; ratio réalisé ", yaml::read_yaml(FICHIER_LIVRABLE_META())$ratio_courts_realise, ")\n", sep = "")
if(nrow(longs) && "TPEC" %in% names(longs)){
  cat("Répartition des longs par TPEC :\n"); print(longs |> count(TPEC, sort = TRUE) |> mutate(part = sprintf("%.1f %%", 100 * n / sum(n))), n = 50)
}
cat("\nÉchantillon de revue : ", FICHIER_REVUE(), "\nRapport : ", FICHIER_RAPPORT(), " ; méta : ", FICHIER_LIVRABLE_META(), " ; registre : ", DIR_REGISTRE(), "\n",
    "Arborescence : ", PATH_RESULTS, " (00_partiels, 10_references, 20_catalogue, 30_courts = le tirable courts, 90_diagnostics [partagés] ; production/40_campagnes (dont chunks_courts), 50_registre (deux branches), 60_export_final) ; tableau de bord : etat_pipeline()\n",
    "Rappel : scénarios ALÉATOIRES issus d'une base fictive, aucune validité épidémiologique.\n", sep = "")
if(nrow(longs) == 0) stop("Démo : aucun scénario long produit (échec)")
