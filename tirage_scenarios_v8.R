###############################################################################
# tirage_scenarios_v8.R — TIRAGE des scénarios (parquets -> scénarios), SANS base, script d'entrée
#
# Pipeline scenarios_bn_pmsi v8. AUCUN appel pRatihque, aucune connexion : les étapes lisent
# les produits d'EXPORTS_DIR écrits par extraction_associations_codes_v8.R (etapes_v8.R,
# famille tirage). Ce script n'appelle que les étapes, dans l'ordre : etape_tirage_courts() ;
# etape_selection_longs() ; etape_tirage_das_longs() ; etape_habillage_longs() ;
# etape_finalisation(). Pour charger la session sans rien exécuter (RUN.Rmd) :
# SCENARIOS_PMSI_ETAPES_SEULEMENT=1.
###############################################################################

## ---- 0. Bootstrap ----
PATH_PROJET <- Sys.getenv("SCENARIOS_PMSI_PATH",
                          unset = "~/commun/projets_communs/DIM_siege/divers_projets/Scenario_crh_fictifs/")
source(file.path(PATH_PROJET, "config_v8.R"))
source(file.path(PATH_PROJET, "utils.R"))
path_projet <- PATH_PROJET
source(file.path(PATH_PROJET, "exclusions.R"))
source(file.path(PATH_PROJET, "referentiels.R"))   # sans `conn` : les deux appels base de referentiels.R sont gardés par exists("conn")
source(file.path(PATH_PROJET, "helpers_v8.R"))
source(file.path(PATH_PROJET, "etapes_v8.R"))

## ---- 1. Étapes ----
if(!nzchar(Sys.getenv("SCENARIOS_PMSI_ETAPES_SEULEMENT"))){
  etape_tirage_courts()
  etape_selection_longs()
  etape_tirage_das_longs()
  etape_habillage_longs()
  etape_finalisation()
} else {
  cat("Session chargée sans exécution (SCENARIOS_PMSI_ETAPES_SEULEMENT) : étapes disponibles, etat_pipeline() pour le suivi.\n")
}
