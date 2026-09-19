###############################################################################
# tirage.R — TIRAGE des scénarios (parquets -> scénarios), SANS base, script d'entrée
#
# Pipeline scenarios_bn_pmsi v8. AUCUN appel pRatihque, aucune connexion : les étapes lisent
# les produits d'EXPORTS_DIR écrits par extraction.R (etapes.R,
# famille tirage). Ce script n'appelle que les étapes, dans l'ordre : etape_tirage_courts() ;
# etape_selection_longs() ; etape_tirage_das_longs() ; etape_habillage_longs() ;
# etape_finalisation(). Pour charger la session sans rien exécuter (RUN.Rmd) :
# SCENARIOS_PMSI_ETAPES_SEULEMENT=1.
###############################################################################

## ---- 0. Bootstrap ----
PATH_PROJET <- Sys.getenv("SCENARIOS_PMSI_PATH", unset = "")
if(!nzchar(PATH_PROJET)){   # sinon config_locale.R (racine du dépôt, non versionné ; modèle config_locale.exemple.R) — cf. config.R
  if(file.exists("config_locale.R")){ source("config_locale.R"); PATH_PROJET <- get0("SCENARIOS_PMSI_PATH", ifnotfound = "") }
  if(!nzchar(PATH_PROJET)) stop("Racine du projet inconnue : variable d'environnement SCENARIOS_PMSI_PATH, ou config_locale.R à la racine du dépôt (copiez config_locale.exemple.R) définissant SCENARIOS_PMSI_PATH <- \"...\".", call. = FALSE)
  Sys.setenv(SCENARIOS_PMSI_PATH = PATH_PROJET)
}
source(file.path(PATH_PROJET, "config.R"))
source(file.path(PATH_PROJET, "utils.R"))
path_projet <- PATH_PROJET
source(file.path(PATH_PROJET, "exclusions.R"))
source(file.path(PATH_PROJET, "referentiels.R"))   # sans `conn` : les deux appels base de referentiels.R sont gardés par exists("conn")
source(file.path(PATH_PROJET, "helpers.R"))
source(file.path(PATH_PROJET, "etapes.R"))

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
