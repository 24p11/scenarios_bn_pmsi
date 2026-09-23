###############################################################################
# extraction.R — EXTRACTION (base -> agrégats parquet), script d'entrée
#
# Pipeline scenarios_bn_pmsi v8. Fichiers : config.R (config + profils), helpers.R
# (helpers purs), etapes.R (chaînes base déplacées telles quelles + fonctions d'étape),
# tirage.R (tirage, sans base). Spécification : SPEC_V8.md ; journal :
# MODIFICATIONS_V8.md (règle d'or §0 : aucune chaîne base modifiée ; section 14 orchestration).
#
# Ce script n'appelle que les étapes, dans l'ordre : etape_prep_data() ; etape_refs() ;
# etape_partiels_longs() ; etape_catalogue(). Comportement bout-en-bout identique au flux
# antérieur (prouvé par tests/test_chaines_sqlite.R). Pour charger la session sans rien
# exécuter (notebook 01_preparation_donnees.Rmd, étapes individuelles) : SCENARIOS_PMSI_ETAPES_SEULEMENT=1.
###############################################################################

## ---- 0. Bootstrap : config, sources, connexion ----
PATH_PROJET <- Sys.getenv("SCENARIOS_PMSI_PATH", unset = "")
if(!nzchar(PATH_PROJET)){   # sinon config_locale.R (racine du dépôt, non versionné ; modèle config_locale.exemple.R) — cf. config.R
  if(file.exists("config_locale.R")){ source("config_locale.R"); PATH_PROJET <- get0("SCENARIOS_PMSI_PATH", ifnotfound = "") }
  if(!nzchar(PATH_PROJET)) stop("Racine du projet inconnue : variable d'environnement SCENARIOS_PMSI_PATH, ou config_locale.R à la racine du dépôt (copiez config_locale.exemple.R) définissant SCENARIOS_PMSI_PATH <- \"...\".", call. = FALSE)
  Sys.setenv(SCENARIOS_PMSI_PATH = PATH_PROJET)
}
source(file.path(PATH_PROJET, "config.R"))

source(file.path(PATH_PROJET, "utils.R"))
path_projet <- PATH_PROJET     # alias attendu par referentiels.R (et write_xlsx de utils.R)

conn <- pRatihque::connection_database()

source(file.path(PATH_PROJET, "exclusions.R"))
source(file.path(PATH_PROJET, "referentiels.R"))   # définit neo_codes_diabete, codes_diab, hta_autres, cim, ...
source(file.path(PATH_PROJET, "helpers.R"))
source(file.path(PATH_PROJET, "etapes.R"))

creer_dossiers(PATH_RESULTS, DIR_PARTIELS, DIR_REFERENCES, DIR_CATALOGUE_M, DIR_COURTS, DIR_DIAGNOSTICS)

cat("PROFIL = ", PROFIL, " ; AN_REF = ", AN_REF, " ; ANS_HISTORIQUE = ", paste(ANS_HISTORIQUE, collapse = ","),
    " ; TYPES_ETBS_LONGS = ", paste(TYPES_ETBS_LONGS, collapse = ","), "\n", sep = "")
cat("PATH_RESULTS = ", PATH_RESULTS, " (magasins partagés 00_partiels, 10_references, 20_catalogue, 30_courts, 90_diagnostics ; profil : ", PROFIL, "/)\n", sep = "")

## ---- 1. Étapes ----
if(!nzchar(Sys.getenv("SCENARIOS_PMSI_ETAPES_SEULEMENT"))){
  etape_prep_data()
  etape_refs()
  etape_partiels_longs()
  etape_catalogue()
  cat("Extraction terminée. Étape suivante : tirage.R (aucune connexion base).\n")
} else {
  cat("Session chargée sans exécution (SCENARIOS_PMSI_ETAPES_SEULEMENT) : étapes disponibles, etat_pipeline() pour le suivi.\n")
}
