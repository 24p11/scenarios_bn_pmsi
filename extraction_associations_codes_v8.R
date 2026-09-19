###############################################################################
# extraction_associations_codes_v8.R — EXTRACTION (base -> agrégats parquet), script d'entrée
#
# Pipeline scenarios_bn_pmsi v8. Fichiers : config_v8.R (config + profils), helpers_v8.R
# (helpers purs), etapes_v8.R (chaînes base déplacées telles quelles + fonctions d'étape),
# tirage_scenarios_v8.R (tirage, sans base). Spécification : SPEC_V8.md ; journal :
# MODIFICATIONS_V8.md (règle d'or §0 : aucune chaîne base modifiée ; section 14 orchestration).
#
# Ce script n'appelle que les étapes, dans l'ordre : etape_prep_data() ; etape_refs() ;
# etape_partiels_longs() ; etape_catalogue(). Comportement bout-en-bout identique au flux
# antérieur (prouvé par tests/test_chaines_sqlite.R). Pour charger la session sans rien
# exécuter (RUN.Rmd, étapes individuelles) : SCENARIOS_PMSI_ETAPES_SEULEMENT=1.
###############################################################################

## ---- 0. Bootstrap : config, sources, connexion ----
PATH_PROJET <- Sys.getenv("SCENARIOS_PMSI_PATH", unset = "")
if(!nzchar(PATH_PROJET)){   # sinon config_locale.R (racine du dépôt, non versionné ; modèle config_locale.exemple.R) — cf. config_v8.R
  if(file.exists("config_locale.R")){ source("config_locale.R"); PATH_PROJET <- get0("SCENARIOS_PMSI_PATH", ifnotfound = "") }
  if(!nzchar(PATH_PROJET)) stop("Racine du projet inconnue : variable d'environnement SCENARIOS_PMSI_PATH, ou config_locale.R à la racine du dépôt (copiez config_locale.exemple.R) définissant SCENARIOS_PMSI_PATH <- \"...\".", call. = FALSE)
  Sys.setenv(SCENARIOS_PMSI_PATH = PATH_PROJET)
}
source(file.path(PATH_PROJET, "config_v8.R"))

source(file.path(PATH_PROJET, "utils.R"))
path_projet <- PATH_PROJET     # alias attendu par referentiels.R (et write_xlsx de utils.R)
outfile     <- PATH_RESULTS    # alias historique

conn <- pRatihque::connection_database()

source(file.path(PATH_PROJET, "exclusions.R"))
source(file.path(PATH_PROJET, "referentiels.R"))   # définit neo_codes_diabete, codes_diab, hta_autres, cim, ...
source(file.path(PATH_PROJET, "helpers_v8.R"))
source(file.path(PATH_PROJET, "etapes_v8.R"))

for(d in c(PATH_RESULTS, EXPORTS_DIR, PARTIELS_DIR)) if(!dir.exists(d)) dir.create(d, recursive = TRUE)

cat("PROFIL = ", PROFIL, " ; AN_REF = ", AN_REF, " ; ANS_HISTORIQUE = ", paste(ANS_HISTORIQUE, collapse = ","),
    " ; TYPES_ETBS_LONGS = ", paste(TYPES_ETBS_LONGS, collapse = ","), "\n", sep = "")
cat("EXPORTS_DIR = ", EXPORTS_DIR, "\nPARTIELS_DIR = ", PARTIELS_DIR, "\n", sep = "")

## ---- 1. Étapes ----
if(!nzchar(Sys.getenv("SCENARIOS_PMSI_ETAPES_SEULEMENT"))){
  etape_prep_data()
  etape_refs()
  etape_partiels_longs()
  etape_catalogue()
  cat("Extraction terminée. Étape suivante : tirage_scenarios_v8.R (aucune connexion base).\n")
} else {
  cat("Session chargée sans exécution (SCENARIOS_PMSI_ETAPES_SEULEMENT) : étapes disponibles, etat_pipeline() pour le suivi.\n")
}
