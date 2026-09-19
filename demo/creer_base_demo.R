###############################################################################
# demo/creer_base_demo.R — construit demo/base_demo.sqlite (données FICTIVES, graine fixe)
#
# Usage (depuis la racine du dépôt) :
#   Rscript demo/creer_base_demo.R [--n=4000] [--annees=17,20,26] [--graine=20260907] [--sortie=demo/base_demo.sqlite]
# Données aléatoires uniformes dans de petits pools de codes : AUCUNE validité épidémiologique.
# Source unique du générateur : demo/generateur_donnees_fictives.R (aussi utilisé par les tests).
# Affiche le schéma de la base produite (tables et colonnes).
###############################################################################
racine_depot <- function(){   # racine = dossier contenant config.R (dupliqué dans lancer_demo.R : les deux scripts doivent se suffire)
  a <- grep("^--file=", commandArgs(), value = TRUE)
  d <- if(length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else getwd()
  for(cand in c(d, file.path(d, ".."), getwd(), file.path(getwd(), ".."))) if(file.exists(file.path(cand, "config.R"))) return(normalizePath(cand))
  stop("Racine du dépôt introuvable (config.R) : lancer depuis la racine du dépôt, ex. Rscript demo/creer_base_demo.R")
}
arg <- function(nom, defaut){ a <- grep("^--" %+% nom %+% "=", commandArgs(trailingOnly = TRUE), value = TRUE); if(length(a)) sub("^--[^=]+=", "", a[1]) else defaut }
`%+%` <- function(x, y) paste0(x, y)
racine <- racine_depot()
lib_test <- Sys.getenv("R_LIBS_TEST", unset = ""); if(nzchar(lib_test)) .libPaths(c(lib_test, .libPaths()))   # bibliothèque supplémentaire optionnelle (comme les tests)
source(file.path(racine, "demo", "generateur_donnees_fictives.R"))

n_sejours <- as.integer(arg("n", "4000")); annees <- as.integer(strsplit(arg("annees", "17,20,26"), ",")[[1]]); graine <- as.integer(arg("graine", "20260907"))
sortie <- arg("sortie", file.path(racine, "demo", "base_demo.sqlite"))
if(is.na(n_sejours) || any(is.na(annees)) || is.na(graine)) stop("Arguments invalides : --n=<entier> --annees=<aa,aa,...> --graine=<entier>")
if(file.exists(sortie)){ cat("Base existante remplacée : ", sortie, "\n", sep = ""); unlink(c(sortie, sortie %+% "-journal")) }
t0 <- Sys.time()
fx <- generer_donnees_fictives(sortie, n_sejours = n_sejours, annees = annees, graine = graine)
cat("\nBase démo écrite : ", sortie, " (", round(file.size(sortie) / 1e6, 1), " Mo, ", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), " s)\n",
    "  ", n_sejours, " séjours fictifs par millésime ; millésimes 20", paste(annees, collapse = ", 20"), " ; graine ", graine, "\n",
    "  DONNÉES ALÉATOIRES — aucune validité épidémiologique.\n\n== Schéma ==\n", sep = "")
conn <- DBI::dbConnect(RSQLite::SQLite(), sortie)
for(t in DBI::dbListTables(conn)){
  n <- DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM \"" %+% t %+% "\"")$n
  cat(sprintf("%-45s %7d lignes  [%s]\n", t, n, paste(DBI::dbListFields(conn, t), collapse = ", ")))
}
DBI::dbDisconnect(conn)
cat("\nÉtape suivante : Rscript demo/lancer_demo.R\n")
