###############################################################################
# demo/executer_notebook.R — exécute un notebook de parcours (01_preparation_donnees.Rmd, 02_campagne.Rmd) en MODE DÉMO,
# chunk par chunk, DE HAUT EN BAS — c'est le test de la promesse « un notebook = un parcours, tout s'exécute dans l'ordre ».
#
# Usage (racine du dépôt) : Rscript demo/executer_notebook.R [--raz] 01_preparation_donnees.Rmd
#   --raz : vide demo/resultats/ avant (SCENARIOS_PMSI_DEMO_RAZ). Enchaînement CI : --raz 01_preparation_donnees.Rmd, puis 02_campagne.Rmd.
#   03_outils_maintenance.Rmd est HORS démo (boîte à outils d'exception, tous ses chunks en eval=FALSE) : refusé ici.
# Pourquoi pas rmarkdown::render : les notebooks posent knitr::opts_chunk$set(eval = FALSE) (un Knit ne doit
# jamais lancer le pipeline) ; ce lanceur lit les chunks ```{r ...} dans l'ordre et les évalue dans
# l'environnement global, comme un clic sur « Run » : chunk `opts` sauté ; chunk `mode_demo` exécuté
# (malgré son eval=FALSE, c'est le mode démo) ; chunks portant l'option demo=FALSE sautés (aucun dans 01 et 02
# depuis le chantier « notebooks par parcours » : l'option reste reconnue) ; tout autre chunk exécuté, la première
# erreur arrête (code de sortie non nul). View() est remplacé par un head(), les graphiques vont dans un pdf nul.
# Données FICTIVES : aucune validité épidémiologique.
###############################################################################
# Tout le lanceur vit dans local() : ses variables survivent au rm(list = ls()) du chunk `session` (les chunks
# sont évalués dans globalenv, comme dans RStudio).
local({
  args <- commandArgs(trailingOnly = TRUE)
  raz <- "--raz" %in% args; fichier <- setdiff(args, "--raz")
  if(length(fichier) != 1 || !file.exists(fichier)) stop("Usage : Rscript demo/executer_notebook.R [--raz] <01_preparation_donnees.Rmd | 02_campagne.Rmd> (depuis la racine du dépôt)")
  if(grepl("^03_", basename(fichier))) stop("03_outils_maintenance.Rmd est hors mode démo : boîte à outils d'exception (chunks tous en eval=FALSE, à exécuter un par un en connaissance de cause sur un répertoire de travail réel).")
  if(!file.exists("config.R")) stop("À lancer depuis la racine du dépôt (config.R introuvable dans ", getwd(), ")")
  if(raz) Sys.setenv(SCENARIOS_PMSI_DEMO_RAZ = "1") else Sys.unsetenv("SCENARIOS_PMSI_DEMO_RAZ")
  lib_test <- Sys.getenv("R_LIBS_TEST", unset = ""); if(nzchar(lib_test)) .libPaths(c(lib_test, .libPaths()))
  pdf(NULL)
  attach(list(View = function(x, ...) invisible(print(utils::head(x)))), name = "demo_runner", warn.conflicts = FALSE)

  lire_chunks <- function(f){
    l <- readLines(f, warn = FALSE); deb <- grep("^```\\{r", l); chunks <- list()
    for(i in deb){
      fin <- i + which(grepl("^```\\s*$", l[(i + 1):length(l)]))[1]
      entete <- sub("^```\\{r\\s*([^}]*)\\}.*$", "\\1", l[i]); nom <- trimws(strsplit(entete, ",")[[1]][1])
      chunks[[length(chunks) + 1]] <- list(nom = if(nzchar(nom)) nom else "sans_nom", options = entete, code = l[(i + 1):(fin - 1)], ligne = i)
    }
    chunks
  }
  chunks <- lire_chunks(fichier)
  cat("#### ", fichier, " : ", length(chunks), " chunks (mode démo, de haut en bas)\n", sep = "")
  t_tot <- Sys.time(); n_exec <- 0L
  for(ch in chunks){
    saute <- ch$nom == "opts" || grepl("demo\\s*=\\s*FALSE", ch$options)
    if(saute){ cat("\n---- chunk `", ch$nom, "` (l.", ch$ligne, ") : sauté (", if(ch$nom == "opts") "options knitr" else "demo=FALSE", ")\n", sep = ""); next }
    cat("\n---- chunk `", ch$nom, "` (l.", ch$ligne, ") ----\n", sep = "")
    t0 <- Sys.time()
    res <- tryCatch({ eval(parse(text = ch$code), envir = globalenv()); NULL }, error = function(e) conditionMessage(e))
    if(!is.null(res)) stop("chunk `", ch$nom, "` de ", fichier, " (l.", ch$ligne, ") : ", res, call. = FALSE)
    n_exec <- n_exec + 1L
    cat(sprintf("---- chunk `%s` : ok (%.1f s)\n", ch$nom, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
  cat(sprintf("\n#### %s : %d chunks exécutés, %d sautés, %.1f min — VERT (mode démo)\n", fichier, n_exec, length(chunks) - n_exec, as.numeric(difftime(Sys.time(), t_tot, units = "mins"))))
})
