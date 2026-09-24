###############################################################################
# tests/test_chaines_sqlite.R — les SCRIPTS RÉELS du v8 sur une base SQLite FICHIER
#
# Objet : exécuter extraction.R puis tirage.R tels
# quels, dans un projet temporaire (config/helpers/scripts copiés, utils.R et referentiels.R
# remplacés par des stubs sans Excel ni base), avec un faux paquet `pRatihque` dont
# atihble = dplyr::tbl et connection_database = nouvelle connexion SQLite sur un fichier :
# les tables sources persistent entre « sessions », les tables temporaires disparaissent
# à la déconnexion. Vérifie : traduction/exécution des chaînes dbplyr (dont §5.9a, B1-10),
# résolution des besoins en sessions multiples, cache des partiels, reprise, FORCER_REFS,
# garde-fou partiels_meta, phase tirage sans base (mock interdit), reprise des chunks,
# identité parquet, livrables. Ne valide PAS le dialecte ni les colonnes de la base réelle.
#
# Prérequis : dbplyr, DBI, RSQLite, yaml (arrow : mock RDS de repli si absent). Exécution : Rscript tests/test_chaines_sqlite.R
# [R_LIBS_TEST=<lib supplémentaire>]. Le faux paquet, les stubs et le générateur de données fictives sont
# sourcés depuis demo/ (source unique partagée avec le mode démo : demo/mock_pratihque.R,
# demo/generateur_donnees_fictives.R).
###############################################################################
lib_test <- Sys.getenv("R_LIBS_TEST", unset = "")
if(nzchar(lib_test)) .libPaths(c(lib_test, .libPaths()))
for(p in c("dbplyr", "DBI", "RSQLite", "yaml", "tidyr", "readr")) if(!requireNamespace(p, quietly = TRUE)) stop("Paquet manquant : ", p)
# Environnement hors plateforme (source unique, chantier « packaging + démo ») : faux paquets pRatihque
# et arrow (repli RDS, tests/démo uniquement), stubs utils/referentiels, générateur de données fictives.
racine <- normalizePath(c(".", "..")[file.exists(c("config.R", "../config.R"))][1])
`%+%` <- function(x, y) paste0(x, y)
source(file.path(racine, "demo", "mock_pratihque.R"))
source(file.path(racine, "demo", "generateur_donnees_fictives.R"))
ARROW_MOCK <- !requireNamespace("arrow", quietly = TRUE)
if(ARROW_MOCK) installer_mock_arrow()
suppressPackageStartupMessages({library(dplyr); library(tibble); library(stringr); library(dbplyr)})
for(loc in c("fr_FR.UTF-8", "en_US.UTF-8", "C.UTF-8")) if(!is.na(suppressWarnings(Sys.setlocale("LC_CTYPE", loc))) && Sys.getlocale("LC_CTYPE") == loc) break
n_ok <- 0
ok <- function(nom, expr){ if(!isTRUE(expr)) stop("ECHEC : " %+% nom); n_ok <<- n_ok + 1; cat("  ok  ", nom, "\n") }

# ------------------------------------------------ faux paquet pRatihque (fichier) --
installer_mock_pratihque()   # demo/mock_pratihque.R

# ------------------------------------------------ projet temporaire --
creer_projet <- function(nom) creer_projet_stub(nom, racine)   # demo/mock_pratihque.R : copie du code + stubs utils/referentiels
proj <- creer_projet("projet_v8")
db_file <- file.path(tempdir(), "mock_v8.sqlite"); unlink(db_file)
options(pmsi_mock_db = db_file, pmsi_mock_interdit = FALSE)
Sys.setenv(SCENARIOS_PMSI_PATH = proj, SCENARIOS_PMSI_PROFIL = "diagnostic")
# CHUNK_SIZE_FIXE = 40 : force des cas multi-chunks sur les petites fixtures (reprise et garde-fou
# réellement exercés) ; CHUNK_SIZE <- 40L n'est lu que par les anciens scripts d'entrée (référence d'identité).
# NB_CRH_CIBLE = 120 (nouveau nom) ; BUDGET_TOTAL_LONGS = 120 n'est lu que par les anciens scripts d'entrée.
SURCHARGE_BASE <- c("SEUIL_PIVOT <- 1", "SEUIL_REF_PAIRES <- 5", "NB_CRH_CIBLE <- 120L", "BUDGET_TOTAL_LONGS <- 120L", "CHUNK_SIZE_FIXE <- 40L", "CHUNK_SIZE <- 40L",
                    'PAIRES_RECOUVREMENT <- list(c("CHR/U", 17, 26), c("CH", 24, 25))')
surcharger <- function(...){
  f <- file.path(tempdir(), "surcharge.R"); writeLines(c(SURCHARGE_BASE, ...), f); Sys.setenv(SCENARIOS_PMSI_SURCHARGE = f)
}
temp_tables <- function(conn){ x <- DBI::dbGetQuery(conn, "SELECT name FROM sqlite_temp_master WHERE type = 'table'")$name; x[!grepl("^sqlite_", x)] }
fermer <- function(){ if(exists("conn", envir = globalenv()) && DBI::dbIsValid(get("conn", envir = globalenv()))) DBI::dbDisconnect(get("conn", envir = globalenv())) }
lancer <- function(script, ...){ source(file.path(Sys.getenv("SCENARIOS_PMSI_PATH"), script), local = FALSE, ...) }
lire_cat <- function(dir) arrange(as_tibble(arrow::read_parquet(file.path(dir, "catalogue_longs_seuil.parquet"))), across(everything()))
lire_longs <- function() lire_corpus_final(CAMPAGNE, branche = "long")   # branche longs du livrable unique de la campagne courante
# compare un ancien frame (schéma historique) aux lignes du livrable unifié : mêmes colonnes, types de l'ancien
meme_contenu <- function(a, b){ a <- as.data.frame(a); b <- as.data.frame(b); identical(names(a), names(b)) && identical(a[do.call(order, c(a, list(method = "radix"))), , drop = FALSE] |> `rownames<-`(NULL), b[do.call(order, c(b, list(method = "radix"))), , drop = FALSE] |> `rownames<-`(NULL)) }   # même contenu, ordre des lignes indifférent (dataset arrow)
meme_modulo_schema <- function(ancien, nouveau){ n <- nouveau[, names(ancien), drop = FALSE]; for(cc in names(ancien)) n[[cc]] <- coercer(n[[cc]], class(ancien[[cc]])[1]); identical(as.data.frame(ancien), as.data.frame(n)) }

# ------------------------------------------------ tables factices (base fichier) --
# demo/generateur_donnees_fictives.R (source unique) : N = 4000 par millésime (17, 20, 26), graine 20260907,
# fixtures B1-10 (IDENT_B110) et fusion E669 (IDENT_FUSION, GHM 88M991) sur le millésime 26.
fx <- generer_donnees_fictives(db_file); IDENT_B110 <- fx$IDENT_B110; IDENT_FUSION <- fx$IDENT_FUSION
sortie <- function(expr) utils::capture.output(expr, type = "output")

# =============================================================== SESSION 1 ==
cat("\n# session 1 : extraction partielle (années 17 et 26, CHR/U et CH)\n")
surcharger("ANS_HISTORIQUE <- c(17L, 26L)")
log1 <- sortie(lancer("extraction.R"))
ok("plan session 1 : 4 itérations, 10 refs, années 17 et 26, prep_das_chronique",
   sum(ETAPES_ENV$plan$iterations$a_faire) == 4 && all(ETAPES_ENV$plan$refs$a_faire) && identical(ETAPES_ENV$plan$annees_a_preparer, c(17L, 26L)) && ETAPES_ENV$plan$prep_das_chronique)
tt <- temp_tables(conn)
ok("tables temporaires créées : prep_data_17, prep_data_26, prep_das_chro_26 ; pas de prep_data_20",
   all(c("prep_data_17", "prep_data_26", "prep_das_chro_26") %in% tt) && !"prep_data_20" %in% tt)
pd <- pRatihque::atihble(conn, "prep_data_26") |> dplyr::collect()
ok("prep_data_26 : colonnes attendues, GHM 90 exclus, flags", all(c("anonyme","ident","mode_hospit","mode_entree","mode_sortie","sexe","categ_pmsi","age","cage2","cage","racine","ghm2",
   "diabete","hta","diag2","mdp","rumdudp","nbda","duree","type_unite","prep_sc","raac") %in% names(pd)) && !any(substr(pd$ghm2,1,2) == "90") &&
   all(pd$diabete %in% c("N","E10","E11i","E11ni")) && all(pd$hta %in% c("N","I10")))
ok("B1-10 : un ident = une ligne ; §5.9a : une ligne par (anonyme, ghm2)", !any(duplicated(pd$ident)) && !any(duplicated(pd[, c("anonyme","ghm2")])))
b110 <- pd |> dplyr::filter(ident %in% IDENT_B110) |> dplyr::arrange(ident)
ok("B1-10 : HC+SC -> SC/prep_sc 1 ; HC+UHCD -> HC ; UHCD seul -> UHCD ; GERIATRIE+SC -> SC",
   identical(b110$ident, unname(IDENT_B110)) && identical(b110$type_unite, c("SC", "HC", "UHCD", "SC")) && identical(b110$prep_sc, c(1, 0, 0, 1)))
ok("règle cage2 : mineur > 14 ans en GHM C -> ge_18", { p <- pd |> dplyr::filter(age == "lt_18", substr(ghm2,3,3) == "C", cage == "[15-18["); nrow(p) == 0 || all(p$cage2 == "ge_18") })
ok("diag2 = DR quand DP en Z", all(pd$diag2[pd$mdp != "DP"] == "C189"))
pd17 <- pRatihque::atihble(conn, "prep_data_17") |> dplyr::collect()
ok("millésime 17 : mêmes colonnes, raac NA, une ligne par ident", identical(names(pd17), names(pd)) && all(is.na(pd17$raac)) && !any(duplicated(pd17$ident)))
ok("magasin 00_partiels : 4 parquet + _meta.yaml (clés bloquantes)",
   setequal(list.files(DIR_PARTIELS), c("catalogue_partiel_CHRU_17.parquet", "catalogue_partiel_CHRU_26.parquet", "catalogue_partiel_CH_17.parquet", "catalogue_partiel_CH_26.parquet", "_meta.yaml")) && yaml::read_yaml(FICHIER_PARTIELS_META())$magasin == "partiels")
ok("arborescence par étapes : 10_references (9 ref_* + _meta.yaml), 30_courts (le tirable ref_pivots_courts + _meta.yaml), 20_catalogue (monofichier + méta + rapport), 90_diagnostics (apports, recouvrement, mémoire par profil)",
   setequal(list.files(DIR_REFERENCES), c(nom_ref(setdiff(NOMS_REFS, "ref_pivots_courts")), "_meta.yaml")) && setequal(list.files(DIR_COURTS), c("ref_pivots_courts.parquet", "_meta.yaml")) && all(grepl("^ref_", nom_ref(NOMS_REFS))) && yaml::read_yaml(FICHIER_REFERENCES_META())$magasin == "references" &&
     all(c("catalogue_longs_seuil.parquet", "catalogue_longs_seuil_meta.yaml", "rapport_extraction.txt") %in% list.files(DIR_CATALOGUE_M)) &&
     all(c("diagnostic_apports.csv", "recouvrement.csv", "diagnostic_memoire_diagnostic.csv") %in% list.files(DIR_DIAGNOSTICS)) &&
     basename(sub("/$", "", DIR_PARTIELS)) == "00_partiels" && basename(sub("/$", "", DIR_REFERENCES)) == "10_references" && basename(sub("/$", "", DIR_CATALOGUE_M)) == "20_catalogue" && basename(sub("/$", "", DIR_COURTS)) == "30_courts")
ap1 <- utils::read.csv(FICHIER_APPORTS())
ok("diagnostic_apports : 4 lignes calculées, stats du partiel seul (sans colonnes cumul), ordre types × années",
   nrow(ap1) == 4 && all(ap1$statut == "calculé") && identical(names(ap1), c("etbs", "an", "statut", "nb_lignes_partiel", "sum_n_partiel", "nb_diag2_partiel", "nb_diag2_nouveaux")) &&
     identical(ap1$etbs, c("CHR/U", "CHR/U", "CH", "CH")) && identical(ap1$an, c(17L, 26L, 17L, 26L)) && ap1$nb_diag2_nouveaux[1] == ap1$nb_diag2_partiel[1] && all(ap1$sum_n_partiel >= ap1$nb_lignes_partiel))
# refs : contrôles de contenu
df_das_ref <- arrow::read_parquet(file.path(DIR_REFERENCES, "ref_das_aigu.parquet"))
ok("ref_das_aigu : strate + das + n, sans diabète/I10/astérisques", all(c("mode_hospit","sexe","cage","racine","ghm2","diag2","das","n") %in% names(df_das_ref)) && nrow(df_das_ref) > 0 &&
     !any(df_das_ref$das %in% c(code_did, code_dnid, code_dnid_ins, codes_astrisques_diabete, "I10")))
df_chro <- arrow::read_parquet(file.path(DIR_REFERENCES, "ref_das_chronique.parquet"))
ok("ref_das_chronique : Patho_chro, néo-codes appliqués", all(df_chro$type_liste == "Patho_chro") && !any(df_chro$das %in% c(code_did, code_dnid, code_dnid_ins)) && any(df_chro$das %in% neo_codes_diabete))
df_nbc <- arrow::read_parquet(file.path(DIR_REFERENCES, "ref_nb_chroniques.parquet"))
ok("ref_nb_chroniques : zéros inclus, total = séjours longs distincts", any(df_nbc$nb_chro == 0) && sum(df_nbc$nb) == nrow(dplyr::distinct(pd |> dplyr::filter(duree > DUREE_MIN_REF), ident, cage, sexe)))
df_imp <- arrow::read_parquet(file.path(DIR_REFERENCES, "ref_substitution_imprecis.parquet"))
ok("§7.5 : catégories imprécises, niveau joint, seuil", all(df_imp$cat %in% c("I50","J44","N18")) && all(c("cat","code","cage","sexe","nb","niveau","imprecis") %in% names(df_imp)) && all(df_imp$nb >= SEUIL_REF_IMPRECIS) && any(df_imp$code == "N185"))
df_pair <- arrow::read_parquet(file.path(DIR_REFERENCES, "ref_paires_chroniques.parquet"))
ok("§7.6 : paires das_a < das_b, seuil", nrow(df_pair) > 0 && all(df_pair$das_a < df_pair$das_b) && all(df_pair$nb >= SEUIL_REF_PAIRES))
df_pc <- arrow::read_parquet(FICHIER_PIVOTS_COURTS())
ok("pivots_courts : pivots + nb > seuil ; le TIRABLE vit dans 30_courts (+ _meta.yaml : ANS_COURTS, seuils), pas dans 10_references",
   all(c(PIVOTS_COURTS, "nb") %in% names(df_pc)) && all(df_pc$nb > SEUIL_PIVOT) && nrow(df_pc) > 0 && grepl("/30_courts/ref_pivots_courts\\.parquet$", FICHIER_PIVOTS_COURTS()) &&
     !file.exists(file.path(DIR_REFERENCES, "ref_pivots_courts.parquet")) && file.exists(FICHIER_COURTS_META()) && identical(unlist(yaml::read_yaml(FICHIER_COURTS_META())$ANS_COURTS), 26L) && yaml::read_yaml(FICHIER_COURTS_META())$n_pivots == nrow(df_pc))
ok("ref_v_admin_longs : photographie SANS nbda (décision revue clinique), colonnes = clés CLES_ADMIN_LONGS + admin + duree + effectif n ; méta des références porte CLES_ADMIN_LONGS, ANS_COURTS, DUREE_LONGS / DUREE_COURTS",
   { va <- arrow::read_parquet(file.path(DIR_REFERENCES, "ref_v_admin_longs.parquet")); mr <- yaml::read_yaml(FICHIER_REFERENCES_META())
     !"nbda" %in% names(va) && setequal(names(va), c(CLES_ADMIN_LONGS, COLS_ADMIN, "duree", "n")) && !anyDuplicated(va[, setdiff(names(va), "n")]) && all(va$n >= 1) && identical(unlist(mr$CLES_ADMIN_LONGS), CLES_ADMIN_LONGS) && identical(unlist(mr$ANS_COURTS), 26L) &&
       identical(as.integer(unlist(mr$DUREE_LONGS)), as.integer(DUREE_LONGS)) && identical(as.integer(unlist(mr$DUREE_COURTS)), as.integer(DUREE_COURTS)) })
ok("Q72 : aucun candidat admin hors du périmètre de durée de sa branche (longs : DUREE_LONGS ; courts : DUREE_COURTS), la base contenant des séjours des deux durées pour un même profil ; effectifs n = séjours (somme == séjours du périmètre)",
   { va <- arrow::read_parquet(file.path(DIR_REFERENCES, "ref_v_admin_longs.parquet")); vc <- arrow::read_parquet(file.path(DIR_REFERENCES, "ref_v_admin_courts.parquet"))
     pd <- dplyr::collect(dplyr::tbl(conn, "prep_data_26")); prof <- do.call(paste, pd[, CLES_ADMIN_LONGS])
     all(va$duree %in% DUREE_LONGS) && all(vc$duree %in% DUREE_COURTS) && sum(va$n) == sum(pd$duree %in% DUREE_LONGS) && sum(vc$n) == sum(pd$duree %in% DUREE_COURTS) &&
       length(intersect(unique(prof[pd$duree %in% DUREE_COURTS]), unique(prof[pd$duree %in% DUREE_LONGS]))) > 0 })
ok("type_unite côté courts (§26) : ref_v_admin_courts photographié AVEC type_unite (doctrine UHCD de prep_data), ref_v_admin_longs sans ; effectifs par (strate × type_unite) == séjours courts de prep_data (hors E66x convertis) ; des strates portent UHCD ET HC ; filtre de durée et somme des effectifs inchangés ; méta des références porte COLS_ADMIN_COURTS",
   { vc <- arrow::read_parquet(file.path(DIR_REFERENCES, "ref_v_admin_courts.parquet")); va <- arrow::read_parquet(file.path(DIR_REFERENCES, "ref_v_admin_longs.parquet")); mr <- yaml::read_yaml(FICHIER_REFERENCES_META())
     pd <- dplyr::collect(dplyr::tbl(conn, "prep_data_26")); pdc <- pd[pd$duree %in% DUREE_COURTS & !grepl("^E66", pd$diag2), ]; cles <- c("mode_hospit", "sexe", "cage", "ghm2", "diag2", "duree", "type_unite")
     att <- pdc |> dplyr::count(dplyr::across(dplyr::all_of(cles)), name = "n_att"); obs <- vc[!grepl("^E66", vc$diag2), ] |> dplyr::summarise(n_obs = sum(n), .by = dplyr::all_of(cles))
     j <- dplyr::full_join(att, obs, by = cles); multi <- vc |> dplyr::summarise(k = dplyr::n_distinct(type_unite), .by = dplyr::all_of(c("mode_hospit", "sexe", "cage", "ghm2", "diag2", "duree")))
     cat("   photographie courts :", nrow(vc), "combinaisons ;", sum(multi$k >= 2), "strates avec plusieurs types d'unité ; UHCD :", sum(vc$n[vc$type_unite == "UHCD"]), "séjours\n")
     "type_unite" %in% names(vc) && !"type_unite" %in% names(va) && setequal(names(vc), c(PIVOTS_COURTS, COLS_ADMIN_COURTS, "n")) && all(!is.na(vc$type_unite)) && all(vc$duree %in% DUREE_COURTS) &&
       nrow(j) == nrow(att) && !any(is.na(j$n_obs)) && all(j$n_att == j$n_obs) && any(multi$k >= 2) && "UHCD" %in% vc$type_unite && sum(vc$n) == sum(pd$duree %in% DUREE_COURTS) &&
       identical(unlist(mr$COLS_ADMIN_COURTS), COLS_ADMIN_COURTS) && !"type_unite" %in% PIVOTS_COURTS })
ok("statut des références au méta du magasin 10_references (Q88, Q93 actées, §26.7) : un statut par référence ; ref_substitution_imprecis et ref_paires_chroniques exportables (7 internes / 2 exportables) ; photographies v_admin (non seuillées) internes ; toutes les autres internes par défaut",
   { mr <- yaml::read_yaml(FICHIER_REFERENCES_META()); st <- unlist(mr$statut)
     setequal(names(st), setdiff(NOMS_REFS, "ref_pivots_courts")) && st[["ref_substitution_imprecis"]] == "exportable" && st[["ref_paires_chroniques"]] == "exportable" && st[["ref_v_admin_courts"]] == "interne" && st[["ref_v_admin_longs"]] == "interne" &&
       sum(st == "exportable") == 2 && sum(st == "interne") == 7 && all(st %in% c("exportable", "interne")) })
ok("ref_comp_diabete : effectifs bruts (pas de pénalisation côté extraction)", { r <- arrow::read_parquet(file.path(DIR_REFERENCES, "ref_comp_diabete.parquet")); all(r$nb == round(r$nb)) && all(c("cage","diabete","comp","nb") %in% names(r)) })
cat1 <- lire_cat(DIR_CATALOGUE_M)
ok("catalogue seuil : poids > SEUIL_PIVOT, pas de colonne n, graine <= K sans diabète/I10",
   all(cat1$poids > SEUIL_PIVOT) && !"n" %in% names(cat1) && all(lengths(split_das(cat1$diagnostic_associes)) <= K_GRAINE_LONGS) &&
     !any(unlist(split_das(cat1$diagnostic_associes)) %in% c(code_did, code_dnid, code_dnid_ins, "I10")))
meta1 <- yaml::read_yaml(FICHIER_CATALOGUE_MONO_META())
ok("meta.yaml cohérent avec le profil et la surcharge", meta1$PROFIL == "diagnostic" && identical(unlist(meta1$ANS_HISTORIQUE), c(17L, 26L)) && meta1$SEUIL_PIVOT == 1 &&
     meta1$nb_lignes == nrow(cat1) && meta1$MODE_SELECTION == "quota_dp" && meta1$K_GRAINE_LONGS == K_GRAINE_LONGS && meta1$plan_iterations_calculees == 4)
# --- conversion E669 -> E660 (CONVERSION_E669 = TRUE par défaut)
sans_e669 <- function(df, cols) compter_e669(df, cols) == 0
ok("conversion : aucun ^E669 dans le catalogue (diag2, graines)", CONVERSION_E669 && sans_e669(cat1, c("diag2", "diagnostic_associes")) && any(grepl("^E660", cat1$diag2)))
ok("conversion : aucun ^E669 dans les refs (aigu, chronique, paires, imprécis, pivots, v_admin)",
   sans_e669(df_das_ref, c("diag2", "das")) && sans_e669(df_chro, c("diag2", "das")) && sans_e669(df_pair, c("das_a", "das_b")) &&
     sans_e669(df_imp, "code") && sans_e669(df_pc, "diag2") &&
     sans_e669(arrow::read_parquet(file.path(DIR_REFERENCES, "ref_v_admin_courts.parquet")), "diag2") && sans_e669(arrow::read_parquet(file.path(DIR_REFERENCES, "ref_v_admin_longs.parquet")), "diag2"))
ok("conversion : somme des n inchangée sur le catalogue agrégé, distribution E660x exportée",
   ETAPES_ENV$impact_e669$n_total_avant == ETAPES_ENV$impact_e669$n_total_apres && ETAPES_ENV$impact_e669$e669_diag2_suffixe + ETAPES_ENV$impact_e669$e669_graine_suffixe > 0 &&
     file.exists(file.path(DIR_REFERENCES, "ref_distribution_e660.parquet")) && nrow(arrow::read_parquet(file.path(DIR_REFERENCES, "ref_distribution_e660.parquet"))) > 0)
ok("conversion : les partiels restent en codes BRUTS (E669 présents)",
   any(vapply(list.files(DIR_PARTIELS, pattern = "_26\\.parquet$", full.names = TRUE), function(f){ d <- arrow::read_parquet(f); compter_e669(d, c("diag2", "diagnostic_associes")) > 0 }, logical(1))))
ok("conversion : cas de fusion sous-seuil -> au-dessus (GHM 88M991, DP E6690 + E6600 -> E6600, n = 2 > 1)",
   { f <- cat1[cat1$ghm2 == "88M991", ]; nrow(f) == 1 && f$diag2 == "E6600" && f$poids == 2 && ETAPES_ENV$impact_e669$profils_entres >= 1 })
ok("conversion : paires das_a < das_b, aucune paire identique", all(df_pair$das_a < df_pair$das_b))
ok("conversion : meta.yaml porte CONVERSION_E669 et BARE_E669_DEFAUT, rapport d'extraction écrit",
   isTRUE(meta1$CONVERSION_E669) && meta1$BARE_E669_DEFAUT == "0" && file.exists(FICHIER_RAPPORT_EXTRACTION()) &&
     any(grepl("ENTRÉS par fusion", readLines(FICHIER_RAPPORT_EXTRACTION()))))
ok("aucune ligne niveau séjour exportée (pas de colonne ident dans les parquets des magasins)",
   !any(vapply(c(list.files(DIR_REFERENCES, pattern = "\\.parquet$", full.names = TRUE), list.files(DIR_CATALOGUE_M, pattern = "\\.parquet$", full.names = TRUE)), function(f) "ident" %in% names(arrow::read_parquet(f, as_data_frame = FALSE)), logical(1))))
# --- chantier mémoire : P1 équivalence, P2 catalogue deux étages, P3 libération
cat("\n# mémoire : équivalence P1, catalogue deux étages, recouvrement, libération\n")
prep_scenarios2_ancien_20260912<-function(an,type_etbs,nb_journees_aut,nbda_aut,nb_assoc_das,pivots){
  
  anseqta = anseqta_de(an)
  
  
  pRatihque::atihble(conn, 'prep_data_' %+% an ) |>
    dplyr::filter(prep_sc==1) |> 
    dplyr::distinct(ident,prep_sc) |> dplyr::rename(sc= prep_sc) |> 
    dplyr::full_join(pRatihque::atihble(conn, 'prep_data_' %+% an )) |>
    dplyr::mutate(sc = ifelse(is.na(sc),0,1)) |> 
    dplyr::filter(! (prep_sc==0 & sc==1)) |>
    dplyr::filter(categ_pmsi %in% type_etbs,nbda%in%1:nbda_aut,duree %in%nb_journees_aut) |> 
    dplyr::rename(rum =  rumdudp) |> 
    dplyr::left_join(pRatihque::atihble(conn, "PRD_VUE_MCOBL_20" %+% an %+% '.diag') |> 
                       dplyr::filter(typ_diag==5,!diag%in%c(code_dnid_ins,code_dnid,code_did,
                                                            codes_astrisques_diabete,"I10")) |> 
                       dplyr::rename(das = diag) ) |> 
    dplyr::distinct_at(c("ident",pivots,"das")) |>
    
                    
    dplyr::left_join( pRatihque::atihble(conn, 'prd_vue_nompmsi.mco_diag_niveau') |> dplyr::filter(!!dplyr::sym("v20"%+% anseqta)>1) |>   # §5.8 (ex v2025>1)
                        dplyr::select(dplyr::all_of(c("code","v20"%+% anseqta))) |> 
                        dplyr::rename(das = code,niveau = !!dplyr::sym("v20"%+% anseqta))
    ) |> 
    
    dplyr::collect() -> df_das
  
  
  df_das |> 
    dplyr::mutate(niveau = ifelse(is.na(niveau),"0",niveau)) |> 
    dplyr::mutate(nb_das = dplyr::n(),.by= dplyr::all_of(c(pivots,"das"))) -> df_das
  
  df_das |> 
    dplyr::arrange(ident,dplyr::desc(niveau),dplyr::desc(nb_das),das) |>   # §5.9 : `das` en dernier critère (ordre total)
    dplyr::group_by(ident) |> 
    dplyr::slice(1:nb_assoc_das) -> df_das
  
  df_das |> 
    dplyr::group_by_at(c("ident",pivots)) |> 
    dplyr::arrange(das) |> 
    dplyr::summarise(diagnostic_associes = paste0(das,collapse = " "),.groups="drop") |> 
    dplyr::ungroup() |> 
    dplyr::summarise(n = dplyr::n(),.by=dplyr::all_of(c(pivots,"diagnostic_associes"))) -> df_cases
  
  return(df_cases)
  
  
}

comparer <- function(a, b) identical(as.data.frame(dplyr::arrange(tibble::as_tibble(a), dplyr::across(dplyr::everything()))),
                                     as.data.frame(dplyr::arrange(tibble::as_tibble(b), dplyr::across(dplyr::everything()))))
for(cas in list(list("CHR/U", 26L, 2L), list("CH", 17L, 2L), list("CHR/U", 26L, 3L))){
  anc <- prep_scenarios2_ancien_20260912(cas[[2]], cas[[1]], DUREE_LONGS, NBDA_MAX, cas[[3]], PIVOTS_LONGS)
  nv_m <- prep_scenarios2(cas[[2]], cas[[1]], DUREE_LONGS, NBDA_MAX, cas[[3]], PIVOTS_LONGS, TRUE)
  nv_u <- prep_scenarios2(cas[[2]], cas[[1]], DUREE_LONGS, NBDA_MAX, cas[[3]], PIVOTS_LONGS, FALSE)
  ok(sprintf("P1 équivalence %s %s k=%d : nouvelle chaîne (morceaux) == ancienne", cas[[1]], cas[[2]], cas[[3]]), comparer(anc, nv_m) && nrow(anc) > 0)
  ok(sprintf("P1 équivalence %s %s k=%d : collect unique == ancienne", cas[[1]], cas[[2]], cas[[3]]), comparer(anc, nv_u))
}
ok("partiel écrit par le run == ancienne chaîne (partiels antérieurs valides)",
   comparer(arrow::read_parquet(file.path(DIR_PARTIELS, "catalogue_partiel_CH_17.parquet")), prep_scenarios2_ancien_20260912(17L, "CH", DUREE_LONGS, NBDA_MAX, K_GRAINE_LONGS, PIVOTS_LONGS)))
tk <- pRatihque::atihble(conn, "prep_topk_tmp") |> dplyr::collect()
ok("prep_topk_tmp : au plus k lignes par ident, colonnes étroites (ident, pivots, das)",
   max(table(tk$ident)) <= 3 && identical(names(tk), c("ident", PIVOTS_LONGS, "das")))
ok("invariant morceaux : un ident n'apparaît que dans une seule cage", all((tk |> dplyr::distinct(ident, cage) |> dplyr::count(ident))$n == 1))
ok("prep_topk_tmp : une seule table (pas d'empilement), écrasée par la dernière itération (CHR/U 26 k=3 ci-dessus)",
   sum(temp_tables(conn) == "prep_topk_tmp") == 1 && all(tk$ident %in% (pRatihque::atihble(conn, "prep_data_26") |> dplyr::filter(categ_pmsi == "CHR/U") |> dplyr::distinct(ident) |> dplyr::collect())$ident))
# catalogue deux étages == ancien flux (bind_rows global -> conversion -> ré-agrégation -> seuil)
ancien_flux <- function(fichiers, conversion){
  d <- dplyr::bind_rows(lapply(fichiers, arrow::read_parquet)) |> dplyr::summarise(n = sum(n), .by = dplyr::all_of(c(PIVOTS_LONGS, "diagnostic_associes")))
  if(conversion){
    dist <- arrow::read_parquet(file.path(DIR_REFERENCES, "ref_distribution_e660.parquet"))
    d <- d |> convertir_e669_comptes("diag2", c(setdiff(PIVOTS_LONGS, "diag2"), "diagnostic_associes"), "n", dist, BARE_E669_DEFAUT) |>
      convertir_e669_combo("diagnostic_associes", PIVOTS_LONGS, "n", dist, BARE_E669_DEFAUT)
  }
  d |> dplyr::inner_join(d |> dplyr::summarise(nb = sum(n), .by = dplyr::all_of(PIVOTS_LONGS_SEUIL)), by = PIVOTS_LONGS_SEUIL) |>
    dplyr::filter(nb > SEUIL_PIVOT) |> dplyr::select(-n) |> dplyr::rename(poids = nb)
}
ok("P2 catalogue deux étages == ancien flux (conversion TRUE, fixture de fusion E669 incluse)",
   comparer(cat1, ancien_flux(file.path(DIR_PARTIELS, ETAPES_ENV$plan$iterations$fichier), TRUE)) && any(cat1$ghm2 == "88M991"))
ok("recouvrement.csv : (CHR/U 17->26) ok avec parts dans [0,1], (CH 24->25) non calculable",
   { r <- utils::read.csv(FICHIER_RECOUVREMENT()); nrow(r) == 2 && r$statut[1] == "ok" && r$part_combos_B_vues[1] >= 0 && r$part_combos_B_vues[1] <= 1 &&
     r$nb_B[1] == ap1$nb_lignes_partiel[2] && grepl("non calculable", r$statut[2]) })
ok("diagnostic_memoire.csv : schéma, mesures par morceau / partiel / ref / étage",
   { m <- utils::read.csv(FICHIER_DIAG_MEMOIRE()); all(c("etiquette", "horodatage", "taille_objet_mo", "memoire_utilisee_go", "pic_go", "alerte") %in% names(m)) &&
     any(grepl("morceau", m$etiquette)) && any(grepl("^partiel ", m$etiquette)) && any(grepl("^ref ref_v_admin_longs", m$etiquette)) && any(grepl("étage 1", m$etiquette)) && any(grepl("catalogue final", m$etiquette)) })
ok("P3 : aucun objet ref ni cache brut vivant après l'extraction", !exists("df_ref") && !exists("brute", envir = CACHE_E669) && !exists("pivots_bruts") && !exists("combos") && !exists("df_prep_scenarios") && !exists("df_prep_scenarios_seuil"))
fermer()

# =============================================================== SESSION 2 ==
cat("\n# session 2 : reconnexion, année 20 ajoutée -> seules les itérations manquantes ; catalogue régénéré (périmètre étendu : FORCER_CATALOGUE)\n")
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)")
err <- tryCatch({ invisible(sortie(lancer("extraction.R"))); NULL }, error = function(e) conditionMessage(e))
plan2 <- ETAPES_ENV$plan; tt2 <- temp_tables(conn); ap2 <- utils::read.csv(FICHIER_APPORTS()); fermer()   # prep_data, refs et partiels ont tourné ; la garde du catalogue a stoppé ensuite
ok("garde du magasin catalogue : périmètre étendu sans FORCER_CATALOGUE -> stop nommant ANS_HISTORIQUE, le drapeau, la soupape et l'alignement", !is.null(err) && grepl("magasin partagé catalogue", err) && grepl("ANS_HISTORIQUE : magasin = 17,26 ; courant = 17,20,26", err) && grepl("FORCER_CATALOGUE", err) && grepl("CHEMINS_SURCHARGES", err) && grepl("aligner ANS_HISTORIQUE", err))
ok("plan session 2 : 2 itérations (CHR/U 20, CH 20), 0 ref, année 20 seule, pas de prep_das_chronique",
   sum(plan2$iterations$a_faire) == 2 && all(plan2$iterations$an[plan2$iterations$a_faire] == 20) && !any(plan2$refs$a_faire) && identical(plan2$annees_a_preparer, 20L) && !plan2$prep_das_chronique)
ok("seule prep_data_20 recréée (plus la table top-k unique de l'itération)", setequal(tt2, c("prep_data_20", "prep_topk_tmp")))
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)", "FORCER_CATALOGUE <- TRUE")
log2 <- sortie(lancer("extraction.R"))
ok("FORCER_CATALOGUE : magasin régénéré (message), méta du magasin au nouveau périmètre ; partiels et refs déjà là (rien à faire)", any(grepl("FORCER_CATALOGUE : magasin partagé régénéré", log2)) && identical(unlist(yaml::read_yaml(FICHIER_CATALOGUE_MONO_META())$ANS_HISTORIQUE), c(17L, 20L, 26L)) && ETAPES_ENV$plan$rien_a_faire)
ok("refs sautées (message)", any(grepl("ref ref_das_aigu : présente, sautée", log2)))
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)")   # le drapeau FORCER_CATALOGUE ne vaut que pour la régénération de la session 2
ok("diagnostic_apports : 6 lignes, relu pour 17/26 et calculé pour 20 (première tentative de la session 2)",
   nrow(ap2) == 6 && identical(ap2$statut[ap2$an == 20], c("calculé", "calculé")) && all(ap2$statut[ap2$an != 20] == "relu"))
cat_multi <- lire_cat(DIR_CATALOGUE_M)
ok("catalogue étendu (3 années > 2 années)", nrow(cat_multi) > nrow(cat1))
fermer()

# ------------------------------------------------ run mono-session (projet 2) --
cat("\n# run mono-session (3 années d'un coup) : identité du catalogue\n")
proj2 <- creer_projet("projet_v8_mono"); Sys.setenv(SCENARIOS_PMSI_PATH = proj2)
log_mono <- sortie(lancer("extraction.R"))
ok("mono-session : 6 itérations calculées", sum(ETAPES_ENV$plan$iterations$a_faire) == 6)
cat_mono <- lire_cat(DIR_CATALOGUE_M)
ok("identité du catalogue final multi-sessions == mono-session", identical(cat_multi, cat_mono))
ok("identité des partiels (CH, 20)", identical(arrow::read_parquet(file.path(DIR_PARTIELS, "catalogue_partiel_CH_20.parquet")),
                                                arrow::read_parquet(file.path(proj, "results", "00_partiels", "catalogue_partiel_CH_20.parquet"))))
fermer()
Sys.setenv(SCENARIOS_PMSI_PATH = proj)

# =============================================================== SESSION 3 ==
cat("\n# session 3 : tout présent -> rien à faire\n")
log3 <- sortie(lancer("extraction.R"))
ok("plan : rien à faire, message explicite", ETAPES_ENV$plan$rien_a_faire && any(grepl("TOUT EST A JOUR", log3)))
ok("aucune table temporaire créée", length(temp_tables(conn)) == 0)
ok("catalogue sauté (magasin à jour, mêmes paramètres) et identique", any(grepl("catalogue déjà à jour", log3)) && identical(lire_cat(DIR_CATALOGUE_M), cat_multi))
fermer()

# ------------------------------------------------ FORCER_REFS --
cat("\n# FORCER_REFS\n")
mt_avant <- file.info(vapply(NOMS_REFS, FICHIER_REF, character(1)))$mtime; Sys.sleep(1.1)
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)", "FORCER_REFS <- TRUE")
invisible(sortie(lancer("extraction.R")))
ok("FORCER_REFS : 9 refs recalculées, 0 itération, année AN_REF seule, prep_das_chronique",
   all(ETAPES_ENV$plan$refs$a_faire) && !any(ETAPES_ENV$plan$iterations$a_faire) && identical(ETAPES_ENV$plan$annees_a_preparer, 26L) && ETAPES_ENV$plan$prep_das_chronique)
ok("tables temporaires : prep_data_26 et prep_das_chro_26 uniquement", setequal(temp_tables(conn), c("prep_data_26", "prep_das_chro_26")))
ok("refs réécrites (mtime), tirable courts compris", all(file.info(vapply(NOMS_REFS, FICHIER_REF, character(1)))$mtime > mt_avant))
fermer()

# ------------------------------------------------ garde-fou partiels_meta --
cat("\n# garde-fou partiels_meta\n")
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)", "K_GRAINE_LONGS <- 3L")
err <- tryCatch({ invisible(sortie(lancer("extraction.R"))); NULL }, error = function(e) conditionMessage(e))
ok("K_GRAINE_LONGS différent -> stop() de la garde du magasin 00_partiels (drapeau FORCER_PARTIELS)", !is.null(err) && grepl("K_GRAINE_LONGS", err) && grepl("00_partiels", err) && grepl("FORCER_PARTIELS", err))
fermer()

# ------------------------------------------------ reprise des partiels --
cat("\n# reprise des partiels\n")
unlink(file.path(DIR_PARTIELS, "catalogue_partiel_CH_20.parquet"))
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)")
invisible(sortie(lancer("extraction.R")))
ok("un partiel supprimé -> 1 itération, année 20", sum(ETAPES_ENV$plan$iterations$a_faire) == 1 && identical(ETAPES_ENV$plan$annees_a_preparer, 20L))
ok("catalogue identique après reprise", identical(lire_cat(DIR_CATALOGUE_M), cat_multi))
fermer()

# ------------------------------------------------ CONVERSION_E669 = FALSE (toggle effectif) --
cat("\n# CONVERSION_E669 = FALSE : E669 présents, partiels bruts identiques\n")
proj3 <- creer_projet("projet_v8_noconv"); Sys.setenv(SCENARIOS_PMSI_PATH = proj3)
surcharger("ANS_HISTORIQUE <- c(17L, 26L)", "CONVERSION_E669 <- FALSE")
invisible(sortie(lancer("extraction.R")))
cat_nc <- lire_cat(DIR_CATALOGUE_M)
ok("toggle FALSE : ^E669 présents dans le catalogue et les refs", compter_e669(cat_nc, c("diag2", "diagnostic_associes")) > 0 &&
     compter_e669(arrow::read_parquet(file.path(DIR_REFERENCES, "ref_das_chronique.parquet")), "das") > 0 && !isTRUE(yaml::read_yaml(FICHIER_CATALOGUE_MONO_META())$CONVERSION_E669))
ok("toggle FALSE : le cas de fusion n'entre pas au catalogue (deux profils n = 1 <= seuil)", !any(cat_nc$ghm2 == "88M991"))
ok("P2 catalogue deux étages == ancien flux (conversion FALSE)", comparer(cat_nc, ancien_flux(file.path(DIR_PARTIELS, ETAPES_ENV$plan$iterations$fichier), FALSE)))
ok("toggle FALSE : partiels bruts identiques à ceux du projet converti (cache indépendant du toggle)",
   identical(arrow::read_parquet(file.path(DIR_PARTIELS, "catalogue_partiel_CHRU_26.parquet")),
             arrow::read_parquet(file.path(proj, "results", "00_partiels", "catalogue_partiel_CHRU_26.parquet"))) &&
     compter_e669(arrow::read_parquet(file.path(DIR_PARTIELS, "catalogue_partiel_CHRU_26.parquet")), c("diag2", "diagnostic_associes")) > 0)
fermer()
Sys.setenv(SCENARIOS_PMSI_PATH = proj)
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)")
invisible(sortie(lancer("extraction.R")))   # replace la config du projet principal (plan : rien à faire)
fermer()

# =============================================================== TIRAGE (sans base) ==
cat("\n# tirage : phase sans base (mock interdit)\n")
options(pmsi_mock_interdit = TRUE)
ok("aucun appel pRatihque:: dans tirage.R", !any(grepl("pRatihque::", readLines(file.path(proj, "tirage.R")))))
log_t <- sortie(lancer("tirage.R"))
f_courts <- FICHIER_COURTS_CAMPAGNE(); f_longs <- FICHIER_LIVRABLE()
ok("sorties du tirage présentes aux emplacements par étapes : courts DE LA campagne (diagnostic/40_campagnes/C1/habille/courts + _meta), sélection (+ _meta.yaml), diagnostic/60_export_final (livrable + méta + revue + top30 + rapport)",
   all(file.exists(c(f_courts, FICHIER_COURTS_CAMPAGNE_META(), f_longs, FICHIER_LIVRABLE_META(), file.path(DIR_SELECTION(), c("selection_longs.parquet", "selection_longs_effectifs.csv", "_meta.yaml")), FICHIER_REVUE(), FICHIER_TOP30(), FICHIER_RAPPORT()))) &&
     grepl("/diagnostic/40_campagnes/C1/selection$", DIR_SELECTION()) && grepl("/diagnostic/60_export_final/scenarios_C1.parquet$", f_longs) && grepl("/diagnostic/40_campagnes/C1/habille/courts/scenarios_courts.parquet$", f_courts) &&
     !file.exists(file.path(DIR_COURTS, "scenarios_courts.parquet")) && length(list.files(DIR_COURTS)) == 2)
ok("chunks courts (40_campagnes/C1/chunks_courts) et longs (40_campagnes/C1/chunks) écrits", length(list.files(DIR_CHUNKS_COURTS(), pattern = "^courts_chunk_")) >= 1 && length(list.files(DIR_CHUNKS_LONGS(), pattern = "^longs_chunk_")) >= 1 && grepl("/40_campagnes/C1/chunks_courts$", DIR_CHUNKS_COURTS()))
ok("courts de campagne : ordre sélection -> courts dans tirage.R ; budget = RATIO_COURTS × volume longs attendu (ratio 1 : == 120), source « ratio » en bannière ; scénarios gardés <= demandés ; méta du tirage courts",
   { mc <- yaml::read_yaml(FICHIER_COURTS_CAMPAGNE_META()); i_sel <- grep("\\[etape_selection_longs\\] début", log_t); i_c <- grep("\\[etape_tirage_courts\\] début", log_t)
     length(i_sel) == 1 && length(i_c) == 1 && i_sel < i_c && mc$budget == 120 && mc$source_budget == "ratio" && mc$scenarios_demandes == 120 && mc$scenarios_gardes <= 120 && mc$scenarios_gardes > 0 && mc$campagne == "C1" &&
       any(grepl("budget courts = 120 \\(source : ratio", log_t)) && mc$n_pivots_tires <= mc$n_pivots_tirable && identical(unlist(mc$ANS_COURTS), 26L) })
ok("nommage : aucun nom daté ni _v8_ dans l'arborescence produite (la date vit dans les métas)", !any(grepl("_v8_|[0-9]{8}", list.files(PATH_RESULTS, recursive = TRUE))) && !is.null(yaml::read_yaml(FICHIER_LIVRABLE_META())$date) && !is.null(yaml::read_yaml(FICHIER_COURTS_META())$date))
sc_courts <- arrow::read_parquet(f_courts); sc_longs <- lire_longs()
ok("identité parquet relu / objet mémoire (volumétrie du rapport) ; livrable = courts embarqués + longs", nrow(sc_courts) == ETAPES_ENV$rapport$courts$n && nrow(sc_longs) == ETAPES_ENV$rapport$longs$n && nrow(sc_courts) > 0 && nrow(sc_longs) > 0 &&
     { liv <- lire_corpus_final(CAMPAGNE); nrow(liv) == nrow(sc_courts) + nrow(sc_longs) && names(liv)[1] == "branche" && meme_modulo_schema(as.data.frame(sc_courts), liv[liv$branche == "court", ]) })
ok("aucun objet df_scenarios vivant en fin de script", !exists("df_scenarios"))
sel <- arrow::read_parquet(file.path(DIR_SELECTION(), "selection_longs.parquet"))
mt <- yaml::read_yaml(FICHIER_SELECTION_META())
ok("quota_dp : quota exact par diag2, origine renseignée", all(table(sel$diag2) == mt$quota_par_dp) && all(grepl("^plancher_|^libre$", sel$origine)) && nrow(sel) == mt$volume_attendu)
ok("meta_tirage.yaml cohérent avec le profil", mt$PROFIL == "diagnostic" && mt$MODE_SELECTION == "quota_dp" && mt$BUDGET_TOTAL_LONGS == 120 && mt$CHUNK_SIZE_FIXE == 40 && mt$NB_CHUNKS_MAX == NB_CHUNKS_MAX && mt$nrow_catalogue == nrow(cat_multi))
ok("tirage longs indexé (quota_dp) : identique au tirage historique (comparé plus bas aux anciens scripts)", file.exists(f_longs))
ok("sidecars de chunking présents pour les deux branches, cohérents (chunk_size = 40, nb_chunks = fichiers)",
   { sc_c <- yaml::read_yaml(file.path(DIR_CHUNKS_COURTS(), "courts_chunks_meta.yaml")); sc_l <- yaml::read_yaml(file.path(DIR_CHUNKS_LONGS(), "longs_chunks_meta.yaml"))
     sc_c$chunk_size == 40 && sc_l$chunk_size == 40 && sc_c$nb_chunks == length(list.files(DIR_CHUNKS_COURTS(), pattern = "^courts_chunk_")) &&
       sc_l$nb_chunks == length(list.files(DIR_CHUNKS_LONGS(), pattern = "^longs_chunk_")) && sc_l$n == nrow(sel) && sc_c$nb_chunks > 1 && sc_l$nb_chunks > 1 })
ok("tirage : aucun ^E669 dans les sorties (diag2, graine, DAS), effectifs E660x au rapport",
   ETAPES_ENV$rapport$courts$e669_residuels == 0 && ETAPES_ENV$rapport$longs$e669_residuels == 0 && sans_e669(sc_courts, c("diag2", "diagnostic_associes")) &&
     sans_e669(sc_longs, c("diag2", "graine", "diagnostic_associes")) && any(grepl("effectifs E660x par classe", rap <- readLines(FICHIER_RAPPORT()))))
ok("contrôles §8.2 à zéro sur les deux branches", { cc <- ETAPES_ENV$rapport$courts$controles; cl <- ETAPES_ENV$rapport$longs$controles
   cc$doublons_categorie == 0 && cc$diabete_hors_flag == 0 && cc$i10_avec_hta_autres == 0 && cc$poids_sous_seuil == 0 &&
     cl$doublons_categorie == 0 && cl$diabete_hors_flag == 0 && cl$i10_avec_hta_autres == 0 && cl$poids_sous_seuil == 0 })
ok("longs : graine conservée (hors doublon de catégorie interne), HTA et diabète cohérents",
   all(mapply(function(g, d) all(g %in% d) || (any(duplicated(substr(g, 1, 3))) && g[1] %in% d), split_das(sc_longs$graine), split_das(sc_longs$diagnostic_associes))) &&
     all(mapply(function(h, d) h == "N" || "I10" %in% d || any(d %in% hta_autres), sc_longs$hta, split_das(sc_longs$diagnostic_associes))) &&
     all(mapply(function(f, d) f == "N" || any(substr(d,1,3) %in% c("E10","E11")), sc_longs$diabete_scenario, split_das(sc_longs$diagnostic_associes))))
rev <- readr::read_csv2(FICHIER_REVUE(), show_col_types = FALSE)
ok("courts : id_profil (k + 15 hex, recette id_courts_v1), id_scenario = id_profil-variante unique, hash_das ; recalculables ; variantes numérotées par pivot ; DPEC/TPEC/population/campagne/repli_admin portés",
   all(c("id_profil", "id_scenario", "hash_das") %in% names(sc_courts)) && all(grepl("^k[0-9a-f]{15}$", sc_courts$id_profil)) && identical(sc_courts$id_profil, id_profil_courts_de(sc_courts)) &&
     identical(sc_courts$id_scenario, id_scenario_de(sc_courts$id_profil, sc_courts$variante)) && !anyDuplicated(dplyr::distinct(sc_courts, id_scenario, mode_entree, mode_sortie, mdp)) &&
     identical(sc_courts$hash_das, hash_das_de(sc_courts$diagnostic_associes)) && !any(sc_courts$id_profil %in% sc_longs$id_profil) &&
     all(c("DPEC", "TPEC", "population", "campagne", "lettre", "repli_admin", "nb_variantes_demandees") %in% names(sc_courts)) && all(sc_courts$campagne == "C1") && all(sc_courts$repli_admin == 0) && !any(is.na(sc_courts$mode_entree)) &&
     !anyDuplicated(dplyr::distinct(sc_courts, id_profil, hash_das)) && all(sc_courts$variante >= 1))
ok("echantillon_revue.csv : <= 50 lignes, deux branches, libellés et [G] chez les longs",
   nrow(rev) <= 50 && setequal(unique(rev$branche), c("court", "long")) && any(grepl("\\[G\\]", rev$das_libelles[rev$branche == "long"])) &&
     all(c("dp_libelle", "das_libelles", "cmd", "type_unite") %in% names(rev)) && any(grepl("BPCO", rev$dp_libelle)))
ok("revue : id_scenario renseigné pour les courts (k…) comme pour les longs", all(grepl("^k[0-9a-f]{15}-[0-9]{3}$", rev$id_scenario[rev$branche == "court"])))
rap <- readLines(FICHIER_RAPPORT())
ok("rapport : meta en tête, table diag2 × type_unite, top 30, anomalies = 0", any(grepl("^== 0\\. Meta du catalogue", rap)) && any(grepl("PROFIL: diagnostic", rap)) &&
     any(grepl("effectifs sélectionnés diag2", rap)) && any(grepl("== 3\\. Top 30 DAS par CMD", rap)) && any(grepl("TOTAL anomalies = 0", rap)))
# reprise des chunks : identité bit à bit
ch_longs <- sort(list.files(DIR_CHUNKS_LONGS(), pattern = "^longs_chunk_", full.names = TRUE))
unlink(ch_longs[min(2, length(ch_longs))]); unlink(sort(list.files(DIR_CHUNKS_COURTS(), pattern = "^courts_chunk_", full.names = TRUE))[1])
log_t2 <- sortie(lancer("tirage.R"))
ok("reprise : sélection relue, chunks présents sautés", any(grepl("relue depuis", log_t2)) && any(grepl("déjà présent, sauté", log_t2)))
ok("reprise après suppression d'un chunk : courts de la campagne et livrable identiques bit à bit", identical(arrow::read_parquet(f_courts), sc_courts) && identical(lire_longs(), sc_longs))
# courts : parité avec les longs — plages disjointes (parallélisme simulé) == run complet, reprise après suppression d'un chunk d'une plage
ch_c <- sort(list.files(DIR_CHUNKS_COURTS(), pattern = "^courts_chunk_.*\\.parquet$", full.names = TRUE)); nc <- length(ch_c)
unlink(ch_c); unlink(f_courts)
log_c1 <- sortie(etape_tirage_courts(chunk_range = c(1, 1))); log_c2 <- sortie(etape_tirage_courts(chunk_range = c(2, nc)))
ok("courts : chunk_range — plages disjointes 1..1 + 2..n = tous les chunks, aucune écriture tant qu'une plage est demandée",
   nc > 1 && length(list.files(DIR_CHUNKS_COURTS(), pattern = "^courts_chunk_.*\\.parquet$")) == nc && !file.exists(f_courts) && any(grepl("plage traitée : 1\\.\\.1", log_c1)) &&
     any(grepl("plage 1\\.\\.1 \\(session parallèle\\)", log_c1)) && any(grepl("relancer etape_tirage_courts\\(\\) sans plage", log_c2)))
unlink(ch_c[2]); log_c3 <- sortie(etape_tirage_courts())
ok("courts : run complet après plages (un chunk d'une plage supprimé, retiré) == run initial bit à bit ; débit par chunk imprimé",
   identical(as.data.frame(arrow::read_parquet(f_courts)), as.data.frame(sc_courts)) && sum(grepl("déjà présent, sauté", log_c3)) == nc - 1 && any(grepl("débit [0-9]+ scénarios/s", log_c3)))
# =============================================================== ORCHESTRATION ==
# (a) identité bit à bit avec les ANCIENS scripts d'entrée (instantanés tests/ancien_20260914),
#     mêmes fixtures, même seed, même surcharge : extraction puis tirage dans un projet dédié.
cat("\n# orchestration (a) : nouveau flux == anciens scripts d'entrée (bit à bit)\n")
options(pmsi_mock_interdit = FALSE)
proj_anc <- creer_projet("projet_v8_ancien")
# Instantanés FIGÉS (anciens noms de fichiers) : copiés sous leur nom d'origine ; shims config_v8.R / helpers_v8.R
# (sourcent config.R / helpers.R) pour que les instantanés restent intacts après le renommage des fichiers de code.
for(f in c("extraction_associations_codes_v8.R", "tirage_scenarios_v8.R")) file.copy(file.path(racine, "tests", "ancien_20260914", f), file.path(proj_anc, f), overwrite = TRUE)
writeLines(c('source(file.path(PATH_PROJET, "config.R"))',
             '# compat instantanés figés : anciens noms de refs, dossiers plats, DATE_TAG (retirés du code courant)',
             'NOMS_REFS <- c("ref_das_chronique", "distribution_e660", "ref_das_aigu", "ref_nb_chroniques", "ref_comp_diabete", "pivots_courts", "v_admin_courts", "v_admin_longs", "referentiel_substitution_imprecis", "referentiel_paires_chroniques")',
             'REFS_CHRONIQUES <- c("ref_das_chronique", "distribution_e660", "ref_nb_chroniques", "referentiel_paires_chroniques")',
             'EXPORTS_DIR <- paste0(PATH_RESULTS, "exports_ancien/"); PARTIELS_DIR <- paste0(PATH_RESULTS, "partiels_ancien/"); CHUNKS_DIR <- paste0(EXPORTS_DIR, "chunks/")',
             'DATE_TAG <- format(Sys.Date(), "%Y%m%d")',
             'NB_VARIANTES_ADMIN_LONGS <- NA   # comportement v7.2 des instantanés figés (toutes les variantes admin) ; défaut courant = 1L (Q74)',
             'NB_VARIANTES_ADMIN_COURTS <- 2   # comportement v7.1.2 des instantanés figés (2 tenues par scénario court) ; défaut courant = 1L (Q76)'), file.path(proj_anc, "config_v8.R"))
writeLines('source(file.path(PATH_PROJET, "helpers.R"))', file.path(proj_anc, "helpers_v8.R"))
Sys.setenv(SCENARIOS_PMSI_PATH = proj_anc); surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)")
invisible(sortie(lancer("extraction_associations_codes_v8.R"))); fermer()
options(pmsi_mock_interdit = TRUE)
invisible(sortie(lancer("tirage_scenarios_v8.R")))
EXPORTS_ANC <- EXPORTS_DIR
Sys.setenv(SCENARIOS_PMSI_PATH = proj); surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)"); source(file.path(proj, "config.R"))
meme_parquet <- function(a, b) identical(as.data.frame(arrow::read_parquet(a)), as.data.frame(arrow::read_parquet(b)))
# Chantier « courts en campagnes + habillage robuste » : l'identité avec les anciens scripts se limite DÉSORMAIS, par décision,
# à l'extraction (catalogue, refs hors v_admin_longs), au tirable courts, à la sélection et au tirage des DAS longs (chunks) ;
# l'habillage (nbda retiré, repli) et les courts (par campagne, budget, registre) divergent des instantanés figés.
ok("(a) catalogue, 7 refs et le tirable courts identiques aux anciens scripts (anciens noms -> ref_* ; pivots déplacés dans 30_courts) ; photographies admin == anciennes (distinct) SANS nbda, restreintes aux durées de leur branche, + effectifs n",
   meme_parquet(file.path(EXPORTS_ANC, "catalogue_longs_seuil.parquet"), MONO_CATALOGUE()) &&
     all(vapply(setdiff(names(ANCIENS_NOMS_REFS), c("v_admin_longs", "v_admin_courts")), function(o) meme_parquet(file.path(EXPORTS_ANC, o %+% ".parquet"), FICHIER_REF(ANCIENS_NOMS_REFS[[o]])), logical(1))) &&
     { va <- as.data.frame(arrow::read_parquet(file.path(EXPORTS_ANC, "v_admin_longs.parquet"))); vn <- as.data.frame(arrow::read_parquet(FICHIER_REF("ref_v_admin_longs"))); va2 <- unique(va[va$duree %in% DUREE_LONGS, setdiff(names(va), "nbda"), drop = FALSE])
       "nbda" %in% names(va) && !"nbda" %in% names(vn) && any(!va$duree %in% DUREE_LONGS) && nrow(vn) < nrow(va) && "n" %in% names(vn) && meme_contenu(va2, vn[, names(va2), drop = FALSE]) } &&
     { vc <- as.data.frame(arrow::read_parquet(file.path(EXPORTS_ANC, "v_admin_courts.parquet"))); vcn <- as.data.frame(arrow::read_parquet(FICHIER_REF("ref_v_admin_courts"))); vc2 <- unique(vc[vc$duree %in% DUREE_COURTS, , drop = FALSE])
       # §26 : la photographie courts porte type_unite en plus (combinaisons éclatées par type d'unité) : identité modulo cette colonne (distinct des anciennes colonnes)
       any(!vc$duree %in% DUREE_COURTS) && "n" %in% names(vcn) && "type_unite" %in% names(vcn) && !"type_unite" %in% names(vc) && meme_contenu(vc2, unique(vcn[, names(vc2), drop = FALSE])) })
COLS_ID_COURTS <- c("id_profil", "id_scenario", "hash_das")
f_courts_anc <- file.path(EXPORTS_ANC, "scenarios_courts_v8_" %+% format(Sys.Date(), "%Y%m%d") %+% ".parquet"); f_longs_anc <- file.path(EXPORTS_ANC, "scenarios_longs_tirage_v8_" %+% format(Sys.Date(), "%Y%m%d") %+% ".parquet")
lire_chunks_longs <- function(d) purrr::map(sort(list.files(d, pattern = "^longs_chunk_[0-9]{4}\\.parquet$", full.names = TRUE)), function(f) as.data.frame(arrow::read_parquet(f))) |> purrr::list_rbind()
ok("(a) sélection et tirage des DAS longs identiques bit à bit aux anciens scripts (chunks) : doctrine de tirage inchangée ; scénarios longs (pivots, graine, variante, DAS) identiques hors habillage",
   meme_parquet(file.path(EXPORTS_ANC, "selection_longs.parquet"), file.path(DIR_SELECTION(), "selection_longs.parquet")) &&
     identical(lire_chunks_longs(file.path(EXPORTS_ANC, "chunks")), lire_chunks_longs(DIR_CHUNKS_LONGS())) && nrow(lire_chunks_longs(DIR_CHUNKS_LONGS())) > 0 &&
     { cols <- c(PIVOTS_LONGS, "graine", "variante", "diagnostic_associes"); a <- unique(as.data.frame(arrow::read_parquet(f_longs_anc))[, cols]); n <- unique(as.data.frame(lire_longs()[, cols]))
       for(cc in cols) n[[cc]] <- coercer(n[[cc]], class(a[[cc]])[1]); meme_contenu(a, n) })
ok("(a) DIVERGENCES PAR DÉCISION : anciens longs habillés sur 7 clés dont nbda (lignes différentes) ; anciens courts = corpus FIXE (3 variantes × 2 admin par pivot, sans identifiant) vs courts DE campagne (budget, ids, registre)",
   { la <- as.data.frame(arrow::read_parquet(f_longs_anc)); ca <- as.data.frame(arrow::read_parquet(f_courts_anc)); cn <- as.data.frame(arrow::read_parquet(f_courts))
     !identical(nrow(la), nrow(lire_longs())) || !"repli_admin" %in% names(la) } &&
     !any(COLS_ID_COURTS %in% names(arrow::read_parquet(f_courts_anc))) && all(COLS_ID_COURTS %in% names(arrow::read_parquet(f_courts))) && max(arrow::read_parquet(f_courts_anc)$variante) == NB_TIRAGES_COURTS &&
     { rv <- readr::read_csv2(FICHIER_REVUE(), show_col_types = FALSE); t30 <- utils::read.csv(FICHIER_TOP30()); nrow(rv) == NB_REVUE && sum(rv$branche == "court") == round(NB_REVUE * PART_REVUE_COURTS) && setequal(unique(t30$branche), c("court", "long")) })

# (d) étapes hors ordre -> erreur actionnable ; (e) etat_pipeline avant ; (b) étape par étape
cat("\n# orchestration (b)(d)(e) : étape par étape, hors ordre, tableau de bord\n")
proj_et <- creer_projet("projet_v8_etapes"); Sys.setenv(SCENARIOS_PMSI_PATH = proj_et, SCENARIOS_PMSI_ETAPES_SEULEMENT = "1")
options(pmsi_mock_interdit = FALSE)
invisible(sortie(lancer("extraction.R")))   # session chargée sans exécution
etat0 <- etat_pipeline()
ok("(e) avant toute étape : refs, partiels, catalogue, courts, sélection, finalisation À FAIRE",
   all(etat0$statut[etat0$etape %in% c("etape_refs", "etape_partiels_longs", "etape_catalogue", "etape_tirage_courts", "etape_selection_longs", "etape_tirage_das_longs", "etape_finalisation")] == "À FAIRE"))
err <- tryCatch({ invisible(sortie(etape_refs())); NULL }, error = function(e) conditionMessage(e))
ok("(d) etape_refs() sans prep_data -> erreur actionnable", !is.null(err) && grepl("etape_prep_data", err))
err <- tryCatch({ invisible(sortie(etape_tirage_das_longs())); NULL }, error = function(e) conditionMessage(e))
ok("(d) etape_tirage_das_longs() sans sélection -> erreur actionnable", !is.null(err) && grepl("etape_selection_longs|manquant", err))
invisible(sortie(etape_prep_data())); invisible(sortie(etape_refs()))
ok("(b) après etape_refs : refs FAIT, partiels À FAIRE", { e <- etat_pipeline(); e$statut[e$etape == "etape_refs"] == "FAIT" && e$statut[e$etape == "etape_partiels_longs"] == "À FAIRE" })
fermer()   # déconnexion entre etape_refs et etape_partiels_longs
invisible(sortie(lancer("extraction.R")))   # reconnexion (session chargée sans exécution)
err <- tryCatch({ invisible(sortie(etape_partiels_longs())); NULL }, error = function(e) conditionMessage(e))
ok("(b) etape_partiels_longs() après reconnexion sans prep_data -> erreur actionnable", !is.null(err) && grepl("etape_prep_data", err))
invisible(sortie(etape_prep_data()))
ok("(b) reconnexion : seules les années des partiels sont préparées, refs sautées", identical(ETAPES_ENV$plan$annees_a_preparer, c(17L, 20L, 26L)) && !any(ETAPES_ENV$plan$refs$a_faire))
invisible(sortie(etape_partiels_longs())); invisible(sortie(etape_catalogue()))
ref_proj <- function(nom, res) file.path(res, if(nom == "ref_pivots_courts") "30_courts" else "10_references", nom_ref(nom))
ok("(b) catalogue et refs par étapes == bout-en-bout (tirable courts dans 30_courts)",
   meme_parquet(MONO_CATALOGUE(), file.path(proj, "results", "20_catalogue", "catalogue_longs_seuil.parquet")) &&
     all(vapply(NOMS_REFS, function(nom) meme_parquet(FICHIER_REF(nom), ref_proj(nom, file.path(proj, "results"))), logical(1))))
fermer(); rm(conn)
ok("(e) sans connexion : tables temporaires « inconnu hors connexion », catalogue FAIT", { e <- etat_pipeline(); grepl("inconnu hors connexion", e$preuve[e$etape == "etape_prep_data"]) && e$statut[e$etape == "etape_catalogue"] == "FAIT" })
options(pmsi_mock_interdit = TRUE)
invisible(sortie(lancer("tirage.R")))   # session tirage chargée sans exécution
err <- tryCatch({ invisible(sortie(etape_tirage_courts())); NULL }, error = function(e) conditionMessage(e))
ok("(d) etape_tirage_courts() sans sélection (budget par ratio) -> erreur actionnable", !is.null(err) && grepl("etape_selection_longs", err) && grepl("NB_CRH_CIBLE_COURTS", err))
invisible(sortie(etape_selection_longs())); invisible(sortie(etape_tirage_courts()))
invisible(sortie(lancer("tirage.R")))   # nouvelle session : la sélection et les chunks doivent être relus
ok("(b) nouvelle session : état de tirage vide", is.null(etat_tirage("selection")) && is.null(etat_tirage("df_tirage_longs")))
invisible(sortie(etape_tirage_das_longs())); invisible(sortie(etape_habillage_longs())); invisible(sortie(etape_finalisation()))
ok("(b) sorties du tirage par étapes (sessions séparées) == bout-en-bout, bit à bit (courts de la campagne, livrable, revue)",
   meme_parquet(FICHIER_COURTS_CAMPAGNE(), file.path(proj, "results", "diagnostic", "40_campagnes", "C1", "habille", "courts", "scenarios_courts.parquet")) && meme_parquet(FICHIER_LIVRABLE(), file.path(proj, "results", "diagnostic", "60_export_final", "scenarios_C1.parquet")) &&
     identical(readLines(FICHIER_REVUE()), readLines(file.path(proj, "results", "diagnostic", "60_export_final", "echantillon_revue_C1.csv"))))
ok("(e) après finalisation : tout FAIT", { e <- etat_pipeline(); all(e$statut[e$etape %in% c("etape_refs", "etape_partiels_longs", "etape_catalogue", "etape_tirage_courts", "etape_selection_longs", "etape_tirage_das_longs", "etape_finalisation")] == "FAIT") })
# (c) etape_catalogue à périmètre restreint : le magasin existe avec un autre périmètre -> FORCER_CATALOGUE requis ; avertissement périmètre != config
err <- tryCatch({ invisible(sortie(etape_catalogue(ans = c(17L, 26L), etbs = "CHR/U"))); NULL }, error = function(e) conditionMessage(e))
ok("(c) périmètre restreint sur un magasin existant sans FORCER_CATALOGUE -> stop de la garde", !is.null(err) && grepl("magasin partagé catalogue", err) && grepl("FORCER_CATALOGUE", err))
assign("FORCER_CATALOGUE", TRUE, envir = globalenv())
log_c <- sortie(etape_catalogue(ans = c(17L, 26L), etbs = "CHR/U"))
mres <- yaml::read_yaml(FICHIER_CATALOGUE_MONO_META())
ok("(c) etape_catalogue(ans, etbs) forcé : catalogue restreint au périmètre passé, méta cohérent, avertissement « périmètre différent de la config »",
   identical(unlist(mres$ANS_HISTORIQUE), c(17L, 26L)) && identical(unlist(mres$TYPES_ETBS_LONGS), "CHR/U") && identical(unlist(mres$perimetre_ans), c(17L, 26L)) && any(grepl("différent de la config", log_c)) &&
     comparer(lire_cat(DIR_CATALOGUE_M), ancien_flux(file.path(DIR_PARTIELS, nom_partiel("CHR/U", c(17L, 26L))), TRUE)))
invisible(sortie(etape_catalogue()))   # retour au périmètre de la config (forcé) : le magasin partagé sert les tests suivants
assign("FORCER_CATALOGUE", FALSE, envir = globalenv())
ok("(c) magasin ramené au périmètre de la config : chargeable sans garde", verifier_magasin("catalogue", yaml::read_yaml(FICHIER_CATALOGUE_MONO_META()), valeurs_effectives_config())$ok)
Sys.unsetenv("SCENARIOS_PMSI_ETAPES_SEULEMENT")
Sys.setenv(SCENARIOS_PMSI_PATH = proj); surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)"); source(file.path(proj, "config.R"))
invisible(sortie(lancer("tirage.R")))   # rétablit l'état de session du projet principal (chunks présents : reprise)

# =============================================================== AVAL PRODUCTION ==
cat("\n# aval production : repartitionnement, quota_dp_fixe, tirage indexé par population, flux\n")
proj_pr <- creer_projet("projet_v8_prod"); Sys.setenv(SCENARIOS_PMSI_PATH = proj_pr, SCENARIOS_PMSI_ETAPES_SEULEMENT = "1", SCENARIOS_PMSI_PROFIL = "production")   # profil production : dossiers production/ distincts du diagnostic
options(pmsi_mock_interdit = FALSE)
# PARTAGE MAXIMAL : le profil production travaille sur le MÊME PATH_RESULTS que le profil diagnostic (projet principal) :
# partiels, refs, catalogue et courts sont réutilisés SANS recalcul (magasins partagés, gardés par leur _meta.yaml).
# (CHUNK_SIZE_FIXE reste à 40 comme en diagnostic : le chunking définit le contenu des courts — seeds par chunk —, un profil qui en
#  changerait ne pourrait pas partager le magasin 30_courts, la garde le dit)
SURCHARGE_PROD_ISOLE <- c("ANS_HISTORIQUE <- c(17L, 20L, 26L)", "MODE_SELECTION <- 'quota_dp_fixe'", "NB_CRH_CIBLE <- 200L", "NB_LIGNES_PAR_DP <- 1L", "LOT_CHUNKS_FINALISATION <- 2L",
                          'PLAFONDS_DPEC <- list("Accouchement normal mère" = 3L)')
SURCHARGE_PROD <- c(SURCHARGE_PROD_ISOLE, "PATH_RESULTS <- '" %+% file.path(proj, "results") %+% "/'")
surcharger(SURCHARGE_PROD, "CAMPAGNE <- 'C1'", "REGISTRE_ACTIF <- FALSE")
invisible(sortie(lancer("extraction.R")))
mt_mag <- file.info(c(list.files(DIR_PARTIELS, full.names = TRUE), list.files(DIR_REFERENCES, full.names = TRUE), MONO_CATALOGUE(), FICHIER_PIVOTS_COURTS(), FICHIER_COURTS_META()))$mtime
log_pr <- c(sortie(etape_prep_data()), sortie(etape_refs()), sortie(etape_partiels_longs()), sortie(etape_catalogue()))
ok("partage maximal : second profil sur le même PATH_RESULTS -> partiels, refs, catalogue relus SANS recalcul (rien à faire, sauté, à jour), aucun fichier recréé, dossiers par profil distincts",
   PATH_RESULTS == file.path(proj, "results") %+% "/" && DIR_PROFIL == file.path(proj, "results", "production") %+% "/" && ETAPES_ENV$plan$rien_a_faire &&
     any(grepl("catalogue déjà à jour", log_pr)) && any(grepl("présente, sautée", log_pr)) && any(grepl("\\[relu", log_pr)) &&
     identical(file.info(c(list.files(DIR_PARTIELS, full.names = TRUE), list.files(DIR_REFERENCES, full.names = TRUE), MONO_CATALOGUE(), FICHIER_PIVOTS_COURTS(), FICHIER_COURTS_META()))$mtime, mt_mag))
fermer(); rm(conn); options(pmsi_mock_interdit = TRUE)
invisible(sortie(lancer("tirage.R")))
ok("diagnostic_memoire.csv écrit en fin d'etape_refs / etape_partiels_longs (avant etape_catalogue) et listé par etat_pipeline",
   { proj_m <- creer_projet("projet_v8_mem"); Sys.setenv(SCENARIOS_PMSI_PATH = proj_m); options(pmsi_mock_interdit = FALSE)
     surcharger(SURCHARGE_PROD_ISOLE, "CAMPAGNE <- 'C1'", "REGISTRE_ACTIF <- FALSE")   # résultats propres (pas de partage) : magasins vides
     invisible(sortie(lancer("extraction.R"))); invisible(sortie(etape_prep_data())); invisible(sortie(etape_refs()))
     a <- file.exists(FICHIER_DIAG_MEMOIRE()); n1 <- nrow(utils::read.csv(FICHIER_DIAG_MEMOIRE()))
     invisible(sortie(etape_partiels_longs())); n2 <- nrow(utils::read.csv(FICHIER_DIAG_MEMOIRE()))
     e <- etat_pipeline(); fermer(); rm(conn); options(pmsi_mock_interdit = TRUE)
     # message à trois branches quand le catalogue est absent (etape_catalogue non lancée dans ce projet)
     invisible(sortie(lancer("tirage.R")))
     msg <- tryCatch({ invisible(sortie(etape_repartitionner_catalogue())); "" }, error = function(e) conditionMessage(e))
     msg2 <- tryCatch({ invisible(sortie(etape_selection_longs())); "" }, error = function(e) conditionMessage(e))
     Sys.setenv(SCENARIOS_PMSI_PATH = proj_pr); surcharger(SURCHARGE_PROD, "CAMPAGNE <- 'C1'", "REGISTRE_ACTIF <- FALSE"); invisible(sortie(lancer("tirage.R")))
     a && n2 > n1 && grepl("diagnostic_memoire_production.csv : présent", e$preuve[e$etape == "etape_partiels_longs"]) &&
       grepl("magasin partagé", msg) && grepl("20_catalogue", msg) && grepl("etape_catalogue\\(\\)", msg) && grepl("etape_repartitionner_catalogue", msg) && grepl("magasin partagé", msg2) })
mono_avant <- as.data.frame(arrow::read_parquet(MONO_CATALOGUE()))
invisible(sortie(etape_repartitionner_catalogue()))
side <- yaml::read_yaml(FICHIER_CATALOGUE_META())
parts <- lire_catalogue(DIR_CATALOGUE())
ok("repartitionnement : parts recomposées == monofichier d'origine + colonnes lettre/DPEC/TPEC ; monofichier renommé .ancien",
   nrow(parts) == nrow(mono_avant) && identical(as.data.frame(dplyr::arrange(parts[, names(mono_avant)], dplyr::across(dplyr::everything()))), as.data.frame(dplyr::arrange(mono_avant, dplyr::across(dplyr::everything())))) &&
     all(c("lettre", "DPEC", "TPEC") %in% names(parts)) && all(parts$lettre == substr(parts$diag2, 1, 1)) && !file.exists(MONO_CATALOGUE()) && file.exists(MONO_CATALOGUE() %+% ".ancien"))
ok("repartitionnement : id_profil posé (recette id_v1), unique, recalculable ; sidecar version_recette_id",
   "id_profil" %in% names(parts) && !anyDuplicated(parts$id_profil) && identical(parts$id_profil, id_profil_de(parts)) && side$version_recette_id == RECETTE_ID)
ok("repartitionnement : sidecar (nb lignes par part == méta, sum poids, effectifs DPEC, version typologie)",
   side$nb_lignes_total == yaml::read_yaml(FICHIER_CATALOGUE_MONO_META())$nb_lignes && sum(unlist(side$sum_poids_par_part)) == sum(parts$poids) &&
     side$version_typologie == charger_typo()$version && sum(unlist(side$effectifs_dpec)) == nrow(parts) && all(parts$DPEC[substr(parts$ghm2, 3, 3) == "C"] == "Chirurgie adultes > 3 nuits"))
ok("repartitionnement : idempotent (sauté)", { o <- sortie(etape_repartitionner_catalogue()); any(grepl("sauté", o)) })
ok("repartitionnement : garde-fou version typologie", { assign("typo", modifyList(charger_typo(), list(version = "autre")), envir = ETAPES_ENV)
   e <- tryCatch({ invisible(sortie(etape_repartitionner_catalogue())); NULL }, error = function(e) conditionMessage(e)); rm("typo", envir = ETAPES_ENV); !is.null(e) && grepl("re-repartitionner", e) })
ok("etat_pipeline : repartitionnement FAIT", { e <- etat_pipeline(); e$statut[e$etape == "etape_repartitionner_catalogue"] == "FAIT" })
ok("catalogue_complet retiré : stop renvoyant vers quota_dp_fixe", grepl("quota_dp_fixe", tryCatch({ invisible(sortie(etape_selection_longs(budget = 10L, mode = "catalogue_complet"))); "" }, error = function(e) conditionMessage(e))))
invisible(sortie(etape_selection_longs()))
log_cc1 <- sortie(etape_tirage_courts())
mt_f <- yaml::read_yaml(FICHIER_SELECTION_META())
sel_pops <- lapply(names(POPULATIONS), function(pp) lire_catalogue(DIR_SELECTION(pp))); names(sel_pops) <- names(POPULATIONS)
ok("quota_dp_fixe : sélection par population, budget au prorata des DP, volume total == annoncé",
   mt_f$MODE_SELECTION == "quota_dp_fixe" && sum(vapply(mt_f$par_population, function(m) m$budget_population, numeric(1))) == 200 &&
     sum(vapply(sel_pops, function(d) if(is.null(d)) 0L else sum(d$n_var), integer(1))) == mt_f$volume_attendu && mt_f$volume_attendu > 0)
ok("quota_dp_fixe : k = 1 -> une ligne par (DP × groupe) et X_dp variantes ; sans remise ; DPEC/TPEC/id_selection présents",
   all(vapply(sel_pops, function(d){ if(is.null(d)) return(TRUE); st <- utils::read.csv(file.path(DIR_SELECTION(d$population[1]), "selection_longs_stats_dp.csv"))
     all(st$k_eff <= 1) && all(st$variantes == st$X_dp) && !any(duplicated(d[, c(PIVOTS_LONGS, "diagnostic_associes")])) && all(c("DPEC", "TPEC", "id_selection", "n_var") %in% names(d)) }, logical(1))))
ok("quota_dp_fixe : X cohérent avec nb_dp et budget de la population", all(vapply(mt_f$par_population, function(m) m$X == ceiling(m$budget_population / max(m$nb_dp, 1)), logical(1))))
sel_c1 <- purrr::list_rbind(purrr::compact(sel_pops))
ok("plafond de CLASSE (Accouchement normal mère, plafond 3) : 1 représentant par DP prime, dépassement consigné, une ligne × 1 variante par DP de la classe",
   { cl <- sel_c1[sel_c1$DPEC == "Accouchement normal mère", ]
     all(vapply(names(POPULATIONS), function(pp){
       m <- yaml::read_yaml(FICHIER_SELECTION_POP_META(pp))$classes_plafonnees; if(length(m) == 0) return(TRUE)
       m <- m[[1]]; cp <- cl[cl$population == pp, ]
       if(m$nb_dp == 0) return(nrow(cp) == 0)
       !anyDuplicated(cp$diag2) && nrow(cp) == m$nb_dp && sum(cp$n_var) == m$total_retenu &&
         (if(m$nb_dp >= m$plafond) all(cp$n_var == 1) && m$total_retenu == m$nb_dp && m$depassement == m$nb_dp - m$plafond else m$total_retenu == m$plafond && m$depassement == 0) }, logical(1))) &&
       nrow(cl) > 3 && sum(vapply(names(POPULATIONS), function(pp){ m <- yaml::read_yaml(FICHIER_SELECTION_POP_META(pp))$classes_plafonnees; if(length(m)) m[[1]]$depassement else 0 }, numeric(1))) > 0 })
ok("C1 sans registre : origine vierge partout, campagne tracée, id_profil dans la sélection", all(sel_c1$origine_profil == "vierge") && all(sel_c1$campagne == "C1") && all(nchar(sel_c1$id_profil) == 16))
mcc1 <- yaml::read_yaml(FICHIER_COURTS_CAMPAGNE_META()); sc_c1 <- arrow::read_parquet(FICHIER_COURTS_CAMPAGNE())
ok("courts C1 (production, sans registre) : budget = ratio × volume longs attendu de la sélection (== 200 attendu), tous pivots vierges, demandés == budget, chunks sous 40_campagnes/C1/chunks_courts, plage puis run complet identiques",
   mcc1$budget == round(RATIO_COURTS * mt_f$volume_attendu) && mcc1$source_budget == "ratio" && mcc1$n_pivots_recycles == 0 && mcc1$scenarios_demandes == mcc1$budget && mcc1$scenarios_gardes <= mcc1$budget &&
     sum(sc_c1$nb_variantes_demandees[!duplicated(sc_c1$id_profil)]) == mcc1$budget && grepl("/production/40_campagnes/C1/chunks_courts$", DIR_CHUNKS_COURTS()) && any(grepl("vierges = [0-9]+, recyclés = 0", log_cc1)) &&
     { ch <- sort(list.files(DIR_CHUNKS_COURTS(), pattern = "^courts_chunk_.*\\.parquet$", full.names = TRUE)); unlink(ch); unlink(FICHIER_COURTS_CAMPAGNE())
       invisible(sortie(etape_tirage_courts(chunk_range = c(1, 1)))); invisible(sortie(etape_tirage_courts())); identical(as.data.frame(arrow::read_parquet(FICHIER_COURTS_CAMPAGNE())), as.data.frame(sc_c1)) })
ok("courts C1 : budget ABSOLU imposé à l'appel (budget = 37) -> 37 demandés, source « absolu » ; retour au ratio identique bit à bit",
   { unlink(DIR_CHUNKS_COURTS(), recursive = TRUE); unlink(DIR_HABILLE_COURTS(), recursive = TRUE); o <- sortie(etape_tirage_courts(budget = 37L)); m37 <- yaml::read_yaml(FICHIER_COURTS_CAMPAGNE_META())
     unlink(DIR_CHUNKS_COURTS(), recursive = TRUE); unlink(DIR_HABILLE_COURTS(), recursive = TRUE); invisible(sortie(etape_tirage_courts()))
     m37$budget == 37 && m37$source_budget == "absolu" && m37$scenarios_demandes == 37 && any(grepl("source : absolu", o)) && identical(as.data.frame(arrow::read_parquet(FICHIER_COURTS_CAMPAGNE())), as.data.frame(sc_c1)) })
ok("quota_dp_fixe : sélection relue à l'identique (idempotence)", { o <- sortie(etape_selection_longs()); any(grepl("relue", o)) })
# tirage par plages disjointes (parallélisme simulé) puis run complet : identité bit à bit
invisible(sortie(etape_tirage_das_longs(chunk_range = c(1, 1))))
invisible(sortie(etape_tirage_das_longs()))
ch_pops <- lapply(names(POPULATIONS), function(pp) sort(list.files(DIR_CHUNKS_POP(pp), pattern = "^longs_chunk_.*\\.parquet$", full.names = TRUE)))
ok("tirage fixe : chunks par population avec sidecar, nb complet", all(vapply(seq_along(ch_pops), function(i){ pp <- names(POPULATIONS)[i]; f <- file.path(DIR_CHUNKS_POP(pp), "longs_chunks_meta.yaml")
   !file.exists(f) || length(ch_pops[[i]]) == yaml::read_yaml(f)$nb_chunks }, logical(1))))
lu <- function(fs) purrr::map(fs, function(f) as.data.frame(arrow::read_parquet(f))) |> purrr::list_rbind()
tir_A <- lapply(ch_pops, lu)
unlink(unlist(ch_pops)); invisible(sortie(etape_tirage_das_longs()))
ok("tirage fixe : run complet après suppression des chunks == run par plages, bit à bit", identical(tir_A, lapply(ch_pops, lu)))
tir_all <- purrr::list_rbind(tir_A)
ok("unicité souple : variantes dédoublonnées, colonne nb_variantes_demandees, aucun ^E669", !any(duplicated(tir_all[, c(PIVOTS_LONGS, "graine", "diagnostic_associes")])) && all(tir_all$nb_variantes_demandees >= 1) && sans_e669(tir_all, c("diag2", "graine", "diagnostic_associes")))
ok("une ligne et ses variantes dans le même chunk (aucune clé pivots × graine dans deux fichiers)",
   { cles_par_fichier <- lapply(unlist(ch_pops), function(f){ d <- arrow::read_parquet(f); unique(do.call(paste, c(lapply(c(PIVOTS_LONGS, "graine"), function(cc) as.character(d[[cc]])), sep = "\r"))) })
     toutes <- unlist(cles_par_fichier); length(cles_par_fichier) >= 2 && !any(duplicated(toutes)) && length(toutes) > 0 })
invisible(sortie(etape_habillage_longs()))
ok("habillage fixe : lots habillés par population avec DPEC/TPEC", all(vapply(names(POPULATIONS), function(pp){ fs <- list.files(DIR_HABILLE(pp), pattern = "^lot_", full.names = TRUE); length(fs) == 0 || all(c("DPEC", "TPEC", "mode_entree", "population") %in% names(arrow::read_parquet(fs[1]))) }, logical(1))) && sum(vapply(names(POPULATIONS), function(pp) length(list.files(DIR_HABILLE(pp), pattern = "^lot_")), integer(1))) > 0)
invisible(sortie(lancer("tirage.R")))   # session neuve : finalisation en flux depuis les fichiers
invisible(sortie(etape_finalisation()))
rap_f <- readLines(FICHIER_RAPPORT())
finaux <- lire_corpus_final("C1", branche = "long"); liv1 <- lire_corpus_final("C1"); ml1 <- yaml::read_yaml(FICHIER_LIVRABLE_META())
ok("C1 : UN livrable production/60_export_final/scenarios_C1.parquet + scenarios_C1_meta.yaml (campagne, date, populations, volumes) ; etat_pipeline le liste",
   basename(FICHIER_LIVRABLE()) == "scenarios_C1.parquet" && grepl("/production/60_export_final/", FICHIER_LIVRABLE()) && file.exists(FICHIER_LIVRABLE()) && ml1$campagne == "C1" && !is.null(ml1$date) &&
     setequal(unlist(ml1$populations), names(POPULATIONS)) && ml1$n_long == nrow(finaux) && ml1$total == nrow(liv1) && ml1$forme == "monofichier" &&
     { e <- sortie(ep <- etat_pipeline()); grepl("campagne courante C1 : scenarios_C1.parquet", ep$preuve[ep$etape == "etape_finalisation"]) && ep$statut[ep$etape == "etape_finalisation"] == "FAIT" && ep$partage[ep$etape == "etape_finalisation"] == "[profil]" && ep$partage[ep$etape == "etape_refs"] == "[partagé]" })
ok("livrable unique C1 : longs (toutes populations) == lots habillés, DPEC/TPEC ; courts DE LA campagne == 40_campagnes/C1/habille/courts ; branche en tête ; volumes == méta == rapport ; méta : volumes par branche (lignes, scénarios), ratio réalisé",
   nrow(finaux) == sum(vapply(names(POPULATIONS), function(pp) sum(vapply(list.files(DIR_HABILLE(pp), full.names = TRUE), function(f) nrow(arrow::read_parquet(f)), integer(1))), integer(1))) &&
     all(c("DPEC", "TPEC", "population") %in% names(finaux)) && names(liv1)[1] == "branche" && setequal(unique(liv1$branche), c("long", "court")) &&
     sum(liv1$branche == "court") == nrow(arrow::read_parquet(FICHIER_COURTS_CAMPAGNE())) && meme_modulo_schema(as.data.frame(arrow::read_parquet(FICHIER_COURTS_CAMPAGNE())), liv1[liv1$branche == "court", ]) &&
     ml1$n_court == sum(liv1$branche == "court") && any(grepl("branche long = " %+% ml1$n_long %+% " ; branche court = " %+% ml1$n_court, rap_f)) && ml1$courts$source == FICHIER_COURTS_CAMPAGNE() &&
     ml1$volumes$court$scenarios == dplyr::n_distinct(liv1$id_scenario[liv1$branche == "court"]) && ml1$volumes$long$scenarios == dplyr::n_distinct(liv1$id_scenario[liv1$branche == "long"]) &&
     abs(ml1$ratio_courts_realise - ml1$volumes$court$scenarios / ml1$volumes$long$scenarios) < 1e-3 && ml1$ratio_courts_cible == 1 && any(grepl("ratio courts réalisé", rap_f)) && any(grepl("== C\\. Séjours courts de la campagne", rap_f)) &&
     all(c("identite_livrable", "profil_clinique", "diagnostics", "typologie", "tracabilite", "habillage_admin") %in% names(ml1$familles_colonnes)) && setequal(unlist(ml1$familles_colonnes), names(liv1)) && "repli_admin" %in% ml1$familles_colonnes$habillage_admin)
ok("livrable unique : union de schémas — NA typés croisés (graine, racine, nbda NA chez les courts ; nb_cible, source_ref NA chez les longs) ; population, DPEC/TPEC, campagne renseignés sur les DEUX branches (même statut) ; types unifiés (age texte), id_scenario partout",
   all(is.na(liv1$graine[liv1$branche == "court"])) && all(is.na(liv1$racine[liv1$branche == "court"])) && all(is.na(liv1$nbda[liv1$branche == "court"])) &&
     all(!is.na(liv1$population[liv1$branche == "court"])) && all(!is.na(liv1$DPEC[liv1$branche == "court"])) && all(!is.na(liv1$campagne)) && all(liv1$campagne == "C1") &&
     all(!is.na(liv1$graine[liv1$branche == "long"])) && all(!is.na(liv1$population[liv1$branche == "long"])) && all(is.na(liv1$nb_cible[liv1$branche == "long"])) && all(!is.na(liv1$nb_cible[liv1$branche == "court"])) &&
     is.character(liv1$age) && all(!is.na(liv1$id_scenario)) && all(grepl("^k[0-9a-f]{15}-[0-9]{3}$", liv1$id_scenario[liv1$branche == "court"])) && all(grepl("^[0-9a-f]{16}-[0-9]{3}$", liv1$id_scenario[liv1$branche == "long"])) &&
     all(!is.na(liv1$poids)))
ok("habillage robuste : zéro NA (durée, modes) sur les longs ET les courts du livrable ; plus aucune ligne longue à durée < 3 ni courte hors 0-2 (Q72) ; repli_admin porté (0/1/2) ; rapport §5b (na_habillage = 0, duree_hors_perimetre = 0, distribution du repli) ; anomalies = 0",
   !any(is.na(liv1$duree)) && !any(is.na(liv1$mode_entree)) && !any(is.na(liv1$mode_sortie)) && !any(is.na(liv1$mdp)) && all(liv1$repli_admin %in% 0:2) &&
     all(liv1$duree[liv1$branche == "long"] %in% DUREE_LONGS) && all(liv1$duree[liv1$branche == "court"] %in% DUREE_COURTS) && sum(grepl("duree_hors_perimetre \\(hors [0-9]+-[0-9]+\\) = 0", rap_f)) == 3 &&
     NB_VARIANTES_ADMIN_LONGS == 1L && sum(liv1$branche == "long") == dplyr::n_distinct(liv1$id_scenario[liv1$branche == "long"]) && !any(grepl("-a[0-9]+$", liv1$id_scenario)) && sum(grepl("tenues admin N = 1 : .* lignes_hors_multiplication = 0$", rap_f)) == 3 &&
     ml1$habillage_longs$NB_VARIANTES_ADMIN_LONGS == 1L && ml1$habillage_longs$lignes_attendues == ml1$n_long && is.numeric(liv1$poids) && all(!is.na(liv1$poids)) && all(liv1$poids > 0) && grepl("ré-échantillonnage", ml1$notes_familles$audit) &&
     NB_VARIANTES_ADMIN_COURTS == 1L && sum(liv1$branche == "court") == dplyr::n_distinct(liv1$id_scenario[liv1$branche == "court"]) && !anyDuplicated(liv1$id_scenario) && ml1$id_scenario_dupliques == 0 && any(grepl("id_scenario dupliqués dans le livrable \\(toutes branches\\) = 0$", rap_f)) &&
     ml1$habillage_courts$NB_VARIANTES_ADMIN_COURTS == 1L && ml1$habillage_courts$lignes_attendues == ml1$n_court && sum(grepl("tenues admin N = 1 : .* lignes_hors_multiplication = 0$", rap_f)) == 3 &&
     any(grepl("== 5b\\. Habillage admin", rap_f)) && all(grepl("na_habillage = 0", grep("na_habillage", rap_f, value = TRUE))) && sum(grepl("na_habillage", rap_f)) == 3 && ml1$habillage_longs$na_habillage == 0 && any(grepl("TOTAL anomalies = 0", rap_f)))
ok("type_unite côté courts (livrable C1, §26) : courts peuplés (valeurs de la photographie, zéro NA, tirés AVEC la tenue : (pivots, modes, mdp, type_unite) observés ensemble dans la photographie au niveau fin) ; longs = pivot du profil ; méta : provenance par branche (notes_familles$contexte_sejour), colonnes_na_par_branche sans type_unite côté courts ; revue : colonne remplie sur les deux branches ; rapport na_habillage = 0 avec la colonne",
   { vc <- arrow::read_parquet(FICHIER_REF("ref_v_admin_courts")); lc <- liv1[liv1$branche == "court", ]; ll <- liv1[liv1$branche == "long", ]
     cle_v <- do.call(paste, c(lapply(vc[, c(PIVOTS_COURTS, COLS_ADMIN_COURTS)], as.character), sep = "|")); cle_l <- do.call(paste, c(lapply(lc[, c(PIVOTS_COURTS, COLS_ADMIN_COURTS)], as.character), sep = "|"))
     rv <- utils::read.csv2(FICHIER_REVUE(), stringsAsFactors = FALSE); cat_tu <- unique(lire_catalogue(DIR_CATALOGUE(), colonnes = "type_unite")$type_unite)
     all(!is.na(lc$type_unite)) && all(lc$type_unite %in% vc$type_unite) && all(cle_l[lc$repli_admin == 0] %in% cle_v) && all(!is.na(ll$type_unite)) && all(ll$type_unite %in% cat_tu) &&
       grepl("provenance PAR BRANCHE", ml1$notes_familles$contexte_sejour) && !"type_unite" %in% unlist(ml1$colonnes_na_par_branche$court) && all(c("court", "long") %in% names(ml1$colonnes_na_par_branche)) &&
       "type_unite" %in% names(rv) && all(!is.na(rv$type_unite) & nzchar(rv$type_unite)) && all(c("court", "long") %in% rv$branche) && "type_unite" %in% ml1$habillage_courts$colonnes_controlees })
ok("repli parts au-delà de SEUIL_MONOFICHIER : scenarios_C1/part_*.parquet + méta (forme parts) ; lire_corpus_final identique ; retour au monofichier (idempotence)",
   { assign("SEUIL_MONOFICHIER", 10L, envir = globalenv()); invisible(sortie(etape_finalisation())); mlp <- yaml::read_yaml(FICHIER_LIVRABLE_META()); lp <- lire_corpus_final("C1")
     etat_parts <- !file.exists(FICHIER_LIVRABLE()) && dir.exists(DIR_LIVRABLE_PARTS()) && length(list.files(DIR_LIVRABLE_PARTS(), pattern = "^part_")) >= 2
     assign("SEUIL_MONOFICHIER", 5000000L, envir = globalenv()); invisible(sortie(etape_finalisation())); lm <- lire_corpus_final("C1")
     mlp$forme == "parts" && etat_parts && meme_contenu(lp, liv1) && meme_contenu(lm, liv1) && file.exists(FICHIER_LIVRABLE()) && !dir.exists(DIR_LIVRABLE_PARTS()) })
ok("multiplication admin N = 3 (paramètre de campagne) : lignes longues == scénarios × 3 (moins les strates à moins de 3 combinaisons), suffixes -a2 / -a3 uniques, contrôle §8.2 au rapport ; retour à N = 1 : livrable identique bit à bit",
   { assign("NB_VARIANTES_ADMIN_LONGS", 3L, envir = globalenv()); invisible(sortie(etape_habillage_longs())); invisible(sortie(etape_finalisation())); l3 <- lire_corpus_final("C1", branche = "long"); r3 <- readLines(FICHIER_RAPPORT()); m3 <- yaml::read_yaml(FICHIER_LIVRABLE_META())
     assign("NB_VARIANTES_ADMIN_LONGS", 1L, envir = globalenv()); invisible(sortie(etape_habillage_longs())); invisible(sortie(etape_finalisation()))
     n_sc <- dplyr::n_distinct(id_scenario_base(l3$id_scenario))
     nrow(l3) > n_sc && nrow(l3) <= 3 * n_sc && !anyDuplicated(l3$id_scenario) && any(grepl("-a2$", l3$id_scenario)) && setequal(unique(id_scenario_base(l3$id_scenario)), unique(finaux$id_scenario)) &&
       m3$habillage_longs$NB_VARIANTES_ADMIN_LONGS == 3L && any(grepl("tenues admin N = 3", r3)) && meme_contenu(lire_corpus_final("C1"), liv1) })
ok("Q76 : courts N = 2 (paramètre de campagne) : 2 tenues pondérées sans remise par scénario, id_scenario suffixé -a2, lignes = scénarios × 2 au rapport (strates à une combinaison chiffrées à part), unicité toutes branches ; retour à N = 1 identique bit à bit",
   { assign("NB_VARIANTES_ADMIN_COURTS", 2L, envir = globalenv()); unlink(DIR_HABILLE_COURTS(), recursive = TRUE); invisible(sortie(etape_tirage_courts())); c2t <- arrow::read_parquet(FICHIER_COURTS_CAMPAGNE()); m2t <- yaml::read_yaml(FICHIER_COURTS_CAMPAGNE_META())
     invisible(sortie(etape_finalisation())); r2t <- readLines(FICHIER_RAPPORT()); liv2t <- lire_corpus_final("C1")
     assign("NB_VARIANTES_ADMIN_COURTS", 1L, envir = globalenv()); unlink(DIR_HABILLE_COURTS(), recursive = TRUE); invisible(sortie(etape_tirage_courts())); invisible(sortie(etape_finalisation()))
     n_sc <- dplyr::n_distinct(id_scenario_base(c2t$id_scenario))
     nrow(c2t) > n_sc && nrow(c2t) <= 2 * n_sc && !anyDuplicated(c2t$id_scenario) && any(grepl("-a2$", c2t$id_scenario)) && setequal(unique(id_scenario_base(c2t$id_scenario)), unique(sc_c1$id_scenario)) && m2t$lignes_attendues == 2 * m2t$scenarios_gardes &&
       any(grepl("tenues admin N = 2 : lignes = " %+% nrow(c2t), r2t)) && any(grepl("id_scenario dupliqués dans le livrable \\(toutes branches\\) = 0$", r2t)) && !anyDuplicated(liv2t$id_scenario) &&
       identical(as.data.frame(arrow::read_parquet(FICHIER_COURTS_CAMPAGNE())), as.data.frame(sc_c1)) && meme_contenu(lire_corpus_final("C1"), liv1) })
ok("finalisation relancée sans lots habillés ni scénarios courts habillés : reconstruction depuis les chunks des DEUX branches (aucun re-tirage), livrable identique",
   { unlink(file.path(DIR_CAMPAGNE(), "habille"), recursive = TRUE); o <- sortie(etape_finalisation()); any(grepl("reconstruction via etape_habillage_longs", o)) && any(grepl("reconstruction via etape_tirage_courts", o)) && any(grepl("déjà présent, sauté", o)) && meme_contenu(lire_corpus_final("C1"), liv1) })
ok("finalisation sans aucun courts de campagne (chunks_courts absents) -> message actionnable (étape de campagne après la sélection), rien écrasé",
   { d_c <- DIR_CHUNKS_COURTS(); d_tmp <- d_c %+% ".bak"; file.rename(d_c, d_tmp); unlink(DIR_HABILLE_COURTS(), recursive = TRUE)
     err <- tryCatch({ invisible(sortie(etape_finalisation())); NULL }, error = function(e) conditionMessage(e)); file.rename(d_tmp, d_c); invisible(sortie(etape_tirage_courts()))
     !is.null(err) && grepl("scénarios courts de la campagne absents", err) && grepl("étape DE CAMPAGNE", err) && meme_contenu(lire_corpus_final("C1"), liv1) })
ok("rapport fixe : réalisé vs cible, doublons éliminés, manque à gagner, anomalies = 0",
   any(grepl("réalisé vs cible", rap_f)) && any(grepl("Doublons éliminés par DP", rap_f)) && any(grepl("manque à gagner", rap_f)) && any(grepl("TOTAL anomalies = 0", rap_f)))
ok("finalisation en flux == statistiques globales (contrôles, taux) sur les mêmes lignes",
   { st <- stats_branche(finaux, PIVOTS_LONGS, ETAPES_ENV$ctx$codes_imprecis, c("diag2", "graine", "diagnostic_associes")); r <- ETAPES_ENV$rapport$longs_fixe
     cles <- c("doublons_categorie", "diabete_hors_flag", "i10_avec_hta_autres", "poids_sous_seuil")
     tot <- Reduce(`+`, lapply(r, function(x) unlist(x$stats$controles[cles]))); identical(unname(as.integer(tot)), unname(as.integer(unlist(st$controles[cles])))) && sum(vapply(r, function(x) x$stats$n, integer(1))) == st$n })
ok("etat_pipeline (fixe) : sélection, chunks par population, habillage, finalisation FAIT", { e <- etat_pipeline(); all(e$statut[e$etape %in% c("etape_selection_longs", "etape_tirage_das_longs", "etape_habillage_longs", "etape_finalisation")] == "FAIT") })
ok("echantillon_revue : tiré du livrable unifié, branches court et long, id_scenario partout", { rv <- readr::read_csv2(FICHIER_REVUE(), show_col_types = FALSE); "court" %in% rv$branche && "long" %in% rv$branche && "id_scenario" %in% names(rv) && all(!is.na(rv$id_scenario[grepl("^longs_", rv$branche)])) })
ok("C1 sans registre : aucun registre écrit ; chunks porteurs d'id_scenario", !dir.exists(DIR_REGISTRE()) && "id_scenario" %in% names(finaux) && !anyDuplicated(finaux$id_scenario[!duplicated(finaux[, c("id_scenario")])]))
# ---- rétro-inscription de C1 (campagne tirée sans registre), puis campagne C2 SOUS registre
cat("\n# campagnes : rétro-inscription de C1, campagne C2 sous registre\n")
invisible(sortie(etape_retro_inscrire(DIR_SELECTION(), DIR_CHUNKS_LONGS(), "C1")))
reg1 <- lire_registre(DIR_REGISTRE())
ok("rétro-inscription : registre_C1 == scénarios réellement tirés (id recalculés sur pivots + graine, DPEC via sélection)",
   reg1$nb_campagnes == 1 && reg1$nb_scenarios == nrow(dplyr::distinct(finaux, id_scenario)) && setequal(reg1$lignes$id_scenario, unique(finaux$id_scenario)) && all(!is.na(reg1$lignes$DPEC)) && setequal(reg1$lignes$id_profil, sel_c1$id_profil[sel_c1$id_profil %in% reg1$lignes$id_profil]))
ok("rétro-inscription idempotente", identical(sortie(etape_retro_inscrire(DIR_SELECTION(), DIR_CHUNKS_LONGS(), "C1")) |> length() > 0, TRUE) && reg1$nb_scenarios == lire_registre(DIR_REGISTRE())$nb_scenarios)
ok("registre : réinscrire une campagne divergente -> stop append-only", grepl("append-only", tryCatch(ecrire_registre_campagne(dplyr::mutate(reg1$lignes, hash_das = "x"), "C1", DIR_REGISTRE()), error = function(e) conditionMessage(e))))
# rétro-inscription des COURTS de C1 (corpus courts tiré sans registre) : EXTENSION append-only du registre_C1 (branche court ajoutée, longs intacts)
log_rc <- sortie(etape_retro_inscrire_courts("C1", FICHIER_COURTS_CAMPAGNE("C1")))
reg1 <- lire_registre(DIR_REGISTRE()); sc_c1 <- arrow::read_parquet(FICHIER_COURTS_CAMPAGNE("C1"))
ok("rétro-inscription courts : registre_C1 étendu (branche court), scénarios inscrits == scénarios distincts du corpus courts (lignes = variantes admin), longs intacts, ids k…, DPEC renseignés, idempotente",
   any(grepl("étendu \\(append-only\\) — branche court ajoutée", log_rc)) && any(grepl("\\(égaux\\)", log_rc)) && reg1$nb_campagnes == 1 && sum(reg1$lignes$branche == "court") == dplyr::n_distinct(sc_c1$id_scenario) &&
     sum(reg1$lignes$branche == "long") == nrow(dplyr::distinct(finaux, id_scenario)) && setequal(reg1$lignes$id_scenario[reg1$lignes$branche == "court"], unique(sc_c1$id_scenario)) && all(grepl("^k", reg1$lignes$id_profil[reg1$lignes$branche == "court"])) &&
     all(!is.na(reg1$lignes$DPEC)) && reg1$par_campagne$nb_courts == dplyr::n_distinct(sc_c1$id_scenario) && { invisible(sortie(etape_retro_inscrire_courts("C1", FICHIER_COURTS_CAMPAGNE("C1")))); lire_registre(DIR_REGISTRE())$nb_scenarios == reg1$nb_scenarios })
ok("etat_pipeline : ligne registre", { e <- etat_pipeline(); e$statut[e$etape == "registre_tirages"] == "FAIT" && grepl("1 campagne", e$preuve[e$etape == "registre_tirages"]) })
# Épuisement déterministe d'un DP (registre synthétique C1b) pour exercer le recyclage en C2 : toutes les lignes
# adultes du DP le moins fourni (hors classe plafonnée) sont marquées consommées.
parts$pop <- population_de(parts$cage, POPULATIONS)
cand <- parts[parts$pop == "adulte" & !parts$DPEC %in% names(PLAFONDS_DPEC), ] |> dplyr::count(diag2) |> dplyr::arrange(n, diag2)
dp_epuise <- cand$diag2[1]; lig_ep <- parts[parts$pop == "adulte" & parts$diag2 == dp_epuise, ]
reg_c1b <- tibble::tibble(id_profil = lig_ep$id_profil, variante = 1L, id_scenario = id_scenario_de(lig_ep$id_profil, 1L), hash_das = paste0("synth", seq_len(nrow(lig_ep))),
                          campagne = "C1b", population = "adulte", diag2 = dp_epuise, DPEC = lig_ep$DPEC, date = "2026-09-16")
reg_c1b <- reg_c1b[!reg_c1b$id_scenario %in% reg1$lignes$id_scenario, ]   # ne pas dupliquer les id_scenario déjà tirés en C1
if(nrow(reg_c1b) > 0) ecrire_registre_campagne(reg_c1b, "C1b", DIR_REGISTRE())   # sans colonne branche : long implicite
reg1 <- lire_registre(DIR_REGISTRE())
ok("registre synthétique C1b : DP " %+% dp_epuise %+% " entièrement consommé chez les adultes", all(lig_ep$id_profil %in% reg1$par_profil$id_profil) && reg1$nb_campagnes == 2)
# changement de campagne : vider chunks + sélection + méta + habillé (le registre ne se vide JAMAIS)
# arborescence par campagne : 40_campagnes/C2/ est un dossier neuf, rien à vider (C1 reste intact et sera comparé plus bas)
surcharger(SURCHARGE_PROD, "CAMPAGNE <- 'C2'", "REGISTRE_ACTIF <- TRUE"); source(file.path(proj_pr, "config.R"))
invisible(sortie(lancer("tirage.R")))
invisible(sortie(etape_selection_longs()))
sel_c2 <- purrr::list_rbind(purrr::compact(lapply(names(POPULATIONS), function(pp) lire_catalogue(DIR_SELECTION(pp)))))
vierges_c2 <- sel_c2[sel_c2$origine_profil == "vierge", ]; recycles_c2 <- sel_c2[sel_c2$origine_profil == "recycle", ]
ok("C2 : anti-jointure effective (lignes vierges hors registre C1), recyclage seulement quand le DP n'a plus de ligne vierge",
   !any(vierges_c2$id_profil %in% reg1$lignes$id_profil) && all(recycles_c2$id_profil %in% reg1$lignes$id_profil) && nrow(recycles_c2) > 0 &&
     { grp <- function(d) ifelse(d$DPEC %in% names(PLAFONDS_DPEC), d$DPEC, ".reste"); parts$pop <- population_de(parts$cage, POPULATIONS); parts$grp <- grp(parts); recycles_c2$grp <- grp(recycles_c2)
       all(vapply(seq_len(nrow(recycles_c2)), function(i){ lig <- parts[parts$diag2 == recycles_c2$diag2[i] & parts$pop == recycles_c2$population[i] & parts$grp == recycles_c2$grp[i], ]; all(lig$id_profil %in% reg1$lignes$id_profil) }, logical(1))) })
ok("C2 : recyclage numéroté après variante_max de C1, hash_das de C1 exclus",
   all(recycles_c2$variante_debut == reg1$par_profil$variante_max[match(recycles_c2$id_profil, reg1$par_profil$id_profil)] + 1L) && all(nzchar(recycles_c2$hash_exclus)) && all(vierges_c2$variante_debut == 1L))
ok("C2 : plancher automatique — chaque DP du catalogue de la population sélectionné", all(unique(paste(population_de(parts$cage, POPULATIONS), parts$diag2)) %in% unique(paste(sel_c2$population, sel_c2$diag2))))
mt2 <- yaml::read_yaml(FICHIER_SELECTION_META())
ok("meta_tirage C2 : campagne, registre actif, recette tracés ; garde-fou sur CAMPAGNE", mt2$CAMPAGNE == "C2" && isTRUE(mt2$REGISTRE_ACTIF) && mt2$RECETTE_ID == RECETTE_ID && grepl("CAMPAGNE", verifier_meta_tirage(mt2, modifyList(mt2, list(CAMPAGNE = "C3")), c("CAMPAGNE"))))
# courts C2 SOUS registre : recyclage = variantes nouvelles directement (pivots peu nombreux, destinés à resservir)
log_cc2 <- sortie(etape_tirage_courts()); mcc2 <- yaml::read_yaml(FICHIER_COURTS_CAMPAGNE_META()); sc_c2 <- arrow::read_parquet(FICHIER_COURTS_CAMPAGNE())
ok("courts C2 sous registre : pivots déjà tirés en C1 recyclés (variantes numérotées après variante_max de C1, hash_das de C1 exclus), pivots nouveaux vierges ; budget == ratio × volume attendu C2 ; déterminisme par campagne (C2 != C1)",
   { vm1 <- reg1$par_profil; rec <- sc_c2[sc_c2$id_profil %in% vm1$id_profil, ]; vie <- sc_c2[!sc_c2$id_profil %in% vm1$id_profil, ]
     mcc2$n_pivots_recycles > 0 && nrow(rec) > 0 && all(rec$variante > vm1$variante_max[match(rec$id_profil, vm1$id_profil)]) && all(vie$variante >= 1) &&
       !any(paste(rec$id_profil, rec$hash_das) %in% paste(reg1$lignes$id_profil, reg1$lignes$hash_das)) && mcc2$budget == round(RATIO_COURTS * mt2$volume_attendu) && mcc2$seed_base != mcc1$seed_base &&
       any(grepl("recyclés = " %+% mcc2$n_pivots_recycles, log_cc2)) && !anyDuplicated(c(unique(sc_c1$id_scenario), unique(sc_c2$id_scenario))) })
invisible(sortie(etape_tirage_das_longs())); invisible(sortie(etape_habillage_longs())); invisible(sortie(etape_finalisation()))
reg2 <- lire_registre(DIR_REGISTRE())
finaux2 <- lire_corpus_final("C2", branche = "long"); liv2 <- lire_corpus_final("C2")
# ---- identité de la branche LONGS (chantier « type_unite côté courts ») : empreinte canonique de la branche longs de C2 (fixture figée,
# seeds figés), FIGÉE AVANT le chantier — toute modification des longs (sélection, tirage, habillage, livrable) la ferait changer.
# INVARIANT SACRÉ (Q86 actée, CLAUDE.md §3, journal §26.7) : cette valeur, au même titre que les recettes id_v1 et id_courts_v1, ne se
# modifie JAMAIS sans décision utilisateur explicite consignée au journal. Un chantier qui la fait légitimement changer le DIT et le
# JUSTIFIE AVANT de proposer la nouvelle valeur — jamais l'inverse (on ne « constate » pas une nouvelle empreinte pour la recopier).
digest_branche <- function(d){ d <- as.data.frame(d); d <- d[, sort(names(d)), drop = FALSE]; for(cc in names(d)){ v <- as.character(d[[cc]]); v[is.na(v)] <- ""; d[[cc]] <- v }
  d <- d[do.call(order, c(d, list(method = "radix"))), , drop = FALSE]; substr(sha256_vec(paste(c(paste(names(d), collapse = "\t"), do.call(paste, c(d, sep = "\t"))), collapse = "\n")), 1, 16) }
dig_longs_c2 <- digest_branche(finaux2); cat("   empreinte canonique de la branche longs C2 :", dig_longs_c2, "(", nrow(finaux2), "lignes,", ncol(finaux2), "colonnes )\n")
ok("identité de la branche LONGS avant / après le chantier « type_unite côté courts » : empreinte canonique de la branche longs de C2 == valeur figée AVANT le chantier (a54029641743361a, identique avec et sans arrow)",
   identical(dig_longs_c2, "a54029641743361a") && nrow(finaux2) == 54 && ncol(finaux2) == 36)
ok("C2 : registre_C2 (deux branches) écrit automatiquement en fin de finalisation ; 3 campagnes (C1, C1b, C2) ; scénarios C2 == corpus C2 (longs ET courts) ; agrégats par branche",
   reg2$nb_campagnes == 3 && setequal(reg2$lignes$id_scenario[reg2$lignes$campagne == "C2"], unique(liv2$id_scenario)) && setequal(reg2$lignes$id_scenario[reg2$lignes$campagne == "C2" & reg2$lignes$branche == "long"], unique(finaux2$id_scenario)) &&
     reg2$par_campagne$nb_courts[reg2$par_campagne$campagne == "C2"] == dplyr::n_distinct(sc_c2$id_scenario) && reg2$par_campagne$nb_courts[reg2$par_campagne$campagne == "C1b"] == 0 && nrow(reg2$par_branche) == 2 &&
     !anyDuplicated(liv2$id_scenario) && yaml::read_yaml(FICHIER_LIVRABLE_META())$id_scenario_dupliques == 0 &&
     yaml::read_yaml(FICHIER_LIVRABLE_META())$volumes$court$scenarios == dplyr::n_distinct(sc_c2$id_scenario))
ok("C2 : le DP épuisé est recyclé (variantes numérotées après C1/C1b) et retenu par le plancher", dp_epuise %in% recycles_c2$diag2 && dp_epuise %in% sel_c2$diag2)
ok("bout-en-bout : aucun id_scenario dupliqué dans l'union C1 ∪ C2 toutes branches ; aucun hash_das réutilisé pour un même profil (long ou pivot court) entre campagnes",
   !anyDuplicated(reg2$lignes$id_scenario) && !anyDuplicated(reg2$lignes[, c("id_profil", "hash_das")]) && !anyDuplicated(c(unique(finaux$id_scenario), unique(finaux2$id_scenario))) && !anyDuplicated(c(unique(liv1$id_scenario), unique(liv2$id_scenario))))
ok("courts C2 : campagne inscrite côté courts = close (chunks_courts absents -> stop, aucun re-tirage) ; chunks présents -> relecture sûre",
   { d_c <- DIR_CHUNKS_COURTS(); d_tmp <- d_c %+% ".bak"; file.rename(d_c, d_tmp)
     err <- tryCatch({ invisible(sortie(etape_tirage_courts())); NULL }, error = function(e) conditionMessage(e)); file.rename(d_tmp, d_c); o <- sortie(etape_tirage_courts())
     !is.null(err) && grepl("campagne CLOSE côté courts", err) && grepl("Aucun re-tirage", err) && any(grepl("courts déjà inscrits au registre", o)) && any(grepl("déjà présent, sauté", o)) && identical(as.data.frame(arrow::read_parquet(FICHIER_COURTS_CAMPAGNE())), as.data.frame(sc_c2)) })
dp_cat <- unique(parts$diag2)
# Strates de référence vides (sample_das_long renvoie NULL : aucun candidat hors graine) : seules
# causes admises d'absence d'un DP dans un corpus, avec les recyclages entièrement éliminés en C2.
idx_t <- indexer_ref_das(arrow::read_parquet(file.path(DIR_REFERENCES, nom_ref("ref_das_aigu"))))
strate_vide <- function(sel) vapply(seq_len(nrow(sel)), function(i){ tmp <- idx_t[[cle_strate(sel$diag2[i], sel$mode_hospit[i], sel$sexe[i], sel$cage[i], sel$ghm2[i])]]
  is.null(tmp) || nrow(tmp[!tmp$das %in% split_das(sel$diagnostic_associes[i])[[1]], , drop = FALSE]) == 0 }, logical(1))
dp_vides <- function(sel){ v <- strate_vide(sel); setdiff(unique(sel$diag2), unique(sel$diag2[!v])) }
ok("plancher : chaque DP du catalogue sélectionné en C1 comme en C2", all(dp_cat %in% sel_c1$diag2) && all(dp_cat %in% sel_c2$diag2))
ok("chaque DP présent dans le corpus C1 sauf strates de référence vides ; dans C2 sauf strates vides et recyclages entièrement éliminés (souplesse actée, comptés)",
   { m1 <- setdiff(dp_cat, unique(reg2$lignes$diag2[reg2$lignes$campagne == "C1"])); m2 <- setdiff(dp_cat, unique(reg2$lignes$diag2[reg2$lignes$campagne == "C2"]))
     cat("   DP absents : C1 =", length(m1), "(strates vides", length(dp_vides(sel_c1)), ") ; C2 =", length(m2), "\n")
     all(m1 %in% dp_vides(sel_c1)) && all(m2 %in% c(dp_vides(sel_c2), recycles_c2$diag2)) && length(m2) < length(dp_cat) })
ok("classe plafonnée aux volumes attendus dans les deux campagnes (1 par DP de la classe)",
   { c1 <- reg2$lignes[reg2$lignes$campagne == "C1" & reg2$lignes$branche == "long" & reg2$lignes$DPEC == "Accouchement normal mère", ]; c2 <- reg2$lignes[reg2$lignes$campagne == "C2" & reg2$lignes$branche == "long" & reg2$lignes$DPEC == "Accouchement normal mère", ]
     nrow(c1) == dplyr::n_distinct(paste(c1$population, c1$diag2)) && nrow(c2) <= dplyr::n_distinct(paste(sel_c2$population[sel_c2$DPEC == "Accouchement normal mère"], sel_c2$diag2[sel_c2$DPEC == "Accouchement normal mère"])) && nrow(c1) > 3 })
ok("rapport C2 : section campagne (vierges / recyclés), consommation cumulée par DPEC, DP proches de l'épuisement",
   { r2 <- readLines(FICHIER_RAPPORT()); any(grepl("Campagne C2 \\(registre actif\\)", r2)) && any(grepl("consommation cumulée du catalogue par DPEC", r2)) && any(grepl("épuisement total", r2)) && any(grepl("Classes DPEC plafonnées", r2)) })
ok("corpus C2 : id_scenario, origine des profils et DPEC/TPEC en sortie", all(c("id_scenario", "id_profil", "hash_das", "DPEC", "TPEC") %in% names(finaux2)) && "recycle" %in% sel_c2$origine_profil)
# --- lot « notebook campagnes » : deux corpus le même jour, garde-fous, relecture datée inter-sessions
ok("C1 puis C2 : deux livrables (scenarios_C1, scenarios_C2) et deux dossiers 40_campagnes/<C>/, annexes et livrable de C1 intacts (mêmes id_scenario, rapport_C1 présent)",
   all(file.exists(file.path(DIR_EXPORT_FINAL, c("scenarios_C1.parquet", "scenarios_C1_meta.yaml", "rapport_C1.txt", "echantillon_revue_C1.csv", "scenarios_C2.parquet", "rapport_C2.txt")))) && basename(FICHIER_LIVRABLE()) == "scenarios_C2.parquet" &&
     dir.exists(file.path(DIR_CAMPAGNES, "C1", "selection")) && dir.exists(file.path(DIR_CAMPAGNES, "C2", "selection")) && yaml::read_yaml(file.path(DIR_EXPORT_FINAL, "scenarios_C1_meta.yaml"))$campagne == "C1" &&
     setequal(lire_corpus_final("C1", branche = "long")$id_scenario, finaux$id_scenario) && !identical(sort(unique(finaux2$id_scenario)), sort(unique(finaux$id_scenario))))
# Q49 ACTÉE : campagne inscrite = close ; sélection présente de la même campagne -> relecture (reprise sûre) ; sinon stop
ok("Q49 (a) : reprise complète du lanceur après finalisation + registre -> no-op sûr (sélection relue, chunks sautés, corpus repris, registre inchangé)",
   { reg_avant <- lire_registre(DIR_REGISTRE())$nb_scenarios; Sys.unsetenv("SCENARIOS_PMSI_ETAPES_SEULEMENT")
     log_rep <- sortie(lancer("tirage.R")); Sys.setenv(SCENARIOS_PMSI_ETAPES_SEULEMENT = "1")
     any(grepl("déjà inscrite au registre .* sélection relue, aucune nouvelle sélection", log_rep)) && any(grepl("déjà présent, sauté", log_rep)) && any(grepl("même campagne", log_rep)) && any(grepl("courts déjà inscrits au registre", log_rep)) &&
       lire_registre(DIR_REGISTRE())$nb_scenarios == reg_avant && setequal(lire_corpus_final("C2", branche = "long")$id_scenario, finaux2$id_scenario) && setequal(lire_corpus_final("C2", branche = "court")$id_scenario, sc_c2$id_scenario) })
ok("Q49 (b) : campagne inscrite SANS sélection sur disque -> stop « campagne close », renvoi au chunk ouvrir_campagne de 02_campagne.Rmd, aucun fichier touché",
   { f_mt <- FICHIER_SELECTION_META(); f_bak <- f_mt %+% ".bak"; file.rename(f_mt, f_bak)
     avant <- file.info(list.files(DIR_SELECTION(), recursive = TRUE, full.names = TRUE))$mtime
     err <- tryCatch({ invisible(sortie(etape_selection_longs())); NULL }, error = function(e) conditionMessage(e)); file.rename(f_bak, f_mt)
     !is.null(err) && grepl("campagne CLOSE \\(aucune sélection sur disque\\)", err) && grepl("chunk ouvrir_campagne de 02_campagne.Rmd", err) && identical(avant, file.info(list.files(DIR_SELECTION(), recursive = TRUE, full.names = TRUE))$mtime) })
ok("Q49 (c) : sélection présente d'une AUTRE campagne -> stop « campagne close », aucun re-tirage",
   { f_mt <- FICHIER_SELECTION_META(); orig <- readLines(f_mt); mt_x <- yaml::read_yaml(f_mt); mt_x$CAMPAGNE <- "C1"; yaml::write_yaml(mt_x, f_mt)
     err <- tryCatch({ invisible(sortie(etape_selection_longs())); NULL }, error = function(e) conditionMessage(e)); writeLines(orig, f_mt)
     !is.null(err) && grepl("la sélection présente porte la campagne C1", err) && grepl("Aucun re-tirage possible", err) })
ok("Q53 : etape_registre_campagne refuse de s'exécuter sous PALIER_ACTIF (une mesure n'écrit jamais au registre)",
   { assign("PALIER_ACTIF", TRUE, envir = globalenv()); err <- tryCatch({ invisible(sortie(etape_registre_campagne("C2"))); NULL }, error = function(e) conditionMessage(e)); rm("PALIER_ACTIF", envir = globalenv())
     !is.null(err) && grepl("PALIER active", err) && grepl("jamais au registre", err) })
ok("chunk `rapport` de 02_campagne.Rmd exécuté AVANT la finalisation (dossier d'exports vide) -> message actionnable, aucune erreur R",
   { l <- readLines(file.path(racine, "02_campagne.Rmd"), warn = FALSE); i <- grep("^```\\{r rapport\\}", l); j <- i + which(grepl("^```\\s*$", l[(i + 1):length(l)]))[1]
     ex_sauve <- DIR_EXPORT_FINAL; d_vide <- file.path(tempdir(), "export_vide"); dir.create(d_vide, showWarnings = FALSE); assign("DIR_EXPORT_FINAL", d_vide %+% "/", envir = globalenv())
     out <- tryCatch(sortie(eval(parse(text = l[(i + 1):(j - 1)]), envir = globalenv())), error = function(e) "ERREUR : " %+% conditionMessage(e)); assign("DIR_EXPORT_FINAL", ex_sauve, envir = globalenv())
     !any(grepl("^ERREUR", out)) && any(grepl("rapport_C2.txt absent — produit par etape_finalisation\\(\\)", out)) })
ok("garde-fou du corpus : dossier de la campagne courante portant le _meta.yaml d'une AUTRE campagne -> stop, rien écrasé",
   { meta_orig <- readLines(FICHIER_LIVRABLE_META()); yaml::write_yaml(list(campagne = "C1", date = "2000-01-01"), FICHIER_LIVRABLE_META()); n_avant <- length(list.files(DIR_EXPORT_FINAL, recursive = TRUE)); mt_liv <- file.info(FICHIER_LIVRABLE())$mtime
     err <- tryCatch({ invisible(sortie(etape_finalisation())); NULL }, error = function(e) conditionMessage(e))
     writeLines(meta_orig, FICHIER_LIVRABLE_META())
     !is.null(err) && grepl("AUTRE campagne \\(C1, du 2000-01-01\\)", err) && length(list.files(DIR_EXPORT_FINAL, recursive = TRUE)) == n_avant && file.info(FICHIER_LIVRABLE())$mtime == mt_liv })
ok("nom stable, date dans le méta : finalisation relancée dans une session neuve -> mêmes noms (aucun fichier daté), livrable C2 réécrit à l'identique, méta et rapport datés du jour",
   { invisible(sortie(lancer("tirage.R"))); log_f <- sortie(etape_finalisation()); ml <- yaml::read_yaml(FICHIER_LIVRABLE_META())
     any(grepl("même campagne", log_f)) && !any(grepl("[0-9]{8}", list.files(DIR_EXPORT_FINAL))) && ml$date == as.character(Sys.Date()) && grepl(as.character(Sys.Date()), readLines(FICHIER_RAPPORT())[1]) &&
       setequal(lire_corpus_final("C2", branche = "long")$id_scenario, finaux2$id_scenario) })
ok("courts de la campagne absents -> message (étape de campagne après la sélection ; tirable partagé, refs)", { m <- message_courts_absent(DIR_HABILLE_COURTS()); grepl("etape_tirage_courts", m) && grepl("magasin partagé", m) && grepl("etape_selection_longs", m) })
ok("garde des magasins (§26) : méta des références ANTÉRIEUR au chantier type_unite (sans COLS_ADMIN_COURTS) -> etape_tirage_courts stoppe en nommant COLS_ADMIN_COURTS et FORCER_REFS ; méta restauré",
   { f_m <- FICHIER_REFERENCES_META(); orig <- readLines(f_m); m <- yaml::read_yaml(f_m); m$COLS_ADMIN_COURTS <- NULL; yaml::write_yaml(m, f_m)
     err <- tryCatch({ invisible(sortie(etape_tirage_courts())); NULL }, error = function(e) conditionMessage(e)); writeLines(orig, f_m)
     !is.null(err) && grepl("COLS_ADMIN_COURTS : magasin =  ; courant = mode_entree,mode_sortie,mdp,type_unite", err) && grepl("FORCER_REFS <- TRUE", err) })
ok("garde des magasins : clé des références en écart (SEUIL_REF_DAS) -> stop nommant la clé, le drapeau FORCER_REFS et la soupape CHEMINS_SURCHARGES",
   { surcharger(SURCHARGE_PROD, "CAMPAGNE <- 'C2'", "REGISTRE_ACTIF <- TRUE", "SEUIL_REF_DAS <- 21"); invisible(sortie(lancer("tirage.R")))
     err <- tryCatch({ invisible(sortie(etape_tirage_courts())); NULL }, error = function(e) conditionMessage(e))
     !is.null(err) && grepl("magasin partagé references", err) && grepl("SEUIL_REF_DAS : magasin = 20 ; courant = 21", err) && grepl("FORCER_REFS <- TRUE", err) && grepl("CHEMINS_SURCHARGES\\$references", err) })
ok("soupape : chemin du magasin surchargé pour ce profil -> divergence autorisée (la garde ne s'applique plus ; magasin vide -> refs manquantes)",
   { surcharger(SURCHARGE_PROD, "CAMPAGNE <- 'C2'", "REGISTRE_ACTIF <- TRUE", "SEUIL_REF_DAS <- 21", "CHEMINS_SURCHARGES$references <- '" %+% file.path(tempdir(), "refs_prod_seules") %+% "'"); invisible(sortie(lancer("tirage.R")))
     err <- tryCatch({ invisible(sortie(etape_tirage_courts())); NULL }, error = function(e) conditionMessage(e)); ok_dir <- DIR_REFERENCES == file.path(tempdir(), "refs_prod_seules") %+% "/"
     surcharger(SURCHARGE_PROD, "CAMPAGNE <- 'C2'", "REGISTRE_ACTIF <- TRUE"); invisible(sortie(lancer("tirage.R")))
     !is.null(err) && grepl("manquant", err) && !grepl("magasin partagé references", err) && ok_dir })
# =============================================================== RÉORGANISATION SUR PLACE ==
cat("\n# réorganisation sur place : ancien results/ ENCOMBRÉ copié dans _a_reorganiser/, plan, executer, pipeline sans re-extraction\n")
proj_reo <- creer_projet("projet_v8_reorg"); Sys.setenv(SCENARIOS_PMSI_PATH = proj_reo, SCENARIOS_PMSI_ETAPES_SEULEMENT = "1")
surcharger(SURCHARGE_PROD_ISOLE, "CAMPAGNE <- 'C3'", "REGISTRE_ACTIF <- TRUE")
src <- file.path(proj, "results")   # les magasins du projet principal servent de matière (ancienne disposition reconstituée)
old <- file.path(proj_reo, "results", "_a_reorganiser")
for(d in c("partiels", "exports/chunks/adulte", "exports_diagnostic", "exports/catalogue_longs_seuil", "exports/registre_tirages", "exports/selection_longs/adulte", "exports/habille/adulte")) dir.create(file.path(old, d), recursive = TRUE, showWarnings = FALSE)
cp <- function(a, b) invisible(file.copy(a, b, overwrite = TRUE))
for(f in list.files(file.path(src, "00_partiels"), pattern = "\\.parquet$")) cp(file.path(src, "00_partiels", f), file.path(old, "partiels", f))
yaml::write_yaml(yaml::read_yaml(file.path(src, "00_partiels", "_meta.yaml"))[c("K_GRAINE_LONGS", "NBDA_MAX", "DUREE_LONGS", "PIVOTS_LONGS", "VERSION_SCRIPT", "date")], file.path(old, "partiels", "partiels_meta.yaml"))
for(o in names(ANCIENS_NOMS_REFS)){ cp(ref_proj(ANCIENS_NOMS_REFS[[o]], src), file.path(old, "exports", o %+% ".parquet")); cp(ref_proj(ANCIENS_NOMS_REFS[[o]], src), file.path(old, "exports_diagnostic", o %+% ".parquet")) }
invisible(Sys.setFileTime(file.path(old, "exports_diagnostic", names(ANCIENS_NOMS_REFS) %+% ".parquet"), as.POSIXct("2026-01-01 00:00:00")))   # doublons inter-profils plus anciens
for(f in list.files(file.path(src, "20_catalogue", "catalogue_longs_seuil"), pattern = "^part_")) cp(file.path(src, "20_catalogue", "catalogue_longs_seuil", f), file.path(old, "exports", "catalogue_longs_seuil", f))
side_src <- yaml::read_yaml(file.path(src, "20_catalogue", "catalogue_longs_seuil", "_meta.yaml")); yaml::write_yaml(side_src[setdiff(names(side_src), c("magasin", CLES_MAGASINS$catalogue))], file.path(old, "exports", "catalogue_longs_seuil", "_sidecar.yaml"))
cp(file.path(src, "20_catalogue", "catalogue_longs_seuil_meta.yaml"), file.path(old, "exports", "catalogue_longs_seuil_meta.yaml")); cp(file.path(src, "20_catalogue", "catalogue_longs_seuil.parquet.ancien"), file.path(old, "exports", "catalogue_longs_seuil.parquet.ancien"))
# corpus courts HISTORIQUE et corpus longs daté = les sorties RÉELLES des anciens scripts d'entrée (projet_v8_ancien) ; chunks courts anciens (ignorés)
cp(f_courts_anc, file.path(old, "exports", "scenarios_courts_v8_20260918.parquet")); cp(f_courts_anc, file.path(old, "exports", "scenarios_courts_v8_20260101.parquet"))
invisible(Sys.setFileTime(file.path(old, "exports", "scenarios_courts_v8_20260101.parquet"), as.POSIXct("2026-01-01 00:00:00")))
arrow::write_parquet(tibble::tibble(x = 1), file.path(old, "exports", "chunks", "courts_chunk_0001.parquet")); writeLines("n: 1", file.path(old, "exports", "chunks", "courts_chunks_meta.yaml"))
cp(f_longs_anc, file.path(old, "exports", "scenarios_longs_tirage_v8_20260918.parquet"))   # forme fichier (quota_dp)
la_anc <- tibble::as_tibble(arrow::read_parquet(f_longs_anc)); la_anc$.pop <- population_de(as.character(la_anc$cage), POPULATIONS)
for(pp in names(POPULATIONS)){ dir.create(file.path(old, "exports", "scenarios_longs_tirage_v8_20260101", pp), recursive = TRUE, showWarnings = FALSE); d_pp <- la_anc[la_anc$.pop == pp, setdiff(names(la_anc), ".pop")]; if(nrow(d_pp)) arrow::write_parquet(d_pp, file.path(old, "exports", "scenarios_longs_tirage_v8_20260101", pp, "part_0001.parquet")) }   # forme dossier <population>/
writeLines(c("rapport ancien", "x"), file.path(old, "exports", "rapport_v8_20260101.txt"))
for(f in c("diagnostic_apports.csv", "recouvrement.csv")) cp(file.path(src, "90_diagnostics", f), file.path(old, "exports", f)); cp(file.path(src, "90_diagnostics", "diagnostic_memoire_production.csv"), file.path(old, "exports", "diagnostic_memoire.csv"))
for(f in list.files(file.path(src, "production", "50_registre", "registre_tirages"))) cp(file.path(src, "production", "50_registre", "registre_tirages", f), file.path(old, "exports", "registre_tirages", f))
arrow::write_parquet(tibble::tibble(x = 1), file.path(old, "exports", "chunks", "adulte", "longs_chunk_0001.parquet")); arrow::write_parquet(tibble::tibble(x = 1), file.path(old, "exports", "selection_longs", "adulte", "part_J.parquet")); arrow::write_parquet(tibble::tibble(x = 1), file.path(old, "exports", "habille", "adulte", "lot_0001.parquet"))
writeLines("x", file.path(old, "exports", "rapport_v8_20260918.txt")); writeLines("x", file.path(old, "exports", "meta_tirage.yaml")); writeLines("notes", file.path(old, "notes.txt")); arrow::write_parquet(tibble::tibble(x = 1), file.path(old, "exports", "bizarre.parquet"))
options(pmsi_mock_interdit = TRUE); invisible(sortie(lancer("tirage.R")))
avant <- list.files(file.path(proj_reo, "results"), recursive = TRUE)
plan_reo <- etape_reorganiser(mode = "plan", migrer_registre = TRUE)
ok("réorganisation (plan) : rien déplacé, tout classé (3 tables), 2 non reconnus listés, doublons datés et inter-profils ignorés, registre reconnu (migrer_registre)",
   identical(avant, list.files(file.path(proj_reo, "results"), recursive = TRUE)) && all(plan_reo$categorie %in% c("reconnu", "ignore", "inconnu")) && sum(plan_reo$categorie == "inconnu") == 2 &&
     all(c("notes.txt", "exports/bizarre.parquet") %in% plan_reo$source[plan_reo$categorie == "inconnu"]) && plan_reo$categorie[plan_reo$source == "exports/scenarios_courts_v8_20260101.parquet"] == "ignore" &&
     all(plan_reo$categorie[grepl("^exports_diagnostic/", plan_reo$source)] == "ignore") && any(grepl("^production/50_registre/", plan_reo$destination[plan_reo$categorie == "reconnu"])))
invisible(sortie(etape_reorganiser(mode = "executer", migrer_registre = TRUE)))
ok("réorganisation (executer) == plan : magasins peuplés (partiels, 9 refs ref_* + tirable courts dans 30_courts + _meta.yaml, catalogue parts + _meta.yaml, corpus courts historique + scenarios_courts_meta.yaml, diagnostics), registre migré, _a_reorganiser intact, inconnus jamais déplacés",
   all(file.exists(file.path(proj_reo, "results", plan_reo$destination[plan_reo$categorie == "reconnu"]))) && file.exists(FICHIER_REFERENCES_META()) && file.exists(FICHIER_COURTS_META()) && file.exists(FICHIER_CATALOGUE_META()) &&
     file.exists(FICHIER_COURTS_HISTORIQUE()) && file.exists(FICHIER_COURTS_HISTORIQUE_META()) && yaml::read_yaml(FICHIER_COURTS_HISTORIQUE_META())$date_origine == "20260918" && yaml::read_yaml(FICHIER_COURTS_META())$magasin == "courts" &&
     !any(grepl("^30_courts/chunks", plan_reo$destination[plan_reo$categorie == "reconnu"])) && plan_reo$categorie[plan_reo$source == "exports/chunks/courts_chunk_0001.parquet"] == "ignore" &&
     length(list.files(DIR_PARTIELS, pattern = "\\.parquet$")) == 6 && all(file.exists(vapply(NOMS_REFS, FICHIER_REF, character(1)))) && !file.exists(file.path(DIR_REFERENCES, "ref_pivots_courts.parquet")) && yaml::read_yaml(FICHIER_CATALOGUE_META())$magasin == "catalogue" &&
     lire_registre(DIR_REGISTRE())$nb_campagnes == 3 && dir.exists(old) && length(list.files(old, recursive = TRUE)) == nrow(plan_reo) && !file.exists(file.path(proj_reo, "results", "notes.txt")) && !file.exists(file.path(proj_reo, "results", "10_references", "bizarre.parquet")))
o2 <- sortie(etape_reorganiser(mode = "executer", migrer_registre = TRUE))
ok("réorganisation idempotente (relance : tout déjà présent, rien recopié)", any(grepl("0 fichiers copiés", o2)))
ok("réorganisation : garde-fou magasin différent déjà présent -> stop", { m <- yaml::read_yaml(file.path(old, "partiels", "partiels_meta.yaml")); m$K_GRAINE_LONGS <- 9L; yaml::write_yaml(m, file.path(old, "partiels", "partiels_meta.yaml"))
   err <- tryCatch({ invisible(sortie(etape_reorganiser(mode = "executer"))); NULL }, error = function(e) conditionMessage(e)); !is.null(err) && grepl("magasin différent déjà présent", err) })
ok("après réorganisation : etat_pipeline FAIT pour refs, partiels, catalogue (partitionné), tirable courts ; courts de la campagne C3 À FAIRE ; aucune garde en écart ; mention [partagé]",
   { e <- etat_pipeline(); all(e$statut[e$etape %in% c("etape_refs", "etape_partiels_longs", "etape_catalogue", "etape_repartitionner_catalogue", "tirable_courts")] == "FAIT") && e$statut[e$etape == "etape_tirage_courts"] == "À FAIRE" &&
     !any(grepl("GARDE EN ÉCART", e$preuve)) && all(e$partage[e$etape %in% c("etape_refs", "etape_catalogue", "tirable_courts")] == "[partagé]") && e$partage[e$etape == "etape_tirage_courts"] == "[profil]" && grepl("corpus courts historique présent", e$preuve[e$etape == "tirable_courts"]) == FALSE })
# ---- ADOPTION (chantier « courts en campagnes » §4) : longs + courts historiques (sorties réelles des anciens scripts), SANS re-tirage
cat("\n# adoption d'une campagne historique : longs datés + corpus courts historique, registre deux branches, livrable adopté\n")
ok("adoption : plusieurs corpus longs datés dans _a_reorganiser/exports -> stop « jamais de choix silencieux » (rien écrit)",
   { err <- tryCatch({ invisible(sortie(etape_adopter_campagne("C0"))); NULL }, error = function(e) conditionMessage(e)); !is.null(err) && grepl("jamais de choix silencieux", err) && grepl("scenarios_longs_tirage_v8_20260101", err) && !file.exists(FICHIER_LIVRABLE("C0")) })
src_dir <- file.path(old, "exports", "scenarios_longs_tirage_v8_20260101")
log_ad <- sortie(etape_adopter_campagne("C0", source_longs = src_dir))
liv0 <- lire_corpus_final("C0"); ml0 <- yaml::read_yaml(FICHIER_LIVRABLE_META("C0")); reg0 <- lire_registre(DIR_REGISTRE())
c0 <- reg0$lignes[reg0$lignes$campagne == "C0", ]; hist_c <- arrow::read_parquet(FICHIER_COURTS_HISTORIQUE())
ok("adoption C0 (forme dossier <population>/) : livrable scenarios_C0 (longs + courts historiques, branche en tête, méta origine = adoption, sources et dates), registre_C0 deux branches, vérification livrable == registre, annexe rapport renommée",
   file.exists(FICHIER_LIVRABLE("C0")) && ml0$origine == "adoption" && ml0$sources$longs == src_dir && ml0$sources$courts == FICHIER_COURTS_HISTORIQUE() && ml0$sources$courts_date_origine == "20260918" && !is.null(ml0$sources$longs_date) &&
     setequal(unique(liv0$branche), c("long", "court")) && sum(liv0$branche == "court") == nrow(hist_c) && sum(liv0$branche == "long") == nrow(la_anc) && setequal(unique(liv0$population[liv0$branche == "long"]), unique(la_anc$.pop)) &&
     reg0$nb_campagnes == 4 && nrow(c0) == dplyr::n_distinct(liv0$id_scenario) && sum(c0$branche == "court") == dplyr::n_distinct(id_scenario_de(id_profil_courts_de(hist_c), hist_c$variante)) && all(grepl("^k", c0$id_profil[c0$branche == "court"])) &&
     isTRUE(ml0$verification_registre$ok) && any(grepl("vérifications livrable / registre : long : .* \\(égaux\\) ; court : .* \\(égaux\\)", log_ad)) && ml0$volumes$court$scenarios == sum(c0$branche == "court") &&
     file.exists(FICHIER_RAPPORT("C0")) && readLines(FICHIER_RAPPORT("C0"))[1] == "rapport ancien" && all(!is.na(liv0$DPEC)) && all(!is.na(c0$DPEC)) && all(liv0$campagne == "C0") &&
     ml0$id_scenario_dupliques_courts_historiques > 0 && anyDuplicated(liv0$id_scenario[liv0$branche == "court"]) > 0 && grepl("convention v7.1.2", ml0$notes_familles$tracabilite) && any(grepl("adopté tel quel, hors contrôle d'unicité", log_ad)))
ok("adoption C0 (§26) : courts historiques sans type_unite -> NA assumé, listé au méta (colonnes_na_par_branche$court), provenance par branche notée (règle aval par défaut) ; longs adoptés : type_unite du pivot ; AUCUNE re-fabrication",
   all(is.na(liv0$type_unite[liv0$branche == "court"])) && all(!is.na(liv0$type_unite[liv0$branche == "long"])) && "type_unite" %in% unlist(ml0$colonnes_na_par_branche$court) && !"type_unite" %in% unlist(ml0$colonnes_na_par_branche$long) &&
     grepl("campagnes adoptées", ml0$notes_familles$contexte_sejour) && !"type_unite" %in% names(hist_c))
ok("adoption C0 : idempotente (mêmes sources -> réécriture identique, registre inchangé) ; autres sources -> stop « différent, rien n'est écrasé » ; autre campagne depuis la forme FICHIER (population reconstituée par cage) == même contenu longs",
   { o2 <- sortie(etape_adopter_campagne("C0", source_longs = src_dir)); liv0b <- lire_corpus_final("C0")
     err <- tryCatch({ invisible(sortie(etape_adopter_campagne("C0", source_longs = file.path(old, "exports", "scenarios_longs_tirage_v8_20260918.parquet")))); NULL }, error = function(e) conditionMessage(e))
     invisible(sortie(etape_adopter_campagne("C0f", source_longs = file.path(old, "exports", "scenarios_longs_tirage_v8_20260918.parquet")))); livf <- lire_corpus_final("C0f", branche = "long")
     any(grepl("réécriture idempotente", o2)) && meme_contenu(liv0b, liv0) && lire_registre(DIR_REGISTRE())$nb_scenarios == reg0$nb_scenarios + nrow(c0) && !is.null(err) && grepl("différent, rien n'est écrasé", err) &&
       setequal(livf$id_scenario, liv0$id_scenario[liv0$branche == "long"]) && setequal(unique(livf$population), unique(la_anc$.pop)) })
ok("adoption : un livrable de la même campagne NON issu d'une adoption -> stop, rien écrasé",
   { yaml::write_yaml(list(campagne = "C1", date = "2026-01-01"), FICHIER_LIVRABLE_META("C1")); arrow::write_parquet(tibble::tibble(x = 1), FICHIER_LIVRABLE("C1"))
     err <- tryCatch({ invisible(sortie(etape_adopter_campagne("C1", source_longs = src_dir))); NULL }, error = function(e) conditionMessage(e)); unlink(c(FICHIER_LIVRABLE_META("C1"), FICHIER_LIVRABLE("C1")))
     !is.null(err) && grepl("n'est pas une adoption : différent, rien n'est écrasé", err) })
Sys.unsetenv("SCENARIOS_PMSI_ETAPES_SEULEMENT"); log_reo <- sortie(lancer("tirage.R")); Sys.setenv(SCENARIOS_PMSI_ETAPES_SEULEMENT = "1")
ok("pipeline déroulé SANS re-extraction sur le répertoire réorganisé (mock interdit) : campagne C3 sous registre migré + adopté (C0, C0f, C1, C1b, C2, C3), courts C3 recyclent les pivots du corpus historique (variantes après l'ancienne variante_max), livrable scenarios_C3 (catalogue identique au méta près)",
   isTRUE(getOption("pmsi_mock_interdit")) && any(grepl("campagne C3 \\(courts\\) : jamais inscrite", log_reo)) && file.exists(FICHIER_LIVRABLE()) && basename(FICHIER_LIVRABLE()) == "scenarios_C3.parquet" && lire_registre(DIR_REGISTRE())$nb_campagnes == 6 &&
     nrow(lire_corpus_final("C3", branche = "court")) == nrow(arrow::read_parquet(FICHIER_COURTS_CAMPAGNE())) && nrow(lire_catalogue(DIR_CATALOGUE())) == yaml::read_yaml(FICHIER_CATALOGUE_META())$nb_lignes_total &&
     { sc3 <- arrow::read_parquet(FICHIER_COURTS_CAMPAGNE()); rec3 <- sc3[sc3$id_profil %in% c0$id_profil, ]; lr <- lire_registre(DIR_REGISTRE())$lignes
       # C0 / C0f (adoption des mêmes sources) et C1 (tirée SANS registre sur les mêmes pivots, puis rétro-inscrite) partagent des id_scenario : artefact
       # de la fixture ; la propriété attendue = les campagnes tirées SOUS registre (C2, C3) ne réutilisent aucun id_scenario ni (profil, hash) des autres
       nrow(rec3) > 0 && all(rec3$variante > NB_TIRAGES_COURTS) && yaml::read_yaml(FICHIER_COURTS_CAMPAGNE_META())$n_pivots_recycles > 0 &&
         !any(lr$id_scenario[lr$campagne == "C3"] %in% lr$id_scenario[lr$campagne != "C3"]) && !anyDuplicated(lr$id_scenario[lr$campagne %in% c("C2", "C3")]) &&
         !any(paste(lr$id_profil, lr$hash_das)[lr$campagne == "C3"] %in% paste(lr$id_profil, lr$hash_das)[lr$campagne != "C3"]) })

# ---- OUBLI d'une campagne au registre (chantier « notebooks par parcours ») : contrepartie de l'inscription dans le fil nominal de 02
cat("\n# oubli d'une campagne : confirmation exigée, les deux branches retirées, livrable intact, identifiants de nouveau tirables, idempotence\n")
reg_av <- lire_registre(DIR_REGISTRE()); c3 <- reg_av$lignes[reg_av$lignes$campagne == "C3", ]; liv3 <- lire_corpus_final("C3"); mt_liv3 <- file.info(FICHIER_LIVRABLE("C3"))$mtime; f_reg3 <- file.path(DIR_REGISTRE(), nom_registre("C3"))
o_non <- sortie(etape_oublier_campagne("C3"))
ok("oubli SANS confirmation : le compte est affiché (longs et courts ; livrable annoncé non touché), rien n'est fait, registre_C3 intact",
   any(grepl(sprintf("campagne C3 : %d scénarios au registre \\(%d longs, %d courts\\)", nrow(c3), sum(c3$branche == "long"), sum(c3$branche == "court")), o_non)) && any(grepl("Rien fait", o_non)) && any(grepl("ne sont PAS touchés", o_non)) &&
     file.exists(f_reg3) && lire_registre(DIR_REGISTRE())$nb_scenarios == reg_av$nb_scenarios && sum(c3$branche == "court") > 0 && sum(c3$branche == "long") > 0)
o_oui <- sortie(etape_oublier_campagne("C3", JE_CONFIRME_OUBLI = TRUE)); reg_ap <- lire_registre(DIR_REGISTRE())
ok("oubli CONFIRMÉ : registre_C3 supprimé, les deux branches retirées, les cinq autres campagnes intactes, livrable et annexes de C3 intacts, statut « jamais inscrite »",
   any(grepl("campagne C3 OUBLIÉE", o_oui)) && !file.exists(f_reg3) && !any(reg_ap$lignes$campagne == "C3") && reg_ap$nb_campagnes == 5 && reg_ap$nb_scenarios == reg_av$nb_scenarios - nrow(c3) &&
     meme_contenu(reg_ap$lignes, reg_av$lignes[reg_av$lignes$campagne != "C3", ]) && file.exists(FICHIER_LIVRABLE("C3")) && file.info(FICHIER_LIVRABLE("C3"))$mtime == mt_liv3 && file.exists(FICHIER_LIVRABLE_META("C3")) && file.exists(FICHIER_RAPPORT("C3")) &&
     meme_contenu(lire_corpus_final("C3"), liv3) && !statut_campagne_registre("C3", reg_ap)$inscrite)
ok("oubli idempotent : campagne absente -> « rien à oublier », aucune erreur, registre inchangé", { o3 <- sortie(etape_oublier_campagne("C3", JE_CONFIRME_OUBLI = TRUE)); any(grepl("rien à oublier", o3)) && lire_registre(DIR_REGISTRE())$nb_scenarios == reg_ap$nb_scenarios })
# les identifiants redeviennent tirables : une campagne C4 ouverte après l'oubli retrouve VIERGES les profils et pivots que seule C3 avait consommés
seuls_c3 <- setdiff(unique(c3$id_profil), reg_ap$lignes$id_profil)
surcharger(SURCHARGE_PROD_ISOLE, "CAMPAGNE <- 'C4'", "REGISTRE_ACTIF <- TRUE"); invisible(sortie(lancer("tirage.R")))
invisible(sortie(etape_selection_longs())); sel_c4 <- purrr::list_rbind(purrr::compact(lapply(names(POPULATIONS), function(pp) lire_catalogue(DIR_SELECTION(pp)))))
invisible(sortie(etape_tirage_courts())); sc_c4 <- arrow::read_parquet(FICHIER_COURTS_CAMPAGNE())
ok("après l'oubli, C4 re-tire les identifiants libérés : profils longs que seule C3 avait consommés -> sélectionnés VIERGES (variante_debut = 1, non recyclés) ; pivots courts libérés -> variantes reprises à 1 ; C3 absente du registre",
   { v4 <- sel_c4[sel_c4$id_profil %in% seuls_c3, ]; p4 <- sc_c4[sc_c4$id_profil %in% seuls_c3, ]
     cat("   profils libérés par l'oubli :", length(seuls_c3), "; re-sélectionnés en C4 :", nrow(v4), "; pivots courts libérés re-tirés :", dplyr::n_distinct(p4$id_profil), "\n")
     length(seuls_c3) > 0 && nrow(v4) > 0 && all(v4$origine_profil == "vierge") && all(v4$variante_debut == 1L) && (nrow(p4) == 0 || all(tapply(p4$variante, p4$id_profil, min) == 1L)) &&
       !"C3" %in% lire_registre(DIR_REGISTRE())$lignes$campagne })

Sys.unsetenv("SCENARIOS_PMSI_ETAPES_SEULEMENT")
Sys.setenv(SCENARIOS_PMSI_PATH = proj, SCENARIOS_PMSI_PROFIL = "diagnostic"); surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)"); source(file.path(proj, "config.R"))
options(pmsi_mock_interdit = TRUE)
invisible(sortie(lancer("tirage.R")))   # rétablit l'état de session du projet principal

# changement de paramètres -> garde-fou meta_tirage, puis mode catalogue_complet
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)", "MODE_SELECTION <- 'catalogue_complet'", "NB_CRH_CIBLE <- " %+% (3 * nrow(cat_multi)) %+% "L", "BUDGET_TOTAL_LONGS <- " %+% (3 * nrow(cat_multi)) %+% "L")
err <- tryCatch({ invisible(sortie(lancer("tirage.R"))); NULL }, error = function(e) conditionMessage(e))
ok("selection/_meta.yaml : paramètres différents -> stop() renvoyant vers une nouvelle campagne ou le vidage de 40_campagnes/<C>/", !is.null(err) && grepl("selection/_meta.yaml", err) && grepl("40_campagnes", err))
unlink(DIR_CAMPAGNE(), recursive = TRUE)   # vidage de la campagne diagnostic C1 (sélection, chunks, habillé) : le livrable et le registre ne sont pas touchés
log_t3 <- sortie(lancer("tirage.R"))
mt3 <- yaml::read_yaml(FICHIER_SELECTION_META())
ok("catalogue_complet : NB_VARIANTES = 3, volume attendu = nrow × 3, variante max = 3",
   mt3$NB_VARIANTES == 3 && mt3$volume_attendu == 3 * nrow(cat_multi) && max(lire_longs()$variante) == 3 && ETAPES_ENV$rapport$longs_tirage_n <= mt3$volume_attendu)
ok("rapport catalogue_complet : ligne nrow / NB_VARIANTES / volume", any(grepl("mode catalogue_complet : nrow catalogue", readLines(FICHIER_RAPPORT()))))
# ---- trois niveaux de paramètres : écriture des surcharges depuis le notebook, exclusivité palier / campagne (fichiers en tempdir)
surch_test <- Sys.getenv("SCENARIOS_PMSI_SURCHARGE"); d_s <- file.path(tempdir(), "surcharges"); dir.create(d_s, showWarnings = FALSE)
ok("ecrire_surcharge_campagne : campagne.R écrit (contenu attendu), SCENARIOS_PMSI_SURCHARGE posée, message « Restart R puis chunk session »",
   { Sys.setenv(SCENARIOS_PMSI_SURCHARGE = ""); o <- sortie(f <- ecrire_surcharge_campagne("C7", 1234L, 2L, TRUE, fichier = file.path(d_s, "campagne.R")))
     identical(readLines(f), contenu_surcharge_campagne("C7", 1234L, 2L, TRUE)) && Sys.getenv("SCENARIOS_PMSI_SURCHARGE") == normalizePath(f) && any(grepl("Restart R puis chunk `session`", o)) })
ok("exclusivité : palier refusé tant que campagne.R est active (message : Sys.setenv + Restart R) ; campagne réécrite sur elle-même acceptée",
   { err <- tryCatch({ ecrire_surcharge_palier(100L, fichier = file.path(d_s, "palier.R")); NULL }, error = function(e) conditionMessage(e))
     !is.null(err) && grepl("surcharge palier refusée : la CAMPAGNE", err) && grepl("Sys.setenv", err) && !file.exists(file.path(d_s, "palier.R")) &&
       { invisible(sortie(ecrire_surcharge_campagne("C8", 99L, 1L, FALSE, fichier = file.path(d_s, "campagne.R")))); any(grepl('CAMPAGNE <- "C8"', readLines(file.path(d_s, "campagne.R")))) } })
ok("exclusivité : surcharge retirée -> palier accepté ; puis campagne refusée sous palier (message : chunk vider_palier) ; palier retiré -> campagne acceptée",
   { Sys.setenv(SCENARIOS_PMSI_SURCHARGE = ""); invisible(sortie(ecrire_surcharge_palier(100L, fichier = file.path(d_s, "palier.R"))))
     err <- tryCatch({ ecrire_surcharge_campagne("C9", 10L, fichier = file.path(d_s, "campagne.R")); NULL }, error = function(e) conditionMessage(e))
     Sys.setenv(SCENARIOS_PMSI_SURCHARGE = ""); unlink(file.path(d_s, "palier.R")); invisible(sortie(ecrire_surcharge_campagne("C9", 10L, fichier = file.path(d_s, "campagne.R"))))
     file.exists(file.path(d_s, "palier.R")) == FALSE && !is.null(err) && grepl("surcharge campagne refusée : le PALIER", err) && grepl("vider_palier", err) && any(grepl('CAMPAGNE <- "C9"', readLines(file.path(d_s, "campagne.R")))) })
ok("source des paramètres en session : campagne.R active -> CAMPAGNE et NB_CRH_CIBLE « surcharge campagne », PLAFONDS_DPEC « défaut config » ; sans surcharge -> défaut config",
   { Sys.setenv(SCENARIOS_PMSI_SURCHARGE = file.path(d_s, "campagne.R")); source(file.path(proj, "config.R")); o1 <- sortie(s1 <- afficher_sources_campagne())
     Sys.setenv(SCENARIOS_PMSI_SURCHARGE = ""); source(file.path(proj, "config.R")); s0 <- afficher_sources_campagne()
     CAMPAGNE == "C1" && s1[["CAMPAGNE"]] == "surcharge campagne (campagne.R)" && s1[["NB_CRH_CIBLE"]] == "surcharge campagne (campagne.R)" && s1[["PLAFONDS_DPEC"]] == "défaut config" && any(grepl("C9", o1)) && all(s0 == "défaut config") })
Sys.setenv(SCENARIOS_PMSI_SURCHARGE = surch_test); source(file.path(proj, "config.R"))   # rétablit la surcharge de test
ok("la phase tirage n'a jamais touché la base (mock interdit resté silencieux)", isTRUE(getOption("pmsi_mock_interdit")))
options(pmsi_mock_interdit = FALSE)

cat("\nSIMULATION SQLITE (scripts réels, sessions multiples) VERTE :", n_ok, "assertions ; arrow =", if(ARROW_MOCK) "mock RDS" else "réel", "\n")
