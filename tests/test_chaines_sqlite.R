###############################################################################
# tests/test_chaines_sqlite.R — les SCRIPTS RÉELS du v8 sur une base SQLite FICHIER
#
# Objet : exécuter extraction_associations_codes_v8.R puis tirage_scenarios_v8.R tels
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
racine <- normalizePath(c(".", "..")[file.exists(c("config_v8.R", "../config_v8.R"))][1])
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

# ------------------------------------------------ tables factices (base fichier) --
# demo/generateur_donnees_fictives.R (source unique) : N = 4000 par millésime (17, 20, 26), graine 20260907,
# fixtures B1-10 (IDENT_B110) et fusion E669 (IDENT_FUSION, GHM 88M991) sur le millésime 26.
fx <- generer_donnees_fictives(db_file); IDENT_B110 <- fx$IDENT_B110; IDENT_FUSION <- fx$IDENT_FUSION
sortie <- function(expr) utils::capture.output(expr, type = "output")

# =============================================================== SESSION 1 ==
cat("\n# session 1 : extraction partielle (années 17 et 26, CHR/U et CH)\n")
surcharger("ANS_HISTORIQUE <- c(17L, 26L)")
log1 <- sortie(lancer("extraction_associations_codes_v8.R"))
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
ok("partiels écrits : 4 parquet + partiels_meta.yaml",
   setequal(list.files(PARTIELS_DIR), c("catalogue_partiel_CHRU_17.parquet", "catalogue_partiel_CHRU_26.parquet", "catalogue_partiel_CH_17.parquet", "catalogue_partiel_CH_26.parquet", "partiels_meta.yaml")))
ok("exports : 10 refs + catalogue + meta + diagnostic_apports.csv",
   all(c(nom_ref(NOMS_REFS), "catalogue_longs_seuil.parquet", "catalogue_longs_seuil_meta.yaml", "diagnostic_apports.csv") %in% list.files(EXPORTS_DIR)))
ap1 <- utils::read.csv(file.path(EXPORTS_DIR, "diagnostic_apports.csv"))
ok("diagnostic_apports : 4 lignes calculées, stats du partiel seul (sans colonnes cumul), ordre types × années",
   nrow(ap1) == 4 && all(ap1$statut == "calculé") && identical(names(ap1), c("etbs", "an", "statut", "nb_lignes_partiel", "sum_n_partiel", "nb_diag2_partiel", "nb_diag2_nouveaux")) &&
     identical(ap1$etbs, c("CHR/U", "CHR/U", "CH", "CH")) && identical(ap1$an, c(17L, 26L, 17L, 26L)) && ap1$nb_diag2_nouveaux[1] == ap1$nb_diag2_partiel[1] && all(ap1$sum_n_partiel >= ap1$nb_lignes_partiel))
# refs : contrôles de contenu
df_das_ref <- arrow::read_parquet(file.path(EXPORTS_DIR, "ref_das_aigu.parquet"))
ok("ref_das_aigu : strate + das + n, sans diabète/I10/astérisques", all(c("mode_hospit","sexe","cage","racine","ghm2","diag2","das","n") %in% names(df_das_ref)) && nrow(df_das_ref) > 0 &&
     !any(df_das_ref$das %in% c(code_did, code_dnid, code_dnid_ins, codes_astrisques_diabete, "I10")))
df_chro <- arrow::read_parquet(file.path(EXPORTS_DIR, "ref_das_chronique.parquet"))
ok("ref_das_chronique : Patho_chro, néo-codes appliqués", all(df_chro$type_liste == "Patho_chro") && !any(df_chro$das %in% c(code_did, code_dnid, code_dnid_ins)) && any(df_chro$das %in% neo_codes_diabete))
df_nbc <- arrow::read_parquet(file.path(EXPORTS_DIR, "ref_nb_chroniques.parquet"))
ok("ref_nb_chroniques : zéros inclus, total = séjours longs distincts", any(df_nbc$nb_chro == 0) && sum(df_nbc$nb) == nrow(dplyr::distinct(pd |> dplyr::filter(duree > DUREE_MIN_REF), ident, cage, sexe)))
df_imp <- arrow::read_parquet(file.path(EXPORTS_DIR, "referentiel_substitution_imprecis.parquet"))
ok("§7.5 : catégories imprécises, niveau joint, seuil", all(df_imp$cat %in% c("I50","J44","N18")) && all(c("cat","code","cage","sexe","nb","niveau","imprecis") %in% names(df_imp)) && all(df_imp$nb >= SEUIL_REF_IMPRECIS) && any(df_imp$code == "N185"))
df_pair <- arrow::read_parquet(file.path(EXPORTS_DIR, "referentiel_paires_chroniques.parquet"))
ok("§7.6 : paires das_a < das_b, seuil", nrow(df_pair) > 0 && all(df_pair$das_a < df_pair$das_b) && all(df_pair$nb >= SEUIL_REF_PAIRES))
df_pc <- arrow::read_parquet(file.path(EXPORTS_DIR, "pivots_courts.parquet"))
ok("pivots_courts : pivots + nb > seuil", all(c(PIVOTS_COURTS, "nb") %in% names(df_pc)) && all(df_pc$nb > SEUIL_PIVOT) && nrow(df_pc) > 0)
ok("ref_comp_diabete : effectifs bruts (pas de pénalisation côté extraction)", { r <- arrow::read_parquet(file.path(EXPORTS_DIR, "ref_comp_diabete.parquet")); all(r$nb == round(r$nb)) && all(c("cage","diabete","comp","nb") %in% names(r)) })
cat1 <- lire_cat(EXPORTS_DIR)
ok("catalogue seuil : poids > SEUIL_PIVOT, pas de colonne n, graine <= K sans diabète/I10",
   all(cat1$poids > SEUIL_PIVOT) && !"n" %in% names(cat1) && all(lengths(split_das(cat1$diagnostic_associes)) <= K_GRAINE_LONGS) &&
     !any(unlist(split_das(cat1$diagnostic_associes)) %in% c(code_did, code_dnid, code_dnid_ins, "I10")))
meta1 <- yaml::read_yaml(file.path(EXPORTS_DIR, "catalogue_longs_seuil_meta.yaml"))
ok("meta.yaml cohérent avec le profil et la surcharge", meta1$PROFIL == "diagnostic" && identical(unlist(meta1$ANS_HISTORIQUE), c(17L, 26L)) && meta1$SEUIL_PIVOT == 1 &&
     meta1$nb_lignes == nrow(cat1) && meta1$MODE_SELECTION == "quota_dp" && meta1$K_GRAINE_LONGS == K_GRAINE_LONGS && meta1$plan_iterations_calculees == 4)
# --- conversion E669 -> E660 (CONVERSION_E669 = TRUE par défaut)
sans_e669 <- function(df, cols) compter_e669(df, cols) == 0
ok("conversion : aucun ^E669 dans le catalogue (diag2, graines)", CONVERSION_E669 && sans_e669(cat1, c("diag2", "diagnostic_associes")) && any(grepl("^E660", cat1$diag2)))
ok("conversion : aucun ^E669 dans les refs (aigu, chronique, paires, imprécis, pivots, v_admin)",
   sans_e669(df_das_ref, c("diag2", "das")) && sans_e669(df_chro, c("diag2", "das")) && sans_e669(df_pair, c("das_a", "das_b")) &&
     sans_e669(df_imp, "code") && sans_e669(df_pc, "diag2") &&
     sans_e669(arrow::read_parquet(file.path(EXPORTS_DIR, "v_admin_courts.parquet")), "diag2") && sans_e669(arrow::read_parquet(file.path(EXPORTS_DIR, "v_admin_longs.parquet")), "diag2"))
ok("conversion : somme des n inchangée sur le catalogue agrégé, distribution E660x exportée",
   ETAPES_ENV$impact_e669$n_total_avant == ETAPES_ENV$impact_e669$n_total_apres && ETAPES_ENV$impact_e669$e669_diag2_suffixe + ETAPES_ENV$impact_e669$e669_graine_suffixe > 0 &&
     file.exists(file.path(EXPORTS_DIR, "distribution_e660.parquet")) && nrow(arrow::read_parquet(file.path(EXPORTS_DIR, "distribution_e660.parquet"))) > 0)
ok("conversion : les partiels restent en codes BRUTS (E669 présents)",
   any(vapply(list.files(PARTIELS_DIR, pattern = "_26\\.parquet$", full.names = TRUE), function(f){ d <- arrow::read_parquet(f); compter_e669(d, c("diag2", "diagnostic_associes")) > 0 }, logical(1))))
ok("conversion : cas de fusion sous-seuil -> au-dessus (GHM 88M991, DP E6690 + E6600 -> E6600, n = 2 > 1)",
   { f <- cat1[cat1$ghm2 == "88M991", ]; nrow(f) == 1 && f$diag2 == "E6600" && f$poids == 2 && ETAPES_ENV$impact_e669$profils_entres >= 1 })
ok("conversion : paires das_a < das_b, aucune paire identique", all(df_pair$das_a < df_pair$das_b))
ok("conversion : meta.yaml porte CONVERSION_E669 et BARE_E669_DEFAUT, rapport d'extraction écrit",
   isTRUE(meta1$CONVERSION_E669) && meta1$BARE_E669_DEFAUT == "0" && file.exists(file.path(EXPORTS_DIR, "rapport_extraction_v8_" %+% DATE_TAG %+% ".txt")) &&
     any(grepl("ENTRÉS par fusion", readLines(file.path(EXPORTS_DIR, "rapport_extraction_v8_" %+% DATE_TAG %+% ".txt")))))
ok("aucune ligne niveau séjour exportée (pas de colonne ident dans les parquets d'exports)",
   !any(vapply(list.files(EXPORTS_DIR, pattern = "\\.parquet$", full.names = TRUE), function(f) "ident" %in% names(arrow::read_parquet(f, as_data_frame = FALSE)), logical(1))))
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
   comparer(arrow::read_parquet(file.path(PARTIELS_DIR, "catalogue_partiel_CH_17.parquet")), prep_scenarios2_ancien_20260912(17L, "CH", DUREE_LONGS, NBDA_MAX, K_GRAINE_LONGS, PIVOTS_LONGS)))
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
    dist <- arrow::read_parquet(file.path(EXPORTS_DIR, "distribution_e660.parquet"))
    d <- d |> convertir_e669_comptes("diag2", c(setdiff(PIVOTS_LONGS, "diag2"), "diagnostic_associes"), "n", dist, BARE_E669_DEFAUT) |>
      convertir_e669_combo("diagnostic_associes", PIVOTS_LONGS, "n", dist, BARE_E669_DEFAUT)
  }
  d |> dplyr::inner_join(d |> dplyr::summarise(nb = sum(n), .by = dplyr::all_of(PIVOTS_LONGS_SEUIL)), by = PIVOTS_LONGS_SEUIL) |>
    dplyr::filter(nb > SEUIL_PIVOT) |> dplyr::select(-n) |> dplyr::rename(poids = nb)
}
ok("P2 catalogue deux étages == ancien flux (conversion TRUE, fixture de fusion E669 incluse)",
   comparer(cat1, ancien_flux(file.path(PARTIELS_DIR, ETAPES_ENV$plan$iterations$fichier), TRUE)) && any(cat1$ghm2 == "88M991"))
ok("recouvrement.csv : (CHR/U 17->26) ok avec parts dans [0,1], (CH 24->25) non calculable",
   { r <- utils::read.csv(file.path(EXPORTS_DIR, "recouvrement.csv")); nrow(r) == 2 && r$statut[1] == "ok" && r$part_combos_B_vues[1] >= 0 && r$part_combos_B_vues[1] <= 1 &&
     r$nb_B[1] == ap1$nb_lignes_partiel[2] && grepl("non calculable", r$statut[2]) })
ok("diagnostic_memoire.csv : schéma, mesures par morceau / partiel / ref / étage",
   { m <- utils::read.csv(file.path(EXPORTS_DIR, "diagnostic_memoire.csv")); all(c("etiquette", "horodatage", "taille_objet_mo", "memoire_utilisee_go", "pic_go", "alerte") %in% names(m)) &&
     any(grepl("morceau", m$etiquette)) && any(grepl("^partiel ", m$etiquette)) && any(grepl("^ref v_admin_longs", m$etiquette)) && any(grepl("étage 1", m$etiquette)) && any(grepl("catalogue final", m$etiquette)) })
ok("P3 : aucun objet ref ni cache brut vivant après l'extraction", !exists("df_ref") && !exists("brute", envir = CACHE_E669) && !exists("pivots_bruts") && !exists("combos") && !exists("df_prep_scenarios") && !exists("df_prep_scenarios_seuil"))
fermer()

# =============================================================== SESSION 2 ==
cat("\n# session 2 : reconnexion, année 20 ajoutée -> seules les itérations manquantes\n")
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)")
log2 <- sortie(lancer("extraction_associations_codes_v8.R"))
ok("plan session 2 : 2 itérations (CHR/U 20, CH 20), 0 ref, année 20 seule, pas de prep_das_chronique",
   sum(ETAPES_ENV$plan$iterations$a_faire) == 2 && all(ETAPES_ENV$plan$iterations$an[ETAPES_ENV$plan$iterations$a_faire] == 20) && !any(ETAPES_ENV$plan$refs$a_faire) &&
     identical(ETAPES_ENV$plan$annees_a_preparer, 20L) && !ETAPES_ENV$plan$prep_das_chronique)
ok("seule prep_data_20 recréée (plus la table top-k unique de l'itération)", setequal(temp_tables(conn), c("prep_data_20", "prep_topk_tmp")))
ok("refs sautées (message)", any(grepl("ref ref_das_aigu : présente, sautée", log2)))
ap2 <- utils::read.csv(file.path(EXPORTS_DIR, "diagnostic_apports.csv"))
ok("diagnostic_apports : 6 lignes, relu pour 17/26 et calculé pour 20",
   nrow(ap2) == 6 && identical(ap2$statut[ap2$an == 20], c("calculé", "calculé")) && all(ap2$statut[ap2$an != 20] == "relu"))
cat_multi <- lire_cat(EXPORTS_DIR)
ok("catalogue étendu (3 années > 2 années)", nrow(cat_multi) > nrow(cat1))
fermer()

# ------------------------------------------------ run mono-session (projet 2) --
cat("\n# run mono-session (3 années d'un coup) : identité du catalogue\n")
proj2 <- creer_projet("projet_v8_mono"); Sys.setenv(SCENARIOS_PMSI_PATH = proj2)
log_mono <- sortie(lancer("extraction_associations_codes_v8.R"))
ok("mono-session : 6 itérations calculées", sum(ETAPES_ENV$plan$iterations$a_faire) == 6)
cat_mono <- lire_cat(EXPORTS_DIR)
ok("identité du catalogue final multi-sessions == mono-session", identical(cat_multi, cat_mono))
ok("identité des partiels (CH, 20)", identical(arrow::read_parquet(file.path(PARTIELS_DIR, "catalogue_partiel_CH_20.parquet")),
                                                arrow::read_parquet(file.path(dirname(dirname(PARTIELS_DIR)), "..", "projet_v8", "results", "partiels", "catalogue_partiel_CH_20.parquet"))))
fermer()
Sys.setenv(SCENARIOS_PMSI_PATH = proj)

# =============================================================== SESSION 3 ==
cat("\n# session 3 : tout présent -> rien à faire\n")
log3 <- sortie(lancer("extraction_associations_codes_v8.R"))
ok("plan : rien à faire, message explicite", ETAPES_ENV$plan$rien_a_faire && any(grepl("TOUT EST A JOUR", log3)))
ok("aucune table temporaire créée", length(temp_tables(conn)) == 0)
ok("catalogue ré-agrégé identique", identical(lire_cat(EXPORTS_DIR), cat_multi))
fermer()

# ------------------------------------------------ FORCER_REFS --
cat("\n# FORCER_REFS\n")
mt_avant <- file.info(file.path(EXPORTS_DIR, nom_ref(NOMS_REFS)))$mtime; Sys.sleep(1.1)
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)", "FORCER_REFS <- TRUE")
invisible(sortie(lancer("extraction_associations_codes_v8.R")))
ok("FORCER_REFS : 9 refs recalculées, 0 itération, année AN_REF seule, prep_das_chronique",
   all(ETAPES_ENV$plan$refs$a_faire) && !any(ETAPES_ENV$plan$iterations$a_faire) && identical(ETAPES_ENV$plan$annees_a_preparer, 26L) && ETAPES_ENV$plan$prep_das_chronique)
ok("tables temporaires : prep_data_26 et prep_das_chro_26 uniquement", setequal(temp_tables(conn), c("prep_data_26", "prep_das_chro_26")))
ok("refs réécrites (mtime)", all(file.info(file.path(EXPORTS_DIR, nom_ref(NOMS_REFS)))$mtime > mt_avant))
fermer()

# ------------------------------------------------ garde-fou partiels_meta --
cat("\n# garde-fou partiels_meta\n")
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)", "K_GRAINE_LONGS <- 3L")
err <- tryCatch({ invisible(sortie(lancer("extraction_associations_codes_v8.R"))); NULL }, error = function(e) conditionMessage(e))
ok("K_GRAINE_LONGS différent -> stop() demandant de vider PARTIELS_DIR", !is.null(err) && grepl("K_GRAINE_LONGS", err) && grepl("PARTIELS_DIR", err))
fermer()

# ------------------------------------------------ reprise des partiels --
cat("\n# reprise des partiels\n")
unlink(file.path(PARTIELS_DIR, "catalogue_partiel_CH_20.parquet"))
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)")
invisible(sortie(lancer("extraction_associations_codes_v8.R")))
ok("un partiel supprimé -> 1 itération, année 20", sum(ETAPES_ENV$plan$iterations$a_faire) == 1 && identical(ETAPES_ENV$plan$annees_a_preparer, 20L))
ok("catalogue identique après reprise", identical(lire_cat(EXPORTS_DIR), cat_multi))
fermer()

# ------------------------------------------------ CONVERSION_E669 = FALSE (toggle effectif) --
cat("\n# CONVERSION_E669 = FALSE : E669 présents, partiels bruts identiques\n")
proj3 <- creer_projet("projet_v8_noconv"); Sys.setenv(SCENARIOS_PMSI_PATH = proj3)
surcharger("ANS_HISTORIQUE <- c(17L, 26L)", "CONVERSION_E669 <- FALSE")
invisible(sortie(lancer("extraction_associations_codes_v8.R")))
cat_nc <- lire_cat(EXPORTS_DIR)
ok("toggle FALSE : ^E669 présents dans le catalogue et les refs", compter_e669(cat_nc, c("diag2", "diagnostic_associes")) > 0 &&
     compter_e669(arrow::read_parquet(file.path(EXPORTS_DIR, "ref_das_chronique.parquet")), "das") > 0 && !isTRUE(yaml::read_yaml(file.path(EXPORTS_DIR, "catalogue_longs_seuil_meta.yaml"))$CONVERSION_E669))
ok("toggle FALSE : le cas de fusion n'entre pas au catalogue (deux profils n = 1 <= seuil)", !any(cat_nc$ghm2 == "88M991"))
ok("P2 catalogue deux étages == ancien flux (conversion FALSE)", comparer(cat_nc, ancien_flux(file.path(PARTIELS_DIR, ETAPES_ENV$plan$iterations$fichier), FALSE)))
ok("toggle FALSE : partiels bruts identiques à ceux du projet converti (cache indépendant du toggle)",
   identical(arrow::read_parquet(file.path(PARTIELS_DIR, "catalogue_partiel_CHRU_26.parquet")),
             arrow::read_parquet(file.path(proj, "results", "partiels", "catalogue_partiel_CHRU_26.parquet"))) &&
     compter_e669(arrow::read_parquet(file.path(PARTIELS_DIR, "catalogue_partiel_CHRU_26.parquet")), c("diag2", "diagnostic_associes")) > 0)
fermer()
Sys.setenv(SCENARIOS_PMSI_PATH = proj)
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)")
invisible(sortie(lancer("extraction_associations_codes_v8.R")))   # replace la config du projet principal (plan : rien à faire)
fermer()

# =============================================================== TIRAGE (sans base) ==
cat("\n# tirage : phase sans base (mock interdit)\n")
options(pmsi_mock_interdit = TRUE)
ok("aucun appel pRatihque:: dans tirage_scenarios_v8.R", !any(grepl("pRatihque::", readLines(file.path(proj, "tirage_scenarios_v8.R")))))
log_t <- sortie(lancer("tirage_scenarios_v8.R"))
f_courts <- file.path(EXPORTS_DIR, "scenarios_courts_v8_" %+% DATE_TAG %+% ".parquet")
f_longs  <- file.path(EXPORTS_DIR, "scenarios_longs_tirage_v8_" %+% DATE_TAG %+% ".parquet")
ok("exports du tirage présents", all(file.exists(c(f_courts, f_longs, file.path(EXPORTS_DIR, c("selection_longs.parquet", "selection_longs_effectifs.csv", "meta_tirage.yaml",
   "echantillon_revue.csv", "top30_das_par_cmd.csv", "rapport_v8_" %+% DATE_TAG %+% ".txt"))))))
ok("chunks courts et longs écrits", length(list.files(CHUNKS_DIR, pattern = "^courts_chunk_")) >= 1 && length(list.files(CHUNKS_DIR, pattern = "^longs_chunk_")) >= 1)
sc_courts <- arrow::read_parquet(f_courts); sc_longs <- arrow::read_parquet(f_longs)
ok("identité parquet relu / objet mémoire (volumétrie du rapport)", nrow(sc_courts) == ETAPES_ENV$rapport$courts$n && nrow(sc_longs) == ETAPES_ENV$rapport$longs$n && nrow(sc_courts) > 0 && nrow(sc_longs) > 0)
ok("aucun objet df_scenarios vivant en fin de script", !exists("df_scenarios"))
sel <- arrow::read_parquet(file.path(EXPORTS_DIR, "selection_longs.parquet"))
mt <- yaml::read_yaml(file.path(EXPORTS_DIR, "meta_tirage.yaml"))
ok("quota_dp : quota exact par diag2, origine renseignée", all(table(sel$diag2) == mt$quota_par_dp) && all(grepl("^plancher_|^libre$", sel$origine)) && nrow(sel) == mt$volume_attendu)
ok("meta_tirage.yaml cohérent avec le profil", mt$PROFIL == "diagnostic" && mt$MODE_SELECTION == "quota_dp" && mt$BUDGET_TOTAL_LONGS == 120 && mt$CHUNK_SIZE_FIXE == 40 && mt$NB_CHUNKS_MAX == NB_CHUNKS_MAX && mt$nrow_catalogue == nrow(cat_multi))
ok("tirage longs indexé (quota_dp) : identique au tirage historique (comparé plus bas aux anciens scripts)", file.exists(f_longs))
ok("sidecars de chunking présents pour les deux branches, cohérents (chunk_size = 40, nb_chunks = fichiers)",
   { sc_c <- yaml::read_yaml(file.path(CHUNKS_DIR, "courts_chunks_meta.yaml")); sc_l <- yaml::read_yaml(file.path(CHUNKS_DIR, "longs_chunks_meta.yaml"))
     sc_c$chunk_size == 40 && sc_l$chunk_size == 40 && sc_c$nb_chunks == length(list.files(CHUNKS_DIR, pattern = "^courts_chunk_")) &&
       sc_l$nb_chunks == length(list.files(CHUNKS_DIR, pattern = "^longs_chunk_")) && sc_l$n == nrow(sel) && sc_c$nb_chunks > 1 && sc_l$nb_chunks > 1 })
ok("tirage : aucun ^E669 dans les sorties (diag2, graine, DAS), effectifs E660x au rapport",
   ETAPES_ENV$rapport$courts$e669_residuels == 0 && ETAPES_ENV$rapport$longs$e669_residuels == 0 && sans_e669(sc_courts, c("diag2", "diagnostic_associes")) &&
     sans_e669(sc_longs, c("diag2", "graine", "diagnostic_associes")) && any(grepl("effectifs E660x par classe", rap <- readLines(file.path(EXPORTS_DIR, "rapport_v8_" %+% DATE_TAG %+% ".txt")))))
ok("contrôles §8.2 à zéro sur les deux branches", { cc <- ETAPES_ENV$rapport$courts$controles; cl <- ETAPES_ENV$rapport$longs$controles
   cc$doublons_categorie == 0 && cc$diabete_hors_flag == 0 && cc$i10_avec_hta_autres == 0 && cc$poids_sous_seuil == 0 &&
     cl$doublons_categorie == 0 && cl$diabete_hors_flag == 0 && cl$i10_avec_hta_autres == 0 && cl$poids_sous_seuil == 0 })
ok("longs : graine conservée (hors doublon de catégorie interne), HTA et diabète cohérents",
   all(mapply(function(g, d) all(g %in% d) || (any(duplicated(substr(g, 1, 3))) && g[1] %in% d), split_das(sc_longs$graine), split_das(sc_longs$diagnostic_associes))) &&
     all(mapply(function(h, d) h == "N" || "I10" %in% d || any(d %in% hta_autres), sc_longs$hta, split_das(sc_longs$diagnostic_associes))) &&
     all(mapply(function(f, d) f == "N" || any(substr(d,1,3) %in% c("E10","E11")), sc_longs$diabete_scenario, split_das(sc_longs$diagnostic_associes))))
rev <- readr::read_csv2(file.path(EXPORTS_DIR, "echantillon_revue.csv"), show_col_types = FALSE)
ok("courts : id_profil (c + 15 hex, recette id_courts_v1), id_scenario = id_profil-variante unique, hash_das ; recalculables",
   all(c("id_profil", "id_scenario", "hash_das") %in% names(sc_courts)) && all(grepl("^c[0-9a-f]{15}$", sc_courts$id_profil)) && identical(sc_courts$id_profil, id_profil_courts_de(sc_courts)) &&
     identical(sc_courts$id_scenario, id_scenario_de(sc_courts$id_profil, sc_courts$variante)) && !anyDuplicated(dplyr::distinct(sc_courts, id_scenario, mode_entree, mode_sortie)) &&
     identical(sc_courts$hash_das, hash_das_de(sc_courts$diagnostic_associes)) && !any(sc_courts$id_profil %in% sc_longs$id_profil))
ok("echantillon_revue.csv : <= 50 lignes, deux branches, libellés et [G] chez les longs",
   nrow(rev) <= 50 && setequal(unique(rev$branche), c("courts", "longs")) && any(grepl("\\[G\\]", rev$das_libelles[rev$branche == "longs"])) &&
     all(c("dp_libelle", "das_libelles", "cmd", "type_unite") %in% names(rev)) && any(grepl("BPCO", rev$dp_libelle)))
ok("revue : id_scenario renseigné pour les courts (c…) comme pour les longs", all(grepl("^c[0-9a-f]{15}-[0-9]{3}$", rev$id_scenario[rev$branche == "courts"])))
rap <- readLines(file.path(EXPORTS_DIR, "rapport_v8_" %+% DATE_TAG %+% ".txt"))
ok("rapport : meta en tête, table diag2 × type_unite, top 30, anomalies = 0", any(grepl("^== 0\\. Meta du catalogue", rap)) && any(grepl("PROFIL: diagnostic", rap)) &&
     any(grepl("effectifs sélectionnés diag2", rap)) && any(grepl("== 3\\. Top 30 DAS par CMD", rap)) && any(grepl("TOTAL anomalies = 0", rap)))
# reprise des chunks : identité bit à bit
ch_longs <- sort(list.files(CHUNKS_DIR, pattern = "^longs_chunk_", full.names = TRUE))
unlink(ch_longs[min(2, length(ch_longs))]); unlink(sort(list.files(CHUNKS_DIR, pattern = "^courts_chunk_", full.names = TRUE))[1])
log_t2 <- sortie(lancer("tirage_scenarios_v8.R"))
ok("reprise : sélection relue, chunks présents sautés", any(grepl("relue depuis", log_t2)) && any(grepl("déjà présent, sauté", log_t2)))
ok("reprise après suppression d'un chunk : parquets identiques bit à bit", identical(arrow::read_parquet(f_courts), sc_courts) && identical(arrow::read_parquet(f_longs), sc_longs))
# courts : parité avec les longs — plages disjointes (parallélisme simulé) == run complet, reprise après suppression d'un chunk d'une plage
ch_c <- sort(list.files(CHUNKS_DIR, pattern = "^courts_chunk_.*\\.parquet$", full.names = TRUE)); nc <- length(ch_c)
unlink(ch_c); unlink(f_courts)
log_c1 <- sortie(etape_tirage_courts(chunk_range = c(1, 1))); log_c2 <- sortie(etape_tirage_courts(chunk_range = c(2, nc)))
ok("courts : chunk_range — plages disjointes 1..1 + 2..n = tous les chunks, aucun export tant qu'une plage est demandée",
   nc > 1 && length(list.files(CHUNKS_DIR, pattern = "^courts_chunk_.*\\.parquet$")) == nc && !file.exists(f_courts) && any(grepl("plage traitée : 1\\.\\.1", log_c1)) &&
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
for(f in c("extraction_associations_codes_v8.R", "tirage_scenarios_v8.R")) file.copy(file.path(racine, "tests", "ancien_20260914", f), file.path(proj_anc, f), overwrite = TRUE)
Sys.setenv(SCENARIOS_PMSI_PATH = proj_anc); surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)")
invisible(sortie(lancer("extraction_associations_codes_v8.R"))); fermer()
options(pmsi_mock_interdit = TRUE)
invisible(sortie(lancer("tirage_scenarios_v8.R")))
EXPORTS_ANC <- EXPORTS_DIR
Sys.setenv(SCENARIOS_PMSI_PATH = proj); surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)"); source(file.path(proj, "config_v8.R"))
meme_parquet <- function(a, b) identical(as.data.frame(arrow::read_parquet(a)), as.data.frame(arrow::read_parquet(b)))
ok("(a) catalogue et les 10 refs identiques aux anciens scripts",
   all(vapply(c("catalogue_longs_seuil.parquet", nom_ref(NOMS_REFS)), function(f) meme_parquet(file.path(EXPORTS_ANC, f), file.path(EXPORTS_DIR, f)), logical(1))))
# Identifiants des courts (lot notebook campagnes, §7, ADDITIF) : les anciens scripts d'entrée (instantanés figés, tirage
# inline) ne les posent pas ; l'identité est vérifiée hors ces trois colonnes, et leur absence côté ancien est prouvée.
COLS_ID_COURTS <- c("id_profil", "id_scenario", "hash_das")
sans_ids <- function(f){ d <- as.data.frame(arrow::read_parquet(f)); d[, setdiff(names(d), COLS_ID_COURTS), drop = FALSE] }
ok("(a) scenarios_courts (hors identifiants courts ajoutés au §7) et scenarios_longs_tirage identiques bit à bit aux anciens scripts",
   identical(sans_ids(file.path(EXPORTS_ANC, basename(f_courts))), sans_ids(f_courts)) && all(COLS_ID_COURTS %in% names(arrow::read_parquet(f_courts))) &&
     !any(COLS_ID_COURTS %in% names(arrow::read_parquet(file.path(EXPORTS_ANC, basename(f_courts))))) &&
     meme_parquet(file.path(EXPORTS_ANC, basename(f_longs)), f_longs) &&
     meme_parquet(file.path(EXPORTS_ANC, "selection_longs.parquet"), file.path(EXPORTS_DIR, "selection_longs.parquet")))
# revue : comparaison TEXTUELLE ligne à ligne, le champ id_scenario (2e colonne du csv2) des lignes courts neutralisé
revue_sans_id_courts <- function(f) vapply(strsplit(readLines(f), ";", fixed = TRUE), function(x){ if(length(x) > 1 && x[1] == "\"courts\"") x[2] <- "NA"; paste(x, collapse = ";") }, character(1))
ok("(a) echantillon_revue.csv (hors id_scenario des courts, NA chez l'ancien) et top30 identiques",
   { la <- readLines(file.path(EXPORTS_ANC, "echantillon_revue.csv")); ln <- readLines(file.path(EXPORTS_DIR, "echantillon_revue.csv"))
     identical(revue_sans_id_courts(file.path(EXPORTS_ANC, "echantillon_revue.csv")), revue_sans_id_courts(file.path(EXPORTS_DIR, "echantillon_revue.csv"))) &&
       all(grepl("^\"courts\";NA;", la[grepl("^\"courts\";", la)])) && all(grepl("^\"courts\";\"c[0-9a-f]{15}-[0-9]{3}\";", ln[grepl("^\"courts\";", ln)])) && sum(grepl("^\"courts\";", ln)) > 0 } &&
     identical(readLines(file.path(EXPORTS_ANC, "top30_das_par_cmd.csv")), readLines(file.path(EXPORTS_DIR, "top30_das_par_cmd.csv"))))

# (d) étapes hors ordre -> erreur actionnable ; (e) etat_pipeline avant ; (b) étape par étape
cat("\n# orchestration (b)(d)(e) : étape par étape, hors ordre, tableau de bord\n")
proj_et <- creer_projet("projet_v8_etapes"); Sys.setenv(SCENARIOS_PMSI_PATH = proj_et, SCENARIOS_PMSI_ETAPES_SEULEMENT = "1")
options(pmsi_mock_interdit = FALSE)
invisible(sortie(lancer("extraction_associations_codes_v8.R")))   # session chargée sans exécution
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
invisible(sortie(lancer("extraction_associations_codes_v8.R")))   # reconnexion (session chargée sans exécution)
err <- tryCatch({ invisible(sortie(etape_partiels_longs())); NULL }, error = function(e) conditionMessage(e))
ok("(b) etape_partiels_longs() après reconnexion sans prep_data -> erreur actionnable", !is.null(err) && grepl("etape_prep_data", err))
invisible(sortie(etape_prep_data()))
ok("(b) reconnexion : seules les années des partiels sont préparées, refs sautées", identical(ETAPES_ENV$plan$annees_a_preparer, c(17L, 20L, 26L)) && !any(ETAPES_ENV$plan$refs$a_faire))
invisible(sortie(etape_partiels_longs())); invisible(sortie(etape_catalogue()))
ok("(b) catalogue et refs par étapes == bout-en-bout",
   all(vapply(c("catalogue_longs_seuil.parquet", nom_ref(NOMS_REFS)), function(f) meme_parquet(file.path(EXPORTS_DIR, f), file.path(proj, "results", "exports_diagnostic", f)), logical(1))))
fermer(); rm(conn)
ok("(e) sans connexion : tables temporaires « inconnu hors connexion », catalogue FAIT", { e <- etat_pipeline(); grepl("inconnu hors connexion", e$preuve[e$etape == "etape_prep_data"]) && e$statut[e$etape == "etape_catalogue"] == "FAIT" })
options(pmsi_mock_interdit = TRUE)
invisible(sortie(lancer("tirage_scenarios_v8.R")))   # session tirage chargée sans exécution
invisible(sortie(etape_tirage_courts())); invisible(sortie(etape_selection_longs()))
invisible(sortie(lancer("tirage_scenarios_v8.R")))   # nouvelle session : la sélection et les chunks doivent être relus
ok("(b) nouvelle session : état de tirage vide", is.null(etat_tirage("selection")) && is.null(etat_tirage("df_tirage_longs")))
invisible(sortie(etape_tirage_das_longs())); invisible(sortie(etape_habillage_longs())); invisible(sortie(etape_finalisation()))
ok("(b) sorties du tirage par étapes (sessions séparées) == bout-en-bout, bit à bit",
   meme_parquet(file.path(EXPORTS_DIR, basename(f_courts)), f_courts) && meme_parquet(file.path(EXPORTS_DIR, basename(f_longs)), f_longs) &&
     identical(readLines(file.path(EXPORTS_DIR, "echantillon_revue.csv")), readLines(file.path(proj, "results", "exports_diagnostic", "echantillon_revue.csv"))))
ok("(e) après finalisation : tout FAIT", { e <- etat_pipeline(); all(e$statut[e$etape %in% c("etape_refs", "etape_partiels_longs", "etape_catalogue", "etape_tirage_courts", "etape_selection_longs", "etape_tirage_das_longs", "etape_finalisation")] == "FAIT") })
# (c) etape_catalogue à périmètre restreint
invisible(sortie(etape_catalogue(ans = c(17L, 26L), etbs = "CHR/U")))
mres <- yaml::read_yaml(file.path(EXPORTS_DIR, "catalogue_longs_seuil_meta.yaml"))
ok("(c) etape_catalogue(ans, etbs) : catalogue restreint au périmètre passé, méta cohérent",
   identical(unlist(mres$ANS_HISTORIQUE), c(17L, 26L)) && identical(unlist(mres$TYPES_ETBS_LONGS), "CHR/U") && identical(unlist(mres$perimetre_ans), c(17L, 26L)) &&
     comparer(lire_cat(EXPORTS_DIR), ancien_flux(file.path(PARTIELS_DIR, nom_partiel("CHR/U", c(17L, 26L))), TRUE)))
Sys.unsetenv("SCENARIOS_PMSI_ETAPES_SEULEMENT")
Sys.setenv(SCENARIOS_PMSI_PATH = proj); surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)"); source(file.path(proj, "config_v8.R"))
invisible(sortie(lancer("tirage_scenarios_v8.R")))   # rétablit l'état de session du projet principal (chunks présents : reprise)

# =============================================================== AVAL PRODUCTION ==
cat("\n# aval production : repartitionnement, quota_dp_fixe, tirage indexé par population, flux\n")
proj_pr <- creer_projet("projet_v8_prod"); Sys.setenv(SCENARIOS_PMSI_PATH = proj_pr, SCENARIOS_PMSI_ETAPES_SEULEMENT = "1")
options(pmsi_mock_interdit = FALSE)
SURCHARGE_PROD <- c("ANS_HISTORIQUE <- c(17L, 20L, 26L)", "MODE_SELECTION <- 'quota_dp_fixe'", "NB_CRH_CIBLE <- 200L", "NB_LIGNES_PAR_DP <- 1L", "CHUNK_SIZE_FIXE <- 30L", "LOT_CHUNKS_FINALISATION <- 2L",
                    'PLAFONDS_DPEC <- list("Accouchement normal mère" = 3L)')
surcharger(SURCHARGE_PROD, "CAMPAGNE <- 'C1'", "REGISTRE_ACTIF <- FALSE")
invisible(sortie(lancer("extraction_associations_codes_v8.R")))
invisible(sortie(etape_prep_data())); invisible(sortie(etape_refs())); invisible(sortie(etape_partiels_longs())); invisible(sortie(etape_catalogue()))
fermer(); rm(conn); options(pmsi_mock_interdit = TRUE)
invisible(sortie(lancer("tirage_scenarios_v8.R")))
ok("diagnostic_memoire.csv écrit en fin d'etape_refs / etape_partiels_longs (avant etape_catalogue) et listé par etat_pipeline",
   { proj_m <- creer_projet("projet_v8_mem"); Sys.setenv(SCENARIOS_PMSI_PATH = proj_m); options(pmsi_mock_interdit = FALSE)
     invisible(sortie(lancer("extraction_associations_codes_v8.R"))); invisible(sortie(etape_prep_data())); invisible(sortie(etape_refs()))
     a <- file.exists(file.path(EXPORTS_DIR, "diagnostic_memoire.csv")); n1 <- nrow(utils::read.csv(file.path(EXPORTS_DIR, "diagnostic_memoire.csv")))
     invisible(sortie(etape_partiels_longs())); n2 <- nrow(utils::read.csv(file.path(EXPORTS_DIR, "diagnostic_memoire.csv")))
     e <- etat_pipeline(); fermer(); rm(conn); options(pmsi_mock_interdit = TRUE)
     # message à trois branches quand le catalogue est absent (etape_catalogue non lancée dans ce projet)
     invisible(sortie(lancer("tirage_scenarios_v8.R")))
     msg <- tryCatch({ invisible(sortie(etape_repartitionner_catalogue())); "" }, error = function(e) conditionMessage(e))
     msg2 <- tryCatch({ invisible(sortie(etape_selection_longs())); "" }, error = function(e) conditionMessage(e))
     Sys.setenv(SCENARIOS_PMSI_PATH = proj_pr); invisible(sortie(lancer("tirage_scenarios_v8.R")))
     a && n2 > n1 && grepl("diagnostic_memoire.csv : présent", e$preuve[e$etape == "etape_partiels_longs"]) &&
       grepl(sub("/$", "", file.path(proj_m, "results", "exports_diagnostic")), msg, fixed = TRUE) && grepl("copiez-le", msg) && grepl("Q13", msg) && grepl("etape_catalogue\\(\\) \\(extraction, coûteux\\)", msg) &&
       grepl("copiez-le", msg2) })
mono_avant <- as.data.frame(arrow::read_parquet(MONO_CATALOGUE()))
invisible(sortie(etape_repartitionner_catalogue()))
side <- yaml::read_yaml(file.path(DIR_CATALOGUE(), "_sidecar.yaml"))
parts <- lire_catalogue(DIR_CATALOGUE())
ok("repartitionnement : parts recomposées == monofichier d'origine + colonnes lettre/DPEC/TPEC ; monofichier renommé .ancien",
   nrow(parts) == nrow(mono_avant) && identical(as.data.frame(dplyr::arrange(parts[, names(mono_avant)], dplyr::across(dplyr::everything()))), as.data.frame(dplyr::arrange(mono_avant, dplyr::across(dplyr::everything())))) &&
     all(c("lettre", "DPEC", "TPEC") %in% names(parts)) && all(parts$lettre == substr(parts$diag2, 1, 1)) && !file.exists(MONO_CATALOGUE()) && file.exists(MONO_CATALOGUE() %+% ".ancien"))
ok("repartitionnement : id_profil posé (recette id_v1), unique, recalculable ; sidecar version_recette_id",
   "id_profil" %in% names(parts) && !anyDuplicated(parts$id_profil) && identical(parts$id_profil, id_profil_de(parts)) && side$version_recette_id == RECETTE_ID)
ok("repartitionnement : sidecar (nb lignes par part == méta, sum poids, effectifs DPEC, version typologie)",
   side$nb_lignes_total == yaml::read_yaml(file.path(EXPORTS_DIR, "catalogue_longs_seuil_meta.yaml"))$nb_lignes && sum(unlist(side$sum_poids_par_part)) == sum(parts$poids) &&
     side$version_typologie == charger_typo()$version && sum(unlist(side$effectifs_dpec)) == nrow(parts) && all(parts$DPEC[substr(parts$ghm2, 3, 3) == "C"] == "Chirurgie adultes > 3 nuits"))
ok("repartitionnement : idempotent (sauté)", { o <- sortie(etape_repartitionner_catalogue()); any(grepl("sauté", o)) })
ok("repartitionnement : garde-fou version typologie", { assign("typo", modifyList(charger_typo(), list(version = "autre")), envir = ETAPES_ENV)
   e <- tryCatch({ invisible(sortie(etape_repartitionner_catalogue())); NULL }, error = function(e) conditionMessage(e)); rm("typo", envir = ETAPES_ENV); !is.null(e) && grepl("re-repartitionner", e) })
ok("etat_pipeline : repartitionnement FAIT", { e <- etat_pipeline(); e$statut[e$etape == "etape_repartitionner_catalogue"] == "FAIT" })
ok("catalogue_complet retiré : stop renvoyant vers quota_dp_fixe", grepl("quota_dp_fixe", tryCatch({ invisible(sortie(etape_selection_longs(budget = 10L, mode = "catalogue_complet"))); "" }, error = function(e) conditionMessage(e))))
invisible(sortie(etape_tirage_courts()))
invisible(sortie(etape_selection_longs()))
mt_f <- yaml::read_yaml(file.path(EXPORTS_DIR, "meta_tirage.yaml"))
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
       m <- yaml::read_yaml(file.path(DIR_SELECTION(pp), "meta_tirage.yaml"))$classes_plafonnees; if(length(m) == 0) return(TRUE)
       m <- m[[1]]; cp <- cl[cl$population == pp, ]
       if(m$nb_dp == 0) return(nrow(cp) == 0)
       !anyDuplicated(cp$diag2) && nrow(cp) == m$nb_dp && sum(cp$n_var) == m$total_retenu &&
         (if(m$nb_dp >= m$plafond) all(cp$n_var == 1) && m$total_retenu == m$nb_dp && m$depassement == m$nb_dp - m$plafond else m$total_retenu == m$plafond && m$depassement == 0) }, logical(1))) &&
       nrow(cl) > 3 && sum(vapply(names(POPULATIONS), function(pp){ m <- yaml::read_yaml(file.path(DIR_SELECTION(pp), "meta_tirage.yaml"))$classes_plafonnees; if(length(m)) m[[1]]$depassement else 0 }, numeric(1))) > 0 })
ok("C1 sans registre : origine vierge partout, campagne tracée, id_profil dans la sélection", all(sel_c1$origine_profil == "vierge") && all(sel_c1$campagne == "C1") && all(nchar(sel_c1$id_profil) == 16))
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
invisible(sortie(lancer("tirage_scenarios_v8.R")))   # session neuve : finalisation en flux depuis les fichiers
invisible(sortie(etape_finalisation()))
rap_f <- readLines(file.path(EXPORTS_DIR, "rapport_v8_" %+% DATE_TAG %+% ".txt"))
finaux <- lu(list.files(DIR_FINAL(), pattern = "^part_", recursive = TRUE, full.names = TRUE))
ok("C1 : corpus nommé par CAMPAGNE (scenarios_longs_tirage_v8_C1/), _meta.yaml (campagne, date, populations), etat_pipeline le liste",
   basename(DIR_FINAL()) == "scenarios_longs_tirage_v8_C1" && file.exists(FICHIER_META_FINAL()) && { m <- yaml::read_yaml(FICHIER_META_FINAL()); m$campagne == "C1" && m$date == DATE_TAG && setequal(unlist(m$populations), names(POPULATIONS)) && m$total == nrow(finaux) } &&
     { e <- sortie(ep <- etat_pipeline()); grepl("corpus par campagne : C1 \\(dont la campagne courante C1\\)", ep$preuve[ep$etape == "etape_finalisation"]) && ep$statut[ep$etape == "etape_finalisation"] == "FAIT" })
ok("finalisation en flux : parts finales par population == lots habillés ; DPEC/TPEC en sortie ; monofichier fusionné (volume <= seuil)",
   nrow(finaux) == sum(vapply(names(POPULATIONS), function(pp) sum(vapply(list.files(DIR_HABILLE(pp), full.names = TRUE), function(f) nrow(arrow::read_parquet(f)), integer(1))), integer(1))) &&
     all(c("DPEC", "TPEC", "population") %in% names(finaux)) && file.exists(chemin_export("scenarios_longs_tirage")) && nrow(arrow::read_parquet(chemin_export("scenarios_longs_tirage"))) == nrow(finaux))
ok("rapport fixe : réalisé vs cible, doublons éliminés, manque à gagner, anomalies = 0",
   any(grepl("réalisé vs cible", rap_f)) && any(grepl("Doublons éliminés par DP", rap_f)) && any(grepl("manque à gagner", rap_f)) && any(grepl("TOTAL anomalies = 0", rap_f)))
ok("finalisation en flux == statistiques globales (contrôles, taux) sur les mêmes lignes",
   { st <- stats_branche(finaux, PIVOTS_LONGS, ETAPES_ENV$ctx$codes_imprecis, c("diag2", "graine", "diagnostic_associes")); r <- ETAPES_ENV$rapport$longs_fixe
     cles <- c("doublons_categorie", "diabete_hors_flag", "i10_avec_hta_autres", "poids_sous_seuil")
     tot <- Reduce(`+`, lapply(r, function(x) unlist(x$stats$controles[cles]))); identical(unname(as.integer(tot)), unname(as.integer(unlist(st$controles[cles])))) && sum(vapply(r, function(x) x$stats$n, integer(1))) == st$n })
ok("etat_pipeline (fixe) : sélection, chunks par population, habillage, finalisation FAIT", { e <- etat_pipeline(); all(e$statut[e$etape %in% c("etape_selection_longs", "etape_tirage_das_longs", "etape_habillage_longs", "etape_finalisation")] == "FAIT") })
ok("echantillon_revue : courts + longs par population, id_scenario chez les longs", { rv <- readr::read_csv2(file.path(EXPORTS_DIR, "echantillon_revue.csv"), show_col_types = FALSE); "courts" %in% rv$branche && any(grepl("^longs_", rv$branche)) && "id_scenario" %in% names(rv) && all(!is.na(rv$id_scenario[grepl("^longs_", rv$branche)])) })
ok("C1 sans registre : aucun registre écrit ; chunks porteurs d'id_scenario", !dir.exists(DIR_REGISTRE()) && "id_scenario" %in% names(finaux) && !anyDuplicated(finaux$id_scenario[!duplicated(finaux[, c("id_scenario")])]))
# ---- rétro-inscription de C1 (campagne tirée sans registre), puis campagne C2 SOUS registre
cat("\n# campagnes : rétro-inscription de C1, campagne C2 sous registre\n")
invisible(sortie(etape_retro_inscrire(DIR_SELECTION(), CHUNKS_DIR, "C1")))
reg1 <- lire_registre(DIR_REGISTRE())
ok("rétro-inscription : registre_C1 == scénarios réellement tirés (id recalculés sur pivots + graine, DPEC via sélection)",
   reg1$nb_campagnes == 1 && reg1$nb_scenarios == nrow(dplyr::distinct(finaux, id_scenario)) && setequal(reg1$lignes$id_scenario, unique(finaux$id_scenario)) && all(!is.na(reg1$lignes$DPEC)) && setequal(reg1$lignes$id_profil, sel_c1$id_profil[sel_c1$id_profil %in% reg1$lignes$id_profil]))
ok("rétro-inscription idempotente", identical(sortie(etape_retro_inscrire(DIR_SELECTION(), CHUNKS_DIR, "C1")) |> length() > 0, TRUE) && reg1$nb_scenarios == lire_registre(DIR_REGISTRE())$nb_scenarios)
ok("registre : réinscrire une campagne divergente -> stop append-only", grepl("append-only", tryCatch(ecrire_registre_campagne(dplyr::mutate(reg1$lignes, hash_das = "x"), "C1", DIR_REGISTRE()), error = function(e) conditionMessage(e))))
ok("etat_pipeline : ligne registre", { e <- etat_pipeline(); e$statut[e$etape == "registre_tirages"] == "FAIT" && grepl("1 campagne", e$preuve[e$etape == "registre_tirages"]) })
# Épuisement déterministe d'un DP (registre synthétique C1b) pour exercer le recyclage en C2 : toutes les lignes
# adultes du DP le moins fourni (hors classe plafonnée) sont marquées consommées.
parts$pop <- population_de(parts$cage, POPULATIONS)
cand <- parts[parts$pop == "adulte" & !parts$DPEC %in% names(PLAFONDS_DPEC), ] |> dplyr::count(diag2) |> dplyr::arrange(n, diag2)
dp_epuise <- cand$diag2[1]; lig_ep <- parts[parts$pop == "adulte" & parts$diag2 == dp_epuise, ]
reg_c1b <- tibble::tibble(id_profil = lig_ep$id_profil, variante = 1L, id_scenario = id_scenario_de(lig_ep$id_profil, 1L), hash_das = paste0("synth", seq_len(nrow(lig_ep))),
                          campagne = "C1b", population = "adulte", diag2 = dp_epuise, DPEC = lig_ep$DPEC, date = "2026-09-16")
reg_c1b <- reg_c1b[!reg_c1b$id_scenario %in% reg1$lignes$id_scenario, ]   # ne pas dupliquer les id_scenario déjà tirés en C1
if(nrow(reg_c1b) > 0) ecrire_registre_campagne(reg_c1b, "C1b", DIR_REGISTRE())
reg1 <- lire_registre(DIR_REGISTRE())
ok("registre synthétique C1b : DP " %+% dp_epuise %+% " entièrement consommé chez les adultes", all(lig_ep$id_profil %in% reg1$par_profil$id_profil) && reg1$nb_campagnes == 2)
# changement de campagne : vider chunks + sélection + méta + habillé (le registre ne se vide JAMAIS)
unlink(c(CHUNKS_DIR, DIR_SELECTION(), file.path(EXPORTS_DIR, "habille"), file.path(EXPORTS_DIR, "meta_tirage.yaml")), recursive = TRUE)
surcharger(SURCHARGE_PROD, "CAMPAGNE <- 'C2'", "REGISTRE_ACTIF <- TRUE"); source(file.path(proj_pr, "config_v8.R"))
invisible(sortie(lancer("tirage_scenarios_v8.R")))
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
mt2 <- yaml::read_yaml(file.path(EXPORTS_DIR, "meta_tirage.yaml"))
ok("meta_tirage C2 : campagne, registre actif, recette tracés ; garde-fou sur CAMPAGNE", mt2$CAMPAGNE == "C2" && isTRUE(mt2$REGISTRE_ACTIF) && mt2$RECETTE_ID == RECETTE_ID && grepl("CAMPAGNE", verifier_meta_tirage(mt2, modifyList(mt2, list(CAMPAGNE = "C3")), c("CAMPAGNE"))))
invisible(sortie(etape_tirage_das_longs())); invisible(sortie(etape_habillage_longs())); invisible(sortie(etape_finalisation()))
reg2 <- lire_registre(DIR_REGISTRE())
finaux2 <- lu(list.files(DIR_FINAL(), pattern = "^part_", recursive = TRUE, full.names = TRUE))
ok("C2 : registre_C2 écrit automatiquement en fin de finalisation ; 3 campagnes (C1, C1b, C2) ; scénarios C2 == corpus C2", reg2$nb_campagnes == 3 && setequal(reg2$lignes$id_scenario[reg2$lignes$campagne == "C2"], unique(finaux2$id_scenario)))
ok("C2 : le DP épuisé est recyclé (variantes numérotées après C1/C1b) et retenu par le plancher", dp_epuise %in% recycles_c2$diag2 && dp_epuise %in% sel_c2$diag2)
ok("bout-en-bout : aucun id_scenario dupliqué dans l'union C1 ∪ C2 ; aucun hash_das réutilisé pour un même profil entre campagnes",
   !anyDuplicated(reg2$lignes$id_scenario) && !anyDuplicated(reg2$lignes[, c("id_profil", "hash_das")]) && !anyDuplicated(c(unique(finaux$id_scenario), unique(finaux2$id_scenario))))
dp_cat <- unique(parts$diag2)
# Strates de référence vides (sample_das_long renvoie NULL : aucun candidat hors graine) : seules
# causes admises d'absence d'un DP dans un corpus, avec les recyclages entièrement éliminés en C2.
idx_t <- indexer_ref_das(arrow::read_parquet(file.path(EXPORTS_DIR, nom_ref("ref_das_aigu"))))
strate_vide <- function(sel) vapply(seq_len(nrow(sel)), function(i){ tmp <- idx_t[[cle_strate(sel$diag2[i], sel$mode_hospit[i], sel$sexe[i], sel$cage[i], sel$ghm2[i])]]
  is.null(tmp) || nrow(tmp[!tmp$das %in% split_das(sel$diagnostic_associes[i])[[1]], , drop = FALSE]) == 0 }, logical(1))
dp_vides <- function(sel){ v <- strate_vide(sel); setdiff(unique(sel$diag2), unique(sel$diag2[!v])) }
ok("plancher : chaque DP du catalogue sélectionné en C1 comme en C2", all(dp_cat %in% sel_c1$diag2) && all(dp_cat %in% sel_c2$diag2))
ok("chaque DP présent dans le corpus C1 sauf strates de référence vides ; dans C2 sauf strates vides et recyclages entièrement éliminés (souplesse actée, comptés)",
   { m1 <- setdiff(dp_cat, unique(reg2$lignes$diag2[reg2$lignes$campagne == "C1"])); m2 <- setdiff(dp_cat, unique(reg2$lignes$diag2[reg2$lignes$campagne == "C2"]))
     cat("   DP absents : C1 =", length(m1), "(strates vides", length(dp_vides(sel_c1)), ") ; C2 =", length(m2), "\n")
     all(m1 %in% dp_vides(sel_c1)) && all(m2 %in% c(dp_vides(sel_c2), recycles_c2$diag2)) && length(m2) < length(dp_cat) })
ok("classe plafonnée aux volumes attendus dans les deux campagnes (1 par DP de la classe)",
   { c1 <- reg2$lignes[reg2$lignes$campagne == "C1" & reg2$lignes$DPEC == "Accouchement normal mère", ]; c2 <- reg2$lignes[reg2$lignes$campagne == "C2" & reg2$lignes$DPEC == "Accouchement normal mère", ]
     nrow(c1) == dplyr::n_distinct(paste(c1$population, c1$diag2)) && nrow(c2) <= dplyr::n_distinct(paste(sel_c2$population[sel_c2$DPEC == "Accouchement normal mère"], sel_c2$diag2[sel_c2$DPEC == "Accouchement normal mère"])) && nrow(c1) > 3 })
ok("rapport C2 : section campagne (vierges / recyclés), consommation cumulée par DPEC, DP proches de l'épuisement",
   { r2 <- readLines(file.path(EXPORTS_DIR, "rapport_v8_" %+% DATE_TAG %+% ".txt")); any(grepl("Campagne C2 \\(registre actif\\)", r2)) && any(grepl("consommation cumulée du catalogue par DPEC", r2)) && any(grepl("épuisement total", r2)) && any(grepl("Classes DPEC plafonnées", r2)) })
ok("corpus C2 : id_scenario, origine des profils et DPEC/TPEC en sortie", all(c("id_scenario", "id_profil", "hash_das", "DPEC", "TPEC") %in% names(finaux2)) && "recycle" %in% sel_c2$origine_profil)
# --- lot « notebook campagnes » : deux corpus le même jour, garde-fous, relecture datée inter-sessions
dir_c1 <- file.path(EXPORTS_DIR, "scenarios_longs_tirage_v8_C1"); dir_c2 <- file.path(EXPORTS_DIR, "scenarios_longs_tirage_v8_C2")
ok("C1 puis C2 le même jour : deux dossiers de corpus, C1 intact (mêmes id_scenario qu'à sa finalisation)",
   dir.exists(dir_c1) && dir.exists(dir_c2) && basename(DIR_FINAL()) == "scenarios_longs_tirage_v8_C2" && yaml::read_yaml(file.path(dir_c1, "_meta.yaml"))$campagne == "C1" &&
     setequal(lu(list.files(dir_c1, pattern = "^part_", recursive = TRUE, full.names = TRUE))$id_scenario, finaux$id_scenario) && !identical(sort(unique(finaux2$id_scenario)), sort(unique(finaux$id_scenario))))
# Q49 ACTÉE : campagne inscrite = close ; sélection présente de la même campagne -> relecture (reprise sûre) ; sinon stop
ok("Q49 (a) : reprise complète du lanceur après finalisation + registre -> no-op sûr (sélection relue, chunks sautés, corpus repris, registre inchangé)",
   { reg_avant <- lire_registre(DIR_REGISTRE())$nb_scenarios; Sys.unsetenv("SCENARIOS_PMSI_ETAPES_SEULEMENT")
     log_rep <- sortie(lancer("tirage_scenarios_v8.R")); Sys.setenv(SCENARIOS_PMSI_ETAPES_SEULEMENT = "1")
     any(grepl("déjà inscrite au registre .* sélection relue, aucune nouvelle sélection", log_rep)) && any(grepl("déjà présent, sauté", log_rep)) && any(grepl("même campagne", log_rep)) &&
       lire_registre(DIR_REGISTRE())$nb_scenarios == reg_avant && setequal(lu(list.files(DIR_FINAL(), pattern = "^part_", recursive = TRUE, full.names = TRUE))$id_scenario, finaux2$id_scenario) })
ok("Q49 (b) : campagne inscrite SANS sélection sur disque -> stop « campagne close », renvoi section 3 du notebook, aucun fichier touché",
   { f_mt <- file.path(EXPORTS_DIR, "meta_tirage.yaml"); f_bak <- f_mt %+% ".bak"; file.rename(f_mt, f_bak)
     avant <- file.info(list.files(DIR_SELECTION(), recursive = TRUE, full.names = TRUE))$mtime
     err <- tryCatch({ invisible(sortie(etape_selection_longs())); NULL }, error = function(e) conditionMessage(e)); file.rename(f_bak, f_mt)
     !is.null(err) && grepl("campagne CLOSE \\(aucune sélection sur disque\\)", err) && grepl("section 3 du notebook", err) && identical(avant, file.info(list.files(DIR_SELECTION(), recursive = TRUE, full.names = TRUE))$mtime) })
ok("Q49 (c) : sélection présente d'une AUTRE campagne -> stop « campagne close », aucun re-tirage",
   { f_mt <- file.path(EXPORTS_DIR, "meta_tirage.yaml"); orig <- readLines(f_mt); mt_x <- yaml::read_yaml(f_mt); mt_x$CAMPAGNE <- "C1"; yaml::write_yaml(mt_x, f_mt)
     err <- tryCatch({ invisible(sortie(etape_selection_longs())); NULL }, error = function(e) conditionMessage(e)); writeLines(orig, f_mt)
     !is.null(err) && grepl("la sélection présente porte la campagne C1", err) && grepl("Aucun re-tirage possible", err) })
ok("Q53 : etape_registre_campagne refuse de s'exécuter sous PALIER_ACTIF (une mesure n'écrit jamais au registre)",
   { assign("PALIER_ACTIF", TRUE, envir = globalenv()); err <- tryCatch({ invisible(sortie(etape_registre_campagne("C2"))); NULL }, error = function(e) conditionMessage(e)); rm("PALIER_ACTIF", envir = globalenv())
     !is.null(err) && grepl("PALIER active", err) && grepl("jamais au registre", err) })
ok("chunk `rapport` de RUN_aval.Rmd exécuté AVANT la finalisation (dossier d'exports vide) -> message actionnable, aucune erreur R",
   { l <- readLines(file.path(racine, "RUN_aval.Rmd"), warn = FALSE); i <- grep("^```\\{r rapport\\}", l); j <- i + which(grepl("^```\\s*$", l[(i + 1):length(l)]))[1]
     ex_sauve <- EXPORTS_DIR; d_vide <- file.path(tempdir(), "exports_vide"); dir.create(d_vide, showWarnings = FALSE); assign("EXPORTS_DIR", d_vide, envir = globalenv())
     out <- tryCatch(sortie(eval(parse(text = l[(i + 1):(j - 1)]), envir = globalenv())), error = function(e) "ERREUR : " %+% conditionMessage(e)); assign("EXPORTS_DIR", ex_sauve, envir = globalenv())
     !any(grepl("^ERREUR", out)) && any(grepl("rapport_v8_<date>.txt absent — produit par etape_finalisation\\(\\)", out)) })
ok("garde-fou du corpus : dossier de la campagne courante portant le _meta.yaml d'une AUTRE campagne -> stop, rien écrasé",
   { yaml::write_yaml(list(campagne = "C1", date = "20000101"), FICHIER_META_FINAL()); n_avant <- length(list.files(DIR_FINAL(), recursive = TRUE))
     err <- tryCatch({ invisible(sortie(etape_finalisation())); NULL }, error = function(e) conditionMessage(e))
     yaml::write_yaml(list(campagne = "C2", date = DATE_TAG), FICHIER_META_FINAL())
     !is.null(err) && grepl("AUTRE campagne \\(C1, du 20000101\\)", err) && length(list.files(DIR_FINAL(), recursive = TRUE)) == n_avant })
ok("fichiers datés inter-sessions : DATE_TAG d'un autre jour -> scenarios_courts relu depuis le fichier le plus récent (annoncé), corpus C2 repris, rapport daté du jour de session",
   { surcharger(SURCHARGE_PROD, "CAMPAGNE <- 'C2'", "REGISTRE_ACTIF <- TRUE", "DATE_TAG <- '20000102'"); invisible(sortie(lancer("tirage_scenarios_v8.R")))   # session neuve, un autre jour
     log_f <- sortie(etape_finalisation())
     surcharger(SURCHARGE_PROD, "CAMPAGNE <- 'C2'", "REGISTRE_ACTIF <- TRUE")
     any(grepl("relu depuis scenarios_courts_v8_" %+% format(Sys.Date(), "%Y%m%d") %+% ".parquet", log_f)) && any(grepl("même campagne", log_f)) &&
       file.exists(file.path(EXPORTS_DIR, "rapport_v8_20000102.txt")) && yaml::read_yaml(FICHIER_META_FINAL())$date == "20000102" &&
       setequal(lu(list.files(dir_c2, pattern = "^part_", recursive = TRUE, full.names = TRUE))$id_scenario, finaux2$id_scenario) })
ok("courts absent (aucun fichier daté) -> message à trois branches", { m <- message_courts_absent(EXPORTS_DIR); grepl("etape_tirage_courts", m) && grepl("NE copiez PAS", m) })
Sys.unsetenv("SCENARIOS_PMSI_ETAPES_SEULEMENT")
Sys.setenv(SCENARIOS_PMSI_PATH = proj); surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)"); source(file.path(proj, "config_v8.R"))
options(pmsi_mock_interdit = TRUE)
invisible(sortie(lancer("tirage_scenarios_v8.R")))   # rétablit l'état de session du projet principal

# changement de paramètres -> garde-fou meta_tirage, puis mode catalogue_complet
surcharger("ANS_HISTORIQUE <- c(17L, 20L, 26L)", "MODE_SELECTION <- 'catalogue_complet'", "NB_CRH_CIBLE <- " %+% (3 * nrow(cat_multi)) %+% "L", "BUDGET_TOTAL_LONGS <- " %+% (3 * nrow(cat_multi)) %+% "L")
err <- tryCatch({ invisible(sortie(lancer("tirage_scenarios_v8.R"))); NULL }, error = function(e) conditionMessage(e))
ok("meta_tirage : paramètres différents -> stop() demandant de vider les chunks", !is.null(err) && grepl("meta_tirage.yaml", err))
unlink(CHUNKS_DIR, recursive = TRUE); unlink(file.path(EXPORTS_DIR, c("meta_tirage.yaml", "selection_longs.parquet")))
log_t3 <- sortie(lancer("tirage_scenarios_v8.R"))
mt3 <- yaml::read_yaml(file.path(EXPORTS_DIR, "meta_tirage.yaml"))
ok("catalogue_complet : NB_VARIANTES = 3, volume attendu = nrow × 3, variante max = 3",
   mt3$NB_VARIANTES == 3 && mt3$volume_attendu == 3 * nrow(cat_multi) && max(arrow::read_parquet(f_longs)$variante) == 3 && ETAPES_ENV$rapport$longs_tirage_n <= mt3$volume_attendu)
ok("rapport catalogue_complet : ligne nrow / NB_VARIANTES / volume", any(grepl("mode catalogue_complet : nrow catalogue", readLines(file.path(EXPORTS_DIR, "rapport_v8_" %+% DATE_TAG %+% ".txt")))))
ok("la phase tirage n'a jamais touché la base (mock interdit resté silencieux)", isTRUE(getOption("pmsi_mock_interdit")))
options(pmsi_mock_interdit = FALSE)

cat("\nSIMULATION SQLITE (scripts réels, sessions multiples) VERTE :", n_ok, "assertions ; arrow =", if(ARROW_MOCK) "mock RDS" else "réel", "\n")
