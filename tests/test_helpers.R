###############################################################################
# tests/test_helpers.R — tests unitaires hors base des helpers purs (SPEC §8.1 + brief
# industrialisation §8). Exécution : Rscript tests/test_helpers.R (racine du dépôt ou tests/).
# Source config_v8.R (constantes, sans effet de bord) puis helpers_v8.R : aucune connexion
# base, aucune dépendance à utils.R / referentiels.R. arrow optionnel (repli saveRDS/readRDS
# pour pmap_chunks, dans le test uniquement).
###############################################################################
suppressPackageStartupMessages({library(dplyr); library(tibble); library(stringr)})
for(loc in c("fr_FR.UTF-8", "en_US.UTF-8", "C.UTF-8")) if(!is.na(suppressWarnings(Sys.setlocale("LC_CTYPE", loc))) && Sys.getlocale("LC_CTYPE") == loc) break
lib_test <- Sys.getenv("R_LIBS_TEST", unset = ""); if(nzchar(lib_test)) .libPaths(c(lib_test, .libPaths()))

# Repli arrow (tests UNIQUEMENT) : si arrow est absent, un paquet mock `arrow` est installé dans
# tempdir, dont write_parquet/read_parquet sont saveRDS/readRDS. Limite : les fichiers produits
# sont des RDS nommés .parquet, valables seulement parce qu'ils sont relus par le même mock.
# Les scripts de production continuent d'exiger le vrai arrow.
installer_mock_arrow <- function(){
  lib_mock <- file.path(tempdir(), "lib_mock_arrow"); dir.create(lib_mock, showWarnings = FALSE)
  pkg <- file.path(tempdir(), "arrow"); dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("Package: arrow", "Version: 0.0.0.9000", "Title: Mock", "Description: Mock arrow (RDS) pour tests hors base.",
               "License: MIT", "Encoding: UTF-8"), file.path(pkg, "DESCRIPTION"))
  writeLines("export(write_parquet, read_parquet)", file.path(pkg, "NAMESPACE"))
  writeLines(c("write_parquet <- function(x, sink, ...) saveRDS(x, sink)",
               "read_parquet  <- function(file, ...)  readRDS(file)"), file.path(pkg, "R", "mock.R"))
  utils::install.packages(pkg, repos = NULL, type = "source", lib = lib_mock, quiet = TRUE)
  .libPaths(c(lib_mock, .libPaths()))
  stopifnot(requireNamespace("arrow", quietly = TRUE))
}
ARROW_MOCK <- !requireNamespace("arrow", quietly = TRUE)
if(ARROW_MOCK) installer_mock_arrow()

racine <- c(".", "..")[file.exists(c("config_v8.R", "../config_v8.R"))][1]
stopifnot(!is.na(racine))
Sys.unsetenv("SCENARIOS_PMSI_SURCHARGE")
source(file.path(racine, "config_v8.R"))
source(file.path(racine, "helpers_v8.R"))

`%+%` <- function(x, y) paste0(x, y)
n_ok <- 0
ok <- function(nom, expr){
  if(!isTRUE(expr)) stop("ECHEC : " %+% nom)
  n_ok <<- n_ok + 1
  cat("  ok  ", nom, "\n")
}

# ---------------------------------------------------------------- fixtures --
CAGES <- c("[0-1[", "[1-5[", "[5-10[", "[10-15[", "[15-18[", "[18-30[", "[30-40[",
           "[40-50[", "[50-60[", "[60-70[", "[70-80[", "[80-[")
BORNES <- list("[0-1[" = c(0, 0), "[1-5[" = c(1, 4), "[5-10[" = c(5, 9), "[10-15[" = c(10, 14),
               "[15-18[" = c(15, 17), "[18-30[" = c(18, 29), "[30-40[" = c(30, 39), "[40-50[" = c(40, 49),
               "[50-60[" = c(50, 59), "[60-70[" = c(60, 69), "[70-80[" = c(70, 79), "[80-[" = c(80, 95))

codes_diab_fx <- tibble::tibble(
  code   = c("N083", "H360", "G632", "I792", "M142", "L998", "E1120", "E1128"),
  chemin = c("complications/renal/asterisques_obligatoires/x",
             "complications/oculaire/asterisques_obligatoires/x",
             "complications/neurologique/asterisques_obligatoires/x",
             "complications/vasculaire_peripherique/asterisques_obligatoires/x",
             "complications/autres_precisees/asterisques_obligatoires/x",
             "complications/autres_precisees/asterisques_obligatoires/y",
             "satellites/E1120", "satellites/E1128"))
comp_diabete_fx <- tibble::tibble(
  cage = rep(c("[60-70[", "[30-40["), each = 4),
  diabete = "E11i",
  comp = rep(c("2", "7", "8", "9"), 2),
  nb = c(10, 5, 3, 20, 4, 1, 1, 30)) |>
  dplyr::bind_rows(tibble::tibble(cage = "[60-70[", diabete = "E10", comp = c("1", "7"), nb = c(2, 5)))
code_did_fx      <- c("E102", "E103", "E104", "E105", "E106", "E107", "E108", "E109")
code_dnid_ins_fx <- c("E1120", "E1130", "E1140", "E1150", "E1160", "E1170", "E1180", "E1190")
code_dnid_fx     <- c("E1128", "E1138", "E1148", "E1158", "E1168", "E1178", "E1188", "E1198")
hta_autres_fx    <- c("I110", "I119", "I120", "I129", "I131", "I132", "I139", "I150", "I151", "I152", "I158", "I159")
neo_fx           <- c("E10", "E11i", "E11ni")
paires_fx        <- list(c("E10", "E11"), c("I10", "I15"))

refs_fx <- construire_refs(comp_diabete = comp_diabete_fx, codes_diab = codes_diab_fx,
                           codes_comp_sat_diab = c("N083", "H360"), hta_autres = hta_autres_fx,
                           code_did = code_did_fx, code_dnid_ins = code_dnid_ins_fx, code_dnid = code_dnid_fx,
                           neo_codes = neo_fx, paires_exclues = paires_fx)

# ------------------------------------------------------- sample_age_ligne --
cat("\n# sample_age_ligne / sample_age / decoupe_cage\n")
set.seed(1)
for(cg in CAGES){
  tir <- replicate(1000, sample_age_ligne(cg, age_max = 95))
  ok("bornes semi-ouvertes " %+% cg, all(tir >= BORNES[[cg]][1] & tir <= BORNES[[cg]][2]) && is.integer(tir))
  ok("decoupe_cage(âge tiré) == classe " %+% cg, all(decoupe_cage(tir) == cg))
}
ok("[1-5[ atteint 1 et 4, jamais 5", { t <- replicate(2000, sample_age_ligne("[1-5[")); all(c(1, 4) %in% t) && !5 %in% t })
ok("[80-[ atteint 95", 95 %in% replicate(3000, sample_age_ligne("[80-[", 95)))
ok("libellé inconnu -> NA", is.na(sample_age_ligne("inconnu")))
ok("vectorisation : 100 cages identiques -> > 1 valeur distincte (régression §5.6)",
   length(unique(sample_age(rep("[50-60[", 100)))) > 1)
ok("sample_age conserve la longueur et le type", { v <- sample_age(rep(CAGES, 3)); length(v) == 36 && is.integer(v) })

# ------------------------------------------------------- dedup_categorie --
cat("\n# dedup_categorie\n")
ok("conserve E110+E780+I100", identical(dedup_categorie(c("E110", "E780", "I100")), c("E110", "E780", "I100")))
ok("élimine le second de E110+E119", identical(dedup_categorie(c("E110", "E119")), "E110"))
ok("respecte l'ordre : la graine (en tête) n'est jamais éliminée",
   identical(dedup_categorie(c("I500", "E119", "E110", "I501")), c("I500", "E119")))
ok("exclusion YAML [E10, E11] : E11 écarté si E10 gardé", identical(dedup_categorie(c("E102", "E1120", "I10"), paires_fx), c("E102", "I10")))
ok("exclusion YAML symétrique : E10 écarté si E11 gardé", identical(dedup_categorie(c("E1120", "E102"), paires_fx), "E1120"))
ok("exclusion YAML [I10, I15]", identical(dedup_categorie(c("I10", "I150"), paires_fx), "I10"))
ok("sans YAML : I10 + I150 coexistent (catégories différentes)", identical(dedup_categorie(c("I10", "I150")), c("I10", "I150")))
ok("NA et chaînes vides ignorés", identical(dedup_categorie(c(NA, "", "A000")), "A000"))
ok("vecteur vide -> character(0)", identical(dedup_categorie(character(0)), character(0)))
ok("paire mal formée -> erreur", inherits(try(dedup_categorie("A00", list(c("A"))), silent = TRUE), "try-error"))

# ------------------------------------------- retro_code / get_codes_diabete --
cat("\n# retro_code_diabete / tirer_comp_diabete / get_codes_diabete_from_neo\n")
ok("retro E10 + 2 -> E102", retro_code_diabete("E10", "2") == "E102")
ok("retro E11i + 2 -> E1120 (insulinotraité = 5e caractère 0, cf. code_dnid_ins)", retro_code_diabete("E11i", 2) == "E1120")
ok("retro E11ni + 9 -> E1198", retro_code_diabete("E11ni", "9") == "E1198")
ok("rétro-codes E11 appartiennent aux listes code_dnid_ins / code_dnid",
   all(retro_code_diabete("E11i", 2:9) %in% code_dnid_ins_fx) && all(retro_code_diabete("E11ni", 2:9) %in% code_dnid_fx))

set.seed(2)
ok("comp forcée 8 -> code en 9, sans astérisque",
   identical(get_codes_diabete_from_neo("E11i", "[60-70[", comp_diabete_fx, codes_diab_fx, comp_forcee = "8"), "E1190"))
res7 <- replicate(200, get_codes_diabete_from_neo("E11ni", "[60-70[", comp_diabete_fx, codes_diab_fx, comp_forcee = "7"), simplify = FALSE)
ok("comp forcée 7 -> code E1178 en dernier", all(vapply(res7, function(v) v[length(v)] == "E1178", logical(1))))
ok("comp 7 -> 3 à 4 astérisques distincts, tous dans les chemins asterisques_obligatoires",
   all(vapply(res7, function(v){ a <- v[-length(v)]; length(a) %in% 3:4 && !any(duplicated(a)) && all(a %in% codes_diab_fx$code[1:6]) }, logical(1))))
ok("comp 7 -> les astérisques couvrent des complications distinctes (2:6)",
   all(vapply(res7, function(v){ a <- v[-length(v)]; ch <- codes_diab_fx$chemin[match(a, codes_diab_fx$code)]; !any(duplicated(sub("/asterisques.*", "", ch))) }, logical(1))))
res_marg <- replicate(500, get_codes_diabete_from_neo("E11i", "[60-70[", comp_diabete_fx, codes_diab_fx), simplify = FALSE)
codes_e11 <- vapply(res_marg, function(v) v[length(v)], character(1))
ok("tirage marginal : jamais de code E11 sans 4e+5e caractères valides",
   all(nchar(codes_e11) == 5 & substr(codes_e11, 1, 3) == "E11" & substr(codes_e11, 4, 4) %in% as.character(2:9) & substr(codes_e11, 5, 5) %in% c("0", "8")))
ok("tirage marginal : comp 8 jamais en sortie (remplacé par 9)", !any(substr(codes_e11, 4, 4) == "8"))
ok("tirage marginal : comps 2, 7 et 9 tous observés", all(c("2", "7", "9") %in% substr(codes_e11, 4, 4)))
ok("comp 2 -> exactement un astérisque rénal", all(vapply(res_marg[substr(codes_e11, 4, 4) == "2"], function(v) identical(v, c("N083", "E1120")), logical(1))))
ok("comp 9 -> aucun astérisque", all(vapply(res_marg[substr(codes_e11, 4, 4) == "9"], function(v) identical(v, "E1190"), logical(1))))
ok("strate d'âge absente -> repli toutes classes (pas d'erreur)",
   { v <- get_codes_diabete_from_neo("E11i", "[0-1[", comp_diabete_fx, codes_diab_fx); nchar(v[length(v)]) == 5 })
ok("aucune information -> comp 9", identical(get_codes_diabete_from_neo("E11ni", "[0-1[", comp_diabete_fx[0, ], codes_diab_fx), "E1198"))
ok("E10 comp 1 (sans astérisque) -> E101 seul, pas de NA",
   identical(get_codes_diabete_from_neo("E10", "[60-70[", comp_diabete_fx, codes_diab_fx, comp_forcee = "1"), "E101"))

# ------------------------------------------------------------- ajoute_hta --
cat("\n# ajoute_hta / neo_code_de_diag\n")
ok("hta != N sans hta_autres -> I10 en tête", identical(ajoute_hta(c("J449"), "I10", hta_autres_fx), c("I10", "J449")))
ok("hta != N avec hta_autres -> pas de I10", identical(ajoute_hta(c("I110", "J449"), "I10", hta_autres_fx), c("I110", "J449")))
ok("hta == N -> inchangé", identical(ajoute_hta(c("J449"), "N", hta_autres_fx), "J449"))
ok("I10 déjà présent + hta_autres -> I10 retiré", identical(ajoute_hta(c("I10", "I150"), "I10", hta_autres_fx), "I150"))
ok("neo_code_de_diag", identical(neo_code_de_diag(c("E1120", "E1128", "E102", "J449"), code_did_fx, code_dnid_ins_fx, code_dnid_fx), c("E11i", "E11ni", "E10", "N")))
ok("neo_code_de_diag defaut", neo_code_de_diag("J449", code_did_fx, code_dnid_ins_fx, code_dnid_fx, defaut = "E11i") == "E11i")

# ------------------------------------------------------- sample_das_court --
cat("\n# sample_das_court\n")
ref_chro_brut <- tibble::tibble(
  diag2 = "J449", cage = "[60-70[",
  sexe  = c(rep("1", 25), rep("2", 25)),
  das   = c(paste0("H", sprintf("%02d", 1:25), "0"), paste0("F", sprintf("%02d", 20:44), "0")),
  niveau = "1", type_liste = "Patho_chro", caract = "x",
  nb_das = 10)
ref_chro_fx <- prep_ref_chronique(ref_chro_brut)
ref_nb_fx <- tibble::tibble(cage = "[60-70[", sexe = "1", nb_chro = c(2, 3), nb = c(50, 50))
codes_sexe1 <- ref_chro_brut$das[ref_chro_brut$sexe == "1"]
codes_sexe2 <- ref_chro_brut$das[ref_chro_brut$sexe == "2"]

set.seed(3)
res_c <- purrr::map(1:100, ~ sample_das_court("HC", "1", "[60-70[", "04M05", "J449", 1, nb = 40,
                                              ref_chro = ref_chro_fx, ref_nb_chro = ref_nb_fx, refs = refs_fx,
                                              nb_tirages = 2, seuil_ref = 20, cibles_defaut = list(), age_max = 95)) |> purrr::list_rbind()
das_c <- split_das(res_c$diagnostic_associes)
ok("sexe respecté : aucun code de la strate de l'autre sexe (régression §5.1)", !any(unlist(das_c) %in% codes_sexe2) && all(unlist(das_c) %in% codes_sexe1))
ok("nb_tirages variantes par pivot", nrow(res_c) == 200 && all(sort(unique(res_c$variante)) == 1:2))
ok("nombre de DAS tiré dans ref_nb_chro (2 ou 3)", all(lengths(das_c) %in% 2:3) && all(c(2L, 3L) %in% lengths(das_c)))
ok("jamais deux codes de même catégorie", !any(vapply(das_c, function(v) any(duplicated(substr(v, 1, 3))), logical(1))))
ok("âge par variante dans la classe", all(res_c$age >= 60 & res_c$age <= 69) && length(unique(res_c$age)) > 1)
ok("colonnes de sortie", all(c("mode_hospit", "sexe", "cage", "ghm2", "diag2", "duree", "poids", "variante", "age", "nb_das", "diabete_scenario", "hta_scenario", "diagnostic_associes") %in% names(res_c)))
ok("poids propagé", all(res_c$poids == 40))
ok("source_ref = strate (25 codes >= seuil 20)", all(res_c$source_ref == "strate"))

# repli : strate (diag2 inconnu) vide -> repli (cage, sexe)
res_r <- sample_das_court("HC", "2", "[60-70[", "04M05", "ZZZZ", 1, ref_chro = ref_chro_fx, ref_nb_chro = ref_nb_fx, refs = refs_fx,
                          nb_tirages = 1, seuil_ref = 20, cibles_defaut = list("[60-70[" = c(1, 1)))
ok("repli sur (cage, sexe) quand la strate diag2 est vide", res_r$source_ref == "repli" && all(unlist(split_das(res_r$diagnostic_associes)) %in% codes_sexe2))
ok("cible dégradée utilisée quand ref_nb_chro vide pour la strate", res_r$nb_cible == 1 && res_r$nb_das == 1)
ok("return(NULL) propre quand la strate est vide",
   is.null(sample_das_court("HC", "1", "[80-[", "04M05", "J449", 1, ref_chro = ref_chro_fx, ref_nb_chro = ref_nb_fx, refs = refs_fx)))
ok("GHM en C : R2630 / F0x / F10 exclus, F17 conservé",
   { tmp <- tibble::tibble(das = c("R2630", "F050", "F102", "F172", "J449"), nb_das = 1)
     identical(filtre_das_ghm_c(tmp, "06C04")$das, c("F172", "J449")) && identical(filtre_das_ghm_c(tmp, "06M04")$das, tmp$das) })

# diabète dans les courts : néo-code tiré remplacé par les codes réels, flag positionné
ref_chro_diab <- prep_ref_chronique(tibble::tibble(diag2 = "J449", cage = "[60-70[", sexe = "1", das = c("E11i", "I10", "I110"),
                                                   niveau = "1", type_liste = "Patho_chro", caract = "x", nb_das = c(100, 1, 1)))
set.seed(4)
res_d <- purrr::map(1:50, ~ sample_das_court("HC", "1", "[60-70[", "04M05", "J449", 1, ref_chro = ref_chro_diab, ref_nb_chro = ref_nb_fx[0, ], refs = refs_fx,
                                             nb_tirages = 1, seuil_ref = 1, cibles_defaut = list("[60-70[" = c(3, 3)))) |> purrr::list_rbind()
das_d <- split_das(res_d$diagnostic_associes)
ok("néo-code E11i jamais en sortie", !any(unlist(das_d) %in% neo_fx))
ok("flag diabete_scenario = E11i et code E11xx présent", all(res_d$diabete_scenario == "E11i") && all(vapply(das_d, function(v) any(grepl("^E11[2-9][08]$", v)), logical(1))))
ok("I10 jamais avec un code hta_autres", !any(vapply(das_d, function(v) "I10" %in% v && any(v %in% hta_autres_fx), logical(1))))
ok("hta_scenario cohérent avec I10 tiré", all((res_d$hta_scenario == "I10") == vapply(das_d, function(v) "I10" %in% v, logical(1)) | vapply(das_d, function(v) any(v %in% hta_autres_fx), logical(1))))
res_dp <- sample_das_court("HC", "1", "[60-70[", "04M05", "E1128", 1, ref_chro = ref_chro_fx, ref_nb_chro = ref_nb_fx, refs = refs_fx, nb_tirages = 1)
ok("DP diabète -> flag E11ni depuis le DP", res_dp$diabete_scenario == "E11ni" && any(grepl("^E11[2-9]8$", split_das(res_dp$diagnostic_associes)[[1]])))

# -------------------------------------------------------- sample_das_long --
cat("\n# sample_das_long\n")
ref_aigu_fx <- tibble::tibble(
  mode_hospit = "HC", cage = "[60-70[", racine = "04M05", ghm2 = "04M053", diag2 = "J449",
  sexe = c(rep("1", 30), rep("2", 30)),
  das = c(paste0("N", sprintf("%02d", 10:39)), paste0("K", sprintf("%02d", 20:49))),
  n = 5)
codes_aigu_s1 <- ref_aigu_fx$das[ref_aigu_fx$sexe == "1"]
codes_aigu_s2 <- ref_aigu_fx$das[ref_aigu_fx$sexe == "2"]

set.seed(5)
res_l <- purrr::map(1:100, ~ sample_das_long("HC", "1", "ge_18", "[60-70[", "04M05", "04M053", "N", "N", "J449", 4,
                                             "I500 N189", type_unite = "HC", prep_sc = 0, poids = 123,
                                             ref_das_aigu = ref_aigu_fx, refs = refs_fx, nb_tirage = 1)) |> purrr::list_rbind()
das_l <- split_das(res_l$diagnostic_associes)
ok("sexe respecté : aucun code de la strate de l'autre sexe (régression §5.1)", !any(unlist(das_l) %in% codes_aigu_s2))
ok("codes tirés ∈ strate du sexe ou graine", all(unlist(das_l) %in% c(codes_aigu_s1, "I500", "N189")))
ok("la graine n'est jamais retirée et passe en tête", all(vapply(das_l, function(v) identical(v[1:2], c("I500", "N189")), logical(1))))
ok("jamais deux codes de même catégorie (N18 de la graine bloque N18x tiré)", !any(vapply(das_l, function(v) any(duplicated(substr(v, 1, 3))), logical(1))))
ok("taille tirée = min(nbda, candidats) avant dédoublonnage : <= 2 + 4", all(lengths(das_l) <= 6 & lengths(das_l) >= 3))
ok("colonnes pivots + graine + poids + type_unite/prep_sc conservées",
   all(c("mode_hospit", "sexe", "age", "cage", "racine", "ghm2", "diabete", "hta", "diag2", "nbda", "type_unite", "prep_sc", "poids", "graine", "diabete_scenario", "nb_das", "diagnostic_associes") %in% names(res_l)) &&
     all(res_l$age == "ge_18") && all(res_l$graine == "I500 N189") && all(res_l$poids == 123) && all(res_l$type_unite == "HC"))
ok("return(NULL) propre quand la strate est vide",
   is.null(sample_das_long("HP", "1", "ge_18", "[60-70[", "04M05", "04M053", "N", "N", "J449", 4, "I500", ref_das_aigu = ref_aigu_fx, refs = refs_fx)))
ok("nb_tirage variantes", nrow(sample_das_long("HC", "1", "ge_18", "[60-70[", "04M05", "04M053", "N", "N", "J449", 4, "I500", ref_das_aigu = ref_aigu_fx, refs = refs_fx, nb_tirage = 3)) == 3)

# HTA et diabète dans les longs
set.seed(6)
res_h <- purrr::map(1:50, ~ sample_das_long("HC", "1", "ge_18", "[60-70[", "04M05", "04M053", "E11i", "I10", "J449", 3,
                                            "I500", ref_das_aigu = ref_aigu_fx, refs = refs_fx)) |> purrr::list_rbind()
das_h <- split_das(res_h$diagnostic_associes)
ok("hta != N -> I10 présent (aucun hta_autres possible ici)", all(vapply(das_h, function(v) "I10" %in% v, logical(1))))
ok("flag diabète -> un code E11x0 présent, jamais de néo-code", all(vapply(das_h, function(v) any(grepl("^E11[2-9]0$", v)) && !any(v %in% neo_fx), logical(1))))
ok("graine en tête même avec diabète/HTA", all(vapply(das_h, function(v) v[1] == "I500", logical(1))))
ok("jamais deux codes de même catégorie avec diabète/HTA", !any(vapply(das_h, function(v) any(duplicated(substr(v, 1, 3))), logical(1))))
res_hta2 <- sample_das_long("HC", "1", "ge_18", "[60-70[", "04M05", "04M053", "N", "I10", "J449", 1, "I110", ref_das_aigu = ref_aigu_fx, refs = refs_fx)
ok("hta != N mais hta_autres en graine -> pas de I10", !"I10" %in% split_das(res_hta2$diagnostic_associes)[[1]])
# complication satellite présente -> complications multiples (comp 7)
set.seed(7)
res_sat <- sample_das_long("HC", "1", "ge_18", "[60-70[", "04M05", "04M053", "E11ni", "N", "J449", 1, "N083", ref_das_aigu = ref_aigu_fx, refs = refs_fx)
ok("code satellite en graine -> E1178 (complications multiples)", "E1178" %in% split_das(res_sat$diagnostic_associes)[[1]])
ok("DP diabète -> diabete_scenario depuis le DP, colonne diabete (pivot) inchangée",
   { r <- sample_das_long("HC", "1", "ge_18", "[60-70[", "04M05", "04M053", "N", "N", "E102", 1, "I500", ref_das_aigu = ref_aigu_fx |> dplyr::mutate(diag2 = "E102"), refs = refs_fx)
     r$diabete_scenario == "E10" && r$diabete == "N" })

# ------------------------------------------------------- contrôles §8.2 --
cat("\n# helpers du rapport\n")
df_ctrl <- tibble::tibble(diagnostic_associes = c("E1120 N083 J449", "I10 I110", "E1198 J449", "J449 J440", ""),
                          diabete_scenario = c("E11i", "N", "N", "N", "N"), poids = c(11, 11, 10, 11, 11), cage = "[60-70[")
cc <- controler_scenarios(df_ctrl, hta_autres_fx, 10)
ok("controler_scenarios détecte doublon catégorie / diabète hors flag / I10+hta_autres / poids",
   cc$doublons_categorie == 1 && cc$diabete_hors_flag == 1 && cc$i10_avec_hta_autres == 1 && cc$poids_sous_seuil == 1 && cc$n == 5)
ok("controler_scenarios sans colonne DAS -> NA", is.na(controler_scenarios(tibble::tibble(poids = 11), hta_autres_fx, 10)$doublons_categorie))
ok("taux_imprecis", taux_imprecis(df_ctrl, c("J449")) == round(3 / 9, 4))
ok("distribution_nb_das", { d <- distribution_nb_das(df_ctrl); d$n == 5 && d$min == 0 && d$max == 3 })
ok("codes_imprecis_de_cim", identical(codes_imprecis_de_cim(tibble::tibble(code = c("J44.9", "J44.0", "I10"), libelle = c("BPCO, sans précision", "BPCO avec infection", "HTA non précisée"))), c("J449", "I10")))
ok("split_das gère NA", identical(split_das(c(NA, "A B")), list(character(0), c("A", "B"))))
ok("tirer_nb_chroniques : distribution empirique", { set.seed(8); v <- replicate(200, tirer_nb_chroniques("[60-70[", "1", ref_nb_fx, list())); all(v %in% 2:3) && all(2:3 %in% v) })
ok("tirer_nb_chroniques : cible dégradée", { set.seed(9); v <- replicate(200, tirer_nb_chroniques("[70-80[", "1", ref_nb_fx, list("[70-80[" = c(3, 5)))); all(v %in% 3:5) && all(3:5 %in% v) })
ok("tirer_nb_chroniques : rien -> 0", tirer_nb_chroniques("[70-80[", "1", ref_nb_fx, list()) == 0L)

# ----------------------------------------------------------- déterminisme --
cat("\n# déterminisme\n")
run_all <- function(){
  set.seed(20260907)
  list(sample_age(rep(CAGES, 5)),
       get_codes_diabete_from_neo("E11i", "[60-70[", comp_diabete_fx, codes_diab_fx),
       purrr::map(1:5, ~ sample_das_court("HC", "1", "[60-70[", "04M05", "J449", 1, ref_chro = ref_chro_fx, ref_nb_chro = ref_nb_fx, refs = refs_fx, nb_tirages = 2)) |> purrr::list_rbind(),
       purrr::map(1:5, ~ sample_das_long("HC", "1", "ge_18", "[60-70[", "04M05", "04M053", "E11i", "I10", "J449", 4, "I500", ref_das_aigu = ref_aigu_fx, refs = refs_fx)) |> purrr::list_rbind())
}
ok("deux exécutions sous le même seed donnent le même résultat", identical(run_all(), run_all()))


# =========================================================== industrialisation ==
cat("\n# pmap_chunks\n")
ecrire_t <- arrow::write_parquet; lire_t <- arrow::read_parquet; ext_t <- ".parquet"
cat("   (écriture des chunks :", if(ARROW_MOCK) "mock arrow = RDS (test uniquement)" else "arrow", ")\n")
f_test <- function(id, k){ if(k == 0) return(NULL); tibble::tibble(id = id, k = k, u = round(stats::runif(1), 6), s = sample(letters, 1)) }
df_in <- tibble::tibble(id = 1:23, k = c(rep(1L, 10), 0L, rep(2L, 12)))
run_chunks <- function(dossier, ...) pmap_chunks(df_in, f_test, chunk_size = 5, dossier = dossier, prefixe = "t", seed_base = 100,
                                                 ecrire = ecrire_t, lire = lire_t, ext = ext_t, verbose = FALSE, ...)
d1 <- file.path(tempdir(), "chunks1"); unlink(d1, recursive = TRUE)
r1 <- run_chunks(d1)
ok("découpage exact : 23 lignes / 5 -> 5 chunks", length(list.files(d1, pattern = "^t_chunk_\\d{4}")) == 5 && all(sprintf("t_chunk_%04d%s", 1:5, ext_t) %in% list.files(d1)))
ok("lignes NULL ignorées : 22 lignes en sortie, ordre conservé", nrow(r1) == 22 && identical(r1$id, setdiff(1:23, 11L)))
unlink(file.path(d1, sprintf("t_chunk_%04d%s", 3, ext_t)))
r2 <- run_chunks(d1)
ok("reprise : chunk du milieu supprimé, relance -> identité bit à bit avec le run complet", identical(r1, r2))
d2 <- file.path(tempdir(), "chunks2"); unlink(d2, recursive = TRUE)
ok("déterminisme : second run complet dans un autre dossier identique", identical(r1, run_chunks(d2)))
d3 <- file.path(tempdir(), "chunks3"); unlink(d3, recursive = TRUE)
df_null <- tibble::tibble(id = 1:7, k = 0L)
r3 <- pmap_chunks(df_null, f_test, chunk_size = 3, dossier = d3, prefixe = "n", seed_base = 1, ecrire = ecrire_t, lire = lire_t, ext = ext_t, verbose = FALSE)
ok("chunk dont f retourne NULL : chunks écrits, sortie vide sans colonne .chunk_vide", length(list.files(d3)) == 3 && nrow(r3) == 0 && !".chunk_vide" %in% names(r3))
d4 <- file.path(tempdir(), "chunks4"); unlink(d4, recursive = TRUE)
r4 <- pmap_chunks(df_in, f_test, chunk_size = 5, dossier = d4, prefixe = "g", seed_base = 100, garder_chunks = FALSE, ecrire = ecrire_t, lire = lire_t, ext = ext_t, verbose = FALSE)
ok("garder_chunks = FALSE : chunks supprimés, résultat identique", length(list.files(d4)) == 0 && identical(r4, r1))
ok("df vide -> tibble vide", nrow(pmap_chunks(df_in[0, ], f_test, 5, file.path(tempdir(), "chunks5"), "v", 1, ecrire = ecrire_t, lire = lire_t, ext = ext_t, verbose = FALSE)) == 0)

cat("\n# sélection longs\n")
cat_fx <- tibble::tibble(diag2 = rep(c("J449", "I500", "K802"), c(10, 4, 2)),
                         type_unite = c(rep("HC", 6), rep("SC", 3), "GERIATRIE", rep("HC", 3), "UHCD", "HC", "HC"),
                         poids = c(50, 40, 30, 20, 15, 12, 11, 11, 11, 11, 20, 20, 20, 20, 30, 30),
                         mode_hospit = "HC", diagnostic_associes = "I10 E785")
ok("repartir_equitable : plus forts restes, priorité", identical(repartir_equitable(7L, c("a", "b", "c"), priorite = c(1, 3, 2)), c(a = 2L, b = 3L, c = 2L)))
ok("repartir_equitable : sans priorité -> ordre alphabétique", identical(repartir_equitable(4L, c("z", "y", "x")), c(z = 1L, y = 1L, x = 2L)))
set.seed(11)
sq <- selection_quota_dp(cat_fx, budget = 30, quota_min_par_unite = 5L)
ok("quota_dp : X = ceiling(30/3) = 10, nb_dp = 3", sq$quota_par_dp == 10L && sq$nb_dp == 3L)
cnt <- table(sq$selection$diag2)
ok("quota exact par diag2 (10 chacun, total 30)", all(cnt == 10) && nrow(sq$selection) == 30)
sel_j <- sq$selection |> dplyr::filter(diag2 == "J449")
ok("J449 : 3 types × 5 >= 10 -> répartition équitable 4/3/3, plus fort poids (HC) servi en premier",
   { t <- table(sel_j$type_unite); t[["HC"]] == 4 && t[["SC"]] == 3 && t[["GERIATRIE"]] == 3 && all(grepl("^plancher_", sel_j$origine)) })
sel_i <- sq$selection |> dplyr::filter(diag2 == "I500")
ok("I500 : 2 types × 5 < 10 -> plancher 5 par type (dont UHCD, 1 seule ligne, avec remise) + 0 libre",
   { t <- table(sel_i$type_unite); t[["UHCD"]] == 5 && t[["HC"]] == 5 && !"libre" %in% sel_i$origine })
sel_k <- sq$selection |> dplyr::filter(diag2 == "K802")
ok("K802 : 1 type × 5 < 10 -> 5 plancher + 5 libre, avec remise sur 2 lignes",
   sum(sel_k$origine == "plancher_HC") == 5 && sum(sel_k$origine == "libre") == 5 && nrow(sel_k) == 10)
ok("colonne id_selection unique", identical(sq$selection$id_selection, 1:30))
ok("plancher : type absent du catalogue du DP jamais sélectionné", !any(sel_k$type_unite != "HC"))
set.seed(11); sq2 <- selection_quota_dp(cat_fx, 30, 5L)
ok("déterminisme de la sélection sous seed", identical(sq$selection, sq2$selection))
ok("effectifs_selection : total par DP = quota", all(effectifs_selection(sq$selection)$total == 10))
sc <- selection_catalogue_complet(cat_fx, budget = 40)
ok("catalogue_complet : NB_VARIANTES = ceiling(40/16) = 3, volume = 48", sc$nb_variantes == 3L && sc$volume_attendu == 48 && sc$nrow == 16)
ok("catalogue_complet : budget < nrow -> 1 variante", selection_catalogue_complet(cat_fx, 5)$nb_variantes == 1L)

cat("\n# résolution des besoins\n")
refs_all <- NOMS_REFS
plan0 <- resoudre_besoins(c("CHR/U", "CH"), 17:19, 26L, character(0), character(0), FALSE, refs_all, REFS_CHRONIQUES)
ok("rien de présent : 6 itérations, 9 refs, années 17,18,19,26, prep_das_chronique",
   sum(plan0$iterations$a_faire) == 6 && all(plan0$refs$a_faire) && identical(plan0$annees_a_preparer, c(17:19, 26L)) && plan0$prep_das_chronique && !plan0$rien_a_faire)
ok("ordre des itérations = types × années", identical(plan0$iterations$etbs, rep(c("CHR/U", "CH"), each = 3)) && identical(plan0$iterations$fichier[1], "catalogue_partiel_CHRU_17.parquet"))
plan1 <- resoudre_besoins(c("CHR/U", "CH"), 17:19, 26L,
                          fichiers_partiels = nom_partiel(rep(c("CHR/U", "CH"), each = 3), rep(17:19, 2))[-4],
                          fichiers_exports = nom_ref(refs_all), FALSE, refs_all, REFS_CHRONIQUES)
ok("un seul partiel manquant (CH,17), refs présentes : 1 itération, années = 17, pas de prep_das_chronique",
   sum(plan1$iterations$a_faire) == 1 && plan1$iterations$an[plan1$iterations$a_faire] == 17 && !any(plan1$refs$a_faire) &&
     identical(plan1$annees_a_preparer, 17L) && !plan1$prep_das_chronique && !plan1$rien_a_faire)
plan2 <- resoudre_besoins(c("CHR/U", "CH"), 17:19, 26L, nom_partiel(rep(c("CHR/U", "CH"), each = 3), rep(17:19, 2)), nom_ref(refs_all), FALSE, refs_all, REFS_CHRONIQUES)
ok("tout présent : rien à faire, aucune année", plan2$rien_a_faire && length(plan2$annees_a_preparer) == 0)
plan3 <- resoudre_besoins(c("CHR/U", "CH"), 17:19, 26L, nom_partiel(rep(c("CHR/U", "CH"), each = 3), rep(17:19, 2)), nom_ref(refs_all)[-2], FALSE, refs_all, REFS_CHRONIQUES)
ok("seule ref_das_chronique manque : année AN_REF, prep_das_chronique", identical(plan3$annees_a_preparer, 26L) && plan3$prep_das_chronique && sum(plan3$refs$a_faire) == 1)
plan4 <- resoudre_besoins(c("CHR/U", "CH"), 17:19, 26L, nom_partiel(rep(c("CHR/U", "CH"), each = 3), rep(17:19, 2)), nom_ref(refs_all)[-1], FALSE, refs_all, REFS_CHRONIQUES)
ok("seule ref_das_aigu manque : année AN_REF, pas de prep_das_chronique", identical(plan4$annees_a_preparer, 26L) && !plan4$prep_das_chronique)
plan5 <- resoudre_besoins(c("CHR/U", "CH"), 17:19, 26L, nom_partiel(rep(c("CHR/U", "CH"), each = 3), rep(17:19, 2)), nom_ref(refs_all), TRUE, refs_all, REFS_CHRONIQUES)
ok("FORCER_REFS : toutes les refs à faire, année AN_REF", all(plan5$refs$a_faire) && identical(plan5$annees_a_preparer, 26L) && plan5$prep_das_chronique)
ok("chemins complets acceptés (basename)", !any(resoudre_besoins("CH", 17L, 26L, "/x/y/catalogue_partiel_CH_17.parquet", file.path("/z", nom_ref(refs_all)), FALSE, refs_all, REFS_CHRONIQUES)$iterations$a_faire))
ok("imprimer_plan renvoie le plan", identical(utils::capture.output(p <- imprimer_plan(plan1)) |> length() > 0, TRUE) && identical(p, plan1))

cat("\n# partiels : méta et apports\n")
mc <- meta_partiels_courant(2L, 25L, 3:100, PIVOTS_LONGS, "v8-test")
ok("meta courant", mc$K_GRAINE_LONGS == 2L && identical(mc$DUREE_LONGS, c(3L, 100L)))
rt <- yaml::yaml.load(yaml::as.yaml(mc))
ok("aller-retour yaml compatible", is.null(verifier_partiels_meta(rt, mc)$erreur) && length(verifier_partiels_meta(rt, mc)$avertissements) == 0)
bloque <- function(courant, cle){ v <- verifier_partiels_meta(rt, courant); !is.null(v$erreur) && grepl(cle, v$erreur) && grepl("PARTIELS_DIR", v$erreur) }
ok("K_GRAINE_LONGS différent -> bloquant", bloque(meta_partiels_courant(3L, 25L, 3:100, PIVOTS_LONGS, "v8-test"), "K_GRAINE_LONGS"))
ok("NBDA_MAX différent -> bloquant", bloque(meta_partiels_courant(2L, 20L, 3:100, PIVOTS_LONGS, "v8-test"), "NBDA_MAX"))
ok("DUREE_LONGS différent -> bloquant", bloque(meta_partiels_courant(2L, 25L, 3:60, PIVOTS_LONGS, "v8-test"), "DUREE_LONGS"))
ok("PIVOTS_LONGS différent -> bloquant", bloque(meta_partiels_courant(2L, 25L, 3:100, setdiff(PIVOTS_LONGS, "prep_sc"), "v8-test"), "PIVOTS_LONGS"))
ok("plusieurs clés différentes -> toutes listées dans le message", { v <- verifier_partiels_meta(rt, meta_partiels_courant(3L, 20L, 3:100, PIVOTS_LONGS, "v8-test")); grepl("K_GRAINE_LONGS", v$erreur) && grepl("NBDA_MAX", v$erreur) })
ok("VERSION_SCRIPT différent -> avertissement seulement, non bloquant", { v <- verifier_partiels_meta(rt, meta_partiels_courant(2L, 25L, 3:100, PIVOTS_LONGS, "v8-autre")); is.null(v$erreur) && length(v$avertissements) == 1 && grepl("VERSION_SCRIPT", v$avertissements) })
ok("pas de méta existante -> rien", is.null(verifier_partiels_meta(NULL, mc)$erreur))
ap <- apports_iteration("CH", 17L, "calculé", tibble::tibble(diag2 = c("A", "B")), tibble::tibble(diag2 = c("A", "B", "C")), "C")
ok("apports_iteration", ap$nb_lignes_partiel == 2 && ap$nb_lignes_cumul == 3 && ap$nb_diag2_cumul == 3 && ap$nb_diag2_nouveaux == 2)
ok("verifier_meta_tirage : identique -> NULL ; différent -> message", is.null(verifier_meta_tirage(list(a = 1L, b = "x"), list(a = 1L, b = "x"), c("a", "b"))) &&
     grepl("b", verifier_meta_tirage(list(a = 1L, b = "x"), list(a = 1L, b = "y"), c("a", "b"))))

cat("\n# tirage : pénalisation et livrables\n")
pen <- penaliser_comp_diabete(comp_diabete_fx, CAGE_PED, CAGE_AGES, 0.2, 0.5)
ok("penaliser_comp_diabete : .9 = 20 % du total chez [60-70[, 50 % chez [30-40[, autres inchangés",
   pen$nb[pen$cage == "[60-70[" & pen$diabete == "E11i" & pen$comp == "9"] == 0.2 * 38 &&
     pen$nb[pen$cage == "[30-40[" & pen$comp == "9"] == 0.5 * 36 && all(pen$nb[pen$comp != "9"] == comp_diabete_fx$nb[comp_diabete_fx$comp != "9"]))
lib <- libelles_cim(tibble::tibble(code = c("J44.9", "I10", "J44.9"), libelle = c("BPCO sp", "HTA", "doublon")))
ok("libelles_cim : codes sans point, premier libellé conservé", identical(lib, c(J449 = "BPCO sp", I10 = "HTA")))
df_rev <- tibble::tibble(ghm2 = c(rep("04M053", 6), rep("05M093", 2), "06C041"), sexe = "1", age = 70, cage = "[60-70[", duree = 5,
                         diag2 = "J449", diagnostic_associes = c(rep("I10 E785", 8), "N189"), graine = c(rep("I10", 8), ""), hta = "I10",
                         cmd = substr(c(rep("04M053", 6), rep("05M093", 2), "06C041"), 1, 2))
set.seed(12); e <- echantillonner_revue(df_rev, 5)
ok("echantillonner_revue : 5 lignes, chaque CMD représentée (round-robin)", nrow(e) == 5 && all(c("04", "05", "06") %in% e$cmd))
fr <- formater_revue(e, "longs", lib)
ok("formater_revue : libellés et marque [G] sur la graine", all(grepl("I10 \\(HTA\\) \\[G\\]", fr$das_libelles[fr$dp == "J449" & fr$nb_das == 2])) && all(fr$dp_libelle == "BPCO sp") && all(fr$branche == "longs"))
ok("formater_revue : branche courts (hta_scenario, sans graine) -> colonne hta reprise, pas de [G]",
   { fc <- formater_revue(df_rev |> dplyr::select(-graine, -hta) |> dplyr::mutate(hta_scenario = "I10"), "courts", lib)
     nrow(fc) == nrow(df_rev) && all(fc$hta == "I10") && !any(grepl("\\[G\\]", fc$das_libelles)) })
ok("formater_revue : code inconnu -> ?", any(grepl("N189 \\(\\?\\)", formater_revue(df_rev, "longs", lib)$das_libelles)))
td <- top_das_par_cmd(df_rev, 1)
ok("top_das_par_cmd : 1 par CMD, rang 1", nrow(td) == 3 && all(td$rang == 1) && td$das[td$cmd == "04"] == "E785")

cat("\nTOUS LES TESTS SONT VERTS :", n_ok, "assertions\n")
