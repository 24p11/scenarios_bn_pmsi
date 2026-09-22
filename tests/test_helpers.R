###############################################################################
# tests/test_helpers.R — tests unitaires hors base des helpers purs (SPEC §8.1 + brief
# industrialisation §8). Exécution : Rscript tests/test_helpers.R (racine du dépôt ou tests/).
# Source config.R (constantes, sans effet de bord) puis helpers.R : aucune connexion
# base, aucune dépendance à utils.R / referentiels.R. arrow optionnel (repli saveRDS/readRDS
# pour pmap_chunks, dans le test uniquement).
###############################################################################
suppressPackageStartupMessages({library(dplyr); library(tibble); library(stringr)})
for(loc in c("fr_FR.UTF-8", "en_US.UTF-8", "C.UTF-8")) if(!is.na(suppressWarnings(Sys.setlocale("LC_CTYPE", loc))) && Sys.getlocale("LC_CTYPE") == loc) break
lib_test <- Sys.getenv("R_LIBS_TEST", unset = ""); if(nzchar(lib_test)) .libPaths(c(lib_test, .libPaths()))

# Repli arrow (tests UNIQUEMENT) : si arrow est absent, un paquet mock `arrow` (write_parquet/read_parquet =
# saveRDS/readRDS) est installé dans tempdir — source unique : demo/mock_pratihque.R (chantier « packaging + démo »).
racine <- c(".", "..")[file.exists(c("config.R", "../config.R"))][1]
stopifnot(!is.na(racine))
source(file.path(racine, "demo", "mock_pratihque.R"))
ARROW_MOCK <- !requireNamespace("arrow", quietly = TRUE)
if(ARROW_MOCK) installer_mock_arrow()

Sys.unsetenv("SCENARIOS_PMSI_SURCHARGE"); Sys.setenv(SCENARIOS_PMSI_PATH = normalizePath(racine))   # les tests posent leur chemin (plus de défaut versionné)
source(file.path(racine, "config.R"))
source(file.path(racine, "helpers.R"))

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
cat("\n# taille_chunk / pmap_chunks (chunking dynamique)\n")
ok("taille_chunk : n petit -> un seul chunk de n (plancher)", taille_chunk(23, 50L, 500L, NA_integer_) == 500L && ceiling(23 / taille_chunk(23, 50L, 500L, NA_integer_)) == 1)
ok("taille_chunk : n moyen -> plancher actif (moins de chunks que NB_CHUNKS_MAX)", taille_chunk(10000, 50L, 500L, NA_integer_) == 500L && ceiling(10000 / 500) == 20)
ok("taille_chunk : n grand -> exactement <= NB_CHUNKS_MAX chunks", { cs <- taille_chunk(1234567, 50L, 500L, NA_integer_); cs == 24692L && ceiling(1234567 / cs) <= 50 && ceiling(1234567 / cs) == 50 })
ok("taille_chunk : CHUNK_SIZE_FIXE prioritaire", taille_chunk(1234567, 50L, 500L, 40L) == 40L && taille_chunk(5, 50L, 500L, 2L) == 2L)
ok("taille_chunk : défauts pris dans la config", taille_chunk(10) == max(CHUNK_SIZE_MIN, ceiling(10 / NB_CHUNKS_MAX)))
ecrire_t <- arrow::write_parquet; lire_t <- arrow::read_parquet; ext_t <- ".parquet"
cat("   (écriture des chunks :", if(ARROW_MOCK) "mock arrow = RDS (test uniquement)" else "arrow", ")\n")
f_test <- function(id, k){ if(k == 0) return(NULL); tibble::tibble(id = id, k = k, u = round(stats::runif(1), 6), s = sample(letters, 1)) }
df_in <- tibble::tibble(id = 1:23, k = c(rep(1L, 10), 0L, rep(2L, 12)))
run_chunks <- function(dossier, chunk_size = 5, df = df_in, seed_base = 100, ...) pmap_chunks(df, f_test, chunk_size = chunk_size, dossier = dossier, prefixe = "t", seed_base = seed_base,
                                                                                         ecrire = ecrire_t, lire = lire_t, ext = ext_t, verbose = FALSE, ...)
d1 <- file.path(tempdir(), "chunks1"); unlink(d1, recursive = TRUE)
r1 <- run_chunks(d1)
ok("découpage exact : 23 lignes / 5 -> 5 chunks", length(list.files(d1, pattern = "^t_chunk_\\d{4}")) == 5 && all(sprintf("t_chunk_%04d%s", 1:5, ext_t) %in% list.files(d1)))
ok("lignes NULL ignorées : 22 lignes en sortie, ordre conservé", nrow(r1) == 22 && identical(r1$id, setdiff(1:23, 11L)))
sc <- yaml::read_yaml(file.path(d1, "t_chunks_meta.yaml"))
ok("sidecar écrit et complet (n, chunk_size, seed_base, nb_chunks, date)", sc$n == 23 && sc$chunk_size == 5 && sc$seed_base == 100 && sc$nb_chunks == 5 && nzchar(sc$date))
unlink(file.path(d1, sprintf("t_chunk_%04d%s", 3, ext_t)))
r2 <- run_chunks(d1)
ok("reprise mêmes paramètres : chunk du milieu supprimé, relance -> identité bit à bit avec le run complet", identical(r1, r2))
err <- tryCatch({ run_chunks(d1, df = df_in[1:20, ]); NULL }, error = function(e) conditionMessage(e))
ok("reprise avec n modifié -> stop explicite (attendu / reçu)", !is.null(err) && grepl("découpage incompatible", err) && grepl("attendu", err) && grepl("reçu", err))
err <- tryCatch({ run_chunks(d1, chunk_size = 7); NULL }, error = function(e) conditionMessage(e))
ok("reprise avec chunk_size modifié -> stop", !is.null(err) && grepl("chunk_size", err))
err <- tryCatch({ run_chunks(d1, seed_base = 101); NULL }, error = function(e) conditionMessage(e))
ok("reprise avec seed_base modifié -> stop", !is.null(err) && grepl("seed_base", err))
unlink(file.path(d1, "t_chunks_meta.yaml"))
err <- tryCatch({ run_chunks(d1); NULL }, error = function(e) conditionMessage(e))
ok("chunks présents sans sidecar (dossier antérieur) -> stop explicatif", !is.null(err) && grepl("sans sidecar", err) && grepl("Videz", err))
d2 <- file.path(tempdir(), "chunks2"); unlink(d2, recursive = TRUE)
ok("déterminisme : second run complet dans un autre dossier identique", identical(r1, run_chunks(d2)))
# sidecar écrit AVANT le premier chunk : f qui échoue au premier appel
d_av <- file.path(tempdir(), "chunks_avant"); unlink(d_av, recursive = TRUE)
err <- tryCatch({ pmap_chunks(df_in, function(id, k) stop("boum"), chunk_size = 5, dossier = d_av, prefixe = "t", seed_base = 1, ecrire = ecrire_t, lire = lire_t, ext = ext_t, verbose = FALSE); NULL }, error = function(e) conditionMessage(e))
ok("sidecar écrit avant le premier chunk (présent même si le chunk 1 échoue)", !is.null(err) && file.exists(file.path(d_av, "t_chunks_meta.yaml")) && length(list.files(d_av, pattern = "^t_chunk_")) == 0)
# chunking automatique
d_auto <- file.path(tempdir(), "chunks_auto"); unlink(d_auto, recursive = TRUE)
df_big <- tibble::tibble(id = 1:1200, k = 1L)
r_auto <- pmap_chunks(df_big, f_test, chunk_size = NULL, dossier = d_auto, prefixe = "a", seed_base = 7, ecrire = ecrire_t, lire = lire_t, ext = ext_t, verbose = FALSE)
ok("pmap_chunks auto : nombre de chunks <= NB_CHUNKS_MAX, sidecar cohérent avec taille_chunk", { n_ch <- length(list.files(d_auto, pattern = "^a_chunk_")); sc <- yaml::read_yaml(file.path(d_auto, "a_chunks_meta.yaml"))
   n_ch <= NB_CHUNKS_MAX && n_ch == ceiling(1200 / taille_chunk(1200)) && sc$chunk_size == taille_chunk(1200) && nrow(r_auto) == 1200 })
d3 <- file.path(tempdir(), "chunks3"); unlink(d3, recursive = TRUE)
df_null <- tibble::tibble(id = 1:7, k = 0L)
r3 <- pmap_chunks(df_null, f_test, chunk_size = 3, dossier = d3, prefixe = "n", seed_base = 1, ecrire = ecrire_t, lire = lire_t, ext = ext_t, verbose = FALSE)
ok("chunk dont f retourne NULL : chunks écrits, sortie vide sans colonne .chunk_vide", length(list.files(d3, pattern = "^n_chunk_")) == 3 && nrow(r3) == 0 && !".chunk_vide" %in% names(r3))
d4 <- file.path(tempdir(), "chunks4"); unlink(d4, recursive = TRUE)
r4 <- pmap_chunks(df_in, f_test, chunk_size = 5, dossier = d4, prefixe = "g", seed_base = 100, garder_chunks = FALSE, ecrire = ecrire_t, lire = lire_t, ext = ext_t, verbose = FALSE)
ok("garder_chunks = FALSE : chunks supprimés (sidecar conservé), résultat identique", length(list.files(d4, pattern = "^g_chunk_")) == 0 && identical(r4, r1))
ok("df vide -> tibble vide", nrow(pmap_chunks(df_in[0, ], f_test, 5, file.path(tempdir(), "chunks5"), "v", 1, ecrire = ecrire_t, lire = lire_t, ext = ext_t, verbose = FALSE)) == 0)
ok("deux préfixes dans un même dossier : sidecars distincts", { d6 <- file.path(tempdir(), "chunks6"); unlink(d6, recursive = TRUE)
   pmap_chunks(df_in, f_test, 5, d6, "x", 1, ecrire = ecrire_t, lire = lire_t, ext = ext_t, verbose = FALSE); pmap_chunks(df_in[1:10, ], f_test, 5, d6, "y", 1, ecrire = ecrire_t, lire = lire_t, ext = ext_t, verbose = FALSE)
   all(c("x_chunks_meta.yaml", "y_chunks_meta.yaml") %in% list.files(d6)) })

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
ok("rien de présent : 6 itérations, 10 refs, années 17,18,19,26, prep_das_chronique",
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
plan3 <- resoudre_besoins(c("CHR/U", "CH"), 17:19, 26L, nom_partiel(rep(c("CHR/U", "CH"), each = 3), rep(17:19, 2)), nom_ref(setdiff(refs_all, "ref_das_chronique")), FALSE, refs_all, REFS_CHRONIQUES)
ok("seule ref_das_chronique manque : année AN_REF, prep_das_chronique", identical(plan3$annees_a_preparer, 26L) && plan3$prep_das_chronique && sum(plan3$refs$a_faire) == 1)
plan4 <- resoudre_besoins(c("CHR/U", "CH"), 17:19, 26L, nom_partiel(rep(c("CHR/U", "CH"), each = 3), rep(17:19, 2)), nom_ref(setdiff(refs_all, "ref_das_aigu")), FALSE, refs_all, REFS_CHRONIQUES)
ok("seule ref_das_aigu manque : année AN_REF, pas de prep_das_chronique", identical(plan4$annees_a_preparer, 26L) && !plan4$prep_das_chronique)
plan5 <- resoudre_besoins(c("CHR/U", "CH"), 17:19, 26L, nom_partiel(rep(c("CHR/U", "CH"), each = 3), rep(17:19, 2)), nom_ref(refs_all), TRUE, refs_all, REFS_CHRONIQUES)
ok("FORCER_REFS : toutes les refs à faire, année AN_REF", all(plan5$refs$a_faire) && identical(plan5$annees_a_preparer, 26L) && plan5$prep_das_chronique)
ok("ordre des refs : ref_das_chronique puis distribution_e660 avant toute ref convertie", identical(plan0$refs$nom[1:2], c("ref_das_chronique", "ref_distribution_e660")) && "ref_distribution_e660" %in% REFS_CHRONIQUES)
ok("chemins complets acceptés (basename)", !any(resoudre_besoins("CH", 17L, 26L, "/x/y/catalogue_partiel_CH_17.parquet", file.path("/z", nom_ref(refs_all)), FALSE, refs_all, REFS_CHRONIQUES)$iterations$a_faire))
ok("imprimer_plan renvoie le plan", identical(utils::capture.output(p <- imprimer_plan(plan1)) |> length() > 0, TRUE) && identical(p, plan1))

cat("\n# partiels : méta et apports\n")
mc <- meta_partiels_courant(2L, 25L, 3:100, PIVOTS_LONGS, "v8-test")
ok("meta courant", mc$K_GRAINE_LONGS == 2L && identical(mc$DUREE_LONGS, c(3L, 100L)))
rt <- yaml::yaml.load(yaml::as.yaml(mc))
ok("aller-retour yaml compatible", is.null(verifier_partiels_meta(rt, mc)$erreur) && length(verifier_partiels_meta(rt, mc)$avertissements) == 0)
bloque <- function(courant, cle){ v <- verifier_partiels_meta(rt, courant); !is.null(v$erreur) && grepl(cle, v$erreur) && grepl("00_partiels", v$erreur) && grepl("FORCER_PARTIELS", v$erreur) }
ok("K_GRAINE_LONGS différent -> bloquant", bloque(meta_partiels_courant(3L, 25L, 3:100, PIVOTS_LONGS, "v8-test"), "K_GRAINE_LONGS"))
ok("NBDA_MAX différent -> bloquant", bloque(meta_partiels_courant(2L, 20L, 3:100, PIVOTS_LONGS, "v8-test"), "NBDA_MAX"))
ok("DUREE_LONGS différent -> bloquant", bloque(meta_partiels_courant(2L, 25L, 3:60, PIVOTS_LONGS, "v8-test"), "DUREE_LONGS"))
ok("PIVOTS_LONGS différent -> bloquant", bloque(meta_partiels_courant(2L, 25L, 3:100, setdiff(PIVOTS_LONGS, "prep_sc"), "v8-test"), "PIVOTS_LONGS"))
ok("plusieurs clés différentes -> toutes listées dans le message", { v <- verifier_partiels_meta(rt, meta_partiels_courant(3L, 20L, 3:100, PIVOTS_LONGS, "v8-test")); grepl("K_GRAINE_LONGS", v$erreur) && grepl("NBDA_MAX", v$erreur) })
ok("VERSION_SCRIPT différent -> avertissement seulement, non bloquant", { v <- verifier_partiels_meta(rt, meta_partiels_courant(2L, 25L, 3:100, PIVOTS_LONGS, "v8-autre")); is.null(v$erreur) && length(v$avertissements) == 1 && grepl("VERSION_SCRIPT", v$avertissements) })
ok("pas de méta existante -> rien", is.null(verifier_partiels_meta(NULL, mc)$erreur))
ap <- apports_partiel("CH", 17L, "calculé", tibble::tibble(diag2 = c("A", "B", "A"), n = c(2L, 3L, 1L)), "B")
ok("apports_partiel : stats du partiel seul", ap$nb_lignes_partiel == 3 && ap$sum_n_partiel == 6 && ap$nb_diag2_partiel == 2 && ap$nb_diag2_nouveaux == 1 && !"nb_lignes_cumul" %in% names(ap))
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


# ============================================================ conversion E669 ==
cat("\n# convertir_e669\n")
ok("suffixes conservés : E6690 -> E6600, E6692 -> E6602, E66920 -> E66020, E6691 -> E6601",
   identical(convertir_e669(c("E6690", "E6692", "E66920", "E6691")), c("E6600", "E6602", "E66020", "E6601")))
ok("E669 nu inchangé à ce niveau", convertir_e669("E669") == "E669")
ok("NA-sûre", is.na(convertir_e669(NA)) && identical(convertir_e669(c(NA, "E6690")), c(NA, "E6600")))
ok("E661/E662/E668 et E660 intacts", identical(convertir_e669(c("E6610", "E6620", "E6680", "E6600", "E66")), c("E6610", "E6620", "E6680", "E6600", "E66")))
ok("codes non E66 intacts, vecteur vide", identical(convertir_e669(c("I10", "J449", "K6690")), c("I10", "J449", "K6690")) && length(convertir_e669(character(0))) == 0)
ok("repartir_proportionnel : sum conservée, plus forts restes", identical(repartir_proportionnel(10, c(0.5, 0.3, 0.2)), c(5, 3, 2)) &&
     identical(repartir_proportionnel(7, c(1, 1, 1)), c(3, 2, 2)) && sum(repartir_proportionnel(11, c(0.45, 0.35, 0.2))) == 11 && length(repartir_proportionnel(3, numeric(0))) == 0)

cat("\n# distribution_e660 / classes_e660\n")
ref_e660 <- tibble::tibble(diag2 = "J449", das = c("E6600", "E6601", "E6602", "E6600", "E6602", "E6690", "I10"),
                           sexe = c("1", "1", "1", "2", "2", "1", "1"), cage = c(rep("[60-70[", 5), "[60-70[", "[60-70["),
                           niveau = "1", type_liste = "Patho_chro", caract = "x", nb_das = c(60, 30, 10, 20, 20, 5, 100))
dist <- distribution_e660(ref_e660)
ok("distribution : E660x seulement (E6690 exclu), strates + globale", !any(dist$code == "E6690") && sum(is.na(dist$cage)) == 3 && nrow(dist) == 8)
ok("parts par strate : (1) 0.6/0.3/0.1 ; (2) 0.5/0.5", { s1 <- dist[!is.na(dist$cage) & dist$sexe == "1", ]; s2 <- dist[!is.na(dist$cage) & dist$sexe == "2", ]
   identical(round(s1$part[order(s1$code)], 3), c(0.6, 0.3, 0.1)) && identical(round(s2$part[order(s2$code)], 3), c(0.5, 0.5)) })
ok("part globale : 80/30/30 sur 140", identical(round(dist$part[is.na(dist$cage)][order(dist$code[is.na(dist$cage)])], 4), round(c(80, 30, 30) / 140, 4)))
ok("cascade : strate connue", identical(classes_e660(dist, "[60-70[", "2")$code, c("E6600", "E6602")))
ok("cascade : strate inconnue -> globale", identical(classes_e660(dist, "[80-[", "1")$code, c("E6600", "E6601", "E6602")))
ok("cascade : sans distribution -> défaut", identical(classes_e660(distribution_e660(ref_e660[0, ]), "[60-70[", "1", "0"), tibble::tibble(code = "E6600", part = 1)))
ok("distribution vide sur table sans E660", nrow(distribution_e660(tibble::tibble(das = "I10", nb_das = 1, cage = "a", sexe = "1"))) == 0)

cat("\n# repartir_e669_nu / convertir_e669_comptes\n")
df_nu <- tibble::tibble(diag2 = "J449", das = c("E669", "E669", "E669", "I10"), sexe = c("1", "2", "1", "1"),
                        cage = c("[60-70[", "[60-70[", "[80-[", "[60-70["), nb_das = c(100, 7, 10, 3))
r <- repartir_e669_nu(df_nu, "das", c("diag2", "sexe", "cage"), "nb_das", dist)
ok("proportionnalité exacte strate (1) : 100 -> 60/30/10", { x <- r[r$sexe == "1" & r$cage == "[60-70[" & grepl("^E660", r$das), ]; identical(x$nb_das[order(x$das)], c(60, 30, 10)) })
ok("plus forts restes strate (2) : 7 -> 4/3, sum conservée", { x <- r[r$sexe == "2", ]; identical(x$nb_das[order(x$das)], c(4, 3)) })
ok("cascade globale pour strate inconnue : 10 -> 6/2/2 (80/30/30 sur 140)", { x <- r[r$cage == "[80-[", ]; identical(x$nb_das[order(x$das)], c(6, 2, 2)) })
ok("sum(n) conservée globalement, ligne I10 intacte, schéma préservé", sum(r$nb_das) == sum(df_nu$nb_das) && any(r$das == "I10" & r$nb_das == 3) && identical(names(r), names(df_nu)))
ok("cascade défaut sans distribution", { x <- repartir_e669_nu(df_nu[1, ], "das", c("diag2", "sexe", "cage"), "nb_das", dist[0, ], "0"); x$das == "E6600" && x$nb_das == 100 })
ok("sans E669 nu : df inchangé", identical(repartir_e669_nu(df_nu[4, ], "das", c("diag2", "sexe", "cage"), "nb_das", dist), df_nu[4, ]))
df_c <- tibble::tibble(diag2 = c("J449", "J449", "J449", "E6690", "E669"), das = c("E6690", "E6600", "E669", "I10", "I10"),
                       sexe = "1", cage = "[60-70[", nb_das = c(5, 10, 10, 2, 10))
rc <- convertir_e669_comptes(df_c, "das", c("diag2", "sexe", "cage"), "nb_das", dist)
ok("comptes : E6690 fusionné avec E6600, nu réparti, ré-agrégation, sum conservée",
   sum(rc$nb_das) == sum(df_c$nb_das) && rc$nb_das[rc$diag2 == "J449" & rc$das == "E6600"] == 5 + 10 + 6 && !any(grepl("^E669", rc$das)) && identical(names(rc), names(df_c)))
rc2 <- convertir_e669_comptes(rc, "diag2", c("das", "sexe", "cage"), "nb_das", dist)
ok("comptes sur diag2 : E6690 -> E6600, E669 nu réparti (10 -> 6/3/1), aucun ^E669 résiduel",
   !any(grepl("^E669", rc2$diag2)) && sum(rc2$nb_das) == sum(df_c$nb_das) && rc2$nb_das[rc2$diag2 == "E6600" & rc2$das == "I10"] == 2 + 6)

cat("\n# convertir_e669_combo\n")
df_g <- tibble::tibble(mode_hospit = "HC", sexe = "1", cage = "[60-70[", diag2 = "J449",
                       diagnostic_associes = c("I10 N189", "E6690 I10", "E6600 I10", "E669 I10", "E6600 E669"), n = c(4, 3, 2, 10, 10))
rg <- convertir_e669_combo(df_g, "diagnostic_associes", c("mode_hospit", "sexe", "cage", "diag2"), "n", dist)
ok("combo : tri C et fusion E6690 -> E6600 avec la ligne existante", rg$n[rg$diagnostic_associes == "E6600 I10"] == 3 + 2 + 6)
ok("combo : E669 nu éclaté en 3 lignes (6/3/1), totaux conservés", sum(rg$n) == sum(df_g$n) && rg$n[rg$diagnostic_associes == "E6601 I10"] == 3 && rg$n[rg$diagnostic_associes == "E6602 I10"] == 1)
ok("combo : E6600 E669 -> E6600 seul (dédoublonné) 6, E6600 E6601 3, E6600 E6602 1", rg$n[rg$diagnostic_associes == "E6600"] == 6 && rg$n[rg$diagnostic_associes == "E6600 E6601"] == 3 && rg$n[rg$diagnostic_associes == "E6600 E6602"] == 1)
ok("combo : aucun ^E669, schéma préservé, ligne sans E66 intacte", !any(grepl("E669", rg$diagnostic_associes)) && identical(names(rg), names(df_g)) && rg$n[rg$diagnostic_associes == "I10 N189"] == 4)
ok("combo sans E669 : ré-agrégation seule", identical(convertir_e669_combo(df_g[1, ], "diagnostic_associes", c("mode_hospit", "sexe", "cage", "diag2"), "n", dist)$n, 4))
ok("convertir_e669_distinct : nu -> une ligne par classe de la strate, suffixé converti, distinct",
   { d <- tibble::tibble(sexe = "1", cage = "[60-70[", diag2 = c("E669", "E6690", "E6600", "I10"), mdp = "DP")
     x <- convertir_e669_distinct(d, "diag2", dist); setequal(x$diag2, c("E6600", "E6601", "E6602", "I10")) && nrow(x) == 4 })
ok("compter_e669 / effectifs_e660", compter_e669(df_g, c("diag2", "diagnostic_associes")) == 3 && identical(effectifs_e660(rg, "diagnostic_associes")$code, c("E6600", "E6601", "E6602")))
imp <- impact_conversion_catalogue(df_g, rg, c("mode_hospit", "sexe", "cage", "diag2"), 5, "n")
ok("impact_conversion_catalogue : totaux, lignes fusionnées, effectifs E669", imp$n_total_avant == imp$n_total_apres && imp$lignes_fusionnees == nrow(df_g) - nrow(rg) &&
     imp$e669_graine_suffixe == 3 && imp$e669_graine_nu == 20 && imp$e669_diag2_nu == 0)
ok("impact_niveau_cma : niveau différent compté", { r <- impact_niveau_cma(tibble::tibble(das = c("E6690", "E6600", "E6692"), niveau = c("2", "1", "2"), nb_das = c(5, 1, 3)))
     r$effectif_e669 == 8 && r$niveau_change == 5 && r$cible_inconnue == 3 })


# ============================================================ mémoire 15 GiB ==
cat("\n# collapse_graine\n")
piv_t <- c("mode_hospit", "sexe", "cage", "diag2")
topk <- tibble::tibble(ident = c(3, 3, 1, 2, 2, 4), mode_hospit = "HC", sexe = "1",
                       cage = c("[60-70[", "[60-70[", "[60-70[", "[80-[", "[80-[", "[60-70["), diag2 = "J449",
                       das = c("N189", "I500", "I10", "E785", "A000", NA))
cg2 <- collapse_graine(topk, piv_t, 2)
ok("k = 2 : appariement trié en ordre C, séjour à 1 DAS, séjour sans DAS -> \"NA\"",
   setequal(cg2$diagnostic_associes, c("I500 N189", "I10", "A000 E785", "NA")) && all(cg2$n == 1) && identical(names(cg2), c(piv_t, "diagnostic_associes", "n")))
ok("k = 2 vectorisé == repli générique (k = 3 sur mêmes données à 2 DAS max)", identical(dplyr::arrange(cg2, dplyr::across(dplyr::everything())), dplyr::arrange(collapse_graine(topk, piv_t, 3), dplyr::across(dplyr::everything()))))
ok("k = 2 : agrégation des séjours identiques", { t2 <- dplyr::bind_rows(topk, topk |> dplyr::mutate(ident = ident + 10)); all(collapse_graine(t2, piv_t, 2)$n == 2) })
ok("df vide -> schéma complet, 0 ligne", { e <- collapse_graine(topk[0, ], piv_t, 2); nrow(e) == 0 && identical(names(e), c(piv_t, "diagnostic_associes", "n")) })
ok("k = 2 : ordre d'entrée indifférent", identical(dplyr::arrange(collapse_graine(topk[c(6, 5, 4, 3, 2, 1), ], piv_t, 2), dplyr::across(dplyr::everything())), dplyr::arrange(cg2, dplyr::across(dplyr::everything()))))

cat("\n# agreger_partiels (chemin incrémental et arrow)\n")
mk <- function(i) tibble::tibble(mode_hospit = "HC", sexe = c("1", "2", "1"), cage = "[60-70[", diag2 = c("J449", "J449", "E6690"),
                                 nbda = 2L, diagnostic_associes = c("I10 N189", "I10", "E785"), n = c(1L, 2L, 3L) * i)
dpart <- file.path(tempdir(), "partiels_t"); unlink(dpart, recursive = TRUE); dir.create(dpart)
fp <- file.path(dpart, sprintf("p_%d.parquet", 1:3))
for(i in 1:3) arrow::write_parquet(mk(i), fp[i])
ref_global <- dplyr::bind_rows(lapply(1:3, mk)) |> dplyr::summarise(n = sum(n), .by = c(mode_hospit, sexe, cage, diag2, nbda, diagnostic_associes)) |>
  dplyr::arrange(dplyr::across(dplyr::everything()))
r_inc <- agreger_partiels(fp, c("mode_hospit", "sexe", "cage", "diag2", "nbda", "diagnostic_associes"), "n", chemin = "incremental")
ok("chemin (b) incrémental == bind_rows + summarise global", identical(as.data.frame(r_inc), as.data.frame(ref_global)))
cles <- tibble::tibble(sexe = "1", diag2 = "J449")
ref_f <- ref_global |> dplyr::semi_join(cles, by = c("sexe", "diag2"))
r_inc_f <- agreger_partiels(fp, c("mode_hospit", "sexe", "cage", "diag2", "nbda", "diagnostic_associes"), "n", filtre_cles = cles, chemin = "incremental")
ok("chemin (b) avec filtre de clés == référence filtrée", identical(as.data.frame(r_inc_f), as.data.frame(ref_f)) && nrow(r_inc_f) == 1)
r_piv <- agreger_partiels(fp, c("sexe", "diag2"), "n", chemin = "incremental")
ok("chemin (b) niveau pivots : sommes exactes", r_piv$n[r_piv$sexe == "1" & r_piv$diag2 == "J449"] == 6 && r_piv$n[r_piv$diag2 == "E6690"] == 18)
if(!ARROW_MOCK){
  sans_repli <- function(expr){ w <- NULL; v <- withCallingHandlers(expr, warning = function(x){ w <<- conditionMessage(x); invokeRestart("muffleWarning") }); list(v = v, repli = !is.null(w) && grepl("repli", w)) }
  ra <- sans_repli(agreger_partiels(fp, c("mode_hospit", "sexe", "cage", "diag2", "nbda", "diagnostic_associes"), "n", chemin = "arrow"))
  ok("chemin (a) arrow::open_dataset == chemin (b), SANS repli", !ra$repli && identical(as.data.frame(ra$v), as.data.frame(r_inc)))
  raf <- sans_repli(agreger_partiels(fp, c("mode_hospit", "sexe", "cage", "diag2", "nbda", "diagnostic_associes"), "n", filtre_cles = cles, chemin = "arrow"))
  ok("chemin (a) avec filtre == chemin (b) avec filtre, SANS repli", !raf$repli && identical(as.data.frame(raf$v), as.data.frame(r_inc_f)))
  rap <- sans_repli(agreger_partiels(fp, c("sexe", "diag2"), "n", chemin = "arrow"))
  ok("chemin (a) niveau pivots == chemin (b), SANS repli", !rap$repli && identical(as.data.frame(rap$v), as.data.frame(r_piv)))
  ok("sélection auto = arrow quand disponible", arrow_dataset_disponible())
} else ok("mock arrow : sélection auto = incrémental", !arrow_dataset_disponible())
ok("filtre sans correspondance -> 0 ligne, schéma conservé", { z <- agreger_partiels(fp, c("sexe", "diag2"), "n", filtre_cles = tibble::tibble(diag2 = "ZZZ"), chemin = "incremental"); nrow(z) == 0 && identical(names(z), c("sexe", "diag2", "n")) })

cat("\n# cles_brutes_retenues\n")
cols_s <- c("sexe", "cage", "diag2")
pb <- tibble::tibble(sexe = "1", cage = "[60-70[", diag2 = c("J449", "E6690", "E669", "E669", "I10", "E6691"), n = c(5L, 1L, 1L, 1L, 1L, 1L))
pb$cage[4] <- "[80-["
ret <- tibble::tibble(sexe = "1", cage = c("[60-70[", "[60-70[", "[80-["), diag2 = c("J449", "E6600", "E6602"))
cb <- cles_brutes_retenues(pb, ret, cols_s, dist)
ok("identité : J449 retenu ; suffixé : E6690 -> E6600 retenu ; E6691 -> E6601 non retenu ; I10 non retenu",
   "J449" %in% cb$diag2 && "E6690" %in% cb$diag2 && !"E6691" %in% cb$diag2 && !"I10" %in% cb$diag2)
ok("nu multi-cibles dont une retenue (strate 1 : E6600/1/2, E6600 retenu) -> retenu", any(cb$diag2 == "E669" & cb$cage == "[60-70["))
ok("nu dont une cible retenue via cascade globale ([80-[ -> E6602 retenu) -> retenu", any(cb$diag2 == "E669" & cb$cage == "[80-["))
ok("nu aucune cible retenue -> non retenu", !any(cles_brutes_retenues(pb, ret[ret$diag2 == "J449", ], cols_s, dist)$diag2 == "E669"))
ok("sans clé retenue -> vide ; schéma = cols", { z <- cles_brutes_retenues(pb, ret[0, ], cols_s, dist); nrow(z) == 0 && identical(names(z), cols_s) })
ip <- impact_conversion_pivots(pb, convertir_e669_comptes(pb, "diag2", c("sexe", "cage"), "n", dist), cols_s, 1, "n")
ok("impact_conversion_pivots : totaux, E669 diag2 suffixé/nu, profils", ip$n_total_avant == ip$n_total_apres && ip$e669_diag2_suffixe == 2 && ip$e669_diag2_nu == 2 && ip$profils_seuil_avant == 1 && ip$profils_seuil_apres >= 1)
ok("effectif_e669_combos", { e <- effectif_e669_combos(tibble::tibble(g = c("E6690 I10", "E669", "I10"), n = c(2, 3, 4)), "g", "n"); e$suffixe == 2 && e$nu == 3 })

cat("\n# recouvrement_partiels\n")
A <- tibble::tibble(mode_hospit = "HC", sexe = "1", diag2 = c("J449", "J449", "I500"), diagnostic_associes = c("I10", "N189", "I10"), n = c(10L, 5L, 2L))
B <- tibble::tibble(mode_hospit = "HC", sexe = c("1", "1", "1", "2"), diag2 = c("J449", "J449", "K802", "J449"), diagnostic_associes = c("I10", "E785", "I10", "I10"), n = c(4L, 6L, 3L, 7L))
rc <- recouvrement_partiels(A, B, c("mode_hospit", "sexe", "diag2"))
ok("combinaisons : 1 commune sur 4 (25 %), séjours vus 4/20", rc$nb_A == 3 && rc$nb_B == 4 && rc$nb_communes == 1 && rc$part_combos_B_vues == 0.25 && rc$part_sejours_B_vus == 0.2 && rc$sejours_B_uniques_nouveaux == 16)
ok("pivots : (HC,1,J449) commun sur 3 pivots B, séjours pivots 10/20", rc$nb_pivots_A == 2 && rc$nb_pivots_B == 3 && rc$nb_pivots_communs == 1 && rc$part_pivots_B_vus == 1/3 && rc$part_sejours_pivots_B_vus == 0.5)
ok("diag2 nouveaux : K802", rc$nb_diag2_B_nouveaux == 1)
ok("B vide -> parts NA sans erreur", is.na(recouvrement_partiels(A, B[0, ], c("mode_hospit", "sexe", "diag2"))$part_combos_B_vues))

cat("\n# mesurer_memoire\n")
jm <- mesurer_memoire("etape 1", numeric(1e6), NULL, 10, verbose = FALSE)
jm <- mesurer_memoire("etape 2", NULL, jm, 10, verbose = FALSE)
ok("schéma du csv : etiquette, horodatage, taille_objet_mo, memoire_utilisee_go, pic_go, alerte",
   identical(names(jm), c("etiquette", "horodatage", "taille_objet_mo", "memoire_utilisee_go", "pic_go", "alerte")) && nrow(jm) == 2 && is.na(jm$taille_objet_mo[2]) && jm$taille_objet_mo[1] > 0 && all(!jm$alerte))
ok("seuil d'alerte : avertissement et colonne alerte", { w <- NULL; j3 <- withCallingHandlers(mesurer_memoire("gros", NULL, NULL, 0.000001, verbose = FALSE), warning = function(x){ w <<- conditionMessage(x); invokeRestart("muffleWarning") }); j3$alerte && grepl("SEUIL_ALERTE_GO", w) })


# ============================================================ aval production ==
cat("\n# typologie_sejour\n")
typo <- charger_typologie(file.path(racine, "referentiels", "typologie_sejours.yaml"))
ok("yaml == listes STREAM (recopiées telles quelles)", identical(typo$RACINES_GREFFES_CART, c("27Z02", "27Z03")) && identical(typo$RACINES_TRANSPLANT, c("27C02", "27C03", "27C04", "27C05", "27C06", "27C07")) &&
     identical(typo$RACINES_IMG_FC, c("14Z04", "14C05", "14C06", "14C09", "14Z10", "14Z15", "14Z09")) && identical(typo$RACINES_BB_CHIR, c("15C02", "15C03", "15C04", "15C05", "15C06", "15M10", "15M11", "15M13", "15M14")) &&
     identical(typo$RACINES_AUTRE_NEONAT, c("15M02", "15M03", "15M04")) && length(typo$DPEC_TO_TPEC) == 22 && typo$DPEC_TO_TPEC[["Chirurgie adultes > 3 nuits"]] == "Chirurgie et interventionnel")
cas <- tibble::tibble(
  ghm2 = c("27Z02Z", "27C06A", "22Z02B", "14Z08Z", "14Z04Z", "14Z13T", "14Z13B", "14Z13A", "15M05A", "15M10B", "15C02A", "15M02A", "28Z04Z", "28Z07Z", "28Z07Z", "28Z01Z", "04M05B", "04M05B", "04M05B", "06C04B", "06C04B", "05K10A", "05K10A", "90H00Z"),
  diag2 = c(rep("J449", 12), "Z04801", "Z511", "Z511", "Z512", rep("J449", 8)),
  age = c(rep(70, 14), 15, 70, rep(70, 8)), duree = c(rep(5, 16), 5, 2, 5, 5, 2, 5, 2, 5),
  mode_hospit = c(rep("HC", 18), "HP", rep("HC", 5)))
r <- typologie_sejour(cas, typo)
attendu <- c("Greffes de moelle, CAR-T Cells", "Transplantations", "Brûlés", "IVG", "IMG & fausses couches", "Accouchement normal mère", "Accouchement pathologique mère", "Accouchement normal mère",
             "Bébé normal", "Bébé néonat med", "Bébé néonat chir", "Autre néonat", "Séance polysomno", "Séance chimiothérapie simple adulte", "Séances simples", "Séances simples",
             "Médecine adultes > 3 nuits", "Médecine adultes < 3 nuits", "HDJ médecine adultes", "Chirurgie adultes > 3 nuits", "Chirurgie adultes < 3 nuits", "Interventionnel adultes > 3 nuits", "Interventionnel adultes < 3 nuits", "Autre")
ok("typologie : un cas par classe atteignable (libellés STREAM)", identical(r$DPEC, attendu))
ok("ordre : 15M10 -> Bébé néonat med malgré BB_CHIR ; 14Z13T -> accouchement normal ; 14Z13A (sévérité A) -> normal, pas pathologique",
   r$DPEC[10] == "Bébé néonat med" && r$DPEC[6] == "Accouchement normal mère" && r$DPEC[8] == "Accouchement normal mère" && r$DPEC[7] == "Accouchement pathologique mère")
ok("ordre : Z511 age 15 -> Séances simples (pas chimio adulte) ; CMD 28 avant M/Z", r$DPEC[15] == "Séances simples" && r$DPEC[13] == "Séance polysomno")
ok("ordre : 14Z10 (IMG_FC et ACC_PATHO) -> IMG & fausses couches (IMG_FC testé avant)", typologie_sejour(tibble::tibble(ghm2 = "14Z10B", diag2 = "O03", age = 30, duree = 2, mode_hospit = "HC"), typo)$DPEC == "IMG & fausses couches")
ok("TPEC via DPEC_TO_TPEC, défaut Autre", r$TPEC[1] == "Séjours complexes" && r$TPEC[6] == "Obstétrique" && r$TPEC[24] == "Autre" && all(r$TPEC[17:19] == "Médecine") && all(r$TPEC[20:23] == "Chirurgie et interventionnel") && r$TPEC[13] == "Médecine")
ok("âge en classe ge_18/lt_18 et durée constante (catalogue longs)",
   { c2 <- typologie_sejour(tibble::tibble(ghm2 = c("28Z07Z", "28Z07Z", "04M05B"), diag2 = "Z511", age = c("ge_18", "lt_18", "ge_18"), mode_hospit = "HC"), typo, duree_defaut = 3)
     identical(c2$DPEC, c("Séance chimiothérapie simple adulte", "Séances simples", "Médecine adultes > 3 nuits")) })
ok("chevauchement BB_MED/BB_CHIR présent dans le yaml (inoffensif, l'ordre prime)", all(c("15M10", "15M11", "15M13", "15M14") %in% typo$RACINES_BB_CHIR))

cat("\n# lire_catalogue / parts par lettre\n")
dcat <- file.path(tempdir(), "cat_ds"); unlink(dcat, recursive = TRUE); dir.create(dcat)
cat_fx2 <- tibble::tibble(diag2 = c("J449", "J440", "I500", "K802"), cage = c("[60-70[", "[60-70[", "[70-80[", "[1-5["), poids = c(5, 3, 8, 2), age = c("ge_18", "ge_18", "ge_18", "lt_18"),
                          type_unite = "HC", ghm2 = "04M053", mode_hospit = "HC", diagnostic_associes = "I10")
cat_fx2$lettre <- lettre_de(cat_fx2$diag2)
for(L in unique(cat_fx2$lettre)) arrow::write_parquet(cat_fx2[cat_fx2$lettre == L, ], file.path(dcat, nom_part_lettre(L)))
ok("lettres_catalogue", identical(lettres_catalogue(dcat), c("I", "J", "K")))
ok("lire_catalogue : parts filtrées par lettres et colonnes", { d <- lire_catalogue(dcat, lettres = c("J", "K"), colonnes = c("diag2", "poids")); setequal(d$diag2, c("J449", "J440", "K802")) && identical(names(d), c("diag2", "poids")) })
ok("lire_catalogue : toutes les parts == fixture", setequal(lire_catalogue(dcat)$diag2, cat_fx2$diag2) && nrow(lire_catalogue(dcat)) == 4)
ok("lire_catalogue : lettre absente -> NULL", is.null(lire_catalogue(dcat, lettres = "Z")))
mono <- file.path(tempdir(), "mono.parquet"); arrow::write_parquet(cat_fx2[, setdiff(names(cat_fx2), "lettre")], mono)
ok("lire_catalogue : monofichier déprécié (message) avec lettre ajoutée", { msg <- NULL; d <- withCallingHandlers(lire_catalogue(file.path(tempdir(), "inexistant"), mono, lettres = "I"), message = function(m){ msg <<- conditionMessage(m); invokeRestart("muffleMessage") }); grepl("déprécié", msg) && d$diag2 == "I500" })
ok("lire_catalogue : ni dataset ni monofichier -> stop nommant le magasin partagé et les étapes (etape_catalogue puis repartitionner)", { m <- tryCatch(lire_catalogue(file.path(tempdir(), "x", "catalogue_longs_seuil"), file.path(tempdir(), "x", "y.parquet")), error = function(e) conditionMessage(e)); grepl("magasin partagé", m) && grepl("etape_catalogue\\(\\)", m) && grepl("etape_repartitionner_catalogue", m) })
if(!ARROW_MOCK) ok("lire_catalogue : chemin dataset arrow (réel)", arrow_dataset_disponible() && nrow(lire_catalogue(dcat, lettres = "J")) == 2) else ok("lire_catalogue : dataset arrow non testé (mock, note)", TRUE)

cat("\n# sélection quota_dp_fixe\n")
pops <- list(pediatrie = c("[0-1[", "[1-5[", "[5-10[", "[10-15[", "[15-18["), adulte = c("[18-30[", "[30-40[", "[40-50[", "[50-60[", "[60-70[", "[70-80[", "[80-["))
ok("partition des cages exhaustive et exclusive", isTRUE(verifier_populations(pops, c("[1-5[", "[60-70["))))
ok("partition : cage absente -> stop", grepl("absentes", tryCatch(verifier_populations(pops, "[99-["), error = function(e) conditionMessage(e))))
ok("partition : cage en double -> stop", grepl("en double", tryCatch(verifier_populations(list(a = "[1-5[", b = "[1-5["), "[1-5["), error = function(e) conditionMessage(e))))
ok("population_de", identical(population_de(c("[1-5[", "[80-["), pops), c("pediatrie", "adulte")))
ok("budget au prorata du nb de DP, total exact", { b <- repartir_budget_populations(1000L, c(pediatrie = 30L, adulte = 70L)); identical(b, c(pediatrie = 300L, adulte = 700L)) && sum(repartir_budget_populations(1001L, c(a = 1L, b = 2L))) == 1001 })
ok("variantes_par_ligne : k=1 -> X ; k=3, X=10 -> 4/4/2 ; total exact", identical(variantes_par_ligne(1, 7L), 7L) && identical(variantes_par_ligne(3, 10L), c(4L, 4L, 2L)) && sum(variantes_par_ligne(4, 9L)) == 9)
cat_sel <- tibble::tibble(
  diag2 = c(rep("J449", 6), rep("I500", 2), "K802", "O800", "O800", "O800"),
  ghm2 = c(rep("04M053", 3), rep("04M052", 3), "05M093", "05M093", "06C041", "14Z13A", "14Z13B", "14Z13A"),
  type_unite = c("HC", "HC", "SC", "HC", "GERIATRIE", "HC", "HC", "UHCD", "HC", "HC", "HC", "SC"),
  poids = c(50, 40, 30, 20, 15, 12, 20, 20, 30, 300, 200, 100), cage = "[60-70[", age = "ge_18", mode_hospit = "HC",
  diagnostic_associes = c("I10", "E785", "N189", "I10 E785", "I48", "G20", "I10", "E785", "I10", "Z370", "Z370 O342", "Z371"))
cat_sel$lettre <- lettre_de(cat_sel$diag2)
cat_sel <- typologie_sejour(cat_sel, typo, duree_defaut = 3)
ok("fixture : 14Z13A -> Accouchement normal mère (plafonné), 14Z13B -> pathologique (non plafonné)", cat_sel$DPEC[10] == "Accouchement normal mère" && cat_sel$DPEC[11] == "Accouchement pathologique mère")
set.seed(1); r1 <- selection_quota_dp_fixe_lettre(cat_sel, X = 10L, k = 1L, plafonds_dpec = list("Accouchement normal mère" = 3L))
ok("k = 1 : une ligne par (DP × groupe), X_dp variantes ; total exact", all(r1$stats$k_eff == 1) && all(r1$stats$variantes == r1$stats$X_dp) && sum(r1$selection$n_var) == sum(r1$stats$X_dp))
ok("plafond par (DP × DPEC plafonné) : O800 normal -> X_dp = 3 ; O800 reste -> 10", { st <- r1$stats[r1$stats$dp == "O800", ]; st$X_dp[st$groupe == "Accouchement normal mère"] == 3 && st$X_dp[st$groupe == ".reste"] == 10 && st$plafonne[st$groupe == "Accouchement normal mère"] })
ok("sans remise : aucune ligne dupliquée hors variantes", !any(duplicated(r1$selection[, c("diag2", "ghm2", "type_unite", "diagnostic_associes")])))
ok("planchers d'unités désactivés à k = 1 (mention)", all(!r1$stats$planchers_actifs) && sum(!r1$stats$planchers_actifs & r1$stats$lignes_disponibles > 1) >= 1)
ok("manque à gagner = max(0, X_dp - lignes disponibles)", r1$stats$manque_a_gagner[r1$stats$dp == "K802"] == 9 && r1$stats$manque_a_gagner[r1$stats$dp == "J449"] == 4)
set.seed(2); r3 <- selection_quota_dp_fixe_lettre(cat_sel, X = 10L, k = 3L)
ok("k = 3 : J449 -> 3 lignes distinctes, variantes 4/4/2 (dernière tronquée) ; K802 (1 ligne) -> 1 ligne × 10", { sj <- r3$selection[r3$selection$diag2 == "J449", ]; sk <- r3$selection[r3$selection$diag2 == "K802", ]
   nrow(sj) == 3 && identical(sort(sj$n_var, decreasing = TRUE), c(4L, 4L, 2L)) && nrow(sk) == 1 && sk$n_var == 10 })
ok("k = 3 : planchers actifs pour J449 (3 types, k >= 3) : un type par ligne", { sj <- r3$selection[r3$selection$diag2 == "J449", ]; r3$stats$planchers_actifs[r3$stats$dp == "J449"] && length(unique(sj$type_unite)) == 3 })
ok("DP à moins de k lignes -> toutes ses lignes", nrow(r3$selection[r3$selection$diag2 == "I500", ]) == 2)
set.seed(1); r1b <- selection_quota_dp_fixe_lettre(cat_sel, X = 10L, k = 1L, plafonds_dpec = list("Accouchement normal mère" = 3L))
ok("déterminisme sous seed", identical(r1$selection, r1b$selection))
ok("seed stable par (population, lettre)", seed_selection(1, "adulte", "J", pops) == seed_selection(1, "adulte", "J", pops) && seed_selection(1, "adulte", "J", pops) != seed_selection(1, "pediatrie", "J", pops) && seed_selection(1, "adulte", "J", pops) != seed_selection(1, "adulte", "K", pops))
ok("dedoublonner_variantes : jeux identiques à l'ordre près éliminés", { d <- tibble::tibble(diagnostic_associes = c("I10 E785", "E785 I10", "I10 N189"), variante = 1:3); nrow(dedoublonner_variantes(d)) == 2 })

cat("\n# index des références et tirage indexé\n")
idx <- indexer_ref_das(ref_aigu_fx)
ok("indexer_ref_das : une entrée par strate", length(idx) == 2 && inherits(idx, "index_ref_das"))
set.seed(21); a <- purrr::map(1:30, ~ sample_das_long("HC", "1", "ge_18", "[60-70[", "04M05", "04M053", "E11i", "I10", "J449", 4, "I500 N189", ref_das_aigu = ref_aigu_fx, refs = refs_fx, nb_tirage = 2)) |> purrr::list_rbind()
set.seed(21); b <- purrr::map(1:30, ~ sample_das_long("HC", "1", "ge_18", "[60-70[", "04M05", "04M053", "E11i", "I10", "J449", 4, "I500 N189", ref_das_aigu = idx, refs = refs_fx, nb_tirage = 2)) |> purrr::list_rbind()
ok("sample_das_long indexé == filtré sous même seed (identité)", identical(a, b))
ok("indexé : strate absente -> NULL", is.null(sample_das_long("HP", "1", "ge_18", "[60-70[", "04M05", "04M053", "N", "N", "J449", 4, "I500", ref_das_aigu = idx, refs = refs_fx)))
idx_c <- indexer_ref_chronique(ref_chro_fx)
set.seed(22); a <- purrr::map(1:20, ~ sample_das_court("HC", "1", "[60-70[", "04M05", "J449", 1, ref_chro = ref_chro_fx, ref_nb_chro = ref_nb_fx, refs = refs_fx, nb_tirages = 2)) |> purrr::list_rbind()
set.seed(22); b <- purrr::map(1:20, ~ sample_das_court("HC", "1", "[60-70[", "04M05", "J449", 1, ref_chro = idx_c, ref_nb_chro = ref_nb_fx, refs = refs_fx, nb_tirages = 2)) |> purrr::list_rbind()
ok("sample_das_court indexé == filtré (strate et repli)", identical(a, b) && identical(candidats_chroniques("ZZZ", "2", "[60-70[", idx_c, 20)$source, "repli"))
# unicité souple
set.seed(23)
u_riche <- sample_das_long("HC", "1", "ge_18", "[60-70[", "04M05", "04M053", "N", "N", "J449", 4, "I500", ref_das_aigu = idx, refs = refs_fx, nb_tirage = 5, dedoublonner = TRUE)
ok("unicité souple : strate riche -> jeux distincts, colonne nb_variantes_demandees", nrow(u_riche) >= 4 && all(u_riche$nb_variantes_demandees == 5) && !any(duplicated(vapply(split_das(u_riche$diagnostic_associes), function(v) paste(sort(v), collapse = " "), character(1)))))
ref_pauvre <- ref_aigu_fx[ref_aigu_fx$sexe == "1", ][1:2, ]; ref_pauvre$das <- c("N10", "N11")
set.seed(24)
u_pauvre <- sample_das_long("HC", "1", "ge_18", "[60-70[", "04M05", "04M053", "N", "N", "J449", 1, "I500", ref_das_aigu = indexer_ref_das(ref_pauvre), refs = refs_fx, nb_tirage = 5, dedoublonner = TRUE)
ok("unicité souple : strate pauvre (2 jeux possibles, n_var = 5) -> <= 2 variantes, aucun re-tirage", nrow(u_pauvre) <= 2 && all(u_pauvre$nb_variantes_demandees == 5) && all(u_pauvre$variante <= 5))
set.seed(24); u2 <- sample_das_long("HC", "1", "ge_18", "[60-70[", "04M05", "04M053", "N", "N", "J449", 1, "I500", ref_das_aigu = indexer_ref_das(ref_pauvre), refs = refs_fx, nb_tirage = 5, dedoublonner = TRUE)
ok("unicité souple : déterminisme inchangé", identical(u_pauvre, u2))

cat("\n# pmap_chunks : plages, atomicité, lots\n")
f_var <- function(id, nb_tirage){ tibble::tibble(id = id, variante = seq_len(nb_tirage), u = round(stats::runif(nb_tirage), 6)) }
df_v <- tibble::tibble(id = 1:23, nb_tirage = c(rep(3L, 11), rep(2L, 12)))
dA <- file.path(tempdir(), "pl_A"); dB <- file.path(tempdir(), "pl_B"); unlink(c(dA, dB), recursive = TRUE)
rA <- pmap_chunks(df_v, f_var, chunk_size = 5, dossier = dA, prefixe = "p", seed_base = 9, ecrire = ecrire_t, lire = lire_t, ext = ext_t, verbose = FALSE)
ok("une ligne et ses variantes vivent dans le même chunk", all(vapply(1:5, function(i){ d <- lire_t(file.path(dA, sprintf("p_chunk_%04d%s", i, ext_t))); all(table(d$id) == df_v$nb_tirage[match(unique(d$id), df_v$id)]) }, logical(1))))
r_none <- pmap_chunks(df_v, f_var, chunk_size = 5, dossier = dB, prefixe = "p", seed_base = 9, ecrire = ecrire_t, lire = lire_t, ext = ext_t, verbose = FALSE, chunk_range = c(1, 2))
ok("plage 1..2 : 2 chunks écrits, sidecar écrit, pas d'assemblage", is.null(r_none) && length(list.files(dB, pattern = "^p_chunk_")) == 2 && file.exists(file.path(dB, "p_chunks_meta.yaml")))
pmap_chunks(df_v, f_var, chunk_size = 5, dossier = dB, prefixe = "p", seed_base = 9, ecrire = ecrire_t, lire = lire_t, ext = ext_t, verbose = FALSE, chunk_range = c(4, 5))
err <- tryCatch({ pmap_chunks(df_v, f_var, chunk_size = 5, dossier = dB, prefixe = "p", seed_base = 9, ecrire = ecrire_t, lire = lire_t, ext = ext_t, verbose = FALSE, chunk_range = c(1, 2), assembler = TRUE); NULL }, error = function(e) conditionMessage(e))
ok("assemblage demandé alors qu'un chunk hors plage manque -> stop listant le chunk", !is.null(err) && grepl("p_chunk_0003", err))
rB <- pmap_chunks(df_v, f_var, chunk_size = 5, dossier = dB, prefixe = "p", seed_base = 9, ecrire = ecrire_t, lire = lire_t, ext = ext_t, verbose = FALSE, chunk_range = c(3, 3), assembler = TRUE)
ok("plages disjointes couvrant tout == run complet bit à bit", identical(rA, rB))
ok("sidecar partagé non réécrit entre sessions (même contenu)", identical(yaml::read_yaml(file.path(dA, "p_chunks_meta.yaml"))[c("n", "chunk_size", "seed_base", "nb_chunks")], yaml::read_yaml(file.path(dB, "p_chunks_meta.yaml"))[c("n", "chunk_size", "seed_base", "nb_chunks")]))
unlink(file.path(dB, sprintf("p_chunk_%04d%s", 2, ext_t))); writeLines("partiel", file.path(dB, sprintf("p_chunk_%04d%s.tmp", 2, ext_t)))
rC <- pmap_chunks(df_v, f_var, chunk_size = 5, dossier = dB, prefixe = "p", seed_base = 9, ecrire = ecrire_t, lire = lire_t, ext = ext_t, verbose = FALSE)
ok("atomicité : .tmp orphelin ignoré et chunk recalculé, résultat identique", identical(rA, rC) && !file.exists(file.path(dB, sprintf("p_chunk_%04d%s.tmp", 2, ext_t))))
lots <- list(); lire_chunks_par_lots(dA, "p", 2, function(d, i) lots[[i]] <<- nrow(d), ext = ext_t, lire = lire_t)
ok("lire_chunks_par_lots : 5 chunks en lots de 2 -> 3 lots, total conservé", length(lots) == 3 && sum(unlist(lots)) == nrow(rA))

cat("\n# statistiques accumulées par lot et mémoire de session\n")
df_st <- dplyr::bind_rows(sc_st1 <- tibble::tibble(mode_hospit = "HC", sexe = "1", age = "ge_18", cage = c("[60-70[", "[60-70[", "[70-80["), racine = "04M05", ghm2 = "04M053", diabete = "N", hta = "N", diag2 = "J449", nbda = 2L, type_unite = "HC", prep_sc = 0,
                                                    poids = 11, graine = "I10", diabete_scenario = "N", diagnostic_associes = c("I10 E785", "I10 N189", "I10 E785 J440")))
ref_st <- stats_branche_test <- list(n = nrow(df_st), distribution = distribution_nb_das(df_st), top = top_das_par_cmd(df_st, 30), taux = taux_imprecis(df_st, c("J440")), e660 = effectifs_e660(df_st, c("diag2", "diagnostic_associes")))
acc <- acc_stats_init(); acc <- acc_stats_ajouter(acc, df_st[1:2, ], PIVOTS_LONGS, c("J440"), hta_autres_fx, 10, c("diag2", "graine", "diagnostic_associes")); acc <- acc_stats_ajouter(acc, df_st[3, ], PIVOTS_LONGS, c("J440"), hta_autres_fx, 10, c("diag2", "graine", "diagnostic_associes"))
fs <- acc_stats_final(acc)
ok("acc_stats par lots == stats globales (n, distribution, top, taux, contrôles)",
   fs$n == 3 && identical(as.data.frame(fs$distribution)[, c("cage", "n", "moy", "min", "max")], as.data.frame(ref_st$distribution)[, c("cage", "n", "moy", "min", "max")]) &&
     identical(as.data.frame(fs$top_das), as.data.frame(ref_st$top)) && fs$taux_imprecis == ref_st$taux && fs$controles$doublons_categorie == 0 && fs$pivots == 2)
ok("memoire_session : tableau trié décroissant", { m <- utils::capture.output(d <- memoire_session(list(g = environment()), n_max = 5)); is.data.frame(d) && all(diff(d$taille_mo) <= 0) && all(c("env", "objet", "classe", "taille_mo") %in% names(d)) })


# ============================================================ finitions exploitation / magasins partagés ==
cat("\n# gardes des magasins partagés (verifier_magasin, généralisation de Q13), message catalogue absent\n")
cfg <- valeurs_effectives_config()
ok("verifier_magasin : méta absent -> ok ; méta identique -> ok (références, catalogue, courts, partiels)",
   all(vapply(names(CLES_MAGASINS), function(m) verifier_magasin(m, NULL, cfg)$ok && verifier_magasin(m, meta_magasin(m, cfg), cfg)$ok, logical(1))))
v <- verifier_magasin("references", meta_magasin("references", cfg), modifyList(cfg, list(AN_REF = 25L, CONVERSION_E669 = FALSE)))
ok("verifier_magasin : clés en écart nommées (magasin / courant), drapeau FORCER et soupape de chemin dans le message",
   !v$ok && setequal(v$differences, c("AN_REF", "CONVERSION_E669")) && grepl("AN_REF : magasin = 26 ; courant = 25", v$message) && grepl("FORCER_REFS <- TRUE", v$message) &&
     grepl("sert TOUS les profils", v$message) && grepl("CHEMINS_SURCHARGES\\$references", v$message) && grepl("10_references/_meta.yaml", v$message))
ok("verifier_magasin catalogue : écart de périmètre -> issue (3) aligner ANS_HISTORIQUE ; vecteurs comparés valeur à valeur (3:100 == liste yaml)",
   { m <- meta_magasin("catalogue", cfg); f <- file.path(tempdir(), "m_cat.yaml"); yaml::write_yaml(m, f); rt <- yaml::read_yaml(f)
     verifier_magasin("catalogue", rt, cfg)$ok && { w <- verifier_magasin("catalogue", rt, modifyList(cfg, list(ANS_HISTORIQUE = 22:26))); !w$ok && grepl("aligner ANS_HISTORIQUE", w$message) } })
ok("meta_magasin : magasin, date, clés du magasin, champs libres", { m <- meta_magasin("courts", cfg, n_lignes = 12L); m$magasin == "courts" && !is.null(m$date) && all(CLES_MAGASINS$courts %in% names(m)) && m$n_lignes == 12L })
ok("message_catalogue_absent : magasin partagé nommé, etape_catalogue puis repartitionner", { m <- message_catalogue_absent("x", "/r/20_catalogue/"); grepl("magasin partagé /r/20_catalogue", m) && grepl("etape_catalogue\\(\\)", m) && grepl("etape_repartitionner_catalogue", m) })
ok("message_courts_absent : scénarios courts DE LA campagne, étape de campagne après la sélection, tirable et refs", { m <- message_courts_absent("/r/production/40_campagnes/C2/habille/courts/"); grepl("campagne absents \\(/r/production/40_campagnes/C2/habille/courts\\)", m) && grepl("etape_tirage_courts\\(\\) — étape DE CAMPAGNE", m) && grepl("etape_selection_longs", m) && grepl("30_courts/ref_pivots_courts", m) && grepl("etape_refs", m) })

cat("\n# réorganisation sur place : planifier_reorganisation (plan pur sur un inventaire ENCOMBRÉ)\n")
t_old <- as.POSIXct("2026-09-01 10:00:00"); t_new <- as.POSIXct("2026-09-18 10:00:00")
inv <- data.frame(chemin = c("partiels/catalogue_partiel_CHRU_17.parquet", "partiels/catalogue_partiel_CH_26.parquet", "partiels/partiels_meta.yaml",
                             "exports/ref_das_aigu.parquet", "exports/pivots_courts.parquet", "exports_diagnostic/pivots_courts.parquet", "exports/distribution_e660.parquet",
                             "exports/catalogue_longs_seuil/part_J.parquet", "exports/catalogue_longs_seuil/_sidecar.yaml", "exports/catalogue_longs_seuil_meta.yaml", "exports/catalogue_longs_seuil.parquet.ancien",
                             "exports_diagnostic/catalogue_longs_seuil.parquet", "exports_diagnostic/catalogue_longs_seuil_meta.yaml",
                             "exports/chunks/courts_chunk_0001.parquet", "exports/chunks/courts_chunks_meta.yaml", "exports/scenarios_courts_v8_20260901.parquet", "exports/scenarios_courts_v8_20260918.parquet",
                             "exports/diagnostic_apports.csv", "exports_diagnostic/diagnostic_apports.csv", "exports/diagnostic_memoire.csv", "exports_diagnostic/diagnostic_memoire.csv", "exports/recouvrement.csv",
                             "exports/registre_tirages/registre_C1.parquet", "exports_diagnostic/registre_tirages/registre_C0.parquet",
                             "exports/selection_longs/adulte/part_J.parquet", "exports/chunks/adulte/longs_chunk_0001.parquet", "exports/chunks/longs_chunk_0001.parquet", "exports/habille/adulte/lot_0001.parquet",
                             "exports/scenarios_longs_tirage_v8_C1/adulte/part_0001.parquet", "exports/scenarios_longs_tirage_v8_20260901.parquet", "exports/rapport_v8_20260901.txt", "exports/echantillon_revue.csv",
                             "exports/top30_das_par_cmd.csv", "exports/meta_tirage.yaml", "exports/selection_longs_effectifs.csv", "exports/chunks/longs_chunks_meta.yaml", "exports/x.parquet.tmp",
                             "notes.txt", "exports/bizarre.parquet"),
                  mtime = t_old, stringsAsFactors = FALSE)
inv$mtime[inv$chemin %in% c("exports/scenarios_courts_v8_20260918.parquet", "exports/pivots_courts.parquet", "exports/diagnostic_apports.csv")] <- t_new
plan <- planifier_reorganisation(inv, migrer_registre = FALSE)
dest <- function(src) plan$destination[plan$source == src]; cat_ <- function(src) plan$categorie[plan$source == src]
ok("plan : tout inventorié, rien de non listé (reconnus + ignorés + inconnus = inventaire)", nrow(plan) == nrow(inv) && all(plan$categorie %in% c("reconnu", "ignore", "inconnu")))
ok("plan : partiels -> 00_partiels/, méta convertie", dest("partiels/catalogue_partiel_CHRU_17.parquet") == "00_partiels/catalogue_partiel_CHRU_17.parquet" && dest("partiels/partiels_meta.yaml") == "00_partiels/_meta.yaml")
ok("plan : refs anciens noms -> ref_* dans 10_references/ ; doublon inter-profils : le plus récent retenu, l'autre ignoré (motif doublon)",
   dest("exports/ref_das_aigu.parquet") == "10_references/ref_das_aigu.parquet" && dest("exports/distribution_e660.parquet") == "10_references/ref_distribution_e660.parquet" &&
     dest("exports/pivots_courts.parquet") == "30_courts/ref_pivots_courts.parquet" && cat_("exports_diagnostic/pivots_courts.parquet") == "ignore" && grepl("doublon", plan$motif[plan$source == "exports_diagnostic/pivots_courts.parquet"]))
ok("plan : catalogue parts + sidecar -> 20_catalogue/catalogue_longs_seuil/ (_meta.yaml), méta -> 20_catalogue/, .ancien ignoré, monofichier diagnostic reconnu",
   dest("exports/catalogue_longs_seuil/part_J.parquet") == "20_catalogue/catalogue_longs_seuil/part_J.parquet" && dest("exports/catalogue_longs_seuil/_sidecar.yaml") == "20_catalogue/catalogue_longs_seuil/_meta.yaml" &&
     dest("exports/catalogue_longs_seuil_meta.yaml") == "20_catalogue/catalogue_longs_seuil_meta.yaml" && cat_("exports/catalogue_longs_seuil.parquet.ancien") == "ignore" &&
     dest("exports_diagnostic/catalogue_longs_seuil.parquet") == "20_catalogue/catalogue_longs_seuil.parquet" && cat_("exports_diagnostic/catalogue_longs_seuil_meta.yaml") == "ignore")
ok("plan : courts -> 30_courts/ (corpus historique dé-daté : le plus récent des deux fichiers datés retenu ; chunks courts de l'ancienne génération ignorés, motif explicite)",
   cat_("exports/chunks/courts_chunk_0001.parquet") == "ignore" && grepl("ancienne génération", plan$motif[plan$source == "exports/chunks/courts_chunk_0001.parquet"]) && cat_("exports/chunks/courts_chunks_meta.yaml") == "ignore" &&
     dest("exports/scenarios_courts_v8_20260918.parquet") == "30_courts/scenarios_courts.parquet" && grepl("HISTORIQUE", plan$motif[plan$source == "exports/scenarios_courts_v8_20260918.parquet"]) && cat_("exports/scenarios_courts_v8_20260901.parquet") == "ignore")
ok("plan : diagnostics -> 90_diagnostics/ (apports : plus récent retenu ; mémoire par profil)",
   dest("exports/diagnostic_apports.csv") == "90_diagnostics/diagnostic_apports.csv" && cat_("exports_diagnostic/diagnostic_apports.csv") == "ignore" && dest("exports/recouvrement.csv") == "90_diagnostics/recouvrement.csv" &&
     dest("exports/diagnostic_memoire.csv") == "90_diagnostics/diagnostic_memoire_production.csv" && dest("exports_diagnostic/diagnostic_memoire.csv") == "90_diagnostics/diagnostic_memoire_diagnostic.csv")
ok("plan : registre ignoré sans migrer_registre (motif explicite), registre diagnostic toujours ignoré", cat_("exports/registre_tirages/registre_C1.parquet") == "ignore" && grepl("migrer_registre = FALSE", plan$motif[plan$source == "exports/registre_tirages/registre_C1.parquet"]) && cat_("exports_diagnostic/registre_tirages/registre_C0.parquet") == "ignore")
plan2 <- planifier_reorganisation(inv, migrer_registre = TRUE)
ok("plan : migrer_registre = TRUE -> production/50_registre/registre_tirages/", plan2$destination[plan2$source == "exports/registre_tirages/registre_C1.parquet"] == "production/50_registre/registre_tirages/registre_C1.parquet")
ok("plan : transitoires et ancienne génération ignorés volontairement (sélection, chunks longs, habillé, corpus, rapports, annexes, .tmp)",
   all(vapply(c("exports/selection_longs/adulte/part_J.parquet", "exports/chunks/adulte/longs_chunk_0001.parquet", "exports/chunks/longs_chunk_0001.parquet", "exports/habille/adulte/lot_0001.parquet",
                "exports/scenarios_longs_tirage_v8_C1/adulte/part_0001.parquet", "exports/scenarios_longs_tirage_v8_20260901.parquet", "exports/rapport_v8_20260901.txt", "exports/echantillon_revue.csv",
                "exports/top30_das_par_cmd.csv", "exports/meta_tirage.yaml", "exports/selection_longs_effectifs.csv", "exports/chunks/longs_chunks_meta.yaml", "exports/x.parquet.tmp"), cat_, character(1)) == "ignore"))
ok("plan : inconnus listés pour arbitrage, jamais de destination", cat_("notes.txt") == "inconnu" && cat_("exports/bizarre.parquet") == "inconnu" && all(is.na(plan$destination[plan$categorie == "inconnu"])) && sum(plan$categorie == "inconnu") == 2)
ok("imprimer_plan_reorganisation : trois tables", { o <- utils::capture.output(imprimer_plan_reorganisation(plan)); any(grepl("1. RECONNUS", o)) && any(grepl("2. IGNORÉS", o)) && any(grepl("3. NON RECONNUS", o)) && any(grepl("notes.txt", o)) })

# ============================================================ campagnes ==
cat("\n# identifiants stables (recette id_v1 figée)\n")
ligne_id <- tibble::tibble(mode_hospit = "HC", sexe = "1", age = "ge_18", cage = "[60-70[", racine = "04M05", ghm2 = "04M053", diabete = "N", hta = "N",
                           diag2 = "J449", nbda = 2L, type_unite = "HC", prep_sc = 0, diagnostic_associes = "I10 N189")
ok("recette figée : valeur attendue EN DUR (détecte tout changement involontaire)", id_profil_de(ligne_id) == "095c6d0dfbcf46f4" && RECETTE_ID == "id_v1" && identical(COLONNES_RECETTE_ID, c(PIVOTS_LONGS, "diagnostic_associes")))
ok("16 caractères hex", nchar(id_profil_de(ligne_id)) == 16 && grepl("^[0-9a-f]{16}$", id_profil_de(ligne_id)))
df_ids <- dplyr::bind_rows(ligne_id, dplyr::mutate(ligne_id, diag2 = "I500"), dplyr::mutate(ligne_id, diagnostic_associes = "I10 N189 E785"), dplyr::mutate(ligne_id, nbda = NA))
ok("déterminisme : ordres de lignes différents -> mêmes id", identical(sort(id_profil_de(df_ids)), sort(id_profil_de(df_ids[c(3, 1, 4, 2), ]))))
ok("sensibilité : pivot ou graine changés -> id différents", length(unique(id_profil_de(df_ids))) == 4)
ok("NA normalisé en \"\" ; types numériques (0 / 0L, 2 / 2L) équivalents", id_profil_de(dplyr::mutate(ligne_id, nbda = NA)) == id_profil_de(dplyr::mutate(ligne_id, nbda = NA_character_)) &&
     id_profil_de(dplyr::mutate(ligne_id, prep_sc = 0L, nbda = 2)) == id_profil_de(ligne_id))
ok("colonne manquante -> stop", grepl("colonnes manquantes", tryCatch(id_profil_de(ligne_id[, -1]), error = function(e) conditionMessage(e))))
ok("id_scenario et hash_das (ordre des DAS indifférent)", id_scenario_de("abc", 7) == "abc-007" && hash_das_de("I10 N189") == hash_das_de("N189 I10") && hash_das_de("I10 N189") != hash_das_de("I10") && nchar(hash_das_de("I10")) == 16)
ok("seed_campagne : stable et distinct par campagne", seed_campagne(1, "C1") == seed_campagne(1, "C1") && seed_campagne(1, "C1") != seed_campagne(1, "C2"))

cat("\n# plafonds de classe DPEC (amendement Q33)\n")
cl5 <- tibble::tibble(diag2 = rep(c("O800", "O801", "O802", "O803", "O804"), each = 2), poids = c(50, 50, 30, 10, 10, 10, 5, 5, 3, 2))
a3 <- allocation_classe_plafonnee(cl5, 3L)
ok("classe 5 DP, plafond 3 -> 5 lignes (1 par DP, le représentant prime), dépassement 2", a3$nb_dp == 5 && a3$total == 5 && a3$depassement == 2 && all(a3$quotas$quota == 1))
a12 <- allocation_classe_plafonnee(cl5, 12L)
ok("plafond 12 -> 5 + 7 au poids (plus forts restes), total exact, O800 le mieux servi", a12$total == 12 && a12$depassement == 0 && a12$quotas$quota[a12$quotas$diag2 == "O800"] == max(a12$quotas$quota) && all(a12$quotas$quota >= 1))
ok("classe vide -> 0", allocation_classe_plafonnee(cl5[0, ], 3L)$total == 0)
cat_cl <- tibble::tibble(diag2 = c("O800", "O800", "O800", "O801", "J449", "J449"), ghm2 = c("14Z13A", "14Z13A", "14Z13B", "14Z13A", "04M053", "04M052"),
                         DPEC = c("Accouchement normal mère", "Accouchement normal mère", "Accouchement pathologique mère", "Accouchement normal mère", "Médecine adultes > 3 nuits", "Médecine adultes > 3 nuits"),
                         type_unite = "HC", poids = c(10, 5, 8, 3, 6, 4), cage = "[30-40[", age = "ge_18", mode_hospit = "HC", diagnostic_associes = c("Z370", "Z371", "Z370 O342", "Z370", "I10", "E785"))
cat_cl$id_profil <- paste0("p", seq_len(nrow(cat_cl)))
qc <- dplyr::mutate(allocation_classe_plafonnee(cat_cl[cat_cl$DPEC == "Accouchement normal mère", ], 5L)$quotas, DPEC = "Accouchement normal mère")
set.seed(3); rc <- selection_campagne_lettre(cat_cl, X = 10L, k = 1L, quotas_classe = qc)
ok("DP multi-DPEC : lignes de la classe plafonnée suivent le quota de classe (1 ligne, variantes = quota), autres lignes du DP à X",
   { st <- rc$stats; st$X_dp[st$dp == "O800" & st$groupe == "Accouchement normal mère"] == qc$quota[qc$diag2 == "O800"] && st$X_dp[st$dp == "O800" & st$groupe == ".reste"] == 10 &&
     st$k_eff[st$dp == "O800" & st$groupe == "Accouchement normal mère"] == 1 && sum(rc$selection$n_var[rc$selection$DPEC == "Accouchement normal mère"]) == 5 })
ok("colonnes campagne présentes, origine vierge sans registre", all(c("origine_profil", "variante_debut", "hash_exclus", "n_var") %in% names(rc$selection)) && all(rc$selection$origine_profil == "vierge") && all(rc$selection$variante_debut == 1L))

cat("\n# registre des tirages (append-only) et sélection sous registre\n")
dreg <- file.path(tempdir(), "registre_t"); unlink(dreg, recursive = TRUE)
reg1 <- tibble::tibble(id_profil = c("p1", "p1", "p2"), variante = c(1L, 2L, 1L), id_scenario = c("p1-001", "p1-002", "p2-001"), hash_das = c("h1", "h2", "h3"),
                       campagne = "C1", population = "adulte", diag2 = c("O800", "O800", "O800"), DPEC = "Accouchement normal mère", date = "2026-09-16")
f1 <- ecrire_registre_campagne(reg1, "C1", dreg)
ok("registre écrit ; réécriture identique -> idempotent", file.exists(f1) && identical(ecrire_registre_campagne(reg1[c(3, 1, 2), ], "C1", dreg), f1))
ok("réécriture divergente -> stop (append-only)", grepl("append-only", tryCatch(ecrire_registre_campagne(dplyr::mutate(reg1, hash_das = "x"), "C1", dreg), error = function(e) conditionMessage(e))))
ecrire_registre_campagne(dplyr::mutate(reg1[1, ], variante = 3L, id_scenario = "p1-003", hash_das = "h4", campagne = "C2"), "C2", dreg)
lr <- lire_registre(dreg)
ok("lire_registre : agrégats (variante_max, nb, hash_das, consommations par diag2 / DPEC / campagne) ; registres SANS colonne branche relus avec branche = long implicite (par_branche, par_campagne nb_longs / nb_courts)",
   lr$nb_campagnes == 2 && lr$nb_scenarios == 4 && lr$par_profil$variante_max[lr$par_profil$id_profil == "p1"] == 3 && setequal(lr$par_profil$hash_das[[which(lr$par_profil$id_profil == "p1")]], c("h1", "h2", "h4")) &&
     lr$par_diag2$nb_scenarios == 4 && lr$par_dpec$nb_profils == 2 && identical(sort(lr$par_campagne$campagne), c("C1", "C2")) &&
     all(lr$lignes$branche == "long") && lr$par_branche$branche == "long" && lr$par_branche$nb_scenarios == 4 && all(lr$par_campagne$nb_courts == 0) && sum(lr$par_campagne$nb_longs) == 4 &&
     { d0 <- file.path(tempdir(), "registre_ancien"); unlink(d0, recursive = TRUE); dir.create(d0); arrow::write_parquet(reg1, file.path(d0, "registre_C0.parquet"))   # fichier ANCIEN, sans colonne branche
       !"branche" %in% names(arrow::read_parquet(file.path(d0, "registre_C0.parquet"))) && all(lire_registre(d0)$lignes$branche == "long") && lire_registre(d0)$par_campagne$nb_longs == 3 })
ok("registre vide -> tables vides", { v <- lire_registre(file.path(tempdir(), "registre_vide")); v$nb_campagnes == 0 && nrow(v$par_profil) == 0 })
set.seed(4)
ch <- choisir_lignes_dp_registre(cat_cl[cat_cl$diag2 == "O800", ], k = 1L, lr$par_profil)
ok("fraîcheur d'abord : ligne vierge choisie tant qu'il en reste (p3 seule vierge)", ch$lignes$origine_profil == "vierge" && ch$lignes$id_profil == "p3" && ch$nb_vierges == 1 && ch$nb_recycles == 0)
d_epuise <- cat_cl[cat_cl$id_profil %in% c("p1", "p2"), ]
ch2 <- choisir_lignes_dp_registre(d_epuise, k = 3L, lr$par_profil)
ok("DP épuisé : recyclage, variantes numérotées après variante_max, hash_das déjà enregistrés exclus",
   all(ch2$lignes$origine_profil == "recycle") && ch2$lignes$variante_debut[ch2$lignes$id_profil == "p1"] == 4L && ch2$lignes$variante_debut[ch2$lignes$id_profil == "p2"] == 2L &&
     setequal(strsplit(ch2$lignes$hash_exclus[ch2$lignes$id_profil == "p1"], " ")[[1]], c("h1", "h2", "h4")) && ch2$nb_recycles == 2)
set.seed(5); s1 <- selection_campagne_lettre(cat_cl, 10L, 1L, NULL, NULL)
set.seed(5); s0 <- selection_quota_dp_fixe_lettre(cat_cl, 10L, 1L, list())
ok("REGISTRE_ACTIF = FALSE (registre NULL, sans classe) == comportement antérieur sans plafond", identical(as.data.frame(s1$selection[, names(s0$selection)]), as.data.frame(s0$selection)))
set.seed(6); s2a <- selection_campagne_lettre(cat_cl, 10L, 1L, qc, lr$par_profil); set.seed(6); s2b <- selection_campagne_lettre(cat_cl, 10L, 1L, qc, lr$par_profil)
ok("déterminisme sous seed avec registre", identical(s2a$selection, s2b$selection) && any(s2a$selection$origine_profil == "recycle"))

cat("\n# tirage avec identifiants : variante_debut, exclusion des hash déjà enregistrés\n")
set.seed(7); t1 <- sample_das_long("HC", "1", "ge_18", "[60-70[", "04M05", "04M053", "N", "N", "J449", 4, "I500", ref_das_aigu = idx, refs = refs_fx, nb_tirage = 3, dedoublonner = TRUE, id_profil = "abcd", variante_debut = 5L)
ok("colonnes id_profil / id_scenario / hash_das, variantes 5..7", all(c("id_profil", "id_scenario", "hash_das") %in% names(t1)) && all(t1$variante >= 5 & t1$variante <= 7) && all(t1$id_scenario == paste0("abcd-", sprintf("%03d", t1$variante))) && all(t1$hash_das == hash_das_de(t1$diagnostic_associes)))
set.seed(7); t2 <- sample_das_long("HC", "1", "ge_18", "[60-70[", "04M05", "04M053", "N", "N", "J449", 4, "I500", ref_das_aigu = idx, refs = refs_fx, nb_tirage = 3, dedoublonner = TRUE, id_profil = "abcd", variante_debut = 5L, hash_exclus = paste(t1$hash_das[1], "zzz"))
ok("hash déjà enregistré (collision volontaire) -> variante éliminée sans re-tirage", nrow(t2) == nrow(t1) - 1 && !t1$hash_das[1] %in% t2$hash_das && all(t2$hash_das %in% t1$hash_das))
set.seed(7); t0 <- sample_das_long("HC", "1", "ge_18", "[60-70[", "04M05", "04M053", "N", "N", "J449", 4, "I500", ref_das_aigu = idx, refs = refs_fx, nb_tirage = 3, dedoublonner = TRUE)
ok("sans id_profil : aucune colonne d'identifiant (schéma antérieur conservé)", !any(c("id_profil", "id_scenario", "hash_das") %in% names(t0)) && identical(t0$diagnostic_associes, t1$diagnostic_associes))
ok("registre_depuis_chunks : id_profil recalculé sur pivots + graine, hash sur DAS tirés, DPEC par profil",
   { ch <- t1; for(cc in PIVOTS_LONGS) if(!cc %in% names(ch)) ch[[cc]] <- "x"
     r <- registre_depuis_chunks(ch, "C9", "adulte", dpec_par_profil = stats::setNames("DPEC test", id_profil_de(dplyr::mutate(ch, diagnostic_associes = graine))[1]))
     nrow(r) == nrow(t1) && all(r$campagne == "C9") && all(r$DPEC == "DPEC test") && all(r$hash_das == t1$hash_das) && identical(names(r), COLONNES_REGISTRE) && all(r$branche == "long") })


# ================================== lot « notebook campagnes » : identifiants courts, section H ==
cat("\n# identifiants des séjours courts (recette id_courts_v1 figée)\n")
ligne_c <- tibble::tibble(mode_hospit = "HC", sexe = "1", cage = "[60-70[", ghm2 = "04M053", diag2 = "J449", duree = 2L)
ok("recette courts figée : valeur attendue EN DUR, préfixe k + 15 hex (k hors alphabet hexadécimal)", id_profil_courts_de(ligne_c) == "ke3839ff42c0f1ca" && RECETTE_ID_COURTS == "id_courts_v1" &&
     identical(COLONNES_RECETTE_ID_COURTS, PIVOTS_COURTS) && grepl("^k[0-9a-f]{15}$", id_profil_courts_de(ligne_c)) && nchar(id_profil_courts_de(ligne_c)) == 16)
df_c4 <- dplyr::bind_rows(ligne_c, dplyr::mutate(ligne_c, duree = 1L), dplyr::mutate(ligne_c, sexe = "2"), dplyr::mutate(ligne_c, diag2 = "I10"))
ok("courts : unicité et déterminisme (ordre des lignes indifférent, duree 2 / 2L équivalents)", length(unique(id_profil_courts_de(df_c4))) == 4 &&
     identical(sort(id_profil_courts_de(df_c4)), sort(id_profil_courts_de(df_c4[4:1, ]))) && id_profil_courts_de(dplyr::mutate(ligne_c, duree = 2)) == id_profil_courts_de(ligne_c))
ok("courts : domaine distinct des longs — tout id court commence par k, aucun id long ne le peut (alphabet hexadécimal strict) ; colonne manquante -> stop",
   all(grepl("^k", id_profil_courts_de(df_c4))) && all(grepl("^[0-9a-f]{16}$", id_profil_de(df_ids))) && !grepl("^k", id_profil_de(ligne_id)) &&
     grepl("colonnes manquantes", tryCatch(id_profil_courts_de(ligne_c[, -1]), error = function(e) conditionMessage(e))) && id_scenario_de(id_profil_courts_de(ligne_c), 2) == "ke3839ff42c0f1ca-002")

cat("\n# section H : fichiers datés, dossier final par campagne, statut au registre, message courts absent\n")
# (resoudre_export_date retirée : règle « nom stable, date dans le méta » — section 22 du journal)
ok("aucune résolution de fichiers datés ni DATE_TAG dans le code", !exists("resoudre_export_date") && !exists("chemin_export_lecture") && !exists("DATE_TAG") && !any(grepl("DATE_TAG|chemin_export", readLines(file.path(racine, "etapes.R")))))
ok("verifier_dossier_final : absent -> creer ; sans méta -> reprise ; même campagne -> reprise ; autre campagne -> stop",
   verifier_dossier_final(NULL, "C1", "/d")$action == "creer" && verifier_dossier_final(list(), "C1", "/d")$action == "reprise" &&
     verifier_dossier_final(list(campagne = "C1", date = "20260919"), "C1", "/d")$action == "reprise" &&
     { v <- verifier_dossier_final(list(campagne = "C1", date = "20260919"), "C2", "/d"); v$action == "stop" && grepl("AUTRE campagne \\(C1, du 20260919\\)", v$message) && grepl("Rien n'est écrasé", v$message) })
reg_fx <- list(lignes = tibble::tibble(campagne = c("C1", "C1", "C2"), date = c("2026-09-01", "2026-09-02", "2026-09-03")))
ok("statut_campagne_registre : inscrite (nb, dernière date) / jamais inscrite / registre vide",
   { s1 <- statut_campagne_registre("C1", reg_fx); s3 <- statut_campagne_registre("C3", reg_fx); s0 <- statut_campagne_registre("C1", NULL)
     s1$inscrite && s1$nb == 2 && grepl("2 scénarios \\(2 longs, 0 courts\\) le 2026-09-02", s1$texte) && grepl("changez d'identifiant", s1$texte) && !s3$inscrite && s3$texte == "jamais inscrite au registre" && !s0$inscrite })


# ============================ lot « correctifs post-contrôle » : lecteurs à repli, lecture robuste, extrapolation, gardes notebooks ==
cat("\n# lecteurs à repli (lire_corpus_final : monofichier ET parts), lecture robuste (lire_si_present, dernier_fichier)\n")
dex <- file.path(tempdir(), "export_final"); unlink(dex, recursive = TRUE); dir.create(dex, recursive = TRUE)
liv <- tibble::tibble(branche = c("court", "long", "long", "long"), population = c(NA, "adulte", "adulte", "pediatrie"), DPEC = c("z", "a", "b", "c"), x = 1:4)
arrow::write_parquet(liv, file.path(dex, "scenarios_C9.parquet"))
dir.create(file.path(dex, "scenarios_C8")); arrow::write_parquet(liv[1:2, ], file.path(dex, "scenarios_C8", "part_0001.parquet")); arrow::write_parquet(liv[3:4, ], file.path(dex, "scenarios_C8", "part_0002.parquet"))
cf <- lire_corpus_final("C9", dir_export = dex); cp <- lire_corpus_final("C8", dir_export = dex)
ok("lire_corpus_final : monofichier et parts lus à l'identique ; branche, population, colonnes ; NULL si absent ; chemin " %+% if(ARROW_MOCK) "rbind (mock)" else "dataset arrow",
   nrow(cf) == 4 && identical(as.data.frame(cf), as.data.frame(cp)) && nrow(lire_corpus_final("C9", branche = "long", dir_export = dex)) == 3 && nrow(lire_corpus_final("C9", branche = "court", dir_export = dex)) == 1 &&
     nrow(lire_corpus_final("C8", populations = "pediatrie", branche = "long", dir_export = dex)) == 1 && identical(names(lire_corpus_final("C9", colonnes = c("population", "DPEC"), dir_export = dex)), c("population", "DPEC")) &&
     is.null(lire_corpus_final("C7", dir_export = dex)) && (if(ARROW_MOCK) !arrow_dataset_disponible() else arrow_dataset_disponible()))
cat("\n# livrable unique : union de schémas, familles de colonnes, échantillon de revue\n")
dc <- tibble::tibble(branche = "court", mode_hospit = "HC", sexe = "1", cage = "[60-70[", ghm2 = "04M053", diag2 = "J449", duree = 2L, variante = 1:3, age = 65L, diagnostic_associes = "I10", id_scenario = paste0("c", 1:3), mode_entree = "8")
dl_ <- tibble::tibble(branche = "long", mode_hospit = "HC", sexe = "2", age = "ge_18", cage = "[70-80[", racine = "04M05", ghm2 = "04M053", diag2 = "J449", nbda = 3L, graine = "I10 E785", diagnostic_associes = "I10 E785 N189", poids = 12, id_scenario = paste0("l", 1:2), population = "adulte", DPEC = "d", TPEC = "Médecine", mode_entree = "8")
mod <- modele_schema(dc, dl_); ordre <- unlist(familles_colonnes(names(mod)), use.names = FALSE)
ok("modele_schema : union des colonnes, branche en tête ; types conservés (age integer chez les courts vs character chez les longs -> character)",
   names(mod)[1] == "branche" && setequal(names(mod), union(names(dc), names(dl_))) && is.character(mod$age) && is.integer(mod$duree) && is.numeric(mod$poids))
hc <- harmoniser(dc, mod[, ordre]); hl <- harmoniser(dl_, mod[, ordre])
ok("harmoniser : mêmes colonnes dans le même ordre, NA typés croisés (poids NA chez les courts, duree NA chez les longs), lignes conservées",
   identical(names(hc), names(hl)) && nrow(hc) == 3 && nrow(hl) == 2 && all(is.na(hc$poids)) && all(is.na(hl$duree)) && all(hl$population == "adulte") && all(is.na(hc$population)) && identical(hc$id_scenario, paste0("c", 1:3)))
fam <- familles_colonnes(names(mod))
ok("familles_colonnes : chaque colonne dans une famille, ordre des familles, 'autres' pour l'inconnu", setequal(unlist(fam), names(mod)) && names(fam)[1] == "identite_livrable" && "typologie" %in% names(fam) && !is.null(familles_colonnes(c("branche", "zzz"))$autres))
u <- dplyr::bind_rows(hc, hl)
ok("echantillonner_livrable : n × part courts, reste longs, déterministe sous seed", { e1 <- echantillonner_livrable(u, 4, 0.5, 7); e2 <- echantillonner_livrable(u, 4, 0.5, 7); sum(e1$branche == "court") == 2 && sum(e1$branche == "long") == 2 && identical(e1, e2) })
dl <- file.path(tempdir(), "lsp"); unlink(dl, recursive = TRUE); dir.create(dl)
writeLines(c("a;b", "1;2"), file.path(dl, "x2.csv")); writeLines(c("a,b", "1,2"), file.path(dl, "x1.csv")); writeLines(c("l1", "l2"), file.path(dl, "rapport_v8_20260918.txt"))
writeLines("l3", file.path(dl, "rapport_v8_20260919.txt")); yaml::write_yaml(list(k = 1), file.path(dl, "m.yaml"))
msg <- utils::capture.output(r0 <- lire_si_present(file.path(dl, "absent.csv"), "etape_x()"))
ok("lire_si_present : absent -> message actionnable (fichier, étape, etat_pipeline), NULL, aucune erreur", is.null(r0) && any(grepl("absent.csv absent — produit par etape_x\\(\\), pas encore exécutée", msg)) && any(grepl("etat_pipeline", msg)))
msg2 <- utils::capture.output(r1 <- lire_si_present(NA_character_, "etape_finalisation()", nom = "rapport_v8_<date>.txt"))
ok("lire_si_present : motif sans fichier (NA) -> message avec le nom donné", is.null(r1) && any(grepl("^rapport_v8_<date>.txt absent", msg2)))
ok("lire_si_present : csv2 détecté, csv, lignes, yaml, chemin", identical(lire_si_present(file.path(dl, "x2.csv"))$b, 2L) && identical(lire_si_present(file.path(dl, "x1.csv"))$b, 2L) &&
     identical(lire_si_present(file.path(dl, "rapport_v8_20260918.txt")), c("l1", "l2")) && lire_si_present(file.path(dl, "m.yaml"))$k == 1 && lire_si_present(file.path(dl, "m.yaml"), mode = "chemin") == file.path(dl, "m.yaml"))
ok("dernier_fichier : le plus récent au motif, NA si aucun ou dossier absent", basename(dernier_fichier(dl, "^rapport_v8_[0-9]{8}\\.txt$")) == "rapport_v8_20260919.txt" && is.na(dernier_fichier(dl, "^zz")) && is.na(dernier_fichier(file.path(dl, "nope"), ".")))
dx <- file.path(tempdir(), "chunks_extrap"); unlink(dx, recursive = TRUE)
lg <- utils::capture.output(r <- pmap_chunks(tibble::tibble(v = 1:25), function(v) tibble::tibble(v = v), chunk_size = 1, dossier = dx, prefixe = "t", seed_base = 1))
ok("pmap_chunks : débit par chunk et extrapolation tous les 10 chunks (25 chunks -> 2 bannières, restant 15 puis 5)",
   sum(grepl("— débit", lg)) == 25 && sum(grepl("restant dans la plage", lg)) == 2 && any(grepl("restant dans la plage : 15 ", lg)) && any(grepl("restant dans la plage : 5 ", lg)) && nrow(r) == 25)
rmd <- lapply(c("RUN.Rmd", "RUN_aval.Rmd"), function(f) readLines(file.path(racine, f), warn = FALSE))
ok("notebooks : aucun appel direct à un dataset arrow (open_dataset / write_dataset) — lecteurs à repli seulement", !any(grepl("arrow::open_dataset|arrow::write_dataset|open_dataset\\(", unlist(rmd))))
ok("notebooks : le chunk palier_surcharge passe par ecrire_surcharge_palier (REGISTRE_ACTIF <- FALSE imposé par contenu_surcharge_palier) ; chunk ouvrir_campagne avec paramètres en clair et ecrire_surcharge_campagne",
   { l <- rmd[[2]]; i <- grep("^```\\{r palier_surcharge", l); j <- i + which(grepl("^```\\s*$", l[(i + 1):length(l)]))[1]
     i2 <- grep("^```\\{r ouvrir_campagne", l); j2 <- i2 + which(grepl("^```\\s*$", l[(i2 + 1):length(l)]))[1]
     any(grepl("ecrire_surcharge_palier\\(", l[i:j])) && !any(grepl("writeLines", l[i:j])) && length(i2) == 1 && i2 < i &&
       any(grepl("^CAMPAGNE_A_OUVRIR\\s*<-\\s*\"", l[i2:j2])) && any(grepl("^NB_CRH_CIBLE_CAMP\\s*<-\\s*[0-9]+L", l[i2:j2])) && any(grepl("ecrire_surcharge_campagne\\(", l[i2:j2])) && !any(grepl("^CAMPAGNE\\s*<-", l)) })


# ============================ lot « trois niveaux de paramètres » : surcharges campagne / palier, sources, frontière de config.R ==
cat("\n# trois niveaux de paramètres : contenu de campagne.R, exclusivité palier / campagne, sources des paramètres, frontière de config.R\n")
lc <- contenu_surcharge_campagne("C2", 500000, 1L, TRUE)
ok("contenu_surcharge_campagne : marqueur, CAMPAGNE, entiers avec L, REGISTRE_ACTIF ; plafonds optionnels ; identifiant et entiers validés",
   any(lc == MARQUEUR_CAMPAGNE) && 'CAMPAGNE <- "C2"' %in% lc && "NB_CRH_CIBLE <- 500000L" %in% lc && "NB_LIGNES_PAR_DP <- 1L" %in% lc && "REGISTRE_ACTIF <- TRUE" %in% lc &&
     any(grepl("^PLAFONDS_DPEC <- list", contenu_surcharge_campagne("C3", 10L, 1L, FALSE, list(a = 2L)))) && !any(grepl("PLAFONDS", lc)) &&
     grepl("identifiant court", tryCatch(contenu_surcharge_campagne("C 2", 10L), error = function(e) conditionMessage(e))) && grepl("entier", tryCatch(contenu_surcharge_campagne("C2", 1.5), error = function(e) conditionMessage(e))) &&
     { f <- file.path(tempdir(), "campagne_test.R"); writeLines(lc, f); e <- new.env(); sys.source(f, e); e$CAMPAGNE == "C2" && is.integer(e$NB_CRH_CIBLE) && e$NB_CRH_CIBLE == 500000L && isTRUE(e$SURCHARGE_CAMPAGNE_ACTIVE) })
lp <- contenu_surcharge_palier(100000L)
ok("contenu_surcharge_palier : budget, marqueur PALIER_ACTIF, REGISTRE_ACTIF <- FALSE imposé", "NB_CRH_CIBLE <- 100000L" %in% lp && any(lp == MARQUEUR_PALIER) && any(grepl("^REGISTRE_ACTIF <- FALSE", lp)))
ok("type_surcharge : palier / campagne / autre (démo) / aucune", type_surcharge(lp) == "palier" && type_surcharge(lc) == "campagne" && type_surcharge(c("PATH_RESULTS <- '/x/'", "CAMPAGNE <- 'DEMO'")) == "autre" && type_surcharge(NULL) == "aucune" && type_surcharge(character(0)) == "aucune" && type_surcharge("# PALIER_ACTIF <- TRUE (commentaire)") == "autre")
ok("exclusivité : campagne refusée sous palier actif (message : retirer le palier, chunk vider_palier) ; palier refusé sous campagne active ; même type, démo ou aucune -> ok",
   { v1 <- verifier_exclusivite_surcharges("campagne", "/p/palier.R", lp); v2 <- verifier_exclusivite_surcharges("palier", "/p/campagne.R", lc)
     !v1$ok && grepl("PALIER \\(/p/palier.R\\)", v1$message) && grepl("vider_palier", v1$message) && !v2$ok && grepl("CAMPAGNE \\(/p/campagne.R\\)", v2$message) && grepl("Sys.setenv", v2$message) &&
       verifier_exclusivite_surcharges("campagne", "/p/campagne.R", lc)$ok && verifier_exclusivite_surcharges("palier", "/d/surcharge_demo.R", c("PATH_RESULTS <- '/x/'"))$ok && verifier_exclusivite_surcharges("campagne", "", NULL)$ok })
ok("sources_parametres : défaut config / surcharge campagne (campagne.R) / surcharge palier (palier.R), paramètre non défini par la surcharge = défaut",
   { s0 <- sources_parametres(PARAMETRES_CAMPAGNE, NULL, ""); s1 <- sources_parametres(PARAMETRES_CAMPAGNE, lc, "/p/campagne.R"); s2 <- sources_parametres(PARAMETRES_CAMPAGNE, lp, "/p/palier.R")
     all(s0 == "défaut config") && s1[["CAMPAGNE"]] == "surcharge campagne (campagne.R)" && s1[["NB_CRH_CIBLE"]] == "surcharge campagne (campagne.R)" && s1[["PLAFONDS_DPEC"]] == "défaut config" &&
       s2[["NB_CRH_CIBLE"]] == "surcharge palier (palier.R)" && s2[["REGISTRE_ACTIF"]] == "surcharge palier (palier.R)" && s2[["CAMPAGNE"]] == "défaut config" })
cfg_txt <- sub("#.*$", "", readLines(file.path(racine, "config.R"), warn = FALSE))
ok("frontière de config.R : aucun chemin personnel (~/, /home/, /Users/, commun/), aucun pschema ni identifiant en dur — doctrine et défauts seulement",
   !any(grepl("~/|/home/|/Users/|commun/|rflicoteaux|pschema", cfg_txt)) && any(grepl("^CAMPAGNE\\s*<-", cfg_txt)) && any(grepl("DÉFAUT", readLines(file.path(racine, "config.R"), warn = FALSE))))


# ============================ chantier « courts en campagnes + habillage robuste » (helpers K, registre deux branches, plan des besoins) ==
cat("\n# courts en campagnes : budget, répartition au poids, pivots sous registre, tirage courts à variantes nouvelles\n")
ok("budget_courts : absolu prime ; sinon ratio × volume longs attendu (arrondi) ; ni l'un ni l'autre -> stop actionnable",
   { b1 <- budget_courts(500L, 1, 1000); b2 <- budget_courts(NULL, 0.5, 1001); b3 <- budget_courts(NULL, 1, 2200)
     b1$budget == 500 && b1$source == "absolu" && b2$budget == 500 && b2$source == "ratio" && grepl("RATIO_COURTS 0.5", b2$detail) && b3$budget == 2200 &&
       grepl("etape_selection_longs", tryCatch(budget_courts(NULL, 1, NULL), error = function(e) conditionMessage(e))) })
ok("repartir_budget_pivots : somme == budget, au poids (plus forts restes), pivots légers à 0, budget nul -> zéros",
   { r <- repartir_budget_pivots(100L, c(50, 30, 15, 4, 1)); r2 <- repartir_budget_pivots(3L, c(1, 1, 1, 1, 100))
     sum(r) == 100 && r[1] == 50 && r[2] == 30 && r[5] <= 1 && sum(r2) == 3 && r2[5] == 3 && all(repartir_budget_pivots(0L, c(1, 2)) == 0L) && is.integer(r) })
piv <- tibble::tibble(mode_hospit = "HC", sexe = c("1", "2", "1"), cage = "[60-70[", ghm2 = "04M053", diag2 = c("J449", "J449", "I10"), duree = c(2L, 1L, 0L), nb = c(40, 30, 10))
reg_courts <- tibble::tibble(id_profil = id_profil_courts_de(piv[1, ]), variante_max = 4L, nb_scenarios = 4L, hash_das = list(c("hA", "hB")))
ps <- pivots_sous_registre(piv, reg_courts)
ok("pivots_sous_registre : id_profil k… par pivot ; pivot connu = recyclé (variante_debut = variante_max + 1, hash_exclus), sinon vierge ; sans registre tout vierge",
   all(grepl("^k[0-9a-f]{15}$", ps$id_profil)) && ps$origine_profil[1] == "recycle" && ps$variante_debut[1] == 5L && ps$hash_exclus[1] == "hA hB" && all(ps$origine_profil[2:3] == "vierge") && all(ps$variante_debut[2:3] == 1L) &&
     all(pivots_sous_registre(piv, NULL)$origine_profil == "vierge") && all(pivots_sous_registre(piv, reg_courts[0, ])$variante_debut == 1L))
set.seed(11); tc1 <- sample_das_court("HC", "1", "[60-70[", "04M05", "J449", 1, nb = 40, ref_chro = ref_chro_fx, ref_nb_chro = ref_nb_fx, refs = refs_fx, nb_tirages = 4, seuil_ref = 1, dedoublonner = TRUE, id_profil = "kabc", variante_debut = 5L)
ok("sample_das_court sous registre : variantes numérotées à partir de variante_debut, id_profil / id_scenario / hash_das, nb_variantes_demandees, dédoublonnage souple sans re-tirage",
   !is.null(tc1) && all(c("id_profil", "id_scenario", "hash_das", "nb_variantes_demandees") %in% names(tc1)) && all(tc1$variante >= 5 & tc1$variante <= 8) && all(tc1$id_scenario == paste0("kabc-", sprintf("%03d", tc1$variante))) &&
     all(tc1$hash_das == hash_das_de(tc1$diagnostic_associes)) && !anyDuplicated(tc1$hash_das) && all(tc1$nb_variantes_demandees == 4L) && nrow(tc1) <= 4)
set.seed(11); tc2 <- sample_das_court("HC", "1", "[60-70[", "04M05", "J449", 1, nb = 40, ref_chro = ref_chro_fx, ref_nb_chro = ref_nb_fx, refs = refs_fx, nb_tirages = 4, seuil_ref = 1, dedoublonner = TRUE, id_profil = "kabc", variante_debut = 5L, hash_exclus = paste(tc1$hash_das[1], "zzz"))
ok("sample_das_court : hash déjà enregistré pour ce pivot (collision volontaire) -> variante éliminée SANS re-tirage ; déterminisme sous seed",
   nrow(tc2) == nrow(tc1) - 1 && !tc1$hash_das[1] %in% tc2$hash_das && all(tc2$hash_das %in% tc1$hash_das) && identical(tc2$diagnostic_associes, tc1$diagnostic_associes[-1]))
set.seed(11); tc0 <- sample_das_court("HC", "1", "[60-70[", "04M05", "J449", 1, nb = 40, ref_chro = ref_chro_fx, ref_nb_chro = ref_nb_fx, refs = refs_fx, nb_tirages = 4, seuil_ref = 1)
ok("sample_das_court sans registre : schéma antérieur conservé (variantes 1..n, aucune colonne d'identifiant), mêmes DAS que sous registre (doctrine de tirage inchangée)",
   !any(c("id_profil", "id_scenario", "hash_das", "nb_variantes_demandees") %in% names(tc0)) && identical(sort(tc0$variante), 1:4) && identical(sort(unique(tc0$diagnostic_associes)), sort(unique(tc1$diagnostic_associes))))

cat("\n# registre deux branches : colonne branche, extension append-only, statut par branche, registre depuis les courts\n")
dreg2 <- file.path(tempdir(), "registre_2b"); unlink(dreg2, recursive = TRUE)
regL <- tibble::tibble(id_profil = c("p1", "p2"), variante = 1L, id_scenario = c("p1-001", "p2-001"), hash_das = c("h1", "h2"), campagne = "C1", population = "adulte", diag2 = "J449", DPEC = "d", date = "2026-09-20")
regC <- tibble::tibble(id_profil = c("kaaa", "kaaa"), variante = 1:2, id_scenario = c("kaaa-001", "kaaa-002"), hash_das = c("c1", "c2"), campagne = "C1", population = "adulte", diag2 = "J449", DPEC = "dc", date = "2026-09-21", branche = "court")
fL <- ecrire_registre_campagne(regL, "C1", dreg2)
ok("registre : lignes sans branche écrites avec branche = long ; réécriture identique idempotente", identical(COLONNES_REGISTRE[10], "branche") && all(arrow::read_parquet(fL)$branche == "long") && identical(ecrire_registre_campagne(regL, "C1", dreg2), fL))
o_ext <- utils::capture.output(fE <- ecrire_registre_campagne(dplyr::bind_rows(regL, regC), "C1", dreg2))
lE <- lire_registre(dreg2)
ok("registre : EXTENSION append-only (lignes existantes intactes + branche absente ajoutée) acceptée ; agrégats par branche et par campagne",
   any(grepl("étendu \\(append-only\\) — branche court ajoutée : 2", o_ext)) && lE$nb_scenarios == 4 && all(lE$lignes$date[lE$lignes$branche == "long"] == "2026-09-20") && lE$par_branche$nb_scenarios[lE$par_branche$branche == "court"] == 2 &&
     lE$par_campagne$nb_longs == 2 && lE$par_campagne$nb_courts == 2 && identical(ecrire_registre_campagne(dplyr::bind_rows(regL, regC), "C1", dreg2), fL))
ok("registre : extension acceptée depuis un df ne portant QUE la branche nouvelle (rétro-inscription des courts seuls) ; refusée si une ligne d'une branche présente manque ou diffère, ou si les ajouts sont d'une branche déjà présente (append-only strict)",
   identical(ecrire_registre_campagne(regC, "C1", dreg2), fL) && lire_registre(dreg2)$nb_scenarios == 4 &&
     grepl("append-only", tryCatch(ecrire_registre_campagne(dplyr::bind_rows(regL[1, ], regC), "C1", dreg2), error = function(e) conditionMessage(e))) &&
     grepl("append-only", tryCatch(ecrire_registre_campagne(dplyr::bind_rows(dplyr::mutate(regL, hash_das = "x"), regC), "C1", dreg2), error = function(e) conditionMessage(e))) &&
     grepl("append-only", tryCatch(ecrire_registre_campagne(dplyr::bind_rows(regL, regC, dplyr::mutate(regL[1, ], variante = 2L, id_scenario = "p1-002", hash_das = "h9")), "C1", dreg2), error = function(e) conditionMessage(e))) &&
     lire_registre(dreg2)$nb_scenarios == 4)
ok("statut_campagne_registre : deux branches (nb longs / courts dans le texte) ; par branche ; jamais inscrite (branche)",
   { s <- statut_campagne_registre("C1", lE); sc <- statut_campagne_registre("C1", lE, "court"); sl <- statut_campagne_registre("C1", lE, "long"); s2 <- statut_campagne_registre("C2", lE, "court")
     s$inscrite && s$nb == 4 && s$nb_longs == 2 && s$nb_courts == 2 && grepl("4 scénarios \\(2 longs, 2 courts\\)", s$texte) && grepl("changez d'identifiant", s$texte) &&
       sc$inscrite && sc$nb == 2 && sc$nb_courts == 2 && sl$nb_longs == 2 && sl$nb_courts == 0 && !s2$inscrite && grepl("jamais inscrite au registre \\(branche court\\)", s2$texte) })
corpus_c <- tibble::tibble(mode_hospit = "HC", sexe = "1", cage = c("[60-70[", "[60-70[", "[5-10["), ghm2 = "04M053", diag2 = "J449", duree = c(2L, 2L, 1L), variante = c(1L, 1L, 1L), age = c(65L, 65L, 7L),
                           diagnostic_associes = c("I10 E785", "I10 E785", ""), mode_entree = c("8", "URGENCES", "8"))
rc <- registre_depuis_courts(corpus_c, "C1")
ok("registre_depuis_courts : id_profil k… recalculé, hash sur les DAS, UN scénario par id_scenario (variantes d'habillage repliées), population par cage, branche court, DPEC NA sans typologie",
   nrow(rc) == 2 && all(grepl("^k", rc$id_profil)) && identical(names(rc), COLONNES_REGISTRE) && all(rc$branche == "court") && setequal(rc$population, c("adulte", "pediatrie")) && all(rc$hash_das == hash_das_de(c("I10 E785", ""))) && all(is.na(rc$DPEC)))
typo_t <- charger_typologie(file.path(racine, "referentiels", "typologie_sejours.yaml"))
ok("registre_depuis_courts avec typologie : DPEC par la vraie durée et l'âge tiré", { r <- registre_depuis_courts(corpus_c, "C1", typo_t); all(!is.na(r$DPEC)) && all(r$DPEC == "Médecine adultes < 3 nuits") })

cat("\n# habillage admin robuste : nbda hors des clés, repli hiérarchique, jamais de NA silencieux\n")
ok("clés d'habillage : nbda absent des clés (doctrine config CLES_ADMIN_LONGS), niveau 0 == CLES_ADMIN_LONGS, clé du magasin références",
   !"nbda" %in% CLES_ADMIN_LONGS && identical(NIVEAUX_REPLI_ADMIN[[1]], CLES_ADMIN_LONGS) && "CLES_ADMIN_LONGS" %in% CLES_MAGASINS$references && "ANS_COURTS" %in% CLES_MAGASINS$references && !"nbda" %in% unlist(NIVEAUX_REPLI_ADMIN))
v_adm <- tibble::tibble(mode_hospit = "HC", mode_entree = c("8", "URGENCES", "8", "8", "URGENCES"), mode_sortie = "8", sexe = c("1", "1", "2", "1", "1"), age = c("ge_18", "ge_18", "ge_18", "ge_18", "ge_18"),
                        cage = c("[60-70[", "[60-70[", "[60-70[", "[70-80[", "[70-80["), ghm2 = c("04M053", "04M053", "04M053", "04M053", "05M093"), diag2 = c("J449", "J449", "J449", "I500", "I500"), mdp = "6", duree = c(4, 7, 5, 9, 3), n = c(90, 10, 5, 3, 2))
d_l <- tibble::tibble(mode_hospit = "HC", sexe = c("1", "2", "1", "1"), age = "ge_18", cage = c("[60-70[", "[60-70[", "[70-80[", "[70-80["), racine = c("04M05", "04M05", "04M05", "05M09"), ghm2 = c("04M053", "04M053", "04M053", "05M093"),
                      diag2 = c("J449", "J449", "J449", "K802"), nbda = c(3L, 8L, 2L, 4L), variante = 1L, diagnostic_associes = "I10")
set.seed(3); h <- habiller_admin(d_l, v_adm, NIVEAUX_REPLI_ADMIN, c(COLS_ADMIN, "duree"), NA, 2L)
ok("habiller_admin : niveau 0 (6 clés, sans nbda) -> TOUTES les variantes admin (nb_variantes = NA) ; sans candidat fin -> repli 1 (cage pour l'âge) puis repli 2 (mode_hospit × cage × racine) ; colonne repli_admin ; zéro NA",
   sum(h$repli_admin == 0 & h$sexe == "1" & h$cage == "[60-70[") == 2 && all(h$repli_admin[h$sexe == "2"] == 0) && nrow(h[h$sexe == "2", ]) == 1 &&
     all(h$repli_admin[h$cage == "[70-80[" & h$diag2 == "J449"] == 2) && all(h$repli_admin[h$diag2 == "K802"] == 2) && all(h$duree[h$diag2 == "K802"] == 3) &&
     !any(is.na(h$mode_entree)) && !any(is.na(h$duree)) && all(c("nbda", "variante", "diagnostic_associes") %in% names(h)) && controle_habillage(h)$na_habillage == 0 &&
     identical(as.character(controle_habillage(h)$repli$niveau), c("0", "2")))
v_adm2 <- dplyr::bind_rows(v_adm, tibble::tibble(mode_hospit = "HC", mode_entree = "8", mode_sortie = "9", sexe = "1", age = "lt_18", cage = "[70-80[", ghm2 = "04M053", diag2 = "J449", mdp = "6", duree = 12, n = 1))
set.seed(3); h2 <- habiller_admin(d_l[3, ], v_adm2, NIVEAUX_REPLI_ADMIN, c(COLS_ADMIN, "duree"), NA, 2L)
ok("habiller_admin : premier niveau non vide — âge exact absent mais cage présente -> repli 1 (pas 2), nb_repli variantes au plus", nrow(h2) == 1 && h2$repli_admin == 1 && h2$duree == 12)
set.seed(4); h3 <- habiller_admin(d_l[1, ], v_adm, NIVEAUX_REPLI_ADMIN, c(COLS_ADMIN, "duree"), 1L, 2L)
ok("habiller_admin : nb_variantes = 1 au niveau fin -> une ligne tirée parmi les candidats ; déterminisme sous seed", nrow(h3) == 1 && h3$repli_admin == 0 && { set.seed(4); identical(habiller_admin(d_l[1, ], v_adm, NIVEAUX_REPLI_ADMIN, c(COLS_ADMIN, "duree"), 1L, 2L), h3) })
ok("habiller_admin : tous niveaux vides -> stop NOMINATIF (profil cité), jamais de NA silencieux",
   { e <- tryCatch(habiller_admin(dplyr::mutate(d_l[1, ], mode_hospit = "HP"), v_adm, NIVEAUX_REPLI_ADMIN, c(COLS_ADMIN, "duree")), error = function(e) conditionMessage(e)); grepl("AUCUN niveau de repli", e) && grepl("HP/\\[60-70\\[/04M05/J449", e) && grepl("Jamais de NA silencieux", e) })
ok("habiller_admin : courts (une strate = les 6 pivots, colonnes admin seules) ; d vide -> schéma avec repli_admin", { hc <- habiller_admin(tibble::tibble(mode_hospit = "HC", sexe = "1", cage = "[60-70[", ghm2 = "04M053", diag2 = "J449", duree = 4L, variante = 1L), v_adm, list(PIVOTS_COURTS), COLS_ADMIN, 2L, 2L, "courts")
   nrow(hc) == 1 && hc$repli_admin == 0 && hc$duree == 4L && "repli_admin" %in% names(habiller_admin(d_l[0, ], v_adm)) })
ok("controle_habillage : NA comptés par ligne (au moins un NA sur les colonnes apportées) ; durées hors du périmètre de la branche comptées (Q72)",
   { x <- h; x$duree[1] <- NA; x$mode_entree[2] <- NA; x$mode_entree[1] <- NA; y <- h; y$duree[1:2] <- 1
     controle_habillage(x)$na_habillage == 2 && controle_habillage(x)$duree_hors_perimetre == 0 && controle_habillage(y, duree_perimetre = DUREE_LONGS)$duree_hors_perimetre == 2 && controle_habillage(y)$duree_hors_perimetre == 0 && controle_habillage(h, duree_perimetre = DUREE_LONGS)$duree_hors_perimetre == 0 })
cat("\n# micro-lot v_admin : pondération par les effectifs (Q70), photographie sans n refusée\n")
ok("habiller_admin : photographie sans colonne n -> stop (magasin à régénérer, FORCER_REFS)", grepl("FORCER_REFS", tryCatch(habiller_admin(d_l[1, ], v_adm[, setdiff(names(v_adm), "n")], NIVEAUX_REPLI_ADMIN), error = function(e) conditionMessage(e))))
set.seed(21); tir_fin <- purrr::map(1:400, ~ habiller_admin(d_l[1, ], v_adm, NIVEAUX_REPLI_ADMIN, c(COLS_ADMIN, "duree"), 1L, 2L)$mode_entree) |> unlist()
ok("tirage pondéré au niveau fin : effectifs 90 / 10 -> proportions attendues sous seed (entre 82 % et 96 % pour la combinaison majoritaire), déterminisme",
   { p <- mean(tir_fin == "8"); p > 0.82 && p < 0.96 && { set.seed(21); identical(purrr::map(1:400, ~ habiller_admin(d_l[1, ], v_adm, NIVEAUX_REPLI_ADMIN, c(COLS_ADMIN, "duree"), 1L, 2L)$mode_entree) |> unlist(), tir_fin) } })
# niveau de repli 2 (mode_hospit × cage × racine) : strates fusionnées -> n sommés ; candidats (mode_entree 8 : durées 9 et 4/5 ; URGENCES : 7 et 3)
v_rep <- dplyr::bind_rows(v_adm, tibble::tibble(mode_hospit = "HC", mode_entree = "URGENCES", mode_sortie = "8", sexe = "2", age = "ge_18", cage = "[60-70[", ghm2 = "04M053", diag2 = "I500", mdp = "6", duree = 7, n = 10))
d_rep <- dplyr::mutate(d_l[1, ], diag2 = "K802", sexe = "1")   # aucun candidat aux niveaux 0 et 1 -> niveau 2 : mode_hospit HC × cage [60-70[ × racine 04M05
set.seed(22); tir_rep <- purrr::map(1:400, ~ habiller_admin(d_rep, v_rep, NIVEAUX_REPLI_ADMIN, c(COLS_ADMIN, "duree"), NA, 1L)) |> purrr::list_rbind()
ok("tirage pondéré à un niveau de repli : n sommés sur les strates fusionnées (URGENCES/7 : 10 + 10 = 20 vs 8/4 : 90) -> proportions attendues ; repli_admin = 2",
   all(tir_rep$repli_admin == 2) && { p8 <- mean(tir_rep$mode_entree == "8" & tir_rep$duree == 4); pu <- mean(tir_rep$mode_entree == "URGENCES" & tir_rep$duree == 7); p8 > 0.7 && p8 < 0.93 && pu > 0.08 && pu < 0.28 })
ok("niveau fin avec nb_variantes = NA : toutes les combinaisons distinctes conservées (comportement v7.2, non pondéré — Q74), colonne n absente de la sortie", nrow(h[h$sexe == "1" & h$cage == "[60-70[", ]) == 2 && !"n" %in% names(h) && !".n_admin" %in% names(h))

cat("\n# adoption de C1 : choix de la source, préparation des longs et des courts adoptés, vérification livrable / registre\n")
d_ad <- file.path(tempdir(), "adopt"); unlink(d_ad, recursive = TRUE); dir.create(file.path(d_ad, "scenarios_longs_tirage_v8_20260901", "adulte"), recursive = TRUE); dir.create(file.path(d_ad, "scenarios_longs_tirage_v8_20260918"))
ok("choisir_source_longs : explicite prime ; candidat unique accepté (message) ; plusieurs -> stop listant, jamais de choix silencieux ; aucun -> stop",
   { cand <- candidats_corpus_longs(d_ad); length(cand) == 2 && grepl("jamais de choix silencieux", tryCatch(choisir_source_longs(NULL, cand), error = function(e) conditionMessage(e))) &&
       choisir_source_longs(NULL, cand[1])$source == cand[1] && grepl("candidat unique", choisir_source_longs(NULL, cand[1])$message) && choisir_source_longs(cand[2], cand)$source == cand[2] &&
       grepl("aucun corpus longs daté", tryCatch(choisir_source_longs(NULL, character(0)), error = function(e) conditionMessage(e))) && grepl("introuvable", tryCatch(choisir_source_longs(file.path(d_ad, "nope")), error = function(e) conditionMessage(e))) &&
       length(candidats_corpus_longs(file.path(d_ad, "absent"))) == 0 })
lg_ad <- tibble::tibble(mode_hospit = "HC", sexe = "1", age = "ge_18", cage = c("[60-70[", "[60-70[", "[5-10["), racine = "04M05", ghm2 = "04M053", diabete = "N", hta = "N", diag2 = "J449", nbda = 3L, type_unite = "HC", prep_sc = 0,
                        graine = c("I10 E785", "I10 E785", "N189"), variante = c(1L, 1L, 2L), diagnostic_associes = c("I10 E785 N189", "I10 E785 N189", "N189 K802"), mode_entree = c("8", "URGENCES", "8"), poids = 12)
pa <- preparer_longs_adoptes(lg_ad, "C1", typo_t)
ok("preparer_longs_adoptes : population reconstituée (cage), DPEC/TPEC, id_profil (id_v1 pivots + graine) et hash recalculés, branche long, registre = scénarios distincts",
   setequal(pa$df$population, c("adulte", "pediatrie")) && all(c("DPEC", "TPEC") %in% names(pa$df)) && all(pa$df$id_profil == id_profil_de(dplyr::mutate(lg_ad, diagnostic_associes = graine))) && all(pa$df$branche == "long") && all(pa$df$campagne == "C1") &&
     nrow(pa$registre) == 2 && all(pa$registre$branche == "long") && setequal(pa$registre$id_scenario, unique(pa$df$id_scenario)) && grepl("colonnes manquantes", tryCatch(preparer_longs_adoptes(lg_ad[, -1], "C1", typo_t), error = function(e) conditionMessage(e))))
pcx <- preparer_courts_adoptes(corpus_c, "C1", typo_t)
ok("preparer_courts_adoptes : ids id_courts_v1, population, DPEC/TPEC (vraie durée), lettre, branche court, registre = scénarios distincts",
   all(grepl("^k", pcx$df$id_profil)) && all(c("DPEC", "TPEC", "lettre", "population") %in% names(pcx$df)) && all(pcx$df$branche == "court") && nrow(pcx$registre) == 2 && all(pcx$registre$branche == "court"))
liv_ad <- dplyr::bind_rows(pa$df[, c("branche", "id_scenario")], pcx$df[, c("branche", "id_scenario")])
ok("verifier_adoption : scénarios distincts par branche == registre ; écart signalé sinon",
   verifier_adoption(liv_ad, dplyr::bind_rows(pa$registre, pcx$registre))$ok && { v <- verifier_adoption(liv_ad, pa$registre); !v$ok && grepl("court : livrable 2 scénarios distincts / registre 0 \\(ÉCART\\)", v$texte) })

cat("\n# plan des besoins avec ANS_COURTS : années du tirable préparées, prep_das_chronique par année ; paramètres de campagne courts\n")
pb <- resoudre_besoins("CH", 26L, 26L, character(0), c("ref_das_aigu.parquet", "ref_comp_diabete.parquet", "ref_substitution_imprecis.parquet", "ref_paires_chroniques.parquet", "ref_v_admin_longs.parquet"), FALSE, NOMS_REFS, REFS_CHRONIQUES, ans_courts = c(24L, 25L, 26L), refs_courts = REFS_COURTS)
ok("resoudre_besoins : refs du tirable manquantes -> prep_data de ANS_COURTS, prep_das_chronique sur ANS_COURTS seulement (paires déjà présente)",
   identical(pb$annees_a_preparer, c(24L, 25L, 26L)) && pb$prep_das_chronique && identical(pb$annees_das_chronique, c(24L, 25L, 26L)))
pb2 <- resoudre_besoins("CH", 26L, 26L, character(0), setdiff(nom_ref(NOMS_REFS), "ref_paires_chroniques.parquet"), FALSE, NOMS_REFS, REFS_CHRONIQUES, ans_courts = c(24L, 25L, 26L), refs_courts = REFS_COURTS)
ok("resoudre_besoins : seule une ref AN_REF manque (paires) -> AN_REF seule préparée, prep_das_chronique(AN_REF)", identical(pb2$annees_a_preparer, 26L) && identical(pb2$annees_das_chronique, 26L))
pb3 <- resoudre_besoins("CH", 26L, 26L, nom_partiel("CH", 26L), nom_ref(NOMS_REFS), FALSE, NOMS_REFS, REFS_CHRONIQUES, ans_courts = c(24L, 25L, 26L), refs_courts = REFS_COURTS)
ok("resoudre_besoins : tout présent -> rien à préparer (compatibilité : prep_das_chronique FALSE)", pb3$rien_a_faire && length(pb3$annees_a_preparer) == 0 && !pb3$prep_das_chronique)
lc2 <- contenu_surcharge_campagne("C3", 500000, 1L, TRUE, nb_crh_cible_courts = 250000, ratio_courts = 0.5)
ok("contenu_surcharge_campagne : NB_CRH_CIBLE_COURTS (entier L) et RATIO_COURTS écrits seulement s'ils sont posés ; ratio validé ; sources des paramètres courts",
   any(grepl("^NB_CRH_CIBLE_COURTS <- 250000L", lc2)) && any(grepl("^RATIO_COURTS <- 0.5", lc2)) && !any(grepl("COURTS", contenu_surcharge_campagne("C3", 10L))) &&
     grepl("RATIO_COURTS", tryCatch(contenu_surcharge_campagne("C3", 10L, ratio_courts = 0), error = function(e) conditionMessage(e))) &&
     all(c("NB_CRH_CIBLE_COURTS", "RATIO_COURTS") %in% PARAMETRES_CAMPAGNE) && sources_parametres(PARAMETRES_CAMPAGNE, lc2, "/p/campagne.R")[["RATIO_COURTS"]] == "surcharge campagne (campagne.R)")
ok("notebooks : chunk ouvrir_campagne expose RATIO_COURTS et NB_CRH_CIBLE_COURTS ; chunk tirage_courts en §4b après la sélection ; chunk adoption_c1 (demo=FALSE) ; RUN.Rmd sans tirage des courts",
   { l <- rmd[[2]]; i2 <- grep("^```\\{r ouvrir_campagne", l); j2 <- i2 + which(grepl("^```\\s*$", l[(i2 + 1):length(l)]))[1]
     any(grepl("^RATIO_COURTS_CAMP\\s*<-", l[i2:j2])) && any(grepl("^NB_CRH_CIBLE_COURTS_CAMP\\s*<-", l[i2:j2])) && grep("^```\\{r tirage_courts", l) > grep("^```\\{r selection\\}", l) && length(grep("^```\\{r adoption_c1, demo=FALSE", l)) == 1 &&
       !any(grepl("^etape_tirage_courts\\(", rmd[[1]])) && any(grepl("^```\\{r tirable_courts", rmd[[1]])) })

cat("\nTOUS LES TESTS SONT VERTS :", n_ok, "assertions\n")
