###############################################################################
# tirage_scenarios_v8.R — TIRAGE des scénarios (parquets -> scénarios), SANS base
#
# Pipeline scenarios_bn_pmsi v8, industrialisation. AUCUN appel pRatihque, aucune
# connexion : lit les produits d'EXPORTS_DIR écrits par extraction_associations_codes_v8.R.
# Déroulé : 0. bootstrap  1. lecture des produits (stop() listant les manquants), meta.yaml
# 2. séjours courts (tirage par chunks, habillage, export)  3. séjours longs : sélection figée
# (§4 du brief) -> tirage par chunks -> habillage -> export  4. livrables : echantillon_revue.csv,
# rapport, meta_tirage.yaml. Un seul nom df_scenarios par branche (écrasé) ; jamais deux
# branches vivantes en mémoire.
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

PAIRES_EXCLUES <- list()
if(file.exists(PATH_PAIRES_EXCLUES)){
  PAIRES_EXCLUES <- lapply(yaml::read_yaml(PATH_PAIRES_EXCLUES), as.character)
} else {
  warning("Fichier absent : " %+% PATH_PAIRES_EXCLUES %+% " ; aucune paire exclue.")
}
if(!dir.exists(CHUNKS_DIR)) dir.create(CHUNKS_DIR, recursive = TRUE)

## ---- 1. Lecture des produits d'extraction ----
FICHIERS_REQUIS <- c("catalogue_longs_seuil.parquet", "catalogue_longs_seuil_meta.yaml",
                     nom_ref(c("pivots_courts", "ref_das_aigu", "ref_das_chronique", "ref_nb_chroniques",
                               "ref_comp_diabete", "v_admin_courts", "v_admin_longs")))
manquants <- fichiers_manquants(EXPORTS_DIR, FICHIERS_REQUIS)
if(length(manquants) > 0) stop("Produits d'extraction manquants dans " %+% EXPORTS_DIR %+% " : " %+%
                               paste(manquants, collapse = ", ") %+% ". Lancer extraction_associations_codes_v8.R (profil " %+% PROFIL %+% ").")

meta_catalogue <- yaml::read_yaml(file.path(EXPORTS_DIR, "catalogue_longs_seuil_meta.yaml"))
cat("== META DU CATALOGUE (catalogue_longs_seuil_meta.yaml) ==\n")
cat(yaml::as.yaml(meta_catalogue))
if(!identical(meta_catalogue$PROFIL, PROFIL)) warning("Le catalogue a été extrait avec le profil " %+% meta_catalogue$PROFIL %+% ", tirage en profil " %+% PROFIL)

lire <- function(nom) arrow::read_parquet(file.path(EXPORTS_DIR, nom_ref(nom)))
df_prep_scenarios_seuil <- lire("catalogue_longs_seuil")
df_pivots_courts        <- lire("pivots_courts")
df_das_ref              <- lire("ref_das_aigu")
df_das_chronique        <- lire("ref_das_chronique")
df_nb_chroniques        <- lire("ref_nb_chroniques")
df_v_admin_courts       <- lire("v_admin_courts")
df_v_admin_longs        <- lire("v_admin_longs")
df_res_epi_comp_diabete <- penaliser_comp_diabete(lire("ref_comp_diabete"), CAGE_PED, CAGE_AGES, PENALITE_9_AGES, PENALITE_9_AUTRES)

REFS <- construire_refs(comp_diabete = df_res_epi_comp_diabete, codes_diab = codes_diab,
                        codes_comp_sat_diab = codes_comp_sat_diab, hta_autres = hta_autres,
                        code_did = code_did, code_dnid_ins = code_dnid_ins, code_dnid = code_dnid,
                        neo_codes = neo_codes_diabete, paires_exclues = PAIRES_EXCLUES)
ref_chro       <- prep_ref_chronique(df_das_chronique)
codes_imprecis <- codes_imprecis_de_cim(cim, MOTIF_IMPRECIS)
lib_cim        <- libelles_cim(cim)

chemin_export <- function(nom) file.path(EXPORTS_DIR, nom %+% "_v8_" %+% DATE_TAG %+% ".parquet")
rapport <- list()   # petits agrégats conservés pour le rapport (jamais un df de scénarios)
revue   <- list()

## ---- 2. Séjours courts ----
suppressWarnings(rm(df_scenarios, df_tirage))
cat("== Séjours courts : tirage par chunks (", nrow(df_pivots_courts), " pivots) ==\n", sep = "")
df_tirage <- pmap_chunks(df_pivots_courts[, c(PIVOTS_COURTS, "nb")], sample_das_court,
                         chunk_size = CHUNK_SIZE, dossier = CHUNKS_DIR, prefixe = "courts", seed_base = SEED,
                         garder_chunks = GARDER_CHUNKS,
                         ref_chro = ref_chro, ref_nb_chro = df_nb_chroniques, refs = REFS,
                         nb_tirages = NB_TIRAGES_COURTS, seuil_ref = SEUIL_REF_DAS,
                         cibles_defaut = CIBLES_NB_CHRONIQUES, age_max = AGE_MAX_OUVERT)
rapport$courts_tirage_n <- nrow(df_tirage)

# Habillage admin : NB_VARIANTES_ADMIN_COURTS variantes tirées au sort par scénario (§5.9b)
set.seed(SEED + 1e6)
df_scenarios <- df_tirage |> 
  dplyr::left_join(df_v_admin_courts,relationship = "many-to-many") |> 
  dplyr::group_by(dplyr::across(-dplyr::any_of(COLS_ADMIN))) |> 
  dplyr::slice_sample(n = NB_VARIANTES_ADMIN_COURTS) |> 
  dplyr::ungroup()
rm(df_tirage)
arrow::write_parquet(df_scenarios, chemin_export("scenarios_courts"))
cat("- Séjours courts : ", nrow(df_scenarios), " lignes -> ", chemin_export("scenarios_courts"), "\n", sep = "")

rapport$courts <- list(n = nrow(df_scenarios), pivots = nrow(dplyr::distinct(df_scenarios[, PIVOTS_COURTS])),
                       distribution = distribution_nb_das(df_scenarios), top_das = top_das_par_cmd(df_scenarios, 30),
                       taux_imprecis = taux_imprecis(df_scenarios, codes_imprecis),
                       controles = controler_scenarios(df_scenarios, hta_autres, SEUIL_PIVOT))
set.seed(SEED + 2e6)
revue$courts <- formater_revue(echantillonner_revue(df_scenarios |> dplyr::mutate(cmd = substr(ghm2, 1, 2)), 25), "courts", lib_cim)
rm(df_scenarios); gc()

## ---- 3. Séjours longs ----
suppressWarnings(rm(df_scenarios, df_tirage))
FICHIER_META_TIRAGE <- file.path(EXPORTS_DIR, "meta_tirage.yaml")
FICHIER_SELECTION   <- file.path(EXPORTS_DIR, "selection_longs.parquet")

# 3a. Sélection figée sous le seed global, écrite AVANT le premier chunk ; la reprise relit le
#     fichier et ne re-tire jamais la sélection (meta_tirage.yaml vérifié).
meta_tirage <- list(MODE_SELECTION = MODE_SELECTION, BUDGET_TOTAL_LONGS = as.integer(BUDGET_TOTAL_LONGS),
                    QUOTA_MIN_PAR_UNITE = as.integer(QUOTA_MIN_PAR_UNITE), CHUNK_SIZE = as.integer(CHUNK_SIZE),
                    SEED = as.integer(SEED), nrow_catalogue = nrow(df_prep_scenarios_seuil))
CLES_META_TIRAGE <- c("MODE_SELECTION", "BUDGET_TOTAL_LONGS", "QUOTA_MIN_PAR_UNITE", "CHUNK_SIZE", "SEED", "nrow_catalogue")
meta_existant <- if(file.exists(FICHIER_META_TIRAGE)) yaml::read_yaml(FICHIER_META_TIRAGE) else NULL
msg <- verifier_meta_tirage(meta_existant, meta_tirage, CLES_META_TIRAGE)
if(!is.null(msg)) stop(msg)

set.seed(SEED + 3e6)
if(MODE_SELECTION == "quota_dp"){
  if(file.exists(FICHIER_SELECTION)){
    df_selection <- arrow::read_parquet(FICHIER_SELECTION)
    cat("== Sélection longs (quota_dp) : relue depuis ", FICHIER_SELECTION, " (", nrow(df_selection), " lignes)\n", sep = "")
    meta_tirage$quota_par_dp <- meta_existant$quota_par_dp
    meta_tirage$nb_dp        <- meta_existant$nb_dp
  } else {
    sel <- selection_quota_dp(df_prep_scenarios_seuil, BUDGET_TOTAL_LONGS, QUOTA_MIN_PAR_UNITE)
    df_selection <- sel$selection
    meta_tirage$quota_par_dp <- sel$quota_par_dp
    meta_tirage$nb_dp        <- sel$nb_dp
    arrow::write_parquet(df_selection, FICHIER_SELECTION)
    cat("== Sélection longs (quota_dp) : ", sel$nb_dp, " diag2, quota par DP = ", sel$quota_par_dp,
        ", ", nrow(df_selection), " lignes -> ", FICHIER_SELECTION, "\n", sep = "")
  }
  nb_tirage_longs <- 1L
  meta_tirage$NB_VARIANTES <- 1L
  meta_tirage$volume_attendu <- nrow(df_selection)
  rapport$selection <- effectifs_selection(df_selection)
  utils::write.csv(rapport$selection, file.path(EXPORTS_DIR, "selection_longs_effectifs.csv"), row.names = FALSE)
} else {
  sel <- selection_catalogue_complet(df_prep_scenarios_seuil, BUDGET_TOTAL_LONGS)
  df_selection <- df_prep_scenarios_seuil
  nb_tirage_longs <- sel$nb_variantes
  meta_tirage$NB_VARIANTES   <- sel$nb_variantes
  meta_tirage$volume_attendu <- sel$volume_attendu
  cat("== Sélection longs (catalogue_complet) : nrow = ", sel$nrow, " ; NB_VARIANTES = ", sel$nb_variantes,
      " ; volume attendu = ", sel$volume_attendu, "\n", sep = "")
}
meta_tirage$PROFIL <- PROFIL; meta_tirage$date <- as.character(Sys.Date())
yaml::write_yaml(meta_tirage, FICHIER_META_TIRAGE)
rm(df_prep_scenarios_seuil)

# 3b. Tirage par chunks
cat("== Séjours longs : tirage par chunks (", nrow(df_selection), " lignes × ", nb_tirage_longs, " variante(s)) ==\n", sep = "")
df_tirage <- pmap_chunks(df_selection |> dplyr::select(dplyr::all_of(c(PIVOTS_LONGS, "diagnostic_associes", "poids"))),
                         sample_das_long, chunk_size = CHUNK_SIZE, dossier = CHUNKS_DIR, prefixe = "longs",
                         seed_base = SEED + 1e5, garder_chunks = GARDER_CHUNKS,
                         ref_das_aigu = df_das_ref, refs = REFS, nb_tirage = nb_tirage_longs)
rapport$longs_tirage_n <- nrow(df_tirage)
rm(df_selection)

# 3c. Habillage admin (v7.2 l.555) puis export
set.seed(SEED + 4e6)
df_scenarios <- df_tirage |> dplyr::left_join(df_v_admin_longs,relationship = "many-to-many")
if(!is.na(NB_VARIANTES_ADMIN_LONGS)){
  df_scenarios <- df_scenarios |>
    dplyr::group_by(dplyr::across(-dplyr::any_of(c(COLS_ADMIN, "duree")))) |>
    dplyr::slice_sample(n = NB_VARIANTES_ADMIN_LONGS) |>
    dplyr::ungroup()
}
rm(df_tirage)
arrow::write_parquet(df_scenarios, chemin_export("scenarios_longs_tirage"))
cat("- Séjours longs : ", nrow(df_scenarios), " lignes -> ", chemin_export("scenarios_longs_tirage"), "\n", sep = "")

rapport$longs <- list(n = nrow(df_scenarios), pivots = nrow(dplyr::distinct(df_scenarios[, PIVOTS_LONGS])),
                      distribution = distribution_nb_das(df_scenarios), top_das = top_das_par_cmd(df_scenarios, 30),
                      taux_imprecis = taux_imprecis(df_scenarios, codes_imprecis),
                      controles = controler_scenarios(df_scenarios, hta_autres, SEUIL_PIVOT))
set.seed(SEED + 5e6)
revue$longs <- formater_revue(echantillonner_revue(df_scenarios |> dplyr::mutate(cmd = substr(ghm2, 1, 2)), 25), "longs", lib_cim)
rm(df_scenarios); gc()

## ---- 4. Livrables de validation ----
df_revue <- dplyr::bind_rows(revue$courts, revue$longs)
readr::write_excel_csv2(df_revue, file.path(EXPORTS_DIR, "echantillon_revue.csv"))
utils::write.csv(dplyr::bind_rows(courts = rapport$courts$top_das, longs = rapport$longs$top_das, .id = "branche"),
                 file.path(EXPORTS_DIR, "top30_das_par_cmd.csv"), row.names = FALSE)

fmt_df <- function(d) if(is.null(d) || nrow(d) == 0) "   (vide)" else "   " %+% utils::capture.output(print(as.data.frame(d), row.names = FALSE))
lignes <- c("RAPPORT DE CONTROLE — tirage_scenarios_v8.R — " %+% DATE_TAG %+% " — PROFIL = " %+% PROFIL,
            "", "== 0. Meta du catalogue (catalogue_longs_seuil_meta.yaml) ==",
            "   " %+% strsplit(yaml::as.yaml(meta_catalogue), "\n")[[1]],
            "", "== 0b. Meta du tirage (meta_tirage.yaml) ==",
            "   " %+% strsplit(yaml::as.yaml(meta_tirage), "\n")[[1]], "")
lignes <- c(lignes, "== 1. Volumétrie ==",
            sprintf("sejours_courts : tirage = %d ; final = %d ; pivots distincts = %d", rapport$courts_tirage_n, rapport$courts$n, rapport$courts$pivots),
            sprintf("sejours_longs  : tirage = %d ; final = %d ; pivots distincts = %d", rapport$longs_tirage_n, rapport$longs$n, rapport$longs$pivots))
if(MODE_SELECTION == "catalogue_complet"){
  lignes <- c(lignes, sprintf("mode catalogue_complet : nrow catalogue = %d ; NB_VARIANTES = %d ; volume attendu = %d ; volume tiré = %d",
                              meta_tirage$nrow_catalogue, meta_tirage$NB_VARIANTES, meta_tirage$volume_attendu, rapport$longs_tirage_n))
} else {
  lignes <- c(lignes, sprintf("mode quota_dp : %d diag2 ; quota par DP = %d ; lignes sélectionnées = %d", meta_tirage$nb_dp, meta_tirage$quota_par_dp, meta_tirage$volume_attendu),
              "-- effectifs sélectionnés diag2 × type_unite (50 premières lignes ; table complète : selection_longs_effectifs.csv)",
              fmt_df(utils::head(rapport$selection, 50)),
              "-- totaux par type_unite :",
              fmt_df(rapport$selection |> dplyr::select(-diag2, -total) |> dplyr::summarise(dplyr::across(dplyr::everything(), sum))))
}
lignes <- c(lignes, "", "== 2. Distribution du nombre de DAS par classe d'âge (à comparer aux cibles de saturation) ==",
            "-- sejours_courts", fmt_df(rapport$courts$distribution), "-- sejours_longs", fmt_df(rapport$longs$distribution),
            "-- cibles dégradées CIBLES_NB_CHRONIQUES :",
            "   " %+% names(CIBLES_NB_CHRONIQUES) %+% " : " %+% vapply(CIBLES_NB_CHRONIQUES, function(x) paste(x, collapse = "-"), character(1)), "")
lignes <- c(lignes, "== 3. Top 30 DAS par CMD (fichier complet : top30_das_par_cmd.csv) ==",
            "-- sejours_courts", fmt_df(rapport$courts$top_das), "-- sejours_longs", fmt_df(rapport$longs$top_das), "")
lignes <- c(lignes, "== 4. Taux de codes « sans précision » parmi les DAS de sortie (mesuré, non corrigé) ==",
            sprintf("sejours_courts : %s", format(rapport$courts$taux_imprecis)),
            sprintf("sejours_longs  : %s", format(rapport$longs$taux_imprecis)), "")
lignes <- c(lignes, "== 5. Vérifications programmatiques §8.2 (0 attendu ; NA = non applicable) ==")
anomalies <- 0
for(b in c("courts", "longs")){
  cc <- rapport[[b]]$controles
  lignes <- c(lignes, sprintf("sejours_%-7s doublons_categorie = %s ; diabete_hors_flag = %s ; i10_avec_hta_autres = %s ; poids_sous_seuil = %s",
                              b, format(cc$doublons_categorie), format(cc$diabete_hors_flag), format(cc$i10_avec_hta_autres), format(cc$poids_sous_seuil)))
  anomalies <- anomalies + sum(unlist(cc[c("doublons_categorie", "diabete_hors_flag", "i10_avec_hta_autres", "poids_sous_seuil")]), na.rm = TRUE)
}
lignes <- c(lignes, "TOTAL anomalies = " %+% anomalies, "",
            "Livrables : echantillon_revue.csv (" %+% nrow(df_revue) %+% " scénarios), top30_das_par_cmd.csv, meta_tirage.yaml" %+%
              if(MODE_SELECTION == "quota_dp") ", selection_longs.parquet, selection_longs_effectifs.csv" else "")
FICHIER_RAPPORT <- file.path(EXPORTS_DIR, "rapport_v8_" %+% DATE_TAG %+% ".txt")
writeLines(lignes, FICHIER_RAPPORT)
cat(lignes, sep = "\n")
cat("Tirage terminé. Rapport : ", FICHIER_RAPPORT, "\n", sep = "")
