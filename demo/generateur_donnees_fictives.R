###############################################################################
# demo/generateur_donnees_fictives.R — générateur de données PMSI FICTIVES (source unique)
#
# Extrait de tests/test_chaines_sqlite.R (chantier « packaging + démo ») : les tests le
# sourcent depuis ici, le mode démo aussi (demo/creer_base_demo.R). AUCUNE donnée réelle :
# tirages aléatoires uniformes dans de petits pools de codes ; aucune validité épidémiologique.
#
# generer_donnees_fictives(db_file, n_sejours = 4000L, annees = c(17L, 20L, 26L), graine = 20260907L)
#   écrit dans la base SQLite `db_file` les vues attendues par les chaînes dbplyr du pipeline :
#   PRD_VUE_MCOBL_20<an>.{fixe, um, diag, rgp} pour chaque millésime, nomgen.finessgeo,
#   prd_vue_nompmsi.mco_diag_niveau, prd_vue_nompmsi.all_cim10_caract_patient ; plus deux jeux
#   de fixtures contrôlées sur le dernier millésime (écart B1-10 : IDENT_B110 ; fusion E669 :
#   IDENT_FUSION, GHM 88M991). Retourne (invisible) la liste des paramètres et fixtures.
#   Même graine + mêmes paramètres = même base (ordre des tirages figé : ne pas réordonner).
#
# Prérequis : DBI, RSQLite, dplyr, tidyr, purrr, tibble.
###############################################################################
generer_donnees_fictives <- function(db_file, n_sejours = 4000L, annees = c(17L, 20L, 26L), graine = 20260907L){
  for(p in c("DBI", "RSQLite", "dplyr", "tidyr", "purrr", "tibble")) if(!requireNamespace(p, quietly = TRUE)) stop("Paquet manquant : ", p)
  stopifnot(is.numeric(n_sejours), n_sejours >= 100, length(annees) >= 1, all(annees %in% 0:99))
  N <- as.integer(n_sejours); annees <- as.integer(annees)
  set.seed(graine)
  pool_das <- c("I10","I110","E1120","E1128","E102","E785","J449","N189","N185","F172","I48","G20","M199",
                "R2630","F050","F102","N083","E1198","I509","I500","K802","J440","C189","Z511","D649","E669","E6690","E6602","E6600")
  pool_ghm <- c("04M053","05M093","06C041","10M021","03K021","14Z081","90Z001","06C042","14Z13A")
  pool_dp  <- c("J449","I500","E1120","E102","Z511","K802","I10","E6690")
  chroniques <- c("I10","I110","E1120","E1128","E102","E785","J449","N189","N185","F172","I48","G20","M199","E1198","I509","I500","J440","C189","E669","E6690","E6602","E6600")
  conn0 <- DBI::dbConnect(RSQLite::SQLite(), db_file); on.exit(DBI::dbDisconnect(conn0), add = TRUE)
  gen_annee <- function(an){
    ident <- seq_len(N) + an * 100000
    fixe <- tibble::tibble(
      anonyme = sample(1:(N * 0.7), N, replace = TRUE), ident = ident,
      dp = sample(pool_dp, N, replace = TRUE), dr = NA_character_,
      age = sample(0:95, N, replace = TRUE), sexe = sample(c("1","2"), N, replace = TRUE),
      provenance = sample(c("5","8",NA), N, replace = TRUE), modesortie = sample(c("8","9","7"), N, replace = TRUE, prob = c(8,1,1)),
      destination = sample(c("1","2",NA), N, replace = TRUE), duree = sample(c(0,0,1,2,3,4,5,8,12,30), N, replace = TRUE),
      rumdudp = 1L, nbda = sample(0:8, N, replace = TRUE), ghm2 = sample(pool_ghm, N, replace = TRUE),
      passage_urg = sample(c("5","U","V","0"), N, replace = TRUE), nbrum = sample(1:2, N, replace = TRUE, prob = c(8, 2)),
      raac = sample(c("0","1"), N, replace = TRUE))
    fixe$dr[fixe$dp == "Z511"] <- "C189"
    um <- fixe |> dplyr::select(ident, nbrum, duree) |> tidyr::uncount(nbrum, .id = "rum") |>
      dplyr::mutate(finessgeo = sample(c("750100042","750100075","920100013"), dplyr::n(), replace = TRUE),
                    type_hospum_1 = ifelse(duree == 0 & runif(dplyr::n()) < 0.5, "P", "C"),
                    type_rum_1 = sample(c("01A","07A","27","04","06","10","10","10"), dplyr::n(), replace = TRUE)) |>
      dplyr::select(ident, rum, finessgeo, type_hospum_1, type_rum_1)
    diag <- fixe |> dplyr::select(ident, dp, nbda) |>
      dplyr::mutate(das = purrr::map(nbda, ~ sample(pool_das, .x, replace = FALSE))) |>
      tidyr::unnest(das) |> dplyr::transmute(ident, rum = 1L, diag = das, typ_diag = 5L) |>
      dplyr::bind_rows(fixe |> dplyr::transmute(ident, rum = 1L, diag = dp, typ_diag = 1L))
    rgp <- fixe |> dplyr::transmute(ident, ghmv2023 = ghm2, ghmv2021 = ghm2)
    DBI::dbWriteTable(conn0, paste0("PRD_VUE_MCOBL_20", an, ".fixe"), as.data.frame(fixe), overwrite = TRUE)
    DBI::dbWriteTable(conn0, paste0("PRD_VUE_MCOBL_20", an, ".um"), as.data.frame(um), overwrite = TRUE)
    DBI::dbWriteTable(conn0, paste0("PRD_VUE_MCOBL_20", an, ".diag"), as.data.frame(diag), overwrite = TRUE)
    DBI::dbWriteTable(conn0, paste0("PRD_VUE_MCOBL_20", an, ".rgp"), as.data.frame(rgp), overwrite = TRUE)
  }
  for(an_ in annees) gen_annee(an_)
  # Fixtures contrôlées (écart B1-10), sur le dernier millésime (26 par défaut)
  an_fx <- max(annees); tbl_fx <- function(x) paste0("PRD_VUE_MCOBL_20", an_fx, ".", x)
  IDENT_B110 <- c(hc_sc = 99001, hc_uhcd = 99002, uhcd_seul = 99003, ger_sc = 99004) + an_fx * 100000
  invisible(DBI::dbAppendTable(conn0, tbl_fx("fixe"), as.data.frame(tibble::tibble(
    anonyme = 999001:999004, ident = unname(IDENT_B110), dp = "J449", dr = NA_character_, age = 70, sexe = "1",
    provenance = "8", modesortie = "8", destination = "1", duree = 5, rumdudp = 1L, nbda = 2L, ghm2 = "04M053",
    passage_urg = "0", nbrum = c(2L, 2L, 1L, 2L), raac = "0"))))
  invisible(DBI::dbAppendTable(conn0, tbl_fx("um"), as.data.frame(tibble::tibble(
    ident = c(rep(IDENT_B110[["hc_sc"]], 2), rep(IDENT_B110[["hc_uhcd"]], 2), IDENT_B110[["uhcd_seul"]], rep(IDENT_B110[["ger_sc"]], 2)),
    rum = c(1L, 2L, 1L, 2L, 1L, 1L, 2L), finessgeo = "750100042", type_hospum_1 = "C",
    type_rum_1 = c("10", "01A", "10", "07A", "07A", "27", "01A")))))
  invisible(DBI::dbAppendTable(conn0, tbl_fx("diag"), as.data.frame(tibble::tibble(ident = rep(unname(IDENT_B110), each = 2), rum = 1L, diag = rep(c("I10", "E785"), 4), typ_diag = 5L))))
  # Fixture fusion E669 (chantier conversion) : deux séjours identiques sauf DP E6690 / E6600 sur un
  # GHM dédié 88M991 -> deux profils n = 1 (<= SEUIL_PIVOT = 1) qui fusionnent (n = 2 > seuil) après conversion.
  IDENT_FUSION <- c(99101, 99102) + an_fx * 100000
  invisible(DBI::dbAppendTable(conn0, tbl_fx("fixe"), as.data.frame(tibble::tibble(
    anonyme = 999101:999102, ident = IDENT_FUSION, dp = c("E6690", "E6600"), dr = NA_character_, age = 72, sexe = "2",
    provenance = "8", modesortie = "8", destination = "1", duree = 6, rumdudp = 1L, nbda = 1L, ghm2 = "88M991",
    passage_urg = "0", nbrum = 1L, raac = "0"))))
  invisible(DBI::dbAppendTable(conn0, tbl_fx("um"), as.data.frame(tibble::tibble(
    ident = IDENT_FUSION, rum = 1L, finessgeo = "750100042", type_hospum_1 = "C", type_rum_1 = "10"))))
  invisible(DBI::dbAppendTable(conn0, tbl_fx("diag"), as.data.frame(tibble::tibble(ident = IDENT_FUSION, rum = 1L, diag = "I48", typ_diag = 5L))))
  DBI::dbWriteTable(conn0, "nomgen.finessgeo", data.frame(finessgeo = c("750100042","750100075","920100013"), categ_pmsi = c("CHR/U","CHR/U","CH")), overwrite = TRUE)
  DBI::dbWriteTable(conn0, "prd_vue_nompmsi.mco_diag_niveau",
                    data.frame(code = pool_das, v2021 = as.character(sample(1:4, length(pool_das), TRUE)), v2023 = as.character(sample(1:4, length(pool_das), TRUE)), v2025 = as.character(sample(1:4, length(pool_das), TRUE))), overwrite = TRUE)
  DBI::dbWriteTable(conn0, "prd_vue_nompmsi.all_cim10_caract_patient",
                    data.frame(code = pool_das, type_liste = ifelse(pool_das %in% chroniques, "Patho_chro", "Aigu"), caract = "x"), overwrite = TRUE)
  invisible(list(db_file = db_file, n_sejours = N, annees = annees, graine = graine, pool_das = pool_das, pool_ghm = pool_ghm, pool_dp = pool_dp,
                 IDENT_B110 = IDENT_B110, IDENT_FUSION = IDENT_FUSION))
}
