cim<-readxl::read_excel(path_projet %+% "/referentiels/referime/cim_2024.xlsx")

cim_chronique<-readxl::read_excel(path_projet %+% "/referentiels/Affections chroniques.xls",
                   col_names = c("code","ind","libelle"))
code_chronique<- cim_chronique |>  dplyr::filter(ind %in% 1:3) |> dplyr::pull(code)

cim_cancer <-readxl::read_excel(path_projet %+% "/referentiels/REFERENTIEL_METHODE_DIM_CANCER_20140411.xls",
                   sheet = "CODES CIM-10 CANCER",
                   col_names = c("code","libelle"),
                   skip = 1)
code_cancer <-cim_cancer |> dplyr::filter(substr(code,1,1)!="Z") |> 
  dplyr::pull(code)

dplyr::copy_to(conn,cim_chronique |>  dplyr::mutate(type= ifelse(code%in%code_cancer,"Cancer",
                                                                 ifelse(ind %in% 1:3,
                                                                 "Chronique","Aigu"))) |> dplyr::select(code,type),"cim_chronique",overwrite = TRUE) 


hta_autres <- c("I110","I119","I120","I129","I131","I132","I139","I150","I151","I152","I158","I159")

code_did =  c("E102","E103","E104","E105","E106",
              "E107","E108","E109")

code_dnid_ins =  c("E1120","E1130","E1140",
                   "E1150","E1160","E1170","E1180","E1190")
code_dnid_ins_ = prep_grep(code_dnid_ins)

code_dnid =  c("E1128","E1138","E1148",
               "E1158","E1168","E1178","E1188","E1198")
code_dnid_ = prep_grep(code_dnid)

# Millésime de la table des niveaux : ANSEQTA_REF est défini par la config de
# extraction_associations_codes_v8.R (§5.8) ; défaut "25" pour compatibilité v7.
if(!exists("ANSEQTA_REF")) ANSEQTA_REF <- "25"
cma<- pRatihque::atihble(conn, 'prd_vue_nompmsi.mco_diag_niveau') |> dplyr::filter(!!dplyr::sym("v20" %+% ANSEQTA_REF)>1) |> dplyr::collect() |> dplyr::pull(code)
codes_diab <- lire_codes_diabete(path_projet %+%"referentiels/codes_diabete.yaml")
codes_diab |> dplyr::filter(grepl("satellites",chemin)) |> dplyr::select(code) |> dplyr::pull(code)->codes_comp_sat_diab
codes_diab |> dplyr::filter(grepl("asterisques_obligatoires",chemin)) |> dplyr::pull(code)->codes_astrisques_diabete
# Alias : v7.2 l.448 référence `comp_sat_diab` (jamais défini) pour les codes satellites ;
# `codes_comp_sat_diab` est la seule définition existante (cf. MODIFICATIONS_V8.md, Q2).
comp_sat_diab <- codes_comp_sat_diab

# Néo-codes diabète (définition unique, §5.10 ; ex v7.1.2 l.127 dans le corps de sample_das)
neo_codes_diabete <- c("E10","E11i","E11ni")



icr <-readr::read_delim(path_projet %+% "referentiels/icr.tsv",delim = ",")

