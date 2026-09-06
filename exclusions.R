z_exclus =c("Z001","Z004","Z005","Z006","Z008","Z012","Z013","Z014","Z015","Z016","Z017","Z018","Z027","Z028","Z029","Z030","Z031",
            "Z032","Z033","Z034","Z035","Z036","Z038","Z039","Z041","Z042","Z043","Z044","Z045","Z046","Z04800","Z04801","Z04802",
            "Z04880","Z080","Z081","Z082","Z087","Z088","Z089","Z090","Z091","Z092","Z093","Z094","Z097","Z098","Z099","Z201","Z202",
            "Z206","Z208","Z209","Z21","Z223","Z226","Z227","Z228","Z229","Z238","Z251","Z258","Z268","Z269","Z271","Z273","Z274","Z278",
            "Z279","Z440","Z441","Z442","Z448","Z449","Z450","Z451","Z452","Z453","Z4580","Z4581","Z4582","Z4583","Z4584","Z4588","Z459",
            "Z462","Z463","Z465","Z466","Z467","Z468","Z470","Z4780","Z4788","Z479","Z480","Z488","Z489","C118","C138","C148","C258","C328")
z_exclus_ = prep_grep(z_exclus)
cat_exclus = "C76"

cat_alzm = c("F000","F001","F002","F009")
cat_alzm_ = prep_grep(cat_alzm)
cat_chute =  c("W00","W01","W02","W03",
               "W04","W05","W06","W07",
               "W08","W09","W10","W11",
               "W12","W13","W14","W15",
               "W16","W17","W18","W19")
cat_chute_ = prep_grep(cat_chute)

cat_cat = c("C00","C03","C04","C05","C09","C10","C50","C67")
cat_cat_ = prep_grep(cat_cat)


diag_pneumopathie_ind = c("J159","J181","J188","J189")
diag_pneumopathie_ind_ = prep_grep(diag_pneumopathie_ind)

diag_vih =c("B20","B21","B22","B23","B24")
diag_vih_ = prep_grep(diag_vih)

racine_exlus = c("23M15","06M12","11M17")
racine_exlus_ = prep_grep(racine_exlus)

