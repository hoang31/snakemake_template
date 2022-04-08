
############################################
## Clean the clinical data
############################################



############################################
## Load the libraries
############################################


library(data.table)
library(tidyverse)
library(corrplot)


############################################
## load the data 
############################################


## load the clinical data
clinical_data_dt <- fread(
    snakemake@input[["cleaned_clical_data"]]
)

## create the directory that will contain the figures
dir.create(snakemake@output[["correlation_figures_dir"]])


############################################
## Perform the analysis
############################################

## get the quantitative variables
quantitative_variable_vector <- c(
    "Age",
    "Poids_kg",
    "Taille_m",
    "BMI",
    "Serum_25OH_VitD",
    "SJC66", 
    "TJC68", 
    "HAQ", 
    "DAS28", 
    "FR", 
    "Anti-CCP", 
    "CRP",
    "Pred_dose",
    "Mtx_dose",
    "miR-106a-5p",
    "miR-1246",
    "miR-142-5p",
    "miR-147b-5p",
    "miR-152-3p",
    "miR-15b-3p",
    "miR-15b-5p",
    "miR-193b-3p",
    "miR-194-5p",
    "miR-200c-3p",
    "miR-23ab-3p.",
    "miR-25-3p",
    "miR-29b-3p",
    "miR-34a-3p",
    "miR-365b-3p",
    "miR-374a-5p",
    "miR-1-3p",
    "miR-17-5p",
    "miR-206",
    "miR-374a-3p",
    "miR-374c-5p",
    "miR-454-3p",
    "miR-511-3p",
    "miR-7-1-3p",
    "miR-7706"

)

qualitative_variable_vector <- c(
    "Sexe",
    "Ménopause",
    "Tabac",
    "Alcool",
    "Raideur_matinal",
    "Mtx_Y_N",
    "DMARD_autre_Y_N",
    "DMARD_autre1",
    "DMARD_autre2",
    "DMARD_autre3",
    "Biologique_Y_N",
    "Biologique_nom",
    "Pred_Y_N",
    "AINS_reguier_Y_N",
    "Biphosphonate_prolia_Y_N",
    "Biphosphonate_prolia_Nom",
    "Calcium_VitD_Y_N"


)


## extract column related to the data table only
qualitative_variable_vector <- intersect(
    qualitative_variable_vector,
    colnames(clinical_data_dt)
)


quantitative_variable_vector <- intersect(
    quantitative_variable_vector,
    colnames(clinical_data_dt)
)
























## extract data related to the quantitative variable
clinical_data_quantitative <- clinical_data_dt[, quantitative_variable_vector, with = F]
clinical_data_quantitative[, (quantitative_variable_vector) := lapply(.SD, as.numeric), .SDcols = quantitative_variable_vector]

## extract data related to the qualitative variable
clinical_data_qualitative <- clinical_data_dt[, qualitative_variable_vector, with = F]
clinical_data_qualitative[, (qualitative_variable_vector) := lapply(.SD, function(x) as.numeric(as.factor(x))), .SDcols = qualitative_variable_vector]







## generate the correlation matrix
quantitative_cor_matrix <- cor(clinical_data_quantitative, use = "pairwise.complete.obs", method = "pearson")
qualitative_cor_matrix <- cor(clinical_data_qualitative, use = "pairwise.complete.obs", method = "spearman")


## generate the corrplot and save it
png(
    filename = paste(
        snakemake@output[["correlation_figures_dir"]],
        "/corrplot_quantitattive.png",
        sep = ""
    ),
    width = 1000,
    height = 800
)
corrplot(quantitative_cor_matrix, type="upper", tl.col="black", tl.srt=45)
dev.off()



png(
    filename = paste(
        snakemake@output[["correlation_figures_dir"]],
        "/corrplot_qualitative.png",
        sep = ""
    ),
    width = 1000,
    height = 800
)
corrplot(qualitative_cor_matrix, type="upper", tl.col="black", tl.srt=45)
dev.off()







### do the combination
#variable_combination <- combn(
#    quantitative_variable_vector,
#    m = 2
#)

#print(variable_combination)

### for each combination, do the pearson correlation
#for (i_combination in seq(1, ncol(variable_combination))) {

#    ## get the variables
#    variable1 <- variable_combination[1, i_combination]
#    variable2 <- variable_combination[2, i_combination]

#    print("====================")
#    print(variable1)
#    print(variable2)


#    ## perform the pearson correlation
#    cor_res <- cor.test(
#        as.numeric(unlist(clinical_data_dt[, variable1, with = F])),
#        as.numeric(unlist(clinical_data_dt[, variable2, with = F]))
#    )

#    print(cor_res)



#}


