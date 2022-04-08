

############################################
## Clean the clinical data
############################################



############################################
## Load the libraries
############################################


library(data.table)
library(tidyverse)


############################################
## load the data 
############################################


## load the clinical data
clinical_data_dt <- fread(
    snakemake@input[["clinical_data"]]
)

## create the directory that will contain the figures
dir.create(snakemake@output[["comparison_figures_dir"]])


############################################
## Homogenizing the data
############################################


##########
## put unknown
##########

clinical_data_dt[clinical_data_dt == ""] <- "unknown"
clinical_data_dt[is.na(clinical_data_dt)] <- "unknown"
clinical_data_dt[clinical_data_dt == 999] <- "unknown"
clinical_data_dt[clinical_data_dt == 888] <- "not_applicable"


## removing no redudand column
col_to_remove <- c(
    grep(x = colnames(clinical_data_dt), pattern = "_Y_N", value = T),
    "Groupe_1E_2NE_3S",
    "Groupe \n(1: NE, 2: E; 3: Ctr)",
    
    "Pred_dose" # a few data
)
clinical_data_dt <- clinical_data_dt[, !col_to_remove, with = F]


##########
## put binary
##########

## function for binarizing the variable
binarizing_variable <- function(
    data_table_input,
    variable_name_of_interest_input
) {
    
    ## copy the data table input
    data_table_input <- copy(data_table_input)

    ## for each variable of interest
    for (i_var in seq(1, length(variable_name_of_interest_input))) {
        
        ## get the variable of interest
        variable_of_interest <- variable_name_of_interest_input[i_var]

        ## get the unique categories from the variable
        categorie_vector <- sort(unique(unlist(data_table_input[, variable_of_interest, with = F])))

        ## for each categories
        for (i_cat in seq(1, length(categorie_vector))) {
            
            ## get the category of interest
            category_of_interest <- categorie_vector[i_cat]

            ## take the position from the data table that correspond to the category of interest
            pos <- grep(
                unlist(data_table_input[, variable_of_interest, with = F]),
                pattern = category_of_interest
            )

            print("--------------------------")
            print(i_cat)
            print(variable_of_interest)
            print(category_of_interest)
            print(pos)

            ## rename the categories of interest
            data_table_input[pos, (variable_of_interest) := lapply(.SD, function(x) return(i_cat)), .SDcols = variable_of_interest]

        }

    }
    

    #return((data_table_input[, variable_of_interest, with = F]))
    return(data_table_input)

}

## set the variable to binarize
variable_to_binarize <- c(
    "Sexe",
    "DMARD_autre1",
    "DMARD_autre2",
    "DMARD_autre3",
    "Biologique_nom",
    "Biphosphonate_prolia_Nom"
)


clinical_data_dt <- binarizing_variable(
    clinical_data_dt,
    variable_to_binarize
)


############################################
## Analyses
############################################

## extract the conditions for the comparison
condition_vector <- unique(unlist(clinical_data_dt[, condition]))

## extract all the variables into a vector
variable_vector <- colnames(clinical_data_dt)[!(colnames(clinical_data_dt) %in% c("Nsujet", "condition", "Groupe_1E_2NE_3S"))]

## define which variables are qualitative or quantitative
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

qualitative_variable_vector <- intersect(
    qualitative_variable_vector,
    colnames(clinical_data_dt)
)

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


quantitative_variable_vector <- intersect(
    quantitative_variable_vector,
    colnames(clinical_data_dt)
)







## for each variable, do a boxplot
for (i_variable in seq(1, length(variable_vector))) {

    ## get the variable of interest
    variable_of_interest <- variable_vector[i_variable]

    ## extract the values associated with the variable of interest
    clinical_data_subset_dt <- copy(
        clinical_data_dt[, c("condition", variable_of_interest), with = F]
    )

    ## remove unknown or not applicable
    clinical_data_subset_dt <- clinical_data_subset_dt[
        !grep(unlist(clinical_data_subset_dt[, variable_of_interest,with = F]), pattern = "unknown|not_applicable")
    ]

    ## if quantitative, create a boxplot
    if (variable_of_interest %in% c(quantitative_variable_vector)) {
        
        boxplot <- ggplot(
            data = clinical_data_subset_dt,
            aes(
                x = condition,
                y = as.numeric(unlist(clinical_data_subset_dt[, variable_of_interest, with = F])),
                fill = condition
            )
        ) +
            geom_boxplot(size = 2) +
            labs(
                title = variable_of_interest,
                y = variable_of_interest
            ) + 
            theme_bw(base_size = 24)

        print(boxplot)

        ggsave(
            plot = boxplot,
            filename = paste(
                snakemake@output[["comparison_figures_dir"]],
                "/",
                variable_of_interest,
                ".png",
                sep = ""
            ),
            height = 8,
            width = 12
        )
    }

    ## if the variable is qualitative
    if (variable_of_interest %in% c(qualitative_variable_vector)) {

        ## generate the barchart        
        barchart <- ggplot(
            data = clinical_data_subset_dt,
            aes(
                x = unlist(clinical_data_subset_dt[, variable_of_interest, with = F]),
                fill = condition
            )
        ) +
            geom_bar(size = 2,  position=position_dodge(), color = "black") +
            labs(
                title = variable_of_interest,
                x = variable_of_interest,
                y = "Number"
            ) + 
            #guides(fill=guide_legend(title=variable_of_interest)) +
            theme_bw(base_size = 24)

        print(barchart)

        ggsave(
            plot = barchart,
            filename = paste(
                snakemake@output[["comparison_figures_dir"]],
                "/",
                variable_of_interest,
                ".png",
                sep = ""
            ),
            height = 8,
            width = 12
        )
    }


}

## write the cleaned clinical data
fwrite(
    clinical_data_dt,
    snakemake@output[["cleaned_clinical_data"]],
    sep = ","
)




