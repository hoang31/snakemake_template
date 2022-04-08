

############################################
## Perform a PCA
############################################



############################################
## Load the libraries
############################################


library(data.table)
library(tidyverse)
library(RColorBrewer)
library(egg)
library(factoextra)
library(FactoMineR)


############################################
## load the data 
############################################


## load the clinical data
clinical_data_dt <- fread(
    snakemake@input[["cleaned_clical_data"]]
)

## create the directory that will contain the figures
dir.create(snakemake@output[["pca_figures"]])


############################################
## Analysis
############################################






#print(clinical_data_dt)
clinical_data_dt[clinical_data_dt == "unknown"] <- 0 
clinical_data_dt[clinical_data_dt == "not_applicable"] <- 0 







## get the quantitative and qualtitative variables
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
    #"Pred_dose",
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

## get all data
clinical_data_all <-  cbind(
    clinical_data_quantitative,
    clinical_data_qualitative
)


















#res.mca <- MCA (clinical_data_dt)
##print(res.mca)
#break



## perform the pca
res.pca <- prcomp(
    clinical_data_quantitative,
    scale = TRUE,
    na.action = "na.omit"
)


## generate the barchart that show the contribution for each coponent
contribution_barchart <- fviz_eig(res.pca)

## generate the plot related to the sample data
sample_plot <- fviz_pca_ind(
    res.pca,
    col.ind = "cos2", # Colorer par le cos2
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE     
) + 
    theme_classic(base_size = 24)


## generate the file path of the figure to save
file_path <- paste(
    snakemake@output[["pca_figures"]],
    "/",
    "sample_plot_quantitative",
    ".svg",
    sep = ""
)

## save the plot of interest
ggsave(
    sample_plot,
    filename = file_path,
    device = "svg",
    height = 8,
    width = 12
)

## generate variable plot
variable_plot <- fviz_pca_var(
    res.pca,
    col.var = "contrib", 
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE,
    labelsize = 7,
    arrowsize = 1
) + 
    theme_classic(base_size = 24)

## generate the file path of the figure to save
file_path <- paste(
    snakemake@output[["pca_figures"]],
    "/",
    "variable_plot_quantitative",
    ".svg",
    sep = ""
)

## save the plot of interest
ggsave(
    variable_plot,
    filename = file_path,
    device = "svg",
    height = 10,
    width = 12
)

#fviz_pca_biplot(
#    res.pca,
#    repel = TRUE,
#    col.var = "#2E9FDF", 
#    col.ind = "#696969"  
#)


## retrieve the variable contribution for each component
contribution <- PCA(
    clinical_data_quantitative,
    scale.unit = FALSE
)

contribution <- get_pca_var(contribution)

print("----- variable contribution -----")
print(contribution$contrib)


## contribution for each variable for the first two component
contribution_1axe <- fviz_contrib(res.pca, choice = "var", axes = 1, top = 10) +
    theme_classic(base_size = 24) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
contribution_2axe <- fviz_contrib(res.pca, choice = "var", axes = 2, top = 10) +
    theme_classic(base_size = 24) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

## generate the file path of the figure to save
file_path <- paste(
    snakemake@output[["pca_figures"]],
    "/",
    "contribution_axe1_quantitative",
    ".svg",
    sep = ""
)
file_path2 <- paste(
    snakemake@output[["pca_figures"]],
    "/",
    "contribution_axe2_quantitative",
    ".svg",
    sep = ""
)

## save the plot of interest
ggsave(
    contribution_1axe,
    filename = file_path,
    device = "svg",
    height = 10,
    width = 12
)
ggsave(
    contribution_2axe,
    filename = file_path2,
    device = "svg",
    height = 10,
    width = 12
)

#####
## generate the variable contribution plot
#####

sample2_plot <- fviz_pca_ind(
    res.pca,
    label = 'none',
    col.ind = as.factor(unlist(clinical_data_dt[, c("condition"), with = F])), 
    palette = c("#00AFBB",  "#FC4E07", "#fcf807", "#3dac21"),
    #palette = c("#ffffff00",  "#ffffff00", "#fcf807", "#3dac21"),

    addEllipses = TRUE,
    #ellipse.type = "confidence",
    legend.title = "Groups",
    repel = TRUE
) + 
    theme_classic(base_size = 24)

## generate the file path of the figure to save
file_path <- paste(
    snakemake@output[["pca_figures"]],
    "/",
    "sample2_plot_quantitative",
    ".svg",
    sep = ""
)

## save the plot of interest
ggsave(
    sample2_plot,
    filename = file_path,
    device = "svg",
    height = 8,
    width = 12
)



































































## perform the pca
res.pca <- prcomp(
    clinical_data_qualitative,
    scale = FALSE,
    na.action = "na.omit"
)


## generate the barchart that show the contribution for each coponent
contribution_barchart <- fviz_eig(res.pca)

## generate the plot related to the sample data
sample_plot <- fviz_pca_ind(
    res.pca,
    col.ind = "cos2", # Colorer par le cos2
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE     
) + 
    theme_classic(base_size = 24)


## generate the file path of the figure to save
file_path <- paste(
    snakemake@output[["pca_figures"]],
    "/",
    "sample_plot_qualitative",
    ".svg",
    sep = ""
)

## save the plot of interest
ggsave(
    sample_plot,
    filename = file_path,
    device = "svg",
    height = 8,
    width = 12
)

## generate variable plot
variable_plot <- fviz_pca_var(
    res.pca,
    col.var = "contrib", 
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE,
    labelsize = 7,
    arrowsize = 1
) + 
    theme_classic(base_size = 24)

## generate the file path of the figure to save
file_path <- paste(
    snakemake@output[["pca_figures"]],
    "/",
    "variable_plot_qualitative",
    ".svg",
    sep = ""
)

## save the plot of interest
ggsave(
    variable_plot,
    filename = file_path,
    device = "svg",
    height = 10,
    width = 12
)

#fviz_pca_biplot(
#    res.pca,
#    repel = TRUE,
#    col.var = "#2E9FDF", 
#    col.ind = "#696969"  
#)



## retrieve the variable contribution for each component
contribution <- PCA(
    clinical_data_qualitative,
    scale.unit = FALSE
)

contribution <- get_pca_var(contribution)

print("----- variable contribution -----")
print(contribution$contrib)


## contribution for each variable for the first two component
contribution_1axe <- fviz_contrib(res.pca, choice = "var", axes = 1, top = 10) +
    theme_classic(base_size = 24) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
contribution_2axe <- fviz_contrib(res.pca, choice = "var", axes = 2, top = 10) +
    theme_classic(base_size = 24) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

## generate the file path of the figure to save
file_path <- paste(
    snakemake@output[["pca_figures"]],
    "/",
    "contribution_axe1_qualitative",
    ".svg",
    sep = ""
)
file_path2 <- paste(
    snakemake@output[["pca_figures"]],
    "/",
    "contribution_axe2_qualitative",
    ".svg",
    sep = ""
)

## save the plot of interest
ggsave(
    contribution_1axe,
    filename = file_path,
    device = "svg",
    height = 10,
    width = 12
)
ggsave(
    contribution_2axe,
    filename = file_path2,
    device = "svg",
    height = 10,
    width = 12
)

#####
## generate the variable contribution plot
#####

sample2_plot <- fviz_pca_ind(
    res.pca,
    label = 'none',
    col.ind = as.factor(unlist(clinical_data_dt[, c("condition"), with = F])), 
    palette = c("#00AFBB",  "#FC4E07", "#fcf807", "#3dac21"),
    #palette = c("#ffffff00",  "#ffffff00", "#fcf807", "#3dac21"),

    addEllipses = TRUE,
    #ellipse.type = "confidence",
    legend.title = "Groups",
    repel = TRUE
) + 
    theme_classic(base_size = 24)

## generate the file path of the figure to save
file_path <- paste(
    snakemake@output[["pca_figures"]],
    "/",
    "sample2_plot_qualitative",
    ".svg",
    sep = ""
)

## save the plot of interest
ggsave(
    sample2_plot,
    filename = file_path,
    device = "svg",
    height = 8,
    width = 12
)





























## perform the pca
res.pca <- prcomp(
    clinical_data_all,
    scale = FALSE,
    na.action = "na.omit"
)


## generate the barchart that show the contribution for each coponent
contribution_barchart <- fviz_eig(res.pca)

## generate the plot related to the sample data
sample_plot <- fviz_pca_ind(
    res.pca,
    col.ind = "cos2", # Colorer par le cos2
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE     
) + 
    theme_classic(base_size = 24)


## generate the file path of the figure to save
file_path <- paste(
    snakemake@output[["pca_figures"]],
    "/",
    "sample_plot_all",
    ".svg",
    sep = ""
)

## save the plot of interest
ggsave(
    sample_plot,
    filename = file_path,
    device = "svg",
    height = 8,
    width = 12
)

## generate variable plot
variable_plot <- fviz_pca_var(
    res.pca,
    col.var = "contrib", 
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE,
    labelsize = 7,
    arrowsize = 1
) + 
    theme_classic(base_size = 24)

## generate the file path of the figure to save
file_path <- paste(
    snakemake@output[["pca_figures"]],
    "/",
    "variable_plot_all",
    ".svg",
    sep = ""
)

## save the plot of interest
ggsave(
    variable_plot,
    filename = file_path,
    device = "svg",
    height = 10,
    width = 12
)

#fviz_pca_biplot(
#    res.pca,
#    repel = TRUE,
#    col.var = "#2E9FDF", 
#    col.ind = "#696969"  
#)



## retrieve the variable contribution for each component
contribution <- PCA(
    clinical_data_all,
    scale.unit = FALSE
)

contribution <- get_pca_var(contribution)

print("----- variable contribution -----")
print(contribution$contrib)


## contribution for each variable for the first two component
contribution_1axe <- fviz_contrib(res.pca, choice = "var", axes = 1, top = 10) +
    theme_classic(base_size = 24) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
contribution_2axe <- fviz_contrib(res.pca, choice = "var", axes = 2, top = 10) +
    theme_classic(base_size = 24) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

## generate the file path of the figure to save
file_path <- paste(
    snakemake@output[["pca_figures"]],
    "/",
    "contribution_axe1_all",
    ".svg",
    sep = ""
)
file_path2 <- paste(
    snakemake@output[["pca_figures"]],
    "/",
    "contribution_axe2_all",
    ".svg",
    sep = ""
)

## save the plot of interest
ggsave(
    contribution_1axe,
    filename = file_path,
    device = "svg",
    height = 10,
    width = 12
)
ggsave(
    contribution_2axe,
    filename = file_path2,
    device = "svg",
    height = 10,
    width = 12
)

#####
## generate the variable contribution plot
#####

sample2_plot <- fviz_pca_ind(
    res.pca,
    label = 'none',
    col.ind = as.factor(unlist(clinical_data_dt[, c("condition"), with = F])), 
    palette = c("#00AFBB",  "#FC4E07", "#fcf807", "#3dac21"),
    #palette = c("#ffffff00",  "#ffffff00", "#fcf807", "#3dac21"),

    addEllipses = TRUE,
    #ellipse.type = "confidence",
    legend.title = "Groups",
    repel = TRUE
) + 
    theme_classic(base_size = 24)

## generate the file path of the figure to save
file_path <- paste(
    snakemake@output[["pca_figures"]],
    "/",
    "sample2_plot_all",
    ".svg",
    sep = ""
)

## save the plot of interest
ggsave(
    sample2_plot,
    filename = file_path,
    device = "svg",
    height = 8,
    width = 12
)

