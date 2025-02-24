library(readxl)
library(writexl)
library(ggplot2)


New_data_for_rice_diversity <- read_excel("New data for rice diversity.xlsx")
View(New_data_for_rice_diversity)
dataFr <- New_data_for_rice_diversity

dataFr$Rep <- as.factor(dataFr$Rep)
dataFr$Geno <- as.factor(dataFr$Geno)

traits <- colnames(dataFr)[3:ncol(dataFr)]

anova_results <- data.frame(Trait = character(), F_value_Genotype = numeric(), F_value_Replication = numeric())

for (trait in traits) {
  anova_model <- aov(as.formula(paste(trait, "~ Geno + Rep")), data = dataFr)
  anova_summary <- summary(anova_model)[[1]]
  
  # Extract F-values for Genotype and Replication
  F_value_Genotype <- anova_summary["Geno", "F value"]
  F_value_Replication <- anova_summary["Rep", "F value"]
  
  # Store the results
  anova_results <- rbind(anova_results, data.frame(Trait = trait, 
                                                   F_value_Genotype = F_value_Genotype, 
                                                   F_value_Replication = F_value_Replication))
}



