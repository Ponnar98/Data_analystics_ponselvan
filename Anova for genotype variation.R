library(readxl)
library(writexl)

New_data_for_rice_diversity <- read_excel("C:/Users/PONSELVAN/OneDrive/Documents/Data analysis/New data for rice diversity.xlsx")


dataFr <- New_data_for_rice_diversity

dataFr$Rep <- as.factor(dataFr$Rep)
dataFr$Geno <- as.factor(dataFr$Geno)

traits <- colnames(dataFr)[3:ncol(dataFr)]

traits
anova_results <- list()

as.formula(paste('DFF', "~ Geno + Rep"))

aov()
for (trait in traits) {
  anova_model <- aov(as.formula(paste(trait, "~ Geno + Rep")), data = dataFr)
  anova_summery <- as.data.frame(summary(anova_model)[[1]])
  anova_summery$Trait <- trait
  anova_results[[trait]] <- anova_summery
}

anova_combined <-do.call(rbind,anova_results) 

write_xlsx(anova_combined, "anova_results.xlsx")

