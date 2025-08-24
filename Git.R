## Baseline characteristics (including university)

# Load libraries
library(dplyr)
library(gmodels)
library(openxlsx)

# Load your dataset
setwd("C:\\Users\\ey270\\OneDrive - University of Cambridge\\Desktop\\Elias\\RRR")
df <- read.csv("Untitled255555555555.csv")
df$IBSdepend <- factor(df$IBSdepend, labels = c("No IBS", "IBS"))

# Get list of variables to summarise — now INCLUDE 'uni'
vars <- setdiff(names(df), c("IBSdepend"))

# Ensure all variables are factors
df[vars] <- lapply(df[vars], as.factor)

# Create summary table
baseline_table <- data.frame()

for (var in vars) {
  tab <- table(df[[var]], df$IBSdepend)
  total <- margin.table(tab, 1)
  prop_tab <- prop.table(tab, 2) * 100
  
  # Choose appropriate test
  pval <- tryCatch({
    if (all(dim(tab) == c(2, 2))) {
      fisher.test(tab)$p.value
    } else {
      chisq.test(tab, simulate.p.value = TRUE)$p.value
    }
  }, error = function(e) NA)
  
  # Build a long-format output
  for (lvl in rownames(tab)) {
    row <- data.frame(
      Variable = var,
      Level = lvl,
      `No IBS n (%)` = sprintf("%d (%.1f%%)", tab[lvl, "No IBS"], prop_tab[lvl, "No IBS"]),
      `IBS n (%)` = sprintf("%d (%.1f%%)", tab[lvl, "IBS"], prop_tab[lvl, "IBS"]),
      p_value = ifelse(lvl == rownames(tab)[1], signif(pval, 3), "")
    )
    baseline_table <- rbind(baseline_table, row)
  }
}

# Print to console
cat("\n=== Baseline Characteristics by IBS Status (Including University) ===\n")
print(baseline_table, row.names = FALSE)

# Export to Excel
write.xlsx(baseline_table, "IBS_baseline_characteristics_with_uni.xlsx", rowNames = FALSE)
cat("\n✅ Saved to: IBS_baseline_characteristics_with_uni.xlsx\n")

