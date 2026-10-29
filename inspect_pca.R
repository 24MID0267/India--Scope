# inspect_pca.R
library(dplyr)
library(readr)

pca <- read_csv("data/raw/census_2011_pca.csv", col_types = cols())
cat("PCA rows:", nrow(pca), "\n")
cat("Levels in PCA:", table(pca$Level), "\n")

# Check state names
states <- pca %>% filter(Level == "STATE" | Level == "India") %>% select(State, Name, TOT_P)
print(states, n = 40)

# Check a sample district
dist_sample <- pca %>% filter(Level == "DISTRICT") %>% select(State, District, Name, TOT_P, P_LIT, TOT_M, TOT_F) %>% head(5)
print(dist_sample)
