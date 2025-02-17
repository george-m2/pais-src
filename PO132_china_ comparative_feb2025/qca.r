install.packages("QCA", repos = "http://cran.us.r-project.org") #force mirror when not using either RStudio or Posit cloud
library(QCA)
install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(dplyr)

GDP_data <- data.frame(
  Country = c("China", "Japan", "South Korea", "Singapore"),
  GDP_total = c(17794.78, 4231.85, 1712.79, 501.43)  # total GDP, in USD (tr,PPP)
)

GDPpc_data <- data.frame(
  Country = c("China", "Japan", "South Korea", "Singapore"),
  GDP_pc = c(12621.72, 35569.90, 37079.11, 65422.46)  # GDP per capita, in USD (PPP)
)

hdi <- data.frame(
  Country = c("China", "Japan", "South Korea", "Singapore"),
  HDI = c(0.788, 0.920, 0.929, 0.949)
)

gini <- data.frame(
  Country = c("China", "Japan", "South Korea", "Singapore"),
  Gini = c(35.7, 33.8, 32.9, 39.4)
)

gii <- data.frame(
  Country = c("China", "Japan", "South Korea", "Singapore"),
  GII = c(55.3, 54.6, 58.6, 61.5)
)

debt <- data.frame(
  Country = c("China", "Japan", "South Korea", "Singapore"),
  Debt = c(4230, 9396.6, 873.5, 962.6)  # debt, in bn USD (PPP)
)

# merge datasets by Country
data_fsQCA <- GDP_data %>%
  left_join(GDPpc_data, by = "Country") %>%
  left_join(hdi, by = "Country") %>%
  left_join(gini, by = "Country") %>%
  left_join(gii, by = "Country") %>%
  left_join(debt, by = "Country")

# calibrate into fuzzy-set scores.
data_fsQCA <- data_fsQCA %>% mutate(
  GDP_total_fs = calibrate(GDP_total, thresholds = c(1000, 3000, 10000)),
  GDP_pc_fs    = calibrate(GDP_pc,    thresholds = c(5000, 15000, 40000)),
  Gini_fs      = calibrate(Gini,      thresholds = c(30, 35, 40)),
  GII_fs       = calibrate(GII,       thresholds = c(50, 55, 60)),
   # lower debt is better, so reverse the order of thresholds
  Debt_fs      = calibrate(Debt,      thresholds = c(8000, 3000, 1000)) 
)

# composite outcome that combines GDP and HDI:
# firstly, normalise using min-max scaling
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

data_fsQCA <- data_fsQCA %>% mutate(
  GDP_total_norm = normalize(GDP_total),
  HDI_norm       = normalize(HDI)
)

# ...then combine for a simple average.
data_fsQCA <- data_fsQCA %>% mutate(
  EconStrength = (GDP_total_norm + HDI_norm) / 2
)

# composite outcome (EconStrength).
# thresholds, used are:
#   - full non-membership (0): EconStrength <= 0.3
#   - crossover (0.5): EconStrength = 0.5
#   - full membership (1): EconStrength >= 0.7

data_fsQCA <- data_fsQCA %>% mutate(
  EconStrength_fs = calibrate(EconStrength, thresholds = c(0.3, 0.5, 0.7))
)
print(data_fsQCA)

# truth table
tt <- truthTable(data_fsQCA,
                 outcome = "EconStrength_fs",
                 conditions = c("GDP_total_fs", "GDP_pc_fs", "Gini_fs", "GII_fs", "Debt_fs"),
                 incl.cut = 0.8,
                 n.cut = 1,
                 show.cases = TRUE)

print(tt)

# derive the solution through logical minimisation from the truth table
solution <- eqmcc(tt, details = TRUE)
print(solution)
