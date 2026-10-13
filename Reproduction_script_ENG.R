# ============================================================
# REPRODUCTIVE BIOLOGY OF MACKEREL USING R
# Extended classroom script | Teaching manual v2.0
# Run each section in order and discuss the results as a group.
# BD.xlsx must be located in the working directory.
# ============================================================

# 0. PACKAGES -------------------------------------------------
# Install once if any packages are missing:
# install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "car", "sizeMat"))
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)

# 1. DATA IMPORT AND INSPECTION -----------------------------------
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Aulas_ULPGC/2026_2027/Grado_Pesquerias")

getwd()
if (!file.exists("BD.xlsx")) {
  stop("BD.xlsx was not found. Check getwd() or open the project in the correct folder.")
}
Data <- read_excel("BD.xlsx")
head(Data)
str(Data)
names(Data)

# TASK 1. Describe the sample (record your answers).
nrow(Data)
unique(Data$Sex)
sort(unique(Data$Month))
range(Data$TL_mm, na.rm = TRUE)
colSums(is.na(Data))
# Number of observations:
# Sex categories:
# Months represented:
# TL_mm range:
# Variables with relevant missing values:

# 2. VARIABLE PREPARATION ---------------------------------
Data <- Data %>%
  mutate(
    Year = as.factor(Year),
    Sex = as.factor(Sex),
    MS = as.factor(MS),
    across(any_of(c("TL_mm", "TW_g", "EW_g", "GW_g", "GSI")), as.numeric)
  )

# Keep the months in chronological order.
# The levels are based on the numeric month codes in BD.xlsx.
mes_num <- suppressWarnings(as.integer(as.character(Data$Month)))
if (any(is.na(mes_num) & !is.na(Data$Month)) ||
    any(!is.na(mes_num) & !(mes_num %in% 1:12))) {
  stop("Check the coding of Month: numeric values from 1 to 12 are expected.")
}
Data$Month <- factor(mes_num, levels = 1:12,
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
str(Data)

# 3. GONADOSOMATIC INDEX (GSI) -------------------------------
# According to the manual: GSI = GW_g / EW_g * 100.
# The denominator is valid only when EW_g > 0.
Data <- Data %>%
  mutate(GSI = if_else(!is.na(GW_g) & !is.na(EW_g) & EW_g > 0,
                       GW_g / EW_g * 100, NA_real_))
summary(Data$GSI)

# TASK 2. Data quality control.
sum(!is.na(Data$GSI))
sum(is.na(Data$GSI))
sum(!is.na(Data$EW_g) & Data$EW_g <= 0)

# Explain why checking EW_g is important.

# 4. DESCRIPTIVE STATISTICS ----------------------------------
# Overall summary table following Section 6 of the manual.
# min/max are returned as NA when no valid values are available.
resumen_variable <- function(x) {
  n_val <- sum(!is.na(x))
  c(mean = if (n_val > 0) mean(x, na.rm = TRUE) else NA_real_,
    sd   = if (n_val > 1) sd(x, na.rm = TRUE) else NA_real_,
    min  = if (n_val > 0) min(x, na.rm = TRUE) else NA_real_,
    max  = if (n_val > 0) max(x, na.rm = TRUE) else NA_real_,
    n    = n_val)
}

summary_tbl <- Data %>%
  summarise(across(c(TL_mm, TW_g, GW_g),
                   list(mean = ~resumen_variable(.x)["mean"],
                        sd   = ~resumen_variable(.x)["sd"],
                        min  = ~resumen_variable(.x)["min"],
                        max  = ~resumen_variable(.x)["max"],
                        n    = ~resumen_variable(.x)["n"]),
                   .names = "{.col}.{.fn}")) %>%
  pivot_longer(everything(), names_to = c("variable", ".value"),
               names_sep = "\\.") %>%
  mutate(across(c(mean, sd, min, max), ~round(.x, 2)))
print(summary_tbl)

# TASK 3. Prepare an equivalent summary table by sex.
# Compare means and variation between females (F) and males (M).




# 5. NORMALITY AND HOMOGENEITY OF VARIANCE --------------------------------
# Diagnostics for TL_mm (manual, Section 7).
hist(Data$TL_mm, main = "Histogram of TL_mm", xlab = "TL (mm)")
qqnorm(na.omit(Data$TL_mm)); qqline(na.omit(Data$TL_mm))
# Shapiro-Wilk requires between 3 and 5000 valid observations.
shapiro_control <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3 || length(x) > 5000) {
    message("Shapiro-Wilk requires between 3 and 5000 valid observations.")
    return(invisible(NULL))
  }
  shapiro.test(x)
}
shapiro_control(Data$TL_mm)
car::leveneTest(TL_mm ~ Sex, data = Data)

# TASK 4. Repeat the diagnostics for total weight.
hist(Data$TW_g, main = "Histogram of TW_g", xlab = "TW (g)")
qqnorm(na.omit(Data$TW_g)); qqline(na.omit(Data$TW_g))
shapiro_control(Data$TW_g)
car::leveneTest(TW_g ~ Sex, data = Data)
# Describe the Q-Q plot before interpreting the p-values.

# 6. LENGTH-FREQUENCY DISTRIBUTION -------------------------------------
# Histogram for the complete sample.
ggplot(Data, aes(x = TL_mm)) +
  geom_histogram(binwidth = 10, boundary = 0, closed = "left",
                 fill = "grey75", color = "grey20") +
  labs(x = "Total length (mm)", y = "Frequency") + theme_bw()

# Histograms by sex.
ggplot(Data, aes(x = TL_mm)) +
  geom_histogram(binwidth = 10, boundary = 0, closed = "left",
                 fill = "grey80", color = "grey30") +
  facet_wrap(~Sex, scales = "free_y") +
  labs(x = "Total length (mm)", y = "Frequency") + theme_bw()

# Stacked histogram by sex.
ggplot(Data, aes(x = TL_mm, fill = Sex)) +
  geom_histogram(binwidth = 10, boundary = 0, closed = "left",
                 color = "grey15", position = "stack") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Total length (mm)", y = "Frequency") + theme_bw()

# TASK 5. Length-frequency distributions by month.
# Change binwidth from 10 to 20. Compare and interpret the results.





# 7. MATURITY STAGES AND SPAWNING SEASON -------------------
freq_MS <- Data %>%
  count(MS, name = "n") %>%
  mutate(percent = round(100 * n / sum(n), 1))
print(freq_MS)

p_madurez <- ggplot(Data, aes(x = Month, fill = MS)) +
  geom_bar(position = "fill", color = "grey20") +
  scale_y_continuous(name = "Proportion", labels = function(x) paste0(x * 100, "%"),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_x_discrete(name = "Month", drop = TRUE) +
  scale_fill_brewer(palette = "Dark2", name = "Maturity stage") +
  labs(title = "Monthly maturity-stage composition") + theme_bw()
print(p_madurez)
# TASK 6. Identify and justify the months with the strongest evidence of reproductive activity.


# 8. GSI BY MONTH AND SEX ---------------------------------------
# Stages II-V are used as an operational filter in the manual, NOT as a general definition of maturity.
Data_gsi <- Data %>% filter(as.character(MS) %in% c("2", "3", "4", "5"))
GSIFemales <- Data_gsi %>% filter(as.character(Sex) == "F")


# TASK 7. Create the male subset (complete the code below).
GSIMales 


# Compare the number of records for each sex.
nrow(GSIFemales)
nrow(GSIMales)

resumen_gsi <- Data_gsi %>%
  group_by(Sex, Month) %>%
  summarise(mean = if (all(is.na(GSI))) NA_real_ else mean(GSI, na.rm = TRUE),
            sd = sd(GSI, na.rm = TRUE),
            n = sum(!is.na(GSI)), .groups = "drop")
print(resumen_gsi)

# TASK 8. Examine the distributions separately and compare them.
GSI_Females <- ggplot(GSIFemales, aes(x = Month, y = GSI, group = Month)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.4) +
  labs(title = "Females", x = "Month", y = "GSI") + theme_bw()
print(GSI_Females)


### Males








# Alternative: two-panel plot without additional packages.
ggplot(Data_gsi, aes(x = Month, y = GSI, group = Month)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.35, size = 1) +
  facet_wrap(~Sex, ncol = 1) +
  labs(x = "Month", y = "GSI") + theme_bw()
# Question: Do GSI and maturity stages identify the same peak reproductive period?

# 9. L50: MANUAL EXAMPLE AND ADAPTATION EXERCISE ------------
# Complete the sizeMat example before adapting it to BD.xlsx.
# Load the package when you reach this section.
library(sizeMat)
data(matFish, package = "sizeMat")
head(matFish)
my_ogive_fq <- sizeMat::gonad_mature(
  matFish,
  varNames = c("total_length", "stage_mat"),
  inmName = "I", matName = c("II", "III", "IV"),
  method = "fq", niter = 999
)
print(my_ogive_fq)
plot(my_ogive_fq, xlab = "Total length (cm)",
     ylab = "Proportion mature", col = c("blue", "red"),
     onlyOgive = TRUE)

# TASK 9. Complete this section ONLY after justifying the biological classification.
# a) Define which MS stages are classified as immature and mature.
# b) Filter female and male records separately.
# c) Prepare length and maturity-stage data in the format required by sizeMat.
# d) Fit gonad_mature() for each sex and report L50 with its uncertainty.
# e) Check that the estimated length is expressed in mm if using TL_mm.
# Do not automatically classify stages as mature without justifying the maturity scale.

# 10. GROUP SYNTHESIS --------------------------------------
# Main figure:
# Proposed reproductive period:
# Female L50 (with uncertainty):
# Male L50 (with uncertainty):
# Length-frequency distribution:
# Main limitation:
# Implication for fisheries assessment or management:
