# ============================================================================
# REPRODUCTIVE BIOLOGY OF PATELLA WITH R
# Student working script
# Author: Joana Reis Vasconcelos
#
# This script follows the analyses in the practical guide step by step and
# prioritizes simple, readable code that is easy to modify during the practical.
# ============================================================================

# ----------------------------------------------------------------------------
# 0. PACKAGES
# ----------------------------------------------------------------------------
# Install in one step any packages that are not yet installed.
packages <- c(
  "readxl", "dplyr", "ggplot2", "ggpubr", "car",
  "dunn.test", "janitor", "cowplot", "rcompanion", "writexl"
)

new_packages <- packages[!packages %in% rownames(installed.packages())]
if (length(new_packages) > 0) install.packages(new_packages)

# Load the packages used in the main analysis.
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(car)
library(dunn.test)
library(janitor)
library(cowplot)

# ----------------------------------------------------------------------------
# 1. WORKING DIRECTORY AND DATA IMPORT
# ----------------------------------------------------------------------------
# Replace the example path with the folder containing your project.
# setwd("~/path/to/your/folder/Patella")
# IMPORTANT: set as your working directory the folder containing the Excel files.
# In RStudio, for example, use Session > Set Working Directory > Choose Directory.
getwd()

# Check that the Excel files are in the working directory.
list.files(pattern = "\\.xlsx$")

# Import the two datasets used in the practical.
BD_fecundidad <- read_xlsx("BD_fecundidad.xlsx")
BD_Osize <- read_xlsx("Oocyte_size.xlsx")

# Quick inspection.
str(BD_fecundidad)
head(BD_fecundidad)
str(BD_Osize)
head(BD_Osize)

# Convert categorical columns to factors.
# Adjust 2:7 if the column order in your files is different.
BD_fecundidad[2:7] <- lapply(BD_fecundidad[2:7], as.factor)
BD_fecundidad$Maturity_stage <- as.factor(BD_fecundidad$Maturity_stage)
BD_fecundidad$Maturity_stage_Prusina <-
  as.factor(BD_fecundidad$Maturity_stage_Prusina)

BD_Osize[2:7] <- lapply(BD_Osize[2:7], as.factor)
BD_Osize$Maturity_stage <- as.factor(BD_Osize$Maturity_stage)
BD_Osize$Maturity_stage_Prusina <-
  as.factor(BD_Osize$Maturity_stage_Prusina)

# ----------------------------------------------------------------------------
# 2. DESCRIPTIVE SUMMARY
# ----------------------------------------------------------------------------
# Shell length by species.
resultsSL <- BD_fecundidad %>%
  group_by(Species) %>%
  summarise(
    mean = mean(SL_mm, na.rm = TRUE),
    sd   = sd(SL_mm, na.rm = TRUE),
    min  = min(SL_mm, na.rm = TRUE),
    max  = max(SL_mm, na.rm = TRUE),
    n    = n()
  )
resultsSL

# Shell length by species and region.
resultsSL1 <- BD_fecundidad %>%
  group_by(Species, North_South) %>%
  summarise(
    mean = mean(SL_mm, na.rm = TRUE),
    sd   = sd(SL_mm, na.rm = TRUE),
    min  = min(SL_mm, na.rm = TRUE),
    max  = max(SL_mm, na.rm = TRUE),
    n    = n()
  )
resultsSL1

# Shell length by species and substrate.
resultsSL2 <- BD_fecundidad %>%
  group_by(Species, Substrate) %>%
  summarise(
    mean = mean(SL_mm, na.rm = TRUE),
    sd   = sd(SL_mm, na.rm = TRUE),
    min  = min(SL_mm, na.rm = TRUE),
    max  = max(SL_mm, na.rm = TRUE),
    n    = n()
  )
resultsSL2

# Number of vitellogenic oocytes.
ON_VO <- BD_fecundidad %>%
  group_by(Species, North_South, Substrate) %>%
  summarise(
    mean = if (all(is.na(Number_vitelogenic))) NA_real_ else mean(Number_vitelogenic, na.rm = TRUE),
    sd   = if (all(is.na(Number_vitelogenic))) NA_real_ else sd(Number_vitelogenic, na.rm = TRUE),
    min  = if (all(is.na(Number_vitelogenic))) NA_real_ else min(Number_vitelogenic, na.rm = TRUE),
    max  = if (all(is.na(Number_vitelogenic))) NA_real_ else max(Number_vitelogenic, na.rm = TRUE),
    n    = sum(!is.na(Number_vitelogenic)),
    .groups = "drop"
  )

ON_VO

# Number of previtellogenic oocytes.
ON_PO <- BD_fecundidad %>%
  group_by(Species, North_South, Substrate) %>%
  summarise(
    mean = if (all(is.na(Number_pre_vitelogenic))) NA_real_ else mean(Number_pre_vitelogenic, na.rm = TRUE),
    sd   = if (all(is.na(Number_pre_vitelogenic))) NA_real_ else sd(Number_pre_vitelogenic, na.rm = TRUE),
    min  = if (all(is.na(Number_pre_vitelogenic))) NA_real_ else min(Number_pre_vitelogenic, na.rm = TRUE),
    max  = if (all(is.na(Number_pre_vitelogenic))) NA_real_ else max(Number_pre_vitelogenic, na.rm = TRUE),
    n    = sum(!is.na(Number_pre_vitelogenic)),
    .groups = "drop"
  )

ON_PO

# Oocyte size by species and oocyte type.
OS_VO <- BD_Osize %>%
  group_by(Species, Oocyte_type) %>%
  summarise(
    mean = mean(Size_microns, na.rm = TRUE),
    sd   = sd(Size_microns, na.rm = TRUE),
    min  = min(Size_microns, na.rm = TRUE),
    max  = max(Size_microns, na.rm = TRUE),
    n    = n()
  )
OS_VO

# ----------------------------------------------------------------------------
# 3. NORMALITY, HOMOGENEITY AND COMPARISONS AMONG MONTHS
# ----------------------------------------------------------------------------
# P. crenata: oocyte size.
OS_Pcre <- subset(BD_Osize, Species == "Patella crenata")
PO_PC <- subset(OS_Pcre, Oocyte_type == "PO")
VO_PC <- subset(OS_Pcre, Oocyte_type == "VO")

# Q-Q plots.
ggqqplot(PO_PC$Size_microns, title = "Q-Q plot PO (P. crenata)")
ggqqplot(VO_PC$Size_microns, title = "Q-Q plot VO (P. crenata)")

# Shapiro-Wilk.
# If a variable has more than 5000 observations, use a reproducible subsample.
if (sum(!is.na(PO_PC$Size_microns)) <= 5000) {
  shapiro.test(PO_PC$Size_microns)
}

set.seed(1)
vo_values <- VO_PC$Size_microns[!is.na(VO_PC$Size_microns)]
if (length(vo_values) > 5000) {
  vo_values <- sample(vo_values, 5000)
}
shapiro.test(vo_values)

# Treat month as a factor.
PO_PC$Month_name <- factor(PO_PC$Month_name)
VO_PC$Month_name <- factor(VO_PC$Month_name)

# Levene test.
leveneTest(Size_microns ~ Month_name, data = PO_PC)
leveneTest(Size_microns ~ Month_name, data = VO_PC)

# Kruskal-Wallis.
kw_PO_size_PC <- kruskal.test(Size_microns ~ Month_name, data = PO_PC)
kw_VO_size_PC <- kruskal.test(Size_microns ~ Month_name, data = VO_PC)
kw_PO_size_PC
kw_VO_size_PC

# Dunn test. Interpret mainly when Kruskal-Wallis is significant.
dunn_PO_size_PC <- dunn.test(
  x = PO_PC$Size_microns,
  g = PO_PC$Month_name,
  method = "holm",
  alpha = 0.05,
  table = TRUE,
  altp = TRUE
)

dunn_VO_size_PC <- dunn.test(
  x = VO_PC$Size_microns,
  g = VO_PC$Month_name,
  method = "holm",
  alpha = 0.05,
  table = TRUE,
  altp = TRUE
)

# IMPORTANT:
# If several oocytes in BD_Osize belong to the same individual, these tests
# on Size_microns should be considered exploratory unless the data are first
# summarized by individual or a model accounting for the hierarchy is used.

# =============================================================================
# TASK 4. STATISTICAL DIAGNOSIS IN P. ASPERA
# =============================================================================
# Repeat for P. aspera the workflow shown above for P. crenata.
#
# Evaluate separately:
#   a) PO and VO size using BD_Osize
#   b) PO and VO number per individual using BD_fecundidad
#
# For oocyte size:
#   - create OS_Pasp, PO_PA and VO_PA
#   - produce Q-Q plots
#   - evaluate homogeneity using leveneTest()
#   - apply Kruskal-Wallis when justified
#   - perform Dunn tests only when appropriate
#
# For oocyte number:
#   - select Late Active, Ripe and Spawning
#   - compare Number_pre_vitelogenic and Number_vitelogenic among months
#
# Before making inference with Size_microns, identify the independent sampling
# unit. If several measurements come from the same individual and you cannot
# verify that structure, interpret the analysis as exploratory.
#
# DELIVERABLE:
# one Q-Q plot, the global result when appropriate, the adjusted post hoc when
# appropriate, and a short interpretive comment.
#
# WRITE YOUR CODE FOR P. ASPERA HERE:

# Number of oocytes per individual: P. crenata.
PC_num_test <- BD_fecundidad %>%
  filter(
    Species == "Patella crenata",
    Maturity_stage_Prusina %in% c("Late Active", "Ripe", "Spawning")
  )
PC_num_test$Month_name <- factor(PC_num_test$Month_name)

kruskal.test(Number_pre_vitelogenic ~ Month_name, data = PC_num_test)
kruskal.test(Number_vitelogenic ~ Month_name, data = PC_num_test)

# The comparison of oocyte number in P. aspera is part of TASK 4.
# Write that part of the code in the space indicated above.

# ----------------------------------------------------------------------------
# 4. MATURITY STAGES AND SEASONAL COVERAGE
# ----------------------------------------------------------------------------
Pcrenata <- subset(BD_fecundidad, Species == "Patella crenata")

# Overall frequency of maturity stages.
Pcrenata %>%
  tabyl(Maturity_stage_Prusina) %>%
  adorn_pct_formatting(digits = 1)

# Frequency by month and maturity stage.
Pcrenata %>% tabyl(Month_name, Maturity_stage_Prusina)

# Proportion of maturity stages by month.
month_order_maturity <- c(
  "September", "October", "January", "February", "March"
)

PC_plotdata <- Pcrenata %>%
  filter(!is.na(Maturity_stage_Prusina)) %>%
  mutate(Month_name = factor(Month_name, levels = month_order_maturity)) %>%
  filter(!is.na(Month_name))

p_prop_PC <- ggplot(
  PC_plotdata,
  aes(x = Month_name, fill = factor(Maturity_stage_Prusina))
) +
  geom_bar(position = "fill") +
  theme_bw() +
  labs(
    title = expression(italic("Patella crenata")),
    x = NULL,
    y = "Proportion",
    fill = "Maturity stage"
  ) +
  scale_y_continuous(labels = function(x) paste0(round(x * 100), "%"))
p_prop_PC

# =============================================================================
# TASK 5. SEASONAL COVERAGE AND MATURITY IN P. ASPERA
# =============================================================================
# For P. aspera, generate:
#   - the overall table of maturity stages
#   - the frequency table by month and maturity stage
#   - the proportion plot by month
#
# Keep month_order_maturity to order the months.
#
# DELIVERABLE:
# table, figure, and a comment indicating which months are more informative
# and which should be interpreted with caution.
#
# WRITE YOUR CODE FOR P. ASPERA HERE:

# ----------------------------------------------------------------------------
# 5. LINE OF EVIDENCE 1: PO-VO HIATUS
# ----------------------------------------------------------------------------
month_order <- c("October", "January", "February", "March")
cols_ot <- c("PO" = "#2C7FB8", "VO" = "#41B6C4")

# P. crenata.
OS_Pcre <- subset(BD_Osize, Species == "Patella crenata")

PC_faceted <- OS_Pcre %>%
  filter(Month_name %in% month_order, !is.na(Oocyte_type)) %>%
  mutate(Month_name = factor(Month_name, levels = month_order))

PC_facet_plot <- ggplot(
  PC_faceted,
  aes(x = Size_microns, fill = Oocyte_type, colour = Oocyte_type)
) +
  geom_histogram(
    binwidth = 5,
    position = "identity",
    alpha = 0.60,
    colour = "grey25"
  ) +
  facet_wrap(~Month_name, ncol = 2, drop = FALSE) +
  theme_bw() +
  labs(
    title = expression(italic("Patella crenata")),
    x = "Oocyte size (microns)",
    y = "Count",
    fill = NULL
  ) +
  scale_fill_manual(values = cols_ot) +
  scale_colour_manual(values = cols_ot, guide = "none")
PC_facet_plot

hiatus_PC <- PC_faceted %>%
  group_by(Month_name) %>%
  summarise(
    max_PO = max(Size_microns[Oocyte_type == "PO"], na.rm = TRUE),
    min_VO = min(Size_microns[Oocyte_type == "VO"], na.rm = TRUE),
    hiatus = max_PO < min_VO
  )
hiatus_PC

# =============================================================================
# TASK 6. PO-VO HIATUS IN P. ASPERA
# =============================================================================
# Repeat for P. aspera the analysis shown above for P. crenata.
#
# You should:
#   - keep month_order
#   - use histograms faceted by month
#   - keep binwidth = 5
#   - calculate max_PO, min_VO and hiatus for each month
#
# Check that PO and VO information exists before calculating maxima or minima,
# to avoid Inf or -Inf when data are missing.
#
# DELIVERABLE:
# figure, monthly summary table, and a comment distinguishing no overlap,
# partial overlap, or insufficient information.
#
# WRITE YOUR CODE FOR P. ASPERA HERE:

# ----------------------------------------------------------------------------
# 6. LINE OF EVIDENCE 2: NUMBER OF OOCYTES BY MONTH
# ----------------------------------------------------------------------------
PC_num <- BD_fecundidad %>%
  filter(
    Species == "Patella crenata",
    Maturity_stage_Prusina %in% c("Late Active", "Ripe", "Spawning"),
    Month_name %in% month_order
  ) %>%
  mutate(Month_name = factor(Month_name, levels = month_order))

BOXplot_PO_number_PC <- ggplot(
  PC_num,
  aes(x = Month_name, y = Number_pre_vitelogenic)
) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(
    position = position_jitter(width = 0.18, height = 0),
    color = cols_ot["PO"], size = 2.3, alpha = 0.45
  ) +
  labs(title = "Previtellogenic", x = NULL, y = "Oocyte number") +
  theme_bw()

BOXplot_VO_number_PC <- ggplot(
  PC_num,
  aes(x = Month_name, y = Number_vitelogenic)
) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(
    position = position_jitter(width = 0.18, height = 0),
    color = cols_ot["VO"], size = 2.3, alpha = 0.45
  ) +
  labs(title = "Vitellogenic", x = NULL, y = "Oocyte number") +
  theme_bw()

Fig4_PC <- ggarrange(
  BOXplot_PO_number_PC,
  BOXplot_VO_number_PC,
  ncol = 1,
  labels = c("A", "B")
)
Fig4_PC

# =============================================================================
# TASK 7. NUMBER OF PO AND VO BY MONTH IN P. ASPERA
# =============================================================================
# Repeat for P. aspera the figure produced above for P. crenata.
#
# Select:
#   - Late Active, Ripe and Spawning
#   - the months included in month_order
#
# Plot separately:
#   - Number_pre_vitelogenic
#   - Number_vitelogenic
#
# Build two panels with boxplot + jitter and combine them into one figure.
#
# DELIVERABLE:
# figure and a comment on the monthly dynamics of PO and VO.
#
# WRITE YOUR CODE FOR P. ASPERA HERE:

# ----------------------------------------------------------------------------
# 7. LINE OF EVIDENCE 3: OOCYTE SIZE BY MONTH
# ----------------------------------------------------------------------------
# P. crenata.
PO_PC1 <- OS_Pcre %>%
  filter(Oocyte_type == "PO", Month_name %in% month_order) %>%
  mutate(Month_name = factor(Month_name, levels = month_order)) %>%
  filter(!is.na(Size_microns))

VO_PC1 <- OS_Pcre %>%
  filter(Oocyte_type == "VO", Month_name %in% month_order) %>%
  mutate(Month_name = factor(Month_name, levels = month_order)) %>%
  filter(!is.na(Size_microns))

BOXplot_PO_PC <- ggplot(PO_PC1, aes(x = Month_name, y = Size_microns)) +
  geom_boxplot(outlier.shape = NA, colour = "grey35") +
  geom_jitter(
    position = position_jitter(width = 0.18),
    colour = cols_ot["PO"], size = 2, alpha = 0.35
  ) +
  labs(x = NULL, y = "Oocyte size (microns)") +
  theme_bw()

BOXplot_VO_PC <- ggplot(VO_PC1, aes(x = Month_name, y = Size_microns)) +
  geom_boxplot(outlier.shape = NA, colour = "grey35") +
  geom_jitter(
    position = position_jitter(width = 0.18),
    colour = cols_ot["VO"], size = 2, alpha = 0.35
  ) +
  labs(x = NULL, y = "Oocyte size (microns)") +
  theme_bw()

Fig5_PC <- plot_grid(
  BOXplot_PO_PC,
  BOXplot_VO_PC,
  ncol = 1,
  labels = c("A", "B"),
  align = "v"
)
Fig5_PC

# =============================================================================
# TASK 8. OOCYTE SIZE DISTRIBUTION IN P. ASPERA
# =============================================================================
# Repeat for P. aspera the figure produced above for P. crenata.
#
# Create separate subsets for PO and VO, keep month_order, and remove missing
# Size_microns values.
#
# Build:
#   - boxplot + jitter for PO
#   - boxplot + jitter for VO
#   - one combined figure
#
# Describe the monthly shift in the VO distribution and compare it with PO.
# Remember that several points may correspond to the same individual.
#
# DELIVERABLE:
# figure and a 3-4 line comment.
#
# WRITE YOUR CODE FOR P. ASPERA HERE:

# ----------------------------------------------------------------------------
# 8. LINE OF EVIDENCE 4: ATRESIA
# ----------------------------------------------------------------------------
# P. crenata.
PC_atr <- BD_fecundidad %>%
  filter(
    Species == "Patella crenata",
    Month_name %in% month_order,
    Maturity_stage_Prusina %in% c("Late Active", "Ripe", "Spawning")
  ) %>%
  mutate(Month_name = factor(Month_name, levels = month_order))

# Monthly prevalence.
prev_PC <- PC_atr %>%
  group_by(Month_name) %>%
  summarise(
    n_evaluable = sum(!is.na(Number_Atresia)),
    n_atresia = sum(Number_Atresia > 0, na.rm = TRUE),
    prevalence_pct = 100 * n_atresia / n_evaluable
  )
prev_PC

# Relative intensity.
PC_atr_int <- PC_atr %>%
  filter(!is.na(Relative_intensity_atresia))

ggqqplot(
  PC_atr_int$Relative_intensity_atresia,
  title = "Q-Q: Relative intensity (P. crenata)"
)

leveneTest(Relative_intensity_atresia ~ Month_name, data = PC_atr_int)
kw_atresia_PC <- kruskal.test(
  Relative_intensity_atresia ~ Month_name,
  data = PC_atr_int
)
kw_atresia_PC

# Run and interpret Dunn mainly if Kruskal-Wallis is significant.
dunn_atresia_PC <- dunn.test(
  x = PC_atr_int$Relative_intensity_atresia,
  g = PC_atr_int$Month_name,
  method = "holm",
  alpha = 0.05,
  table = TRUE,
  altp = TRUE
)

BOXplot_Atresia_PC <- ggplot(
  PC_atr_int,
  aes(x = Month_name, y = Relative_intensity_atresia)
) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(
    position = position_jitter(width = 0.18, height = 0),
    color = "#238B8D", size = 2.4, alpha = 0.45
  ) +
  labs(
    title = expression(italic("Patella crenata")),
    x = NULL,
    y = "Relative intensity of atresia"
  ) +
  theme_bw()
BOXplot_Atresia_PC

# =============================================================================
# TASK 9. PREVALENCE AND INTENSITY OF ATRESIA IN P. ASPERA
# =============================================================================
# Repeat for P. aspera the workflow shown above for P. crenata.
#
# PREVALENCE
# For each month calculate:
#   - number of evaluable individuals
#   - number of individuals with Number_Atresia > 0
#   - prevalence (%)
#
# RELATIVE INTENSITY
#   - remove NA values from Relative_intensity_atresia
#   - produce a Q-Q plot
#   - evaluate homogeneity using leveneTest()
#   - apply Kruskal-Wallis
#   - perform Dunn only when appropriate
#   - build the boxplot + jitter
#
# DELIVERABLE:
# prevalence table, intensity plot, and statistical results when appropriate.
# Compare whether prevalence and intensity tell the same biological story.
#
# WRITE YOUR CODE FOR P. ASPERA HERE:

# ----------------------------------------------------------------------------
# 9. OPTIONAL: SAVE FIGURES
# ----------------------------------------------------------------------------
# Uncomment only the figures you want to export.
# ggsave("Figure3_PC_hiatus.jpeg", PC_facet_plot, width = 8, height = 7, dpi = 300)
# ggsave("Figure4_PC_number.jpeg", Fig4_PC, width = 7, height = 9, dpi = 300)
# ggsave("Figure5_PC_size.jpeg", Fig5_PC, width = 7, height = 9, dpi = 300)
# ggsave("Figure6_PC_atresia.jpeg", BOXplot_Atresia_PC, width = 8, height = 5, dpi = 300)


# =============================================================================
# TASK 10. FINAL EVIDENCE REPORT
# =============================================================================
# Integrate the different lines of evidence for both species:
#   - PO-VO hiatus
#   - cohort dynamics
#   - VO size
#   - atresia
#   - seasonal maturity pattern
#
# For each species, write a 150-200 word conclusion on fecundity strategy.
# Use at least three lines of evidence, indicate one sampling or analytical
# limitation, and clearly separate observation from biological interpretation.
#
# Do not write code here unless you want to create your own synthesis table.
