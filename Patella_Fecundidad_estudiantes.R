# ============================================================================
# BIOLOGIA REPRODUCTIVA DE PATELLA CON R
# Script de trabajo para estudiantes
# Author: Joana Reis Vasconcelos
#
# El script sigue paso a paso los analisis de la guia y prioriza un codigo
# sencillo, legible y facil de modificar durante la practica.
# ============================================================================

# ----------------------------------------------------------------------------
# 0. PAQUETES
# ----------------------------------------------------------------------------
# Instalar de una sola vez los paquetes que todavia no esten instalados.
packages <- c(
  "readxl", "dplyr", "ggplot2", "ggpubr", "car",
  "dunn.test", "janitor", "cowplot", "rcompanion", "writexl"
)

new_packages <- packages[!packages %in% rownames(installed.packages())]
if (length(new_packages) > 0) install.packages(new_packages)

# Cargar los paquetes utilizados en el analisis principal.
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(car)
library(dunn.test)
library(janitor)
library(cowplot)

# ----------------------------------------------------------------------------
# 1. CARPETA DE TRABAJO E IMPORTACION
# ----------------------------------------------------------------------------
# Cambia la ruta siguiente por la carpeta de tu proyecto.
# setwd("~/ruta/a/tu/carpeta/Patella_Master")
# IMPORTANTE: fija como directorio de trabajo la carpeta que contiene los Excel.
# Por ejemplo, en RStudio puedes usar Session > Set Working Directory > Choose Directory.
getwd()

# Comprobar que los archivos Excel estan en la carpeta data
list.files(pattern = "\\.xlsx$")

# Importar las dos bases utilizadas en la practica
BD_fecundidad <- read_xlsx("BD_fecundidad.xlsx")
BD_Osize <- read_xlsx("Oocyte_size.xlsx")

# Inspeccion rapida
str(BD_fecundidad)
head(BD_fecundidad)
str(BD_Osize)
head(BD_Osize)

# Convertir a factor las columnas categoricas.
# Ajusta 2:7 si el orden de columnas de tus archivos es diferente.
BD_fecundidad[2:7] <- lapply(BD_fecundidad[2:7], as.factor)
BD_fecundidad$Maturity_stage <- as.factor(BD_fecundidad$Maturity_stage)
BD_fecundidad$Maturity_stage_Prusina <-
  as.factor(BD_fecundidad$Maturity_stage_Prusina)

BD_Osize[2:7] <- lapply(BD_Osize[2:7], as.factor)
BD_Osize$Maturity_stage <- as.factor(BD_Osize$Maturity_stage)
BD_Osize$Maturity_stage_Prusina <-
  as.factor(BD_Osize$Maturity_stage_Prusina)

# ----------------------------------------------------------------------------
# 2. RESUMEN DESCRIPTIVO
# ----------------------------------------------------------------------------
# Longitud de concha por especie
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

# Longitud de concha por especie y region
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

# Longitud de concha por especie y sustrato
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

# Numero de ovocitos vitelogenicos
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

# Numero de ovocitos previtelogenicos
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

# Tamano de ovocitos por especie y tipo
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
# 3. NORMALIDAD, HOMOGENEIDAD Y COMPARACIONES ENTRE MESES
# ----------------------------------------------------------------------------
# P. crenata: tamano de ovocitos
OS_Pcre <- subset(BD_Osize, Species == "Patella crenata")
PO_PC <- subset(OS_Pcre, Oocyte_type == "PO")
VO_PC <- subset(OS_Pcre, Oocyte_type == "VO")

# Q-Q plots
ggqqplot(PO_PC$Size_microns, title = "Q-Q plot PO (P. crenata)")
ggqqplot(VO_PC$Size_microns, title = "Q-Q plot VO (P. crenata)")

# Shapiro-Wilk
# Si una variable supera 5000 observaciones, usar una submuestra reproducible.
if (sum(!is.na(PO_PC$Size_microns)) <= 5000) {
  shapiro.test(PO_PC$Size_microns)
}

set.seed(1)
vo_values <- VO_PC$Size_microns[!is.na(VO_PC$Size_microns)]
if (length(vo_values) > 5000) {
  vo_values <- sample(vo_values, 5000)
}
shapiro.test(vo_values)

# Mes como factor
PO_PC$Month_name <- factor(PO_PC$Month_name)
VO_PC$Month_name <- factor(VO_PC$Month_name)

# Levene
leveneTest(Size_microns ~ Month_name, data = PO_PC)
leveneTest(Size_microns ~ Month_name, data = VO_PC)

# Kruskal-Wallis
kw_PO_size_PC <- kruskal.test(Size_microns ~ Month_name, data = PO_PC)
kw_VO_size_PC <- kruskal.test(Size_microns ~ Month_name, data = VO_PC)
kw_PO_size_PC
kw_VO_size_PC

# Dunn. Interpretar principalmente cuando Kruskal-Wallis sea significativo.
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

# IMPORTANTE:
# Si varios ovocitos de BD_Osize pertenecen al mismo individuo, estas pruebas
# sobre Size_microns deben considerarse exploratorias salvo que primero se
# resuma por individuo o se use un modelo que represente la jerarquia.

# =============================================================================
# TAREA 4. DIAGNOSTICO ESTADISTICO EN P. ASPERA
# =============================================================================
# Repite para P. aspera el flujo anterior mostrado con P. crenata.
#
# Evalua por separado:
#   a) el tamano de PO y VO con BD_Osize
#   b) el numero de PO y VO por individuo con BD_fecundidad
#
# Para el tamano:
#   - crea OS_Pasp, PO_PA y VO_PA
#   - realiza Q-Q plots
#   - evalua homogeneidad con leveneTest()
#   - aplica Kruskal-Wallis cuando sea justificable
#   - realiza Dunn solamente cuando proceda
#
# Para el numero:
#   - selecciona Late Active, Ripe y Spawning
#   - compara Number_pre_vitelogenic y Number_vitelogenic entre meses
#
# Antes de realizar inferencia con Size_microns, identifica la unidad
# independiente. Si varias medidas proceden del mismo individuo y no puedes
# verificar esa estructura, interpreta ese analisis como exploratorio.
#
# ENTREGA:
# un Q-Q plot, el resultado global cuando proceda, el post hoc ajustado cuando
# proceda y un breve comentario interpretativo.
#
# ESCRIBE AQUI TU CODIGO PARA P. ASPERA:










# Numero de ovocitos por individuo: P. crenata
PC_num_test <- BD_fecundidad %>%
  filter(
    Species == "Patella crenata",
    Maturity_stage_Prusina %in% c("Late Active", "Ripe", "Spawning")
  )
PC_num_test$Month_name <- factor(PC_num_test$Month_name)

kruskal.test(Number_pre_vitelogenic ~ Month_name, data = PC_num_test)
kruskal.test(Number_vitelogenic ~ Month_name, data = PC_num_test)

# La comparacion del numero de ovocitos de P. aspera forma parte de la TAREA 4.
# Escribe esa parte del codigo en el espacio indicado anteriormente.

# ----------------------------------------------------------------------------
# 4. ESTADIOS DE MADUREZ Y COBERTURA ESTACIONAL
# ----------------------------------------------------------------------------
Pcrenata <- subset(BD_fecundidad, Species == "Patella crenata")

# Frecuencia global de estadios
Pcrenata %>%
  tabyl(Maturity_stage_Prusina) %>%
  adorn_pct_formatting(digits = 1)

# Frecuencia por mes y estadio
Pcrenata %>% tabyl(Month_name, Maturity_stage_Prusina)

# Proporcion de estadios por mes
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
# TAREA 5. COBERTURA ESTACIONAL Y MADUREZ EN P. ASPERA
# =============================================================================
# Genera para P. aspera:
#   - la tabla global de estadios de madurez
#   - la tabla de frecuencia por mes y estadio
#   - el grafico de proporciones por mes
#
# Mantén month_order_maturity para ordenar los meses.
#
# ENTREGA:
# tabla, figura y un comentario indicando que meses son mas informativos y
# cuales deben interpretarse con cautela.
#
# ESCRIBE AQUI TU CODIGO PARA P. ASPERA:










# ----------------------------------------------------------------------------
# 5. LINEA DE EVIDENCIA 1: HIATO PO-VO
# ----------------------------------------------------------------------------
month_order <- c("October", "January", "February", "March")
cols_ot <- c("PO" = "#2C7FB8", "VO" = "#41B6C4")

# P. crenata
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
# TAREA 6. HIATO PO-VO EN P. ASPERA
# =============================================================================
# Repite para P. aspera el analisis realizado arriba para P. crenata.
#
# Debes:
#   - mantener month_order
#   - usar histogramas facetados por mes
#   - mantener binwidth = 5
#   - calcular por mes max_PO, min_VO y hiatus
#
# Comprueba que exista informacion de PO y VO antes de calcular maximos o
# minimos, para evitar Inf o -Inf cuando falten datos.
#
# ENTREGA:
# figura, cuadro por mes y comentario sobre ausencia de solapamiento,
# solapamiento parcial o informacion insuficiente.
#
# ESCRIBE AQUI TU CODIGO PARA P. ASPERA:














# ----------------------------------------------------------------------------
# 6. LINEA DE EVIDENCIA 2: NUMERO DE OVOCITOS POR MES
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
# TAREA 7. NUMERO DE PO Y VO POR MES EN P. ASPERA
# =============================================================================
# Repite para P. aspera la figura realizada arriba para P. crenata.
#
# Selecciona:
#   - Late Active, Ripe y Spawning
#   - los meses de month_order
#
# Representa por separado:
#   - Number_pre_vitelogenic
#   - Number_vitelogenic
#
# Construye dos paneles con boxplot + jitter y combinalos en una figura.
#
# ENTREGA:
# figura y comentario sobre la dinamica mensual de PO y VO.
#
# ESCRIBE AQUI TU CODIGO PARA P. ASPERA:














# ----------------------------------------------------------------------------
# 7. LINEA DE EVIDENCIA 3: TAMANO DE OVOCITOS POR MES
# ----------------------------------------------------------------------------
# P. crenata
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
# TAREA 8. DISTRIBUCION DEL TAMANO DE OVOCITOS EN P. ASPERA
# =============================================================================
# Repite para P. aspera la figura realizada arriba para P. crenata.
#
# Crea subconjuntos separados para PO y VO, conserva month_order y elimina
# los valores ausentes de Size_microns.
#
# Construye:
#   - boxplot + jitter para PO
#   - boxplot + jitter para VO
#   - una figura combinada
#
# Describe el desplazamiento mensual de la distribucion de VO y comparalo con
# PO. Recuerda que varios puntos pueden corresponder al mismo individuo.
#
# ENTREGA:
# figura y comentario de 3-4 lineas.
#
# ESCRIBE AQUI TU CODIGO PARA P. ASPERA:












# ----------------------------------------------------------------------------
# 8. LINEA DE EVIDENCIA 4: ATRESIA
# ----------------------------------------------------------------------------
# P. crenata
PC_atr <- BD_fecundidad %>%
  filter(
    Species == "Patella crenata",
    Month_name %in% month_order,
    Maturity_stage_Prusina %in% c("Late Active", "Ripe", "Spawning")
  ) %>%
  mutate(Month_name = factor(Month_name, levels = month_order))

# Prevalencia mensual
prev_PC <- PC_atr %>%
  group_by(Month_name) %>%
  summarise(
    n_evaluable = sum(!is.na(Number_Atresia)),
    n_atresia = sum(Number_Atresia > 0, na.rm = TRUE),
    prevalence_pct = 100 * n_atresia / n_evaluable
  )
prev_PC

# Intensidad relativa
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

# Ejecutar e interpretar Dunn principalmente si Kruskal-Wallis es significativo.
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
# TAREA 9. PREVALENCIA E INTENSIDAD DE ATRESIA EN P. ASPERA
# =============================================================================
# Repite para P. aspera el flujo mostrado arriba para P. crenata.
#
# PREVALENCIA
# Por mes calcula:
#   - numero de individuos evaluables
#   - numero de individuos con Number_Atresia > 0
#   - prevalencia (%)
#
# INTENSIDAD RELATIVA
#   - elimina NA de Relative_intensity_atresia
#   - realiza Q-Q plot
#   - evalua homogeneidad con leveneTest()
#   - aplica Kruskal-Wallis
#   - realiza Dunn solo cuando proceda
#   - construye el boxplot + jitter
#
# ENTREGA:
# tabla de prevalencia, grafico de intensidad y resultados estadisticos cuando
# proceda. Compara si prevalencia e intensidad cuentan la misma historia.
#
# ESCRIBE AQUI TU CODIGO PARA P. ASPERA:










# ----------------------------------------------------------------------------
# 9. OPCIONAL: GUARDAR FIGURAS
# ----------------------------------------------------------------------------
# Descomenta solo las figuras que quieras exportar.
# ggsave("Figure3_PC_hiatus.jpeg", PC_facet_plot, width = 8, height = 7, dpi = 300)
# ggsave("Figure4_PC_number.jpeg", Fig4_PC, width = 7, height = 9, dpi = 300)
# ggsave("Figure5_PC_size.jpeg", Fig5_PC, width = 7, height = 9, dpi = 300)
# ggsave("Figure6_PC_atresia.jpeg", BOXplot_Atresia_PC, width = 8, height = 5, dpi = 300)


# =============================================================================
# TAREA 10. INFORME FINAL DE EVIDENCIA
# =============================================================================
# Integra para ambas especies las distintas lineas de evidencia:
#   - hiato PO-VO
#   - dinamica de cohortes
#   - tamano de VO
#   - atresia
#   - madurez estacional
#
# Para cada especie redacta una conclusion de 150-200 palabras.
# Utiliza al menos tres lineas de evidencia, indica una limitacion de muestreo
# o analisis y separa claramente observacion e interpretacion biologica.
#
# No escribas codigo aqui salvo que quieras crear tu propia tabla de sintesis.
