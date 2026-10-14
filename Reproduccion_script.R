# ============================================================
# BIOLOGIA REPRODUCTIVA DE LA CABALLA CON R
# Guion ampliado de clase | Manual docente v2.0
# Ejecutar cada apartado en orden y comentar resultados en grupo.
# BD.xlsx debe estar en el directorio de trabajo.
# ============================================================

# 0. PAQUETES -------------------------------------------------
# Instalar una sola vez, si faltan:
# install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "car", "sizeMat"))
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)

# 1. IMPORTACION Y REVISION -----------------------------------
setwd("")

getwd()
if (!file.exists("BD.xlsx")) {
  stop("No se encuentra BD.xlsx. Revisa getwd() o abre el proyecto en la carpeta correcta.")
}
Data <- read_excel("BD.xlsx")
head(Data)
str(Data)
names(Data)

# TAREA 1. Describir la muestra (registrar las respuestas).
nrow(Data)
unique(Data$Sex)
sort(unique(Data$Month))
range(Data$TL_mm, na.rm = TRUE)
colSums(is.na(Data))
# n observaciones:
# niveles Sex:
# meses disponibles:
# rango de TL_mm:
# variables con NA relevantes:

# 2. PREPARACION DE VARIABLES ---------------------------------
Data <- Data %>%
  mutate(
    Year = as.factor(Year),
    Sex = as.factor(Sex),
    MS = as.factor(MS),
    across(any_of(c("TL_mm", "TW_g", "EW_g", "GW_g", "GSI")), as.numeric)
  )

# Mantener el orden cronologico real de los meses disponibles.
# Los niveles se toman de la codificacion numerica de BD.xlsx.
mes_num <- suppressWarnings(as.integer(as.character(Data$Month)))
if (any(is.na(mes_num) & !is.na(Data$Month)) ||
    any(!is.na(mes_num) & !(mes_num %in% 1:12))) {
  stop("Revisar codificacion de Month: se esperan meses numericos de 1 a 12.")
}
Data$Month <- factor(mes_num, levels = 1:12,
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
str(Data)

# 3. INDICE GONADOSOMATICO (GSI) -------------------------------
# Segun el manual: GSI = GW_g / EW_g * 100.
# El denominador solo se acepta cuando EW_g > 0.
Data <- Data %>%
  mutate(GSI = if_else(!is.na(GW_g) & !is.na(EW_g) & EW_g > 0,
                       GW_g / EW_g * 100, NA_real_))
summary(Data$GSI)

# TAREA 2. Control de calidad.
sum(!is.na(Data$GSI))
sum(is.na(Data$GSI))
sum(!is.na(Data$EW_g) & Data$EW_g <= 0)

# Explicar la importancia de comprobar EW_g.

# 4. ESTADISTICA DESCRIPTIVA ----------------------------------
# Tabla general segun la seccion 6 del manual.
# min/max se muestran como NA cuando la variable carece de datos validos.
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

# TAREA 3. Tabla equivalente por sexo: codigo preparado.
# Comparar medias y dispersiones entre F y M.




# 5. NORMALIDAD Y HOMOGENEIDAD --------------------------------
# Diagnostico para TL_mm (manual, seccion 7).
hist(Data$TL_mm, main = "Histograma de TL_mm", xlab = "TL (mm)")
qqnorm(na.omit(Data$TL_mm)); qqline(na.omit(Data$TL_mm))
# Shapiro-Wilk requiere entre 3 y 5000 datos validos.
shapiro_control <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3 || length(x) > 5000) {
    message("Shapiro-Wilk: se necesitan de 3 a 5000 observaciones validas.")
    return(invisible(NULL))
  }
  shapiro.test(x)
}
shapiro_control(Data$TL_mm)
car::leveneTest(TL_mm ~ Sex, data = Data)

# TAREA 4. Repetir el diagnostico con el peso total.
hist(Data$TW_g, main = "Histograma de TW_g", xlab = "TW (g)")
qqnorm(na.omit(Data$TW_g)); qqline(na.omit(Data$TW_g))
shapiro_control(Data$TW_g)
car::leveneTest(TW_g ~ Sex, data = Data)
# Describir el Q-Q antes de interpretar los valores p.

# 6. ESTRUCTURA DE TALLAS -------------------------------------
# Histograma completo.
ggplot(Data, aes(x = TL_mm)) +
  geom_histogram(binwidth = 10, boundary = 0, closed = "left",
                 fill = "grey75", color = "grey20") +
  labs(x = "Longitud total (mm)", y = "Frecuencia") + theme_bw()

# Histogramas separados por sexo.
ggplot(Data, aes(x = TL_mm)) +
  geom_histogram(binwidth = 10, boundary = 0, closed = "left",
                 fill = "grey80", color = "grey30") +
  facet_wrap(~Sex, scales = "free_y") +
  labs(x = "Longitud total (mm)", y = "Frecuencia") + theme_bw()

# Histograma apilado por sexo.
ggplot(Data, aes(x = TL_mm, fill = Sex)) +
  geom_histogram(binwidth = 10, boundary = 0, closed = "left",
                 color = "grey15", position = "stack") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Longitud total (mm)", y = "Frecuencia") + theme_bw()

# TAREA 5. Estructura de tallas por mes.
# Cambiar binwidth = 10 a 20. Comparar e interpretar.





# 7. FASES DE MADUREZ Y TEMPORADA DE PUESTA -------------------
freq_MS <- Data %>%
  count(MS, name = "n") %>%
  mutate(percent = round(100 * n / sum(n), 1))
print(freq_MS)

p_madurez <- ggplot(Data, aes(x = Month, fill = MS)) +
  geom_bar(position = "fill", color = "grey20") +
  scale_y_continuous(name = "Proporcion", labels = function(x) paste0(x * 100, "%"),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_x_discrete(name = "Mes", drop = TRUE) +
  scale_fill_brewer(palette = "Dark2", name = "Fase de madurez") +
  labs(title = "Composicion mensual de madurez") + theme_bw()
print(p_madurez)
# TAREA 6. Justificar los meses con mayor evidencia reproductiva.


# 8. GSI POR MES Y SEXO ---------------------------------------
# Fases II-V como filtro operativo del manual, NO definicion general de maduro.
Data_gsi <- Data %>% filter(as.character(MS) %in% c("2", "3", "4", "5"))
GSIFemales <- Data_gsi %>% filter(as.character(Sex) == "F")


# TAREA 6. Crea el subconjunto de Machos
GSIMales 


# TAREA 7. Comparar el numero de registros por sexo.
nrow(GSIFemales)
nrow(GSIMales)

resumen_gsi <- Data_gsi %>%
  group_by(Sex, Month) %>%
  summarise(mean = if (all(is.na(GSI))) NA_real_ else mean(GSI, na.rm = TRUE),
            sd = sd(GSI, na.rm = TRUE),
            n = sum(!is.na(GSI)), .groups = "drop")
print(resumen_gsi)

# TAREA 8. Observar por separado y comparar las distribuciones.
GSI_Females <- ggplot(GSIFemales, aes(x = Month, y = GSI, group = Month)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.4) +
  labs(title = "Hembras", x = "Mes", y = "GSI") + theme_bw()
print(GSI_Females)


### Machos








# Alternativa de dos paneles sin paquetes adicionales.
ggplot(Data_gsi, aes(x = Month, y = GSI, group = Month)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.35, size = 1) +
  facet_wrap(~Sex, ncol = 1) +
  labs(x = "Mes", y = "GSI") + theme_bw()
# Pregunta: ¿coinciden el GSI y las fases de madurez en el pico reproductivo?

# 9. L50: EJEMPLO DEL MANUAL Y TAREA DE ADAPTACION ------------
# Se conserva el ejercicio de sizeMat antes de adaptar a BD.xlsx.
# Cargar el paquete solo al llegar a esta seccion.
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
plot(my_ogive_fq, xlab = "Longitud total (cm)",
     ylab = "Proporcion madura", col = c("blue", "red"),
     onlyOgive = TRUE)

# TAREA 9. Completar SOLO tras justificar la clasificacion biologica.
# a) Definir que fases de MS son inmaduras y maduras.
# b) Filtrar los registros de hembras y de machos por separado.
# c) Preparar longitud y estado de madurez en el formato adecuado a sizeMat.
# d) Ajustar gonad_mature() para cada sexo y comunicar L50 e incertidumbre.
# e) Comprobar que la unidad de longitud del resultado es mm si se usa TL_mm.
# No convertir fases en maduras automaticamente sin justificar la escala.

# 10. SINTESIS DEL GRUPO --------------------------------------
# Figura principal:
# Periodo reproductivo propuesto:
# L50 hembras (con incertidumbre):
# L50 machos (con incertidumbre):
# Estructura de tallas:
# Limitacion principal:
# Implicacion para evaluacion o gestion:
