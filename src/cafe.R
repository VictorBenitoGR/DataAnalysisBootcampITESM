# *** Bootcamp de análisis de datos *** ---------------------------------------

# *** Librería ----------------------------------------------------------------
library(openxlsx) # Leer archivos xlsx
library(dplyr) # Manipulación de datos
library(ggplot2) # Visualización de datos

# *** Carga de datos ----------------------------------------------------------

claves_primarias <- read.xlsx(
  "data/SP_BaseDatos_EmprenimientoCafe boot camp.xlsx",
  sheet = 1
)

View(claves_primarias)

# Cambiar la columna "No." por "id"
claves_primarias <- claves_primarias %>%
  rename(id = `No.`)

# Añadir una "P" antes de cada id
claves_primarias$id <- paste0("P", claves_primarias$id)

View(claves_primarias)

resultados <- read.xlsx(
  "data/SP_BaseDatos_EmprenimientoCafe boot camp.xlsx",
  sheet = 2
)

View(resultados)

# Eliminar la primera fila
resultados <- resultados[-1, ]

View(resultados)

# Transformar a numérico la columna P12 de resultados
resultados$P12 <- as.numeric(resultados$P12)

# *** Gráficos ----------------------------------------------------------------

# Histograma de la columna P12, el título debe ser claves_primarias$Preguntas de
# la columna P12

histograma_p12 <- ggplot(resultados, aes(x = P12)) +
  geom_histogram(binwidth = 1, fill = "#000059", color = "black") +
  labs(
    title = "Consumo diario de taza de café",
    x = "Puntuación",
    y = "Frecuencia"
  ) +
  theme_minimal()

# Exportar el gráfico
ggsave("assets/histograma_p12.jpg", plot = histograma_p12, width = 9, height = 6)

# Boxplot de la columna P12

boxplot_p12 <- ggplot(resultados, aes(x = "", y = P12)) +
  geom_boxplot(fill = "#000059", color = "black") +
  labs(
    title = "Consumo diario de taza de café",
    x = "",
    y = "Puntuación"
  ) +
  theme_minimal()

# Exportar el gráfico
ggsave("assets/boxplot_p12.jpg", plot = boxplot_p12, width = 6, height = 9)
