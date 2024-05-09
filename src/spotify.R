# *** Bootcamp de análisis de datos *** ---------------------------------------
# *** Licencia: GPL-3.0 *** ---------------------------------------------------

# *** Librerías ---------------------------------------------------------------
library(openxlsx) #   Leer archivos xlsx
library(dplyr) #      Manipulación de datos
library(tidyverse) #  Manipulación de datos
library(ggplot2) #    Visualización de datos
library(gmodels) #    Pruebas de hipótesis
library(shiny) #      Crear aplicaciones web
library(plotly) #     Gráficos interactivos
library(rsconnect) #  Subir aplicaciones a shinyapps.io
library(scales) #     Reescalar datos
library(stringr) #    Manipulación de cadenas de texto
library(reshape2) #   Reestructurar datos
library(fmsb) #       Gráficos de radar

# *** Carga de datos ----------------------------------------------------------
# ? openxlsx

# ? name          Nombre de la canción.

# ? artists       Nombre del artista.

# ? acoustic      Medida de confianza de 0,0 a 1,0 sobre si la pista es
# ?               acústica. 1,0 representa alta confianza en que la pista es
# ?               acústica.

# ? duration_min  Duración de la pista en minutos.

# ? energy        La energía es una medida de 0,0 a 1,0 y representa una medida
# ?               perceptual de intensidad y actividad.

# ? instrumental  Predice si una pista no contiene voces. Los sonidos "Ooh" y
# ?               "aah" se tratan como instrumentales en este contexto. Las
# ?               pistas de rap o de palabra hablada son claramente "vocales".
# ?               Cuanto más cerca esté el valor de instrumentalidad de 1,0,
# ?               mayor será la probabilidad de que la pista no contenga
# ?               contenido vocal. Los valores superiores a 0,5 pretenden
# ?               representar pistas instrumentales, pero la confianza es mayor
# ?               a medida que el valor se acerca a 1,0.

# ? live          Detecta la presencia de una audiencia en la grabación. Los
# ?               valores de vida más altos representan una mayor probabilidad
# ?               de que la pista se haya interpretado en vivo. Un valor
# ?               superior a 0,8 proporciona una gran probabilidad de que la
# ?               pista esté en vivo

# ? loudness      El volumen general de una pista en decibelios (dB). Los
# ?               valores de sonoridad se promedian en toda la pista y son
# ?               útiles para comparar el volumen relativo de las pistas. El
# ?               volumen es la cualidad de un sonido que es el principal
# ?               correlato psicológico de la fuerza física (amplitud). Los
# ?               valores suelen oscilar entre -60 y 0 db.

# ? speech        Speechiness detecta la presencia de palabras habladas en una
# ?               pista. Cuanto más exclusivamente hablada sea la grabación
# ?               (por ejemplo, un programa de entrevistas, un audiolibro,
# ?               poesía), más cercano a 1,0 será el valor del atributo. Los
# ?               valores superiores a 0,66 describen pistas que probablemente
# ?               estén compuestas exclusivamente de palabras habladas. Los
# ?               valores entre 0,33 y 0,66 describen pistas que pueden contener
# ?               música y voz, ya sea en secciones o en capas, incluidos casos
# ?               como la música rap. Los valores inferiores a 0,33
# ?               probablemente representen música y otras pistas que
# ?               no sean de voz.

# ? valence       Una medida de 0,0 a 1,0 que describe la positividad musical
# ?               que transmite una pista. Las pistas con valencia alta suenan
# ?               más positivas (por ejemplo, felices, alegres, eufóricas),
# ?               mientras que las pistas con valencia baja suenan más negativas
# ?               (por ejemplo, tristes, deprimidas, enojadas).

# ? popularity    La popularidad de la pista. La popularidad de la pista es un
# ?               valor entre 0 y 100, siendo 100 la más popular. La popularidad
# ?               de la pista se calcula a partir de la popularidad de todas las
# ?               listas de reproducción de Spotify.

# ? top           Es 1 cuando popularity > 70 (top 1000) y 0 en caso contrario.

spotify <- read.xlsx(
  "data/datos_spotify_2020_top.xlsx"
)

View(spotify)

# *** Limpieza de datos -------------------------------------------------------
# ? tidyverse
# ? scales
# ? stringr

# Definir los patrones de caracteres y sus reemplazos
reemplazos <- c(
  "â€™" = "'", "Ã¡" = "á", "Ã©" = "é", "Ã­" = "í", "Ã³" = "ó",
  "Ãº" = "ú", "Ã" = "Á", "Ã‰" = "É", "Ã" = "Í", "Ã“" = "Ó",
  "Ãš" = "Ú", "Ã±" = "ñ", "Ã‘" = "Ñ"
)

# Aplicar los reemplazos a las columnas name y artists
spotify$name <- str_replace_all(spotify$name, reemplazos)
spotify$artists <- str_replace_all(spotify$artists, reemplazos)

# Pasar loudness (db) a linear y negativo
spotify$loudness <- 10^(spotify$loudness / 10) * -1
spotify$loudness <- rescale(spotify$loudness, to = c(0, 100))

# Eliminar el último y los primeros 2 caracteres de la columna artists
spotify$artists <- substr(spotify$artists, 2, nchar(spotify$artists) - 1)

# Eliminar los '
spotify$artists <- gsub("'", "", spotify$artists)

nrow(spotify) # 1,756

# Dividir la columna artists en varias filas
spotify <- spotify %>%
  separate_rows(artists, sep = ", ")

nrow(spotify) # 2,654

View(spotify)

# *** Canciones por artistas --------------------------------------------------
# ? ggplot2

# ¿Cuántas canciones hay en el dataset?
length(unique(spotify$name)) # 1,593

# ¿Cuántos artistas hay en el dataset?
length(unique(spotify$artists)) # 961

# ¿Cuántas canciones tiene cada artista del top 1000?
agrupacion_artists <- spotify %>%
  filter(top == 1) %>%
  group_by(artists) %>%
  summarise(canciones = n()) %>%
  arrange(desc(canciones)) %>%
  top_n(5)

View(agrupacion_artists)

canciones_por_artista <- ggplot(agrupacion_artists, aes(
  x = reorder(artists, canciones), y = canciones
)) +
  geom_bar(stat = "identity", fill = "#042e10") +
  coord_flip() +
  labs(
    title = "¿Cuántas canciones tienen los artistas de mayor popularidad?",
    x = "Artista",
    y = "Número de canciones"
  ) +
  theme_minimal() +
  theme(title = element_text(face = "bold"))

# Agrupar por artista y calcular el número de canciones
agrupacion_artists <- spotify %>%
  group_by(artists) %>%
  summarise(canciones = n())

# Calcular el promedio de canciones por artista
promedio_canciones <- mean(agrupacion_artists$canciones)

# Agregar línea de promedio
canciones_por_artista <- canciones_por_artista +
  geom_hline(
    yintercept = promedio_canciones,
    color = "#000000", linetype = "dashed", linewidth = 1.5
  )

# ! Exportar la gráfica
ggsave(
  "assets/canciones_por_artista.jpg",
  canciones_por_artista,
  width = 10,
  height = 6
)

# *** Gráfico de araña del promedio de características ------------------------
# ? reshape2
# ? fmsb

# Abrir el dispositivo gráfico
jpeg("assets/radar_chart.jpg", width = 1800, height = 1800, res = 300)

# Calcular el promedio de cada característica
spotify_means <- spotify %>%
  summarise(
    valence = mean(valence, na.rm = TRUE),
    acoustic = mean(acoustic, na.rm = TRUE),
    energy = mean(energy, na.rm = TRUE),
    instrumental = mean(instrumental, na.rm = TRUE),
    live = mean(live, na.rm = TRUE),
    loudness = mean(loudness, na.rm = TRUE),
    speech = mean(speech, na.rm = TRUE)
  )

# Añadir una fila al principio y al final para cerrar el polígono
spotify_means <- rbind(
  rep(max(spotify_means), times = ncol(spotify_means)),
  rep(min(spotify_means), times = ncol(spotify_means)),
  spotify_means
)

# Crear el gráfico de radar
radarchart(spotify_means,
  axistype = 1,
  pcol = rgb(0.2, 0.5, 0.5, 0.9),
  pfcol = rgb(0.2, 0.5, 0.5, 0.5),
  plwd = 4,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "grey",
  caxislabels = seq(0, 20, 5),
  cglwd = 0.8,
  vlcex = 0.8
)

# Agregar el título
title(
  main = "Gráfico de radar de características promedio de Spotify",
  font.main = 4
)

# Cerrar el dispositivo gráfico
dev.off()

# Densidad de instrumental excluyendo los valores iguales a 0
ggplot(data = spotify %>% filter(instrumental != 0), aes(x = instrumental)) +
  geom_density(fill = "#042e10", alpha = 0.5) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# En escala logarítmica
ggplot(data = spotify %>% filter(instrumental != 0), aes(x = instrumental)) +
  geom_density(fill = "#042e10", alpha = 0.5) +
  scale_x_log10() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Multiplicar por 100 los valores de instrumental que sean menores a 1
spotify_i_100 <- spotify %>%
  mutate(
    instrumental = if_else(instrumental < 1, instrumental * 100, instrumental)
  )

# Densidad de instrumental excluyendo los valores iguales a 0
densidad_instrumental <- ggplot(data = spotify_i_100 %>%
  filter(instrumental != 0), aes(
  x = instrumental
)) +
  geom_density(fill = "#042e10", alpha = 0.5) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Exportar gráfico
ggsave(
  "assets/densidad_instrumental.jpg",
  densidad_instrumental,
  width = 10,
  height = 6
)

# *** Boxplot general, cuando top = 0 y cuando top = 1 ------------------------
# ? reshape

# Seleccionar las columnas que representan las características
spotify$duration_min <- spotify$duration_min * 10
features <- spotify[, c(
  "acoustic", "dance", "duration_min", "energy",
  "instrumental", "live", "loudness", "speech", "valence"
)]

# Reorganizar los datos
spotify_melted <- melt(features)

boxplots_caracteristicas <- ggplot(spotify_melted, aes(
  x = variable, y = value, fill = variable
)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_manual(values = c(
    "acoustic" = "#6ba683", "dance" = "#e5e580",
    "duration_min" = "#8d8da6", "energy" = "#d16f6f",
    "instrumental" = "#5f8ca3", "live" = "#b38f59",
    "loudness" = "#b2db53", "speech" = "#b3759e",
    "valence" = "#efb134"
  )) +
  theme_gray() +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(face = "bold", color = "black"),
    axis.text.y = element_text(face = "bold", color = "black")
  )

# Exportar la gráfica
ggsave(
  "assets/boxplots_caracteristicas.jpg",
  boxplots_caracteristicas,
  width = 10,
  height = 6
)

# *** Correlación entre la popularidad y el resto de variables

# Calcular la correlación entre la popularidad y las demás variables numéricas
correlation <- cor(spotify[, c(
  "acoustic", "duration_min", "energy", "instrumental",
  "live", "loudness", "speech", "valence", "popularity"
)])

# Extraer la correlación de popularidad
popularity_correlation <- correlation["popularity", ]

# Convertir el vector en un dataframe
correlation_df <- data.frame(
  Feature = names(popularity_correlation), Correlation = popularity_correlation
)

# Ordenar el dataframe de mayor a menor
sorted_correlation <- correlation_df[order(
  correlation_df$Correlation,
  decreasing = TRUE
), ]

View(sorted_correlation)

# speech 0.0852
# loudness 0.0234

# *** Dispersión Popularity - Speech ------------------------------------------

# Crear una gráfica de dispersión de popularity vs speech con un diseño mejorado
popularity_speech <- ggplot(data = spotify, aes(x = popularity, y = speech)) +
  geom_point(alpha = 0.5, size = 3, color = "#042e10") +
  theme_minimal() +
  labs(
    title = "Scatterplot of Popularity vs Speech",
    x = "Popularity", y = "Speech"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# Exportar la gráfica como un archivo .png
ggsave(
  filename = "popularity_speech.jpg",
  plot = popularity_speech, width = 10,
  height = 6, dpi = 300
)

# *** Dispersión Popularity - Loudness ----------------------------------------

# Gráfica de dispersión de popularity vs loudness
popularity_loudness <- ggplot(
  data = spotify, aes(x = popularity, y = loudness)
) +
  geom_point(alpha = 0.5, size = 3, color = "#042e10") +
  theme_minimal() +
  labs(
    title = "Scatterplot of Popularity vs Loudness",
    x = "Popularity", y = "Loudness"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# Exportar la gráfica
ggsave(
  filename = "popularity_loudness.jpg",
  plot = popularity_loudness, width = 10, height = 6, dpi = 300
)

# *** Boxplot de Speech General/Top=1/Top=0 y Loudness General/Top=1/Top=0 ----

# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)

# Crear el dataframe datos_relevantes
datos_relevantes <- spotify %>%
  select(speech, loudness, top) %>%
  pivot_longer(
    cols = c(speech, loudness),
    names_to = "feature", values_to = "value"
  ) %>%
  mutate(
    top = ifelse(top == 1, "Top 1", "Top 0"),
    top = factor(top, levels = c("Top 1", "Top 0", "Todos los datos"))
  )

# Añadir una fila para "Todos los datos"
datos_relevantes <- rbind(
  datos_relevantes,
  mutate(datos_relevantes, top = "Todos los datos")
)

# Crear el boxplot
boxplots_speech_loudness <- ggplot(datos_relevantes, aes(
  x = top, y = value, fill = top
)) +
  geom_boxplot() +
  labs(y = "Valor") +
  facet_grid(~feature) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none")

# Exportar
ggsave(
  filename = "assets/boxplots_speech_loudness.jpg",
  plot = boxplots_speech_loudness, width = 10, height = 6
)

# *** Dashboard ---------------------------------------------------------------

# Cargar las librerías necesarias
# Cargar las bibliotecas necesarias
library(shiny)
library(shinydashboard)
library(plotly)

# Definir la interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard de Spotify"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(plotlyOutput("plot_canciones_por_artista"), width = 6),
      box(plotlyOutput("plot_boxplots_caracteristicas"), width = 6),
      box(plotlyOutput("plot_popularity_speech"), width = 6),
      box(plotlyOutput("plot_popularity_loudness"), width = 6),
      box(plotlyOutput("plot_boxplots_speech_loudness"), width = 12)
    )
  )
)

# Definir el servidor
server <- function(input, output) {
  output$plot_canciones_por_artista <- renderPlotly({
    ggplotly(canciones_por_artista)
  })
  output$plot_boxplots_caracteristicas <- renderPlotly({
    ggplotly(boxplots_caracteristicas)
  })
  output$plot_popularity_speech <- renderPlotly({
    ggplotly(popularity_speech)
  })
  output$plot_popularity_loudness <- renderPlotly({
    ggplotly(popularity_loudness)
  })
  output$plot_boxplots_speech_loudness <- renderPlotly({
    ggplotly(boxplots_speech_loudness)
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)

rsconnect::setAccountInfo(
  name = "victorbenitogr",
  token = "6155FAC68D2682068F5AE7BE74791CF1",
  secret = "7Ht8qkfFLZ5owTuav3dUtMgazRJbNhAeaBNuyi/c"
)

deployApp(
  appDir = "/home/victor/Documents/DataAnalysisBootcampITESM/RShinySpotify",
  appName = "SpotifyDashboard"
)
