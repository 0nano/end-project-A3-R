# Création d'une carte de France avec leaflet

# Importation des paquets R
library(leaflet)
library(geojsonio)
library(htmltools)
library(dplyr)
library(mapview)

data <- read.csv("new_cities.csv", sep = ",")

# Obtention d'un geojson de la France
france <- geojsonio::geojson_read("departements_et_regions_outre_mer_sans_mayotte.geojson", what = "sp")
# france <- geojsonio::geojson_read("regions_sans_mayotte.geojson", what = "sp")

# Ajout d'une colonne accident au geojson en utilisant dplyr qui possède le nombre d'accident par région
data_nb_accident <- data %>% group_by(department_name) %>% count()
france$accident <- 0
france$accident <- data_nb_accident$n[match(gsub("-", " ", tolower(france$nom)), gsub("-", " ", tolower(data_nb_accident$department_name)))]

# Création d'une palette de couleur avec nuance arc-en-ciel
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = france$accident
)

labels <- sprintf(
  "<strong>%s</strong><br/>%s</br>%s accident(s)",
  france$nom, france$code, france$accident
) %>% lapply(htmltools::HTML)

# Création d'une carte de France avec leaflet
m <- leaflet(france) %>%
  addPolygons(
    fillColor = ~pal(accident),
    weight = 2,
    opacity = 0.3,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = ~accident,
    opacity = 0.7,
    title = "Nombre d'accidents",
    position = "bottomright"
  )
