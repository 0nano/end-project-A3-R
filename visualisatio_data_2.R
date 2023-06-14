# Création d'une carte de France avec leaflet

# Importation des paquets R
library(leaflet)
library(geojsonio)
library(htmltools)
library(dplyr)

data <- read.csv("new_cities.csv", sep = ",")

# Obtention d'un geojson de la France
france_dep <- geojsonio::geojson_read("departements_et_regions_outre_mer_sans_mayotte.geojson", what = "sp")
france_reg <- geojsonio::geojson_read("regions_sans_mayotte.geojson", what = "sp")

# Ajout d'une colonne accident au geojson en utilisant dplyr qui possède le nombre d'accident par région
data_nb_accident_dep <- data %>% group_by(department_name) %>% count()
data_nb_accident_reg <- data %>% group_by(region_name) %>% count()
france_dep$accident <- 0
france_reg$accident <- 0
france_dep$accident <- data_nb_accident_dep$n[match(gsub("-", " ", tolower(france_dep$nom)), gsub("-", " ", tolower(data_nb_accident_dep$department_name)))]
france_reg$accident <- data_nb_accident_reg$n[match(gsub("-", " ", tolower(france_reg$nom)), gsub("-", " ", tolower(data_nb_accident_reg$region_name)))]

# On récupère seulement les bléssés hospitalisés et tués
data_accident_grave <- data %>% filter(descr_grav == "2" | descr_grav == "3")
data_nb_accident_grave_dep <- data_accident_grave %>% group_by(department_name) %>% count()
data_nb_accident_grave_reg <- data_accident_grave %>% group_by(region_name) %>% count()

# On réalise le taux d'accidents grave
france_dep$taux_accident_grave <- 0
france_reg$taux_accident_grave <- 0
france_dep$taux_accident_grave <- data_nb_accident_grave_dep$n[match(gsub("-", " ", tolower(france_dep$nom)), gsub("-", " ", tolower(data_nb_accident_grave_dep$department_name)))] / france_dep$accident
france_dep$taux_accident_grave[is.na(france_dep$taux_accident_grave)] <- 0
france_reg$taux_accident_grave <- data_nb_accident_grave_reg$n[match(gsub("-", " ", tolower(france_reg$nom)), gsub("-", " ", tolower(data_nb_accident_grave_reg$region_name)))] / france_reg$accident
france_reg$taux_accident_grave[is.na(france_reg$taux_accident_grave)] <- 0

# Création des palette de couleur avec nuance pour les nombres d'accidents par départements et régions
# ainsi que les labels respectifs
pal_nb_accident_dep <- colorNumeric(
  palette = "YlOrRd",
  domain = france_dep$accident
)

labels_nb_accident_dep <- sprintf(
  "<strong>%s</strong> %s</br>%s accident(s)",
  france_dep$nom, france_dep$code, france_dep$accident
) %>% lapply(htmltools::HTML)

pal_nb_accident_reg <- colorNumeric(
  palette = "YlOrRd",
  domain = france_reg$accident
)

labels_nb_accident_reg <- sprintf(
  "<strong>%s</strong> %s</br>%s accident(s)",
  france_reg$nom, france_reg$code, france_reg$accident
) %>% lapply(htmltools::HTML)

# Création des différentes cartes de France pour les nombres d'accidents par département et par région
m_dep <- leaflet(france_dep) %>%
  # addTiles() %>% # permet d'ajouté une carte de fond
  setView(lng=5.4255179, lat=46.2346321, zoom=6) %>%
  addPolygons(
    fillColor = ~pal_nb_accident_dep(accident),
    weight = 2,
    opacity = 0.3,
    color = "white",
    dashArray = "5",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels_nb_accident_dep,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal_nb_accident_dep,
    values = ~accident,
    opacity = 0.7,
    title = "Nombre d'accidents",
    position = "bottomright"
  )

m_reg <- leaflet(france_reg) %>%
  # addTiles() %>% # permet d'ajouté une carte de fond
  setView(lng=5.4255179, lat=46.2346321, zoom=6) %>%
  addPolygons(
    fillColor = ~pal_nb_accident_reg(accident),
    weight = 2,
    opacity = 0.3,
    color = "white",
    dashArray = "5",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels_nb_accident_reg,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal_nb_accident_reg,
    values = ~accident,
    opacity = 0.7,
    title = "Nombre d'accidents",
    position = "bottomright"
  )

# Création des différentes palettes pour les taux d'accidents grave par département et par région
# ainsi que les labels respectifs
pal_grave_dep <- colorNumeric(
  palette = "YlOrRd",
  domain = france_dep$taux_accident_grave
)

labels_grave_dep <- sprintf(
  "<strong>%s</strong> %s</br>%s Taux d'accident graves(s)",
  france_dep$nom, france_dep$code, france_dep$taux_accident_grave
) %>% lapply(htmltools::HTML)

pal_grave_reg <- colorNumeric(
  palette = "YlOrRd",
  domain = france_reg$taux_accident_grave
)

labels_grave_reg <- sprintf(
  "<strong>%s</strong> %s</br>%s Taux d'accident graves(s)",
  france_reg$nom, france_reg$code, france_reg$taux_accident_grave
) %>% lapply(htmltools::HTML)

# On crée les cartes pour les taux d'accidents grave par département et par région
n_dep <- leaflet(france_dep) %>%
  # addTiles() %>% # permet d'ajouté une carte de fond
  # On rahoute un zoom sur la france métropolitaine
  setView(lng=5.4255179, lat=46.2346321, zoom=6) %>%
  addPolygons(
    fillColor = ~pal_grave_dep(taux_accident_grave),
    weight = 2,
    opacity = 0.3,
    color = "white",
    dashArray = "5",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels_grave_dep,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal_grave_dep,
    values = ~taux_accident_grave,
    opacity = 0.7,
    title = "Taux d'accidents graves",
    position = "bottomright"
  )

n_reg <- leaflet(france_reg) %>%
  # addTiles() %>% # permet d'ajouté une carte de fond
  # On rahoute un zoom sur la france métropolitaine
  setView(lng=5.4255179, lat=46.2346321, zoom=6) %>%
  addPolygons(
    fillColor = ~pal_grave_reg(taux_accident_grave),
    weight = 2,
    opacity = 0.3,
    color = "white",
    dashArray = "5",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels_grave_reg,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal_grave_reg,
    values = ~taux_accident_grave,
    opacity = 0.7,
    title = "Taux d'accidents graves",
    position = "bottomright"
  )

# On enregistre la carte en PNG
library(mapview)

# On installe phantomjs dans le cas où cela n'est pas fait
# On définit la conf de openssl à null car la version ne correspond plus à la version actuelle d'openssl
# https://github.com/ariya/phantomjs/issues/15449
# https://stackoverflow.com/questions/72496479/phantomjs-launcher-fails-on-latest-pop-os-cannot-find-shared-library-libprovid/72679175#72679175
Sys.setenv(OPENSSL_CONF="/dev/null")
webshot::install_phantomjs()

# On réalise les captures des différentes cartes et on les place dans le dossier ressources
mapshot(m_dep, file="ressources/plot France métropolitaine nb accidents départements.png")
mapshot(m_reg, file="ressources/plot France métropolitaine nb accidents régions.png")
mapshot(n_dep, file="ressources/plot France métropolitaine taux accidents graves départements.png")
mapshot(n_reg, file="ressources/plot France métropolitaine taux accidents graves régions.png")