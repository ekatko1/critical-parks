library(dplyr)
library(stringr)
library(glue)
library(RCurl)

# sdf = readr::read_csv("Data/batiments-municipaux.csv") |> 
#   select(buildingName) |> 
#   mutate(pageName = 
#            buildingName |> 
#            tolower() |> 
#            iconv(to='ASCII//TRANSLIT') |> 
#            gsub(" st-", " saint-", x=_, fixed=T) |>
#            gsub(" ", "-", x=_, fixed=T) |>
#            gsub("(", "", x=_, fixed=T)|>
#            gsub(")", "", x=_, fixed=T), 
#          webAddr = paste0("https://montreal.ca/lieux/", pageName)
#          ) |> 
#   filter(grepl("chalet", pageName , fixed=T)
#  ) |> 
#   mutate(
#     exists = url.exists(webAddr, timeout = 1)
#   )

# web_sites = sdf |> pull(webAddr)

# dir.create("Out/")
readr::write_csv(sdf, "Out/MontrealList.csv")

sdf = sdf |>
  filter(exists==TRUE)

library(rvest)
library(purrr)
pages = sdf |> 
  pull(webAddr) |> 
  map(
    \(webAddr) {
      list(
        webAddr = webAddr,
        html = read_html(webAddr))
  })
    

content = pages |>
  map(\(x) {
    elements = x$html |> 
      html_elements(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "list-item-icon-content", " " ))]') |>
      html_text2()
    tibble(
      name = x$html |> html_element("h1") |> html_text2(),
      addr = elements[2] |> 
        str_replace_all(coll("Adresse\n"), "") |> 
        str_replace_all(coll("\nVoir la carte"), ""),
      closed.now = elements[1] |> str_detect(coll("Fermé")),
      hours = elements[1] |> str_replace_all(coll("\nHeures d'ouverture habituelles"), ""),
      markerText = 
        paste0(
          x$html |> html_element("h1"), 
          "\n",
          paste0(x$html |> 
                   html_elements(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "list-item-icon-content", " " ))]'),
                 collapse="\n"
          )
        )
    )
  }
  ) |> 
  list_rbind() |>
  mutate(open.now = !closed.now) |> 
  select(-closed.now) |> 
  mutate(hours = hours |> str_replace_all(coll("Ouvert aujourd'hui\n"), ""),
         hours= na_if(hours, "Horaire\nNon indiqué"),
         closing = hours |> str_sub(-8, -1)
         )

library(tidygeocoder)

content |> filter(open.now) |> pull(closing) |> unique()

map_data = content |> 
  filter(open.now) |> 
  filter(closing %in% c("20 h 00", "21 h 00﻿", "22 h 00﻿", "23 h 00﻿")) |>
  geocode(addr, method = 'osm', lat = lat , long = lng)
map_data
## plot map

library(leaflet)
leam =flet() |>
  addTiles() |>
  setView(lng = -73.6, lat = 45.5, zoom = 12) |>
  addAwesomeMarkers(popup = ~markerText, data=map_data)

  addMarkers(data = map_data)
  

savhtmlwidgets::eWidget(m, m, e = "ParOut/csOuverts_.htJusqua20hml")
