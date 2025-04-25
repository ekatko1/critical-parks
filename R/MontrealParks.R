library(dplyr)
library(stringr)
library(glue)
library(RCurl)
library(purrr)
library(rvest)

## Scrape search page for chalets
srch_pgs = c(1,2) |> 
  map( 
    \(p) {
      paste0("https://montreal.ca/lieux?mtl_content.lieux.installation.code=CHA1&page=",p) |>
        read_html()
    } 
) 

urls = paste0(
  "https://montreal.ca",
  
  srch_pgs |> 
  map(
    \(pg) {
      pg |> 
        html_elements(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "p-2", " " ))]') |>
        html_attr("href")
    }
  ) |> 
  c() |> 
  unlist()
)

## Scrape each chalet's page
pages = urls |> 
  map(
    \(url) {
      list(
        webAddr = url,
        html = read_html(url))
  })
    
## Process Content
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
