# setup
library(leaflet, quietly = TRUE)
library(htmltools, quietly = TRUE)
library(htmlwidgets, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(rvest, quietly = TRUE)
library(plotly, quietly = TRUE)
library(ggmap, quietly = TRUE)


# scrape the dhss website

url <- "http://www.dhss.alaska.gov/dph/Epi/id/Pages/COVID-19/monitoring.aspx"

url  %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="ctl00_PlaceHolderMain_PageContent__ControlWrapper_RichHtmlField"]/div[2]/table') %>%
  html_table(header = TRUE) %>%
  .[[1]] -> covid_cases

colnames(covid_cases) <- c("Region", "Travel-Related", "Non-Travel", "Total")

# sometimes there are some blank space characters in the table, they need to be removed
if(is.character(covid_cases$`Non-Travel`)) {
covid_cases %>%
  mutate(`Non-Travel` = as.numeric(gsub(`Non-Travel`, 
            pattern = "[^[:alnum:][:blank:]?&/\\-]" , replacement = ""))) -> covid_cases
} else {
  NULL
}

# filter out the regional summaries
covid_cases %>% filter(Region != "TOTAL") -> covid_cases

covid_cases %>%
  filter(Region %in% c("Gulf Coast", "Interior", 
                       "Mat-Su", "Northern", "Southeast", "Southwest")) -> region

cities <- setdiff(covid_cases, region)

# get geolocation data from google geolocate API
register_google(key = "AIzaSyB8fBrQMA06KfbJoXkyUaB20k0RISl9T8s")

gis <- geocode(paste(cities$Region, "Alaska", sep=", "))
city_data <- cbind(gis, cities)

# initial parameters for leaflet map
initial_lat <- 62
initial_lng <- -145
initial_zoom <- 4

# 
url  %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="ctl00_PlaceHolderMain_PageContent__ControlWrapper_RichHtmlField"]/div[2]/div[1]/ul/li[4]') %>%
  html_text() %>%
  str_replace_all(pattern = ";", "") %>%
  substr(0, 22) -> updated_date

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
                                 transform: translate(-50%,20%);
                                 position: fixed !important;
                                 left: 50%;
                                 text-align: center;
                                 padding-left: 10px; 
                                 padding-right: 10px; 
                                 background: rgba(255,255,255,0.75);
                                 font-weight: bold;
                                 font-size: 20px;
                                 }
                                 "))


title <- tags$div(
  tag.map.title, HTML("COVID-19 Cases in Alaska")
)  

tag.map.subtitle <- tags$style(HTML("
  .leaflet-control.map-subtitle { 
                                 transform: translate(-50%,20%);
                                 position: fixed !important;
                                 left: 50%;
                                 text-align: center;
                                 padding-left: 10px; 
                                 padding-right: 10px;
                                 padding-top: 30px;
                                 font-size: 16px;
                                 }
                                 "))


subtitle <- tags$div(
  tag.map.subtitle, HTML(updated_date)
)  

output$covid_map <- renderLeaflet({
leaflet(city_data) %>%
  addTiles() %>%
  addCircleMarkers(lat=~lat, lng=~lon, radius = ~Total, 
                   popup = paste("Region:", cities$Region, "<br>",
                                 "Travel-Related:", cities$`Travel-Related`, "<br>",
                                 "Non-Travel:", cities$`Non-Travel`, "<br>",
                                 "Total:", cities$Total, "<br>")) %>%
  setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom) %>%
  addControl(title, position = "topright", className="map-title") %>%
  addControl(subtitle, position = "topright", className="map-subtitle")
})



# testing over time

counts <- data.table::fread("http://www.dhss.alaska.gov/dph/Epi/id/SiteAssets/Pages/HumanCoV/COVID-19_epi_testingcurve.csv")


# the following case counts were manually counted from DHHS archives, going forward they
# will update automatically later


cases <- data.frame(Date = seq.Date(from = as.Date("2020-03-10"),
                           to=as.Date("2020-03-25"), by="day"),
          positiveCases= c(0, 0, 0, 1, 1, 1, 1, 3, 6, 9, 12, 14, 14, 22, 36, 42))
