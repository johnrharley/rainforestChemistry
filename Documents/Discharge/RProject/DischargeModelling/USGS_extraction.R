# Scraping USGS data 
# Load the USGS package dataRetrieval which interfaces to the NWIS data
library(dataRetrieval)
library(tidyverse)
# function to get a list of sites back with data for particular parameters
# a full list of paremeter codes can be found at https://help.waterdata.usgs.gov/code/parameter_cd_query?fmt=rdb&inline=true&group_cd=%

# 00060 - discharge in cfs (1 day average)
# 00400 - pH (field)
# 00403 - pH (laboratory)
# 00405	- CO2
# 00421	- alkalinity (lab)
# 
params <- c("00060", "00400", "00405", "00421") 

sites <- whatNWISsites(bBox=c(-137,54,-130.0,60), 
              parameterCd=params)


sitesToQuery <- unique(sites$site_no)

dat <- readNWISdata(siteNumbers = sitesToQuery, parameterCd = "00421", startDate = "1970-01-01")

sites <- whatNWISsites(bBox=c(-137,54,-130.0,60), 
                       parameterCd="00400")

dat <- readNWISqw(siteNumbers = sitesToQuery, parameterCd = "39086", startDate = "1970-01-01")

unique(dat$site_no)
head(dat)

ggplot(dat, aes(x=sample_dt, y=result_va , color=site_no)) +
  geom_point() +
  guides(color=FALSE)

readNWISqw(siteNumbers = "15049900")

alk <- read.csv("USGS_ALL_DIC_FOR_PHREEQC_mbEdits.csv")

alk %>%
  mutate(Date = as.Date(DATE, format = "%m/%d/%Y")) %>%
  mutate(Year = lubridate::year(Date)) %>%
  group_by(Year, site_no) %>%
  add_tally() %>%
  filter(n > 5) %>%
  mutate(yday = lubridate::yday(Date)) %>%
  ggplot(aes(x=yday, y=mean_daily_tempC)) +
  geom_point()

library(seacarb)

alk %>%
  #left_join(sites, by="site_no") %>%
  #filter(dec_lat_va  > 0) %>% # TA = TA.*(1/1000)*(1/100.09)*2*10^6; % Conversion to umol/kg; factor of 2 related to carbonate -2 charge
  mutate(alkMol = mean_daily_alk_mgl_asCaCO3 *  19.98264) -> southeast_chemistry

s1 <- carb(flag = 8, var1 = southeast_chemistry$mean_daily_pH, var2 = southeast_chemistry$alkMol, S=1, T  = southeast_chemistry$mean_daily_tempC, k1k2 = "w14") %>%
  mutate(S = "1")

s0 <- carb(flag = 8, var1 = southeast_chemistry$mean_daily_pH, var2 = southeast_chemistry$alkMol, S=0, T  = southeast_chemistry$mean_daily_tempC, k1k2 = "w14") %>%
  mutate(S = "0")

bind_cols(S1 = s1$DIC, S0 = s0$DIC) %>%
  mutate(Percentdiffernce = (1-(S1/S0))*100) %>%
  summarise(avgPercent = median(Percentdiffernce, na.omit=TRUE)) -> annotateLayer
# plot comparison of S
bind_cols(S1 = s1$DIC, S0 = s0$DIC) %>%
  ggplot(aes(x=S1, y=S0)) +
  geom_point(alpha=0.2) +
  geom_abline() +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw() +
  labs(y="DIC where S=0", x="DIC where S=1", title="DIC calculation", subtitle = "using Waters et al. (2014)") +
  annotate(geom = "text", x=20, y=1190, label = paste0("average difference ", round(annotateLayer$avgPercent), "%"), hjust=0)

# read in chem data from Stackpoole via Mariela
dat2 <- read.table("AKriverChemistry_072920.txt", sep = "\t", header = TRUE)

# make comparison
dat2 %>%
  bind_cols(s0) %>%
  mutate(Percentdiffernce = (1-(DIC/calcDIC_umol_kg))*100) -> comparisonData
# plot comparison
comparisonData %>%  
  ggplot(aes(x=calcDIC_umol_kg, y=DIC)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline() +
  theme_bw() +
  labs(y="DIC where S=0 using Waters et al. (2014)", x="DIC where S=0 Millero et al. (2014)", title="DIC calculation", subtitle = "Waters (2014) vs. Millero (1979)") +
  annotate(geom = "text", x=20, y=1190, label = paste0("average difference ", round(mean(comparisonData$Percentdiffernce),2), "%"), hjust=0)

# geographic distribution of data

require(ggmap)
require(sf)

SEAKDB <- read_sf("./SEAKDB/USFS_Southeast_Alaska_Drainage_Basin__SEAKDB__Watersheds.shp")

dat2 %>%
  mutate(site_no = as.character(site_no)) %>%
  left_join(sites, by="site_no") %>%
  select(site_no, lat = dec_lat_va, lon = dec_long_va) -> sites_withC

ggplot(data = SEAKDB) +
  geom_sf(alpha=0.7) +
  theme_bw() +
  geom_point(aes(x=lon, y=lat), data = sites_withC, color="#00fbff") +
  labs(title = "USGS sites with alkalinity data") 

# highlight watersheds with consistent data

dat2 %>%
  mutate(Date = as.Date(DATE, format = "%m/%d/%Y")) %>%
  mutate(Year = lubridate::year(Date)) %>%
  group_by(Year, site_no) %>%
  add_tally() %>%
  filter(n > 5) %>%
  mutate(yday = lubridate::yday(Date)) %>%
  mutate(site_no = as.character(site_no)) %>%
  left_join(sites, by="site_no") %>%
  distinct(site_no, longitude = dec_long_va, latitude = dec_lat_va) %>%
  filter(!is.na(longitude)) -> consistent_sites

sites_sf <- st_as_sf(consistent_sites, coords = c("longitude", "latitude"), crs = st_crs(SEAKDB), agr = "constant")

intersection <- st_within(sites_sf, SEAKDB)


closest <- list()
for(i in seq_len(nrow(sites_sf))){
  closest[[i]] <- SEAKDB[which.min(
    st_distance(SEAKDB, sites_sf[i,])),]
}
bind_rows(closest) %>%
  pull(OBJECTID) -> highlighSheds

toFilter <- c(unlist(intersection), highlighSheds)

SEAKDB %>%
  mutate(highlight = ifelse(OBJECTID %in% toFilter, "data", "nodata")) -> SEAKDB

require(leaflet)  
factpal <- colorFactor(topo.colors(2), SEAKDB$highlight)

SEAKDB %>%
  filter(highlight == "data") %>%
  leaflet() %>%
  addPolygons(fillColor = ~"#d694ff", fillOpacity = 0.8, smoothFactor = 0.5, color="black") %>%
  addProviderTiles("CartoDB.Positron")

