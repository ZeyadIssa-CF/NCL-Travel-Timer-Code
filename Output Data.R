
source('/Users/zeyadissa/Desktop/NCL handover/Generate Data.R')

#LSOA bounds
url_lsoa11 <- 'https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA_Dec_2011_Boundaries_Generalised_Clipped_BGC_EW_V3/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson'
lsoa <- sf::st_read(url_lsoa11) 

hospitals <- read.csv('/Users/zeyadissa/Desktop/NCL handover/all_hospitals.csv')
mapping <- read.csv('/Users/zeyadissa/Desktop/NCL handover/lsoa_ics_mapping.csv')

#Generate Map
lsoa_map <- lsoa %>% 
  left_join(.,mapping,by='LSOA11CD')

lsoa_filter <- (lsoa_map %>% filter(ICB22NM == 'NHS North Central London Integrated Care Board'))$LSOA11CD

# PEAK DATA -----------------------------------------------------------------

travel_type <- 'travel_time'
method <- 'driving'
fast <- T

peak_data<-create_matrix(hospitals,lsoa_map,lsoa_filter,provider,time,method,travel_type,fast)

# PUBLIC TRANSPORT -----------------------------------------------------------------

method <- 'public_transport'
fast <- T
travel_type <- 'travel_time'

public_transport_data<-create_matrix(lsoa_filter,provider,time,method,travel_type,fast)

# OFF PEAK -----------------------------------------------------------------

travel_type <- 'travel_time'
method <- 'driving'
fast <- F
time <- strftime(as.POSIXlt('2022-04-12','UTC')+hours(3),"%Y-%m-%dT%H:%M:%S%z")

off_peak_data<-create_matrix(lsoa_filter,provider,time,method,travel_type,fast)
