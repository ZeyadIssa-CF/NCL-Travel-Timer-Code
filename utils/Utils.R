
library(traveltimeR)
library(ggplot2)
library(sf)
library(geojsonsf)
library(plotly)
library(ggmap)
library(tidyverse)
library(lubridate)
library(dplyr)
library(httr)
library(geojsonio)
library(geojsonsf)
library(sf)

#FUNCTION FROM UTILS
create_matrix<-function(hospitals,lsoa_map,lsoa_filter,provider,time,method,travel_type,fast){
  
  #Key: add it to your envs, 
  Sys.setenv(TRAVELTIME_ID = "INSERT ID")
  Sys.setenv(TRAVELTIME_KEY = "INSERT KEY")
  
  #Selections
  value <- 'travel_time'
  ics_limit <- T

# Create Matrix -----------------------------------------------------------------
  
  #Loops; inefficient but works, just gets you every single hospital in the list and compares to it. 
  for(i in 1:length(hospitals$hospital)){
    
    # Generate Destination ID (LONG/LAT)
    hospital_id <- hospitals$hospital[i]
    hospital_lat <- hospitals$lat[i]
    hospital_lon <- hospitals$lon[i]
    
    data<-c(make_location(
      id=hospital_id,
      coords = list(
        lat=hospital_lat,
        lng = hospital_lon
      )
    ))
    
    for(i in 1:length(lsoa_map$LSOA11CD)){
      data<-c(data,
              make_location(
                id=lsoa_map$LSOA11CD[i],
                coords = list(lat = lsoa_map$LAT[i],
                              lng = lsoa_map$LONG[i])))}
    
    if(fast==F){
      
      arrival<- make_search(id = "arrival",
                            arrival_location_id = hospital_id,
                            departure_location_ids = as.list(lsoa_map$LSOA11CD),
                            travel_time = 7200,
                            transportation = list(type = method),
                            properties = list(travel_type),
                            arrival_time = time)
      
      result <- time_filter(locations=data,arrival_searches=arrival)
      
    } else {
      
      arrival<- make_search(id = "arrival_fast",
                            arrival_location_id = hospital_id,
                            departure_location_ids = as.list(lsoa_map$LSOA11CD),
                            travel_time = 7200,
                            transportation = list(type = method),
                            properties = list(travel_type),
                            arrival_time_period = "weekday_morning")
      
      result <- time_filter_fast(data,arrival)
      
    }
    
    id<-c()
    tt<-c()
    
    #FAST is peak time; when FAST is false it will be set to the preset time which is in output data
    for (i in 1:length(result[["contentParsed"]][["results"]][[1]][["locations"]])){
      id <- c(id,result[["contentParsed"]][["results"]][[1]][["locations"]][[i]][["id"]])
      if(fast == T){
        tt <- c(tt,result[["contentParsed"]][["results"]][[1]][["locations"]][[i]][["properties"]][[travel_type]])} else {
          tt <- c(tt,result[["contentParsed"]][["results"]][[1]][["locations"]][[i]][["properties"]][[1]][["travel_time"]])}
    }
    
    assign(paste0('raw_',hospital_id),as.data.frame(1:length(result[["contentParsed"]][["results"]][[1]][["locations"]])))
    
    assign(paste0('con_',hospital_id),
           assign(paste0('con_',hospital_id),bind_cols(tt,id)) %>%
             rename('value'=`...1`,
                    'LSOA11CD'=`...2`) %>%
             mutate(provider = hospital_id))
  }
  
  # Combine -----------------------------------------------------------------
  
  dfs <- lapply(ls(pattern="con_"), function(x) get(x))
  
  All_providers <- rbindlist(dfs) %>%
    group_by(LSOA11CD) %>%
    mutate(rank = rank(value))
  
  All_providers_2 <- All_providers %>%
    group_by(LSOA11CD) %>%
    slice(which.min(value)) %>%
    mutate(ncl_check =
             case_when(provider %in% provider ~ 'NCL',
                       TRUE ~ 'Not NCL'))
return(All_providers)

}
