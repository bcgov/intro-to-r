## Demo 1. 

# determine sampling points for a fish monitoring survey


# read in libraries 

library(bcdata)
library(bcmaps)
library(sf)
library(dplyr)


# bring in BC boundary 
bc <- bcmaps::bc_bound()



# download the water drainage and use as an area of interest 

ws <- get_layer("wsc_drainages", class = "sf") %>%
  select(SUB_DRAINAGE_AREA_NAME, SUB_SUB_DRAINAGE_AREA_NAME) %>%
  filter(SUB_DRAINAGE_AREA_NAME %in% c("Nechako", "Skeena - Coast"))

## plot the data to verify locations 

#plot(ws["SUB_DRAINAGE_AREA_NAME"], key.pos = NULL)
#mapview::mapview(ws)

study_area <- ws %>% 
  filter(SUB_SUB_DRAINAGE_AREA_NAME == "Babine") %>%
  st_union()

# read in the VRI data 

vri <- bcdc_query_geodata("2ebb35d8-c82f-4a17-9c96-612ac3532d55") %>%
  bcdata::filter(INTERSECTS(study_area)) %>% 
  bcdata::select(c("BCLCS_LEVEL_2", "HARVEST_DATE")) %>% # Treed sites
  collect() 

# Download BEC - # Gets bec_sf zone shape and filters the desired subzones
bec_sf <- bec(class = "sf") %>%
  st_intersection(study_area)

if(!is.null(subzones)){
  bec_sf <- dplyr::filter(bec_sf, as.character(MAP_LABEL) %in% subzones)
  study_area <- st_intersection(study_area, bec_sf) %>% # The AOI is trimmed according to the bec_sf zones included
    summarise()
  st_write(study_area, file.path(res_folder, "AOI.gpkg"), delete_layer = TRUE)
  

# Road processing
roads <- bcdc_query_geodata("bb060417-b6e6-4548-b837-f9060d94743e") %>%
  bcdata::filter(INTERSECTS(study_area)) %>% 
  collect()



waterbodies <- study_area[0, ] # creates an empty sf dataframe
waterbodies <- bcdc_query_geodata("cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6", crs = epsg) %>% # lakes
  bcdata::filter(INTERSECTS(study_area)) %>% 
  collect() %>% {if(nrow(.) > 0){rbind(., waterbodies)} else waterbodies}
waterbodies <- bcdc_query_geodata("f7dac054-efbf-402f-ab62-6fc4b32a619e", crs = epsg) %>% # rivers
  bcdata::filter(INTERSECTS(study_area)) %>% 
  collect() %>% {if(nrow(.) > 0){rbind(., waterbodies)} else waterbodies}
waterbodies <- bcdc_query_geodata("93b413d8-1840-4770-9629-641d74bd1cc6", crs = epsg) %>% # wetlands
  bcdata::filter(INTERSECTS(study_area)) %>% 
  collect() %>% {if(nrow(.) > 0){rbind(., waterbodies)} else waterbodies}




Complete some spatial analysis to determine a random stratified sampling 


2)	Demo’s 
Bring in watershed (stratified random sampling) 
a.	Eca  Equiv cc..area 
b.	Salmon spawning ? FISS (intersect), https://catalogue.data.gov.bc.ca/dataset/known-bc-fish-observations-and-bc-fish-distributions
c.	Filter by watershed – 
d.	Promimity to road –  
e.	
f.	Road density? 
  g.	Generate random points ? 
  h.	, clearing , roads access, random site selection 
i.	Generate KML 
j.	Geopackages  export 

Double check tih Don and AMR 

