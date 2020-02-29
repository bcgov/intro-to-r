## Demo 1. 

# lets read in the wetland data and explore the spatial data


# First install packages if needed 

install.packages(c("bcdata", "bcmaps", "sf","dplyr"))


# read in libraries 
library(bcdata)
library(bcmaps)
library(sf)
library(dplyr)
library(readxl)
library(mapview)
library(plotKML)

# read in our wetland data: 
site <- read_excel("data/WetPlots.xlsx", 1)
plot <- read_excel("data/WetPlots.xlsx", 2)
veg <- read_excel("data/WetPlots.xlsx", 3)

# check the data 
site
plot
veg

# format the veg data 
veg <- veg %>%
  mutate(pc_cover = ifelse(pc_cover == "T", 1, pc_cover),
         pc_cover = as.numeric(pc_cover),
         latin_family = word(latin, 1)) %>%
  filter(!is.na(species.name), 
         !is.na(pc_cover))


# format the plot data 

# We can see geographic co-ordinates and some NAs so lets deal with those
plot <- plot %>%
  filter(!is.na(utm_zone))

unique(plot$utm_zone)

convert_utm <- function(data, utm, input_crs, output_crs = 4236) { 
   tdata <- data %>%
    filter(utm_zone == utm) %>%
    st_as_sf(coords = c("easting", "northing"), crs = input_crs) %>%
    st_transform(crs = output_crs)
   
   cbind(tdata, st_coordinates(tdata)) %>%
   st_drop_geometry()
}    

utm9 <- convert_utm(data = plot, utm = "9U" , input_crs =26909,  output_crs = 4236) 
utm10 <- convert_utm(data = plot, utm = "10U" , input_crs =26910,  output_crs = 4236) 

plot_wgs <- bind_rows(utm9, utm10)

# plot the location of the sample points
plot_sf <- st_as_sf(plot_wgs, coords = c("X","Y"),  crs = 4236)

mapview::mapview(plot_sf)

# write out data as kml file to view in google earth 

library(plotKML)

plot_sp <- as(plot_sf, "Spatial") 
kml(plot_sp,
    file.name    = file.path("demo/wetland_plots.kml"),
    points_names = plot_sf$fid,
    colour    = "#FF0000",
    alpha     = 0.6,
    size      = 1,
    shape     = "http://maps.google.com/mapfiles/kml/pal2/icon18.png")



# read in other freely available data sets 

# first we need to transform plot data so we can use BC data packaeg 
plot <- plot_sf %>% st_transform(3005)


# bring in BC boundary 
bc <- bcmaps::bc_bound()

# download the water drainage and use as an area of interest 
ws <- get_layer("wsc_drainages", class = "sf") %>%
  select(SUB_DRAINAGE_AREA_NAME, SUB_SUB_DRAINAGE_AREA_NAME)

study_area_drain <- ws %>% st_intersection(plot)

ws <- ws %>%
  filter(SUB_SUB_DRAINAGE_AREA_NAME %in% 
          study_area_drain$SUB_SUB_DRAINAGE_AREA_NAME) 

mapview(ws) + mapview(plot)


# get a subset of area for faster processing 

AOI <- ws %>%
  filter(SUB_SUB_DRAINAGE_AREA_NAME == "Bulkley")


# Road processing
roads <- bcdc_query_geodata("bb060417-b6e6-4548-b837-f9060d94743e") %>%
  bcdata::filter(INTERSECTS(AOI)) %>% 
  collect()

readRDS(file.path("demo", "baselayers.RDS"))





# read in the VRI data 
vri <- bcdc_query_geodata("2ebb35d8-c82f-4a17-9c96-612ac3532d55") %>%
  bcdata::filter(INTERSECTS(AOI)) %>% 
  bcdata::select(c("BCLCS_LEVEL_2", "HARVEST_DATE")) %>% # Treed sites
  collect() 

# Download BEC - # Gets bec_sf zone shape and filters the desired subzones
bec_sf <- bec(class = "sf") %>%
  st_intersection(AOI)

save(roads, vri,bec_sf, file = "baselayers.RDS")
readRDS(file.path("demo", "baselayers.RDS"))


if(!is.null(subzones)){
  bec_sf <- dplyr::filter(bec_sf, as.character(MAP_LABEL) %in% subzones)
  study_area <- st_intersection(study_area, bec_sf) %>% # The AOI is trimmed according to the bec_sf zones included
    summarise()
  st_write(study_area, file.path(res_folder, "AOI.gpkg"), delete_layer = TRUE)
  

# Road processing
roads <- bcdc_query_geodata("bb060417-b6e6-4548-b837-f9060d94743e") %>%
  bcdata::filter(INTERSECTS(study_area)) %>% 
  collect()


 # creates an empty sf dataframe
waterbodies <- bcdc_query_geodata("cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6", crs = 3005) %>% # lakes
  bcdata::filter(INTERSECTS(AOI)) %>% 
  collect() 


rivers <- bcdc_query_geodata("f7dac054-efbf-402f-ab62-6fc4b32a619e", crs = 3005) %>% # rivers
  bcdata::filter(INTERSECTS(AOI)) %>% 
  collect() 


mapview(waterbodies) + mapview(rivers) 





# fix this bit stilll 
save(roads, vri,bec_sf, file = "baselayers.RDS")
load(file.path("demo", "baselayers.RDS"))

saveRDS(roads, waterbodies, rivers, file = "demo/baselayers.RDS")

readRDS(file.path("demo", "baselayers.RDS"))







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

