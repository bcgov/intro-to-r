# Copyright 2020 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


## Introduction to R Demonstration script. 

# written by genevieve perkins (genevieve.perkins@gov.bc.ca) 
# This script explored wetland data for the ESI project to 
# vizualise the spread of the data and utilise specific packages
# to access data from the BC data catalogue. 


# requires : WetPlots.xlsx


# set up R session --------------------------------------------------------

#install.packages(c("bcdata", "bcmaps", "sf","dplyr"))


# read in libraries 
library(bcdata)
library(bcmaps)
library(sf)
library(dplyr)
library(readxl)
library(mapview)
library(plotKML)
library(stringr)
library(purrr)


# read in and check field data --------------------------------------------

site <- read_excel("data/WetPlots.xlsx", 1)
plot <- read_excel("data/WetPlots.xlsx", 2)
veg <- read_excel("data/WetPlots.xlsx", 3)

# check the data 
site
plot
veg

# Format the veg data
# 1) convert the pc_cover to numeric by assigning a 1% to 
#    species with a "T" or Trace catergory. 
# 2) create a new column with the latin family name 

veg <- veg %>%
  mutate(pc_cover = ifelse(pc_cover == "T", 1, pc_cover),
         pc_cover = as.numeric(pc_cover),
         latin_family = word(latin, 1)) %>%
  filter(!is.na(species.name), 
         !is.na(pc_cover))


# Format the plot data 
# 
# 1) We can see geographic co-ordinates and some NAs - so we will remove these
plot <- plot %>%
  filter(!is.na(utm_zone))


# 2) The coordinates are in two UTM zones (9 and 10) 
unique(plot$utm_zone)


# we can convert both of these to a common projection (i.e WGS84). 
# We can build a function to do this ourselves. 

convert_utm <- function(data, utm, input_crs, output_crs = 4236) { 
   tdata <- data %>%
    filter(utm_zone == utm) %>%
    st_as_sf(coords = c("easting", "northing"), crs = input_crs) %>%
    st_transform(crs = output_crs)
   
   cbind(tdata, st_coordinates(tdata)) %>%
   st_drop_geometry()
}    

# now we can use the function with wrote to convert the two parts of the data 
# and join them back together into a single table. Note we now have an X, Y columns
# with out WGS coordinates. 

utm9 <- convert_utm(data = plot, utm = "9U" , input_crs =26909,  output_crs = 4236) 
utm10 <- convert_utm(data = plot, utm = "10U" , input_crs =26910,  output_crs = 4236) 

plot_wgs <- bind_rows(utm9, utm10) 

# the plot_wgs is a data frame but to plot this on a map we want to convert 
# this to a spatial object. There are two main packages to do this (sp) an 
# older package and (sf) a newer and more robust package. Lets convert firstly 
# to an sf package. 

plot_sf <- st_as_sf(plot_wgs, coords = c("X","Y"),  crs = 4236)

# we can check this in an interactive map using the package mapview
mapview(plot_sf)


#  Accessing other BC data sets -------------------------------------------

# lets now look at other datasets that are freely available. As most datasets 
# are stored in BC Albers projects (EPSG: 3005) we will firstly transform our
# data to 3005 so everything is in the same projection 

# first we need to transform plot data so we can use BC data packaeg 
plot <- plot_sf %>% st_transform(3005)


# 1. Water Drainage data 
#  read in the water drainage data set to set up and AOI: 
# https://catalogue.data.gov.bc.ca/dataset/water-survey-of-canada-sub-sub-drainage-areas

# download the water drainage and use as an area of interest using the bcdata package

ws <- get_layer("wsc_drainages", class = "sf") %>%
  select(SUB_DRAINAGE_AREA_NAME, SUB_SUB_DRAINAGE_AREA_NAME)

# lets subset this data set to include only the drainages in which out wetland plots 
# occur. First we can use an intersect to see what is the name of the polgons 
# (sub drainages) in which the points fall. 

study_area_drain <- ws %>% st_intersection(plot)  

# note this is point data with a new column added
# we can use this column to then subset our drainage polygon 

ws <- ws %>%
  filter(SUB_SUB_DRAINAGE_AREA_NAME %in% 
          study_area_drain$SUB_SUB_DRAINAGE_AREA_NAME) 

mapview(ws) + mapview(plot)


# For this demonstration I will pick one of the sub sub drainage areas for
# faster processing. This will be our area of interest (AOI) 

AOI <- ws %>%
  filter(SUB_SUB_DRAINAGE_AREA_NAME == "Bulkley")



# 2. Lets read in the road network for the AOI 

if(file.exists(file.path("demo","roads.rds"))) { 
  roads <- readRDS(file = file.path("demo","roads.rds"))

  } else {
  
  roads <- bcdc_query_geodata("bb060417-b6e6-4548-b837-f9060d94743e") %>%
    bcdata::filter(INTERSECTS(AOI)) %>% 
    collect()
  
  saveRDS(roads, file = "demo/roads.rds")
  }

head(roads)

# we only want to keep the columns with road class and surface type
roads <- roads %>%
  select(c(ROAD_CLASS, ROAD_SURFACE))

# lets see what types of roads we have 
roads.sum <- roads %>%
  st_drop_geometry() %>%
  group_by(ROAD_CLASS) %>%
  summarise(count = n())

# lets check the road surface types 
roads.surf.sum <- roads %>%
  st_drop_geometry() %>%
  group_by(ROAD_SURFACE) %>%
  summarise(count = n())

# lets keep only the main types of roads and not include overgrown surface
roads <- roads %>%
  filter(ROAD_CLASS %in% c("highway", "local", "unclassifed","resource"))%>%
  filter(ROAD_SURFACE != "overgrown")

## see what this looks like 
# mapview(roads)
# plot(st_geometry(roads))


# 3. Lets read in the wetland and river data 
waterbodies <- bcdc_query_geodata("cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6", crs = 3005) %>% # lakes
  bcdata::filter(INTERSECTS(AOI)) %>% 
  collect() 

rivers <- bcdc_query_geodata("f7dac054-efbf-402f-ab62-6fc4b32a619e", crs = 3005) %>% # rivers
  bcdata::filter(INTERSECTS(AOI)) %>% 
  collect() 

wetlands <- bcdc_query_geodata("93b413d8-1840-4770-9629-641d74bd1cc6", crs = 3005) %>% # wetlands
  bcdata::filter(INTERSECTS(AOI)) %>% 
  collect() 

mapview(waterbodies) + 
mapview(rivers) + 
mapview(wetlands)

# lets select only the smallest lakes 
waterbodies <- waterbodies %>%
  select(c(WATERBODY_POLY_ID, WATERBODY_TYPE, AREA_HA, GNIS_NAME_1)) %>%
  filter(AREA_HA < 0.5)
  
wetlands <- wetlands %>% # lakes
  select(c(WATERBODY_POLY_ID, WATERBODY_TYPE, AREA_HA, GNIS_NAME_1))


# lets get the centroid of each of the waterbodies and wetlands 
waterbodiesXY <- st_centroid(waterbodies) 
wbpt <- st_coordinates(waterbodiesXY) 
wbpt <- waterbodiesXY %>%
  cbind(wbpt)%>%
  st_drop_geometry()

wetlandsXY <- st_centroid(wetlands)
wetpt <- st_coordinates(wetlandsXY) 
wetpt <- wetlandsXY %>%
  cbind(wetpt) %>%
  st_drop_geometry()

waterpt <- bind_rows(wbpt, wetpt ) 
waterpt <- st_as_sf(waterpt, coords= c("X","Y"), crs = 3005)

mapview(waterpt)


# 4.BEC information 

# Download BEC - # Gets bec_sf zone shape and filters the desired subzones
bec_sf <- bec(class = "sf") %>%
  st_intersection(AOI) %>%
  select(ZONE, MAP_LABEL, ZONE_NAME ) %>%
  st_cast("MULTIPOLYGON")




# Part 2: Stratified random sampling of wetlands  ---------------------------------------------

# lets now create a random sample of wetlands 
# the criteria we want to work with 
#       - wetlands must be within X distance to a road
#       - create random points within the bec zones ( ) 
#       - dont want to sample where we already have data


# lets create a buffer around the roads and then determine which points fall within the 
# buffer 

mapview(roads) 

roads_b1000 <- st_buffer(roads, dist = 1000) %>% st_union()

roads_b50 <- st_buffer(roads, dist = 50) %>% st_union()

mapview(roads) + mapview(roads_b1000)+ mapview(roads_b50)

# now lets get the area more than 50 m and within 1000km 

road_aoi <- st_difference(roads_b1000,  roads_b50)

mapview(road_aoi) + mapview(waterpt)

# now lets keep only the wetlands that fall within our given distance from the road
# this might take some time (as it is a large data set)

if(file.exists(file.path("demo","waterpt.rds"))) { 
  waterpt <- readRDS(file = file.path("demo","waterpt.rds"))
  
} else {
  
  waterpt <-st_intersection(waterpt, road_aoi)

  saveRDS(waterpt, file = "demo/waterpt.rds")
}

waterpt

# We now have 1842 wetlands!


# we can now eliminate any points that have already been sampled - lets buffer out sites by 500m 
# to ensure no overlap with new points 

plot_exclude <- st_buffer(plot, dist = 500)

overlap_pts <- st_intersection(waterpt, plot_exclude)

waterpt <- waterpt %>%
  filter(!WATERBODY_POLY_ID %in% overlap_pts$WATERBODY_POLY_ID)


# We can now randomly select using the bec types to stratify first let us intersect the 
# point to add the bec zone name and id

bec_pts <- st_intersection(waterpt, bec_sf)

# make a list of unique bec variants 
bgc.ls <- as.list(unique(bec_pts$MAP_LABEL))


# If we want to sample 100 sites first we need to calculate the proportion of sites 
# to sample within each variant 

prop.site <- bec_pts %>%
  group_by(MAP_LABEL)%>%
  summarise(no.pts = n()) %>%
  st_drop_geometry() %>%
  mutate(perc = ceiling(no.pts / sum(no.pts)*100))


out <- lapply(bgc.ls, function(x) {
  no.pts <- prop.sites %>% 
    filter(MAP_LABEL == x) %>% 
    select(perc) %>% 
    pull
  sdata <- bec_pts  %>%  filter(MAP_LABEL == x)
  sample_n(sdata, no.pts)
})

out <- do.call("rbind", out)

mapview(out) + mapview(plot)

  
  
# write out as csv   
  
write.csv(out, file = "demo/wetland_sample.csv")

  
# Write out a KML file ----------------------------------------------------

# Now we can write out a KML file so we can 
# view this is google earth. First we need to convert to a sp object. 

out_sp <- as(out, "Spatial") 

# write out a kml 
kml(out_sp,
    file.name    = file.path("demo/wetland_points.kml"),
    points_names = out_sp$MAP_LABEL,
    colour    = "#FF0000",
    alpha     = 0.6,
    size      = 1,
    shape     = "http://maps.google.com/mapfiles/kml/pal2/icon18.png")


