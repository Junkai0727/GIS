library(sf)
library(here)
st_layers(here("data", "gadm36_AUS.gpkg"))

library(sf)
Ausoutline <- st_read(here("data", "gadm36_AUS.gpkg"), 
                      layer='gadm36_AUS_0')
print(Ausoutline)

AusoutlinePROJECTED <- Ausoutline %>%
  st_transform(.,3112)

print(AusoutlinePROJECTED)
library(sp)
library(raster)
jan<-raster(here("data", "wc2.1_5m_bio_1.tif"))
# have a look at the raster layer jan
jan
plot(jan)

# set the proj 4 to a new object
newproj<-"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# get the jan raster and give it the new proj4
pr1 <- jan %>%
  projectRaster(., crs=newproj)
plot(pr1)

pr1 <- pr1 %>%
  projectRaster(., crs="+init=epsg:4326")
plot(pr1)

library(fs)
dir_info("data/") 

library(tidyverse)
listfiles2<-dir_info("data/") %>%
  filter(str_detect(path, ".tif")) %>%
  dplyr::select(path)%>%
  pull()

#have a look at the file names 
listfiles

worldclimtemp <- listfiles %>%
  stack()

worldclimtemp

worldclimtemp[[1]]

worldclimtemp[[2]]

month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
           "13","14","15","16","17","18","19")

names(worldclimtemp) <- month

site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
          "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
          "Wollongong", "Logan City" )
lon <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
         138.6, 153.43, 149.13, 151.78, 150.89, 153.12)
lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)
#Put all of this inforamtion into one list 
samples <- data.frame(site, lon, lat, row.names="site")
# Extract the data from the Rasterstack for all points 
AUcitytemp<- raster::extract(worldclimtemp, samples)

Aucitytemp2 <- AUcitytemp %>% 
  as_tibble()%>% 
  add_column(Site = site, .before = "Jan")

Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")

Perthtemp <- Aucitytemp2[3,]

hist(as.numeric(Perthtemp))

userbreak<-c(8,10,12,14,16,18,20,22,24,26)
hist(as.numeric(Perthtemp), 
     breaks=userbreak, 
     col="red", 
     main="Histogram of as.numeric(Perthtemp)", 
     xlab="as.numeric(Perthtemp)", 
     ylab="Frequency")

plot(Ausoutline$geom)

install.packages("rmapshaper")

#load the rmapshaper package
library(rmapshaper)
#simplify the shapefile
#keep specifies the % of points
#to keep
AusoutSIMPLE<-Ausoutline %>%
  ms_simplify(.,keep=0.05)

plot(AusoutSIMPLE$geom)

print(Ausoutline)

crs(worldclimtemp)

Austemp <- Ausoutline %>%
  # now crop our temp data to the extent
  crop(worldclimtemp,.)

# plot the output
plot(Austemp)

exactAus <- Austemp %>%
  mask(.,Ausoutline, na.rm=TRUE)

squishdata<-exactAusdf%>%
  pivot_longer(
    cols = 1:12,
    names_to = "Month",
    values_to = "Temp"
  )