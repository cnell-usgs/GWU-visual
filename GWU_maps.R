
### Open RStudio  
 
# download the `GWU_spatial.R` script  

# data wrangling & plotting
library(tidyverse) # dplyr and ggplot2
library(reshape2)

# spatial analyses - for later
library(sf) # working with vectors - polygons, lines, etc
library(raster) # raster
library(USAboundaries) 

# color scales
library(viridis)


theme_set(theme_bw())# set plotting theme for session


## Spatial analyses  

### resources  

### vector data  

# get MD boundaries - USA boundaries apckage
md.state<-us_states(resolution = 'high', states='maryland')
glimpse(md.state) # look at spatial object - basically a dataframe
md.state$geometry # contains spatial info

md.state[,10] # indexing similar to df

# get MD county shapefile
md.cos<-us_counties(resolution = 'high', states='maryland')
unique(md.cos$name) 

# make simple plot of MD counties using geom_sf
md.map<-ggplot(md.cos)+
  geom_sf(fill='blue', color='white')
md.map

st_crs(md.cos)

md.map+coord_sf(crs = st_crs(4326))

md.map+coord_sf(crs = st_crs(3857))

md.map+coord_sf(crs = st_crs(2163))# ESPG 2163 - USA equal area 'Albers' - better to show states proportionally -optimized for USA

md.map+coord_sf(crs = st_crs(2248))# NAD83 Maryland - http://spatialreference.org/ref/epsg/nad83-maryland/

md<-md.state%>%st_transform(2248)
md.cos<-md.cos%>%st_transform(2248)
st_crs(md) # units are in us ft

#### Plot MD shapefile  
md.map<-ggplot(md)+
  geom_sf()
md.map

# lat and lon of seedbank sites
sites<-data.frame(site = c('Blackwater','Elmwood','Anderson'),
                  lat = c(38.437283, 38.16083, 38.23056),
                  lon = c(-76.215753, -75.7961, -75.774267))


# convert coordinates to a sf object
sites.sf <- st_as_sf(sites, coords = c("lon", "lat"), crs=4326)%>%
  st_transform(2248) # assign crs
glimpse(sites.sf) # converting to sf drops coordinates

md.map+
  geom_sf(data=sites.sf, size=2.5, shape=21, fill='red')

# find coordinates for each site
st_coordinates(sites.sf) 

# add coordinates as lat and lon to sites.sf 
sites.sf$lon<-st_coordinates(sites.sf)[,1] # add longitude
sites.sf$lat<-st_coordinates(sites.sf)[,2] # add latitude

glimpse(sites.sf) # examine sf object

md.map+
  geom_sf(data=sites.sf, size=2.5, shape=21, fill='red')+
  geom_label(data=sites.sf, aes(lon, lat, label=site))

library(ggrepel) # spread overlapping labels

md.map+
  geom_sf(data=sites.sf, size=2.5, shape=21, fill='red')
geom_label_repel(data=sites.sf, aes(lon, lat, label=site))

st_distance(sites.sf) # all pairwise distances between sites

md.pt<-md%>%st_cast('POINT') # turn MD border into points
min(st_distance(md.pt, sites.sf)) # find the minimum distance and point along border 

## find the centroid of a polygon 
# add county labels - calculate longitude and latitude for center of each county
glimpse(md.cos)

# find centroid of each county
st_centroid(md.cos)

# use lon & lat for each centroid to place labels 
md.cos$lon<-st_coordinates(st_centroid(md.cos))[,1] # add longitude to sf
md.cos$lat<-st_coordinates(st_centroid(md.cos))[,2] # add latitude to sf

# create mape of MD counties colored by land area (aland)
md.co.map<-ggplot()+
  geom_sf(data=md, fill=NA, color='black')+
  geom_sf(data=md.cos, aes(fill=aland))+
  theme_bw()+
  scale_fill_viridis(alpha=.5)+
  theme(legend.position='none')
md.co.map

# label each county
md.co.map+geom_label_repel(data=md.cos, aes(x=lon, y=lat, label=name))

### Crop plot to region of interest   

#### subsetting vector data  
`st_intersection` finds the intersecting area between two vector objects  
```{r}
## determine which county each site is in, add to sf
sites.sf<-st_intersection(sites.sf, md.cos) # what counties do sites intersect with?
sites.sf$name # named counties

# filter shapefile to desired counties
site.cos<-md.cos%>%filter(name %in% sites.sf$name)

# crop map to just these counties
# recreate same map
co.map<-ggplot()+
  geom_sf(data=site.cos)+
  geom_sf(data=sites.sf, size=2, shape=21, fill='red', color='black')+
  geom_label_repel(data=sites.sf, aes(lon, lat, label=site))
co.map # counties missing wihtin map space - better to crop the map extent to show whole region

# determine full extent around sites
sites.bbox<-st_bbox(sites.sf)# bounding box
co.bbox<-st_crop(md.cos, sites.bbox)
glimpse(co.bbox) # US_survey_foot means spatial data is in US ft

## zoom in map extent to sites
ggplot()+
  geom_sf(data=co.bbox)+
  geom_sf(data=sites.sf)

# points are on edge - need a buffer around to look nice
# repeat same steps but use st_buffer() to add space around bbox
sites.bbox<-st_bbox(st_buffer(sites.sf, 20000))# since map is in ft
co.bbox<-st_crop(md.cos, sites.bbox)

ggplot()+
  geom_sf(data=co.bbox, fill='darkgreen', alpha=.4)+
  geom_sf(data=sites.sf)+
  geom_text(data=sites.sf, aes(label=site, lon, lat), hjust=0.25, vjust=-1)+
  theme_bw()


#### Read in DEM raster  
DEM = digital elevation model  
data source: (http://data.imap.maryland.gov/datasets/1c6ce663eb3b499b9495010cb8c89df6v)  

# download .tif file to working directory, read in
download.file('https://github.com/collnell/GWU-visual/blob/master/MD_DEM.tiff?raw=true', destfile=getwd())
dem<-raster('MD_DEM.tiff')

# look at file
dem
dem@data

projection(dem) # raster objects only take proj4 definitions - http://spatialreference.org/ref/epsg/nad83-maryland/
projection(dem)<-'+proj=lcc +lat_1=39.45 +lat_2=38.3 +lat_0=37.66666666666666 +lon_0=-77 +x_0=400000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=ft +no_def' # set crs

dem.df<-as.data.frame(dem, xy=TRUE)%>%filter(MD_DEM > 0)
str(dem.df)

# plot raster values in histogram
ggplot(dem.df, aes(MD_DEM))+
  geom_histogram()

ggplot(dem.df)+
  geom_raster(data=dem.df, aes(x, y))

# assign color scale to raster values
ggplot(dem.df)+
  geom_raster(data=dem.df, aes(x, y, fill=MD_DEM), alpha=.8)+
  scale_fill_viridis(option='magma', begin=.4)

#### Crop raster to extent of sites  

dem.bbox<-crop(dem,co.bbox)
dem.df<-as.data.frame(dem.bbox, xy=TRUE)%>%filter(MD_DEM != 0)

# plot it with sites
ggplot(dem.df)+
  geom_raster(data=dem.df, aes(x, y, fill=MD_DEM))+
  scale_fill_viridis(option='magma', begin=.4, 'DEM')+
  geom_sf(data=sites.sf, color='white', size=4)

sites.sf$DEM<-extract(dem.bbox, sites.sf, buffer=100, fun=mean)
glimpse(sites.sf)

## mapping with leaflet
# https://rstudio.github.io/leaflet/
# https://cengel.github.io/R-spatial/mapping.html#web-mapping-with-leaflet


# endangered species occurrences in MD from GBIF
# shapefile at : https://github.com/collnell/GWU-visual/tree/master/MD_endangeredspecies
sps<-st_read('MD_endangeredspecies')
glimpse(sps) #252 observations of various taxa
st_crs(sps)

ggplot(sps)+
  geom_sf(aes(color=phylum), alpha=.5)

## see whether any endangered species have been observed near sites

# create buffer around sites
sites.buff<-st_buffer(sites.sf, dist=10000)
#buffering efffectively turns point datain to polygon

# plot buffered points
site.plot<-ggplot(dem.df)+
  geom_sf(data=co.bbox)+
  geom_sf(data=sites.buff, color='red')+
  theme_bw()

site.plot

# add points for species
sps.bbox<-sps[co.bbox,]
glimpse(sps.bbox)

site.plot+
  geom_sf(data=sps.bbox, color='blue')

