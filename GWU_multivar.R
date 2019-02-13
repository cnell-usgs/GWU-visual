
# data wrangling & plotting
library(tidyverse) # dplyr and ggplot2
library(reshape2)

# ordination
library(vegan)

# spatial analyses - for later
library(sf)
library(raster)
library(USAboundaries)

# color scales
library(viridis)


theme_set(theme_classic())


### read in data  

## seedbank data
seedbank<-read.csv('https://raw.githubusercontent.com/collnell/GWU-visual/master/seedbank.csv')
head(seedbank)

## Multivariate data   

#### visualize with heatmap  

seedbank<-seedbank%>%mutate(ID =paste(site, area, upland_type, rep, sep='_')) # collapse sample info

# melt to long form
comm.melt<-seedbank%>%
  dplyr::select(-site:-rep)%>%
  melt(id.vars='ID')
head(comm.melt) # now each row relfects a single species at each site

# plot heatmap
ggplot(comm.melt, aes(ID, variable))+
  geom_tile(aes(fill = value))+
  labs(x='',y='')+
  theme(axis.text.x = element_text(angle=90))+
  scale_fill_gradient(low='seagreen2',high='blue', name='Count', trans='log10', na.value=NA) # adjust scale to control appearance

## Ordination  


#### 1.1 organize data for NMDS    

# community matrix - sites x species
comm<-seedbank%>%
  column_to_rownames('ID')%>%
  dplyr::select(ABTH:VEPE2)
head(comm) 

### Multidimensional scaling (MDS)  

### NMDS workflow  


#### 1.2 look at metaMDS documentation  

?metaMDS



#### Exercise 1:  
#Run an NMDS of our seed bank data in 2 dimensions, using a bray-curtis dissimilarity matrix, with a maximum of 100 random starts  

# template: 
nmds_output <- metaMDS( )

#### Stress  

# visualize MDS stress
stressplot(nmds_output)

#### 1.3 convert output to a data frame & add our sample info. 

plotting <- scores(nmds_output) %>%
  data.frame() %>%
  #function to convert the rownames back into a column named "ID" %>%
  separate(ID, into = c("site","area","upland_type","rep"), sep = "_") %>% # separate is from the tidyr package in tidyverse 
  group_by(site)


### Exercise 2: plotting NMDS with sample points & categorical variables

#### 2.1 plotting points- plot your ordination output!  
r <- ggplot(plotting, aes(x = NMDS1, y = NMDS2))  +
  geom_point(size=3) 

r


#### 2.2 Plot design  
ggplot(plotting, aes(x = NMDS1, y = NMDS2))  +
  geom_point(size=3) 

#### 2.3 adding ellipses  

ggplot(plotting, aes(x = NMDS1, y = NMDS2))  +
  geom_point(size=3) 


#### 2.4 Play!

