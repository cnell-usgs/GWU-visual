#################################
## Data visualization in R wtih ggplot2
# Colleen Nell Ph.D.  |  collnell@gwu.edu  |  [www.collnell.com](www.collnell.com) 
# Jan 30th 2019
#################################
# Workshop materials at www.github.com/collnell/GWU-visual  

## Today's objectives  
# overview of ggplot2 capabilities 
# understand logic behind ggplot2 syntax  
# create common/useful figures types  
# customize plot appearance  

# ggplot2 reference - https://ggplot2.tidyverse.org/reference/    
# ggplot2 cheatsheet - https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf  

#################################
# load libraries
install.packages('ggplot2')
install.packages('dplyr')

library(ggplot2)
library(dplyr)

#################################

# read in directly from github
birds<-read.csv('https://raw.githubusercontent.com/collnell/GWU-visual/master/bird_pred.csv')
str(birds)

# explore data
plot(birds)



ggplot()

# aes()  

ggplot(data=birds, aes())

# Assign 'predation' variable to the x-axis: 
ggplot(data=birds, aes(x=predation))

# geoms  
# https://ggplot2.tidyverse.org/reference/) define what shapes are used to represent the data   

# Assign 'predation' variable to the x-axis: 
ggplot(data=birds, aes(x=predation))+
  geom_histogram()


## distributions      

# geom_histogram(), geom_density(), geom_freqpoly(), geom_area(), geom_dotplot()      

ggplot(data=birds, aes(x=predation))+
  geom_histogram()

# arguments: `bins`, `binwidth`, and `breaks`  
# binwidth = size of bins (in units of variable)

# distributions: density plot     

ggplot(data=birds, aes(x=predation))+
  geom_density()

#################################

## layering  
# use `+` to add components to a plot  

ggplot(data=birds, aes(x=predation))+
  geom_histogram(bins=15)+
  geom_density(bw = .02)

# assign base plot to name, add layers
g<-ggplot(birds, aes(predation))
g+geom_histogram()
g+geom_histogram()+geom_density()

# distributions: by categorical variable         

ggplot(birds, aes(x=predation, group = diversity))+
  geom_density(bw = .02)


## aesthetics https://cran.r-project.org/web/packages/ggplot2/vignettes/ggplot2-specs.html    
#arguments inside the `aes()` are assigned to a variable, outside `aes()` is fixed  

colors()

# aesthetics: fill vs. color    

ggplot(birds, aes(x=predation))+
  geom_density(bw = .02, size=2)

#################################
## relationships: 2 numeric variables  
  
# Show the relationship between predation and another numeric variable
# assign to 'g'
ggplot(birds)

# add trendline  

g+geom_smooth(method='lm')

# layering & aesthetics  

ggplot(birds, aes(x=FD, y=predation))+
  geom_point(size=3, aes(color=diversity))+
  geom_smooth(method = 'lm', se=FALSE)

#################################
## scales: controlling aesthetic mapping    
# how a variable is mapped to an aesthetic (`color`, `size`, `shape` etc)  

ggplot(birds, aes(x=FD, y=predation))+
  geom_point(size=3, shape = 21, aes(fill=diversity))+
  geom_smooth(method = 'lm', se=FALSE, color='black')+
  scale_fill_manual(values=c('black', 'white'))

# set shape scale
ggplot(birds, aes(x=FD, y=predation))+
  geom_point(size=3, aes(shape=diversity, color=diversity))+
  geom_smooth(method = 'lm', se=FALSE, color='black')+
  scale_color_manual(values=c('black', 'grey'))+
  scale_shape_manual(values=c(21,22))

# scales: gradients  
g<-ggplot(birds, aes(x=abundance, y=predation))+
  geom_point(size=3, shape = 21, aes(fill=predation))+
  geom_smooth(method = 'lm', se=FALSE, color='black')


g+scale_fill_gradient(low = 'red',high = 'yellow')
g+scale_fill_gradientn(colors=rainbow(9))
g+scale_fill_gradient2(low = 'red', mid='yellow', high = 'pink', midpoint = 0.2)

# scales: axes  

g+xlim(0, NA) # ensure axis starts at 0

# scales: position  

g+scale_x_reverse()

#################################
## legends, labs, themes 

# legends  

g+scale_fill_manual(values=c('black', 'white'), name='Tree diversity', labels=c('Monoculture', 'Polyculture'))+
  theme(legend.position = 'bottom') # top, bottom, left, right or none

# + labs()     

gg<-g+scale_fill_manual(values=c('black', 'white'), name='Tree diversity', labels=c('Monoculture', 'Polyculture'))+
  theme(legend.position = 'bottom')

## change axis labels on gg
gg

## themes    
# themes set the appearance of non-data elements  

gg+theme(legend.position='top')

gg+theme_classic()
gg+theme_bw()
gg+theme_minimal()
gg+theme_gray()
gg+theme_economist()

## set  theme for sessioj
theme_set(theme_classic())

#################################
#### Using any data:  
# Show the relationship between 2 continuous variables  
# Use aesthetics and scales to customize appearance  
# Adjust labels and theme  

#################################
# comparisons    

ggplot(birds, aes(x = diversity, y = predation))+
  geom_boxplot()

ggplot(birds, aes(x = diversity, y = predation))+
  geom_violin()

plot.box<-ggplot(birds, aes(y = predation, x = diversity, color = diversity))+
  geom_boxplot()

plot.box + geom_point()
plot.box + geom_jitter(width=0.1)

# geom_bar()  
  
g<-ggplot(birds, aes(x=plot, y=height))+
  geom_bar(stat='identity')
g

# use scale_x_discrete to reorder axes  


## comparisons: means with error bars  

# summarizing data with dplyr    
#se = standard error = standard deviation / sqrt of n    

# summarize statistics for predation
summary.df<-birds%>%
  group_by(diversity)%>%
  summarize(mean = mean(predation), n = length(predation), sd = sd(predation))%>%
  mutate(se = sd/sqrt(n))

View(summary.df)


# tree-level data  
trees<-read.csv('https://raw.githubusercontent.com/collnell/GWU-visual/master/tree_pred.csv') 
head(trees)

# multiple grouping variables  
ggplot(trees, aes(diversity, predation))+
  geom_boxplot()+
  geom_jitter(aes(shape=tree))


ggplot(trees, aes(tree, predation))+
  geom_boxplot()+
  geom_jitter(aes(color=diversity))

# aggregate data by plot  

se<-function(x) sd(x, na.rm=TRUE)/sqrt(length(x))

plot.pred<-trees%>%
  group_by(diversity, tree)%>%
  summarize(mean=mean(predation), se=se(predation))
head(plot.pred)

# plot means by tree 

# tree point
ggplot(plot.pred, aes(tree, mean, color=diversity))+
  geom_point()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1)

# position=position_dodge(1))
# tree bar - position dodge
ggplot(plot.pred, aes(tree, mean, fill=diversity))+
  geom_bar(stat='identity', position=position_dodge(1))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1, position=position_dodge(1))

# plot means by diversity 

# div bar - position dodge
ggplot(plot.pred, aes(diversity, mean, fill=tree))+
  geom_bar(stat='identity', position=position_dodge(1))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1, position=position_dodge(1))

# div point position dodge
ggplot(plot.pred, aes(diversity, mean, color=tree))+
  geom_point(position=position_dodge(1))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1, position=position_dodge(1))

## clean it up!!

## +coord_flip() 
#reocrder discrete axis

ggplot(plot.pred, aes(reorder(tree,mean), mean, shape=diversity, group=tree))+
  scale_shape_manual(values=c(21,16))+
  geom_line(size=2, color='grey')+
  geom_point(size=3,fill='white')+
  coord_flip()+
  ylim(0,NA)+
  labs(x='Tree species', y='Predation rate')+
  scale_x_discrete(labels=c('Piscidia','Tabebuia','Swietenia','Cordia','Enterolobium','Ceiba'))+
  theme(legend.position='bottom')

#################################
# facets 
#facets create small multiples of your plot based upon a categorical variable:  

# facet by diversity
ggplot(plot.pred, aes(tree, mean, color=diversity))+
  geom_point(position=position_dodge(1))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1, position=position_dodge(1))+
  facet_wrap(~diversity)

