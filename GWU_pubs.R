########################################

## publication-quality figures

# packages
library(ggplot)
library(dplyr)
library(reshape2)
library(cowplot)

# read in data - DC climate
dc<-read.csv('https://raw.githubusercontent.com/collnell/GWU-visual/master/DC_climate.csv')
head(dc)

# plot current monthly temperatures, tmean
ggplot(dc, aes(month_name, tmean))+
  geom_boxplot()+
  geom_jitter(alpha=.2)

# list of axes in desired order
months<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

# reorder x-axis manually
ggplot(dc, aes(month_name, tmean))+
  geom_boxplot()+
  geom_jitter(alpha=.2)+
  scale_x_discrete(limits=months)

# reorder by variable
plot.tmean<-ggplot(dc, aes(reorder(month_name, month), tmean))+
  geom_boxplot()+
  geom_jitter(alpha=.2)
plot.tmean

# add axis label
plot.tmean+labs(x='Month', y='Temperature (C)', title='DC monthly temperature (1895-2018)')


# plot min and max temps
ggplot(dc, aes(month_name))+
  geom_boxplot(aes(y=tmin), color='blue')+
  geom_boxplot(aes(y=tmax), color='red')+
  scale_x_discrete(limits=months)+
  labs(x='Month', y='Temperature (C)')


## reshaping data
# id.vars are the grouping variables you would like to keep
dc.melt<-melt(dc, id.vars=c('year','month_name','month','period'), value.name = 'temp')


g<-ggplot(dc.melt, aes(month_name, temp, color = variable))+
  geom_jitter(alpha=.1)+
  geom_boxplot()+
  scale_x_discrete(limits=months)+
  labs(x='Month', y='Temperature (C)')

g

### facets
# by period
ggplot(dc.melt, aes(month_name, temp, color = variable))+
  geom_boxplot()+
  scale_x_discrete(limits=months)+
  labs(x='Month', y='Temperature (C)')+
  facet_wrap(~period)

# by variable
ggplot(dc.melt, aes(month_name, temp, color = variable))+
  geom_boxplot()+
  scale_x_discrete(limits=months)+
  labs(x='Month', y='Temperature (C)')+
  facet_wrap(~variable)

### color sclaes

## discrete colors
g<-ggplot(dc.melt, aes(month_name, temp, color = variable))+
  geom_jitter(alpha=.1)+
  geom_boxplot()+
  scale_x_discrete(limits=months)+
  labs(x='Month', y='Temperature (C)')

g+scale_color_manual(labels=c('Max','Min','Mean'),values=c('red','blue','grey'), name='')

## pacakges with color scales
#install.package(c('RColorBrewer','wesanderson','viridis'))
library(RColorBrewer)
display.brewer.all()

g+scale_color_brewer(palette='Set2')

library(wesanderson)
names(wes_palettes)

g+scale_color_manual(values=wes_palette('Darjeeling1'))

## gradient color scale
g<-ggplot(dc, aes(month_name, tmin))+
  geom_line(aes(color=tmin, group=year), alpha=.3)+ # what happens when change to color = year?
  scale_x_discrete(limits=months)+
  labs(x='Month', y='Temperature (C)')

g+scale_color_gradient2(low='gold',mid='orangered',high='purple', midpoint=10)

## colorblind friendly
library(viridis)

g+scale_color_viridis(direction=-1)
g+scale_color_viridis(option='magma')

### summarizing data
se<-function(x) sd(x, na.rm=TRUE)/sqrt(length(x))# standard error

## mean temp by period
temps<-dc.melt%>%
  group_by(period, variable)%>%
  summarize(mean = mean(temp), se = se(temp))

# plot mean and error
# shapes
ggplot(temps, aes(period, mean, shape=variable))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1, size=1)

#linetype
ggplot(temps, aes(period, mean, shape=variable))+
  geom_line(aes(group=variable, linetype=variable),color='gray', size=1)+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1, size=1)+
  scale_linetype_manual(values=c('dashed','solid','dashed'))

#barplot
ggplot(temps, aes(variable, mean, fill=period))+
  geom_bar(stat='identity')

plot.bar<-ggplot(temps, aes(variable, mean, fill=period))+
  geom_bar(stat='identity', position=position_dodge(1), color='black')+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(1))+
  scale_fill_manual(values=c('grey','white'))

##themes
g+theme_classic()
g+theme_bw()
g+theme_minimal()
g+theme_gray()

# set theme for R session - applies to all plots automatically
theme_set(theme_classic(base_size=18)) # scale font sizes

theme_classic

# rotate x-axis labels
g+theme(axis.text.x = element_text(angle=90))

# modify gridlines
g+theme(panel.grid.major=element_line(color='grey', linetype='dotted'))

# add box around plot
g+theme(panel.border = element_rect(color='black', fill=NA, size=2))

# remove elements
g+theme(axis.line = element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), legend.position='none')


## legends
g<-g+scale_color_viridis()
g+theme(legend.position='none')# top, bottom, left, right
g+theme(legend.position = c(0.2,.9), legend.direction='horizontal', legend.background=element_rect(fill=NA, color='black'))

### custom themes
# change parameters and apply to plots!
theme_example <- function (base_size = 12, base_family = "sans") {
  theme_classic(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.text = element_text(colour = "darkgreen"),
          axis.title.x = element_text(colour = "purple", size=20),
          axis.title.y = element_text(colour = "orangered", angle = 90, size=20),
          panel.background = element_rect(fill = "yellow", size=2),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major = element_blank(),
          plot.background = element_blank(),
          legend.position = 'top'
    )
}

g+theme_example()

### plotting temperature trends
head(dc)

dc.yr<-dc%>%
  group_by(period, year)%>%
  summarize(mean = mean(tmean), max=max(tmax), min=min(tmin))

# calculate historical mean
dc.past<-dc.yr%>%filter(period == 'historical')
hist.mean<-mean(dc.past$mean)
hist.se<-se(dc.past$mean)

## annotating plots
timeline<-ggplot(dc.yr, aes(year, mean))+
  geom_line()+
  geom_hline(yintercept =hist.mean, linetype = 'dashed')+
  geom_point(aes(color=anom_color))+
  labs(x='', y='Temperature (C)')+
  theme(legend.position='none')

timeline+geom_ribbon(aes(ymin=hist.mean-hist.se, ymax=hist.mean+hist.se), fill='gray', alpha=.5)


## plot temperature anomaly
dc.yr$anomaly<-dc.yr$mean-mean(dc.past$mean)
dc.yr$anom_color<-ifelse(dc.yr$anomaly > 0, 'higher','lower')

temp.time<-ggplot(dc.yr, aes(year, anomaly, fill=anom_color))+
  geom_bar(stat='identity')+
  geom_hline(yintercept = 0)+
  scale_fill_manual(values=c('red','blue'))+
  theme(legend.position='none')+
  labs(x='', y='Temperature anomaly')

temp.time+
  annotate(geom='text', y=-1, x=2010, label = 'historical mean', size=5)+
  annotate('segment', x=2010, xend=2011, y=-.9, yend=0, arrow=arrow(), size=1)

temp.time+geom_text(aes(label=round(mean,1)))

## multiple plots
library(cowplot)

plot_grid(temp.time, plot.bar, nrow=1, ncol=2, labels=c('a','b'))

### saving
## pdf
temp.time
ggsave("cool_fig.pdf", device='pdf', width = 6, height = 4, units='in', dpi=300)

#png
ggsave("cool_fig.png", device='png', width = 6, height = 4, units='in', dpi=300)