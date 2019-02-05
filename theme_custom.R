
## basic template for custom ggplot2 theme
# see https://ggplot2.tidyverse.org/reference/theme.html
# for more on how to set different theme elements
theme_custom <- function(base_size=12, frame=TRUE) {
  (theme_classic(base_size=base_size, base_family='sans')
   + theme(plot.title = element_text(face = "bold",size = rel(1.2), hjust = 0.5),
           text = element_text(),
           panel.background = element_rect(colour = NA),
           plot.background = element_rect(colour = NA),
           panel.border = element_rect(fill = NA, color='black', size=2, linetype='solid'),
           axis.title = element_text(size = rel(1)),
           axis.title.y = element_text(angle=90,vjust =2),
           axis.title.x = element_text(vjust = -0.2),
           axis.text = element_text(size=rel(.9)), 
           axis.line = element_line(colour="black", size=.5),
           axis.ticks = element_line(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           legend.key = element_rect(colour = NA),
           legend.position = "bottom",
           legend.direction = "horizontal",
           legend.key.size= unit(0.4, "cm"),
           legend.margin = margin(0,0,0,0, unit='pt'),
           legend.title = element_text(size=rel(1)),
           plot.margin=unit(c(10,5,5,5),"mm"),
           strip.background=element_rect(colour=NA,fill=NA),
           strip.text = element_text(face="bold")))
}
g+theme_custom()
