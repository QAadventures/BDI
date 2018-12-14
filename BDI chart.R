#import dataset from .csv
BDI.score.sheet <- read.csv("~/Desktop/BDI score sheet.csv")
View(BDI.score.sheet)

#load libraries
library(ggplot2)
library(grid)
library(gridExtra)

#convert severity column from character data type to factor data type
BDIdateRedo$Severity <- factor(BDIdateRedo$Severity)
BDIdateRedo$Month <- factor(BDIdateRedo$Month)

#create color palette for severity factor data type
MyColors <- c("green","yellow", "orange", "brown1", "firebrick4", "purple")

#make plot with severity as factor & color code points by severity custom colors
g <- ggplot(BDIdateRedo, aes(x=Date, y=BDI, col=Severity)) + 
  geom_point(size=3) +
  scale_color_manual(values = MyColors) +
  
  #add plot title
  ggtitle("Date vs BDI score") +
  
  #add trend line
  stat_smooth(method = "gam", formula = y~s(x), size = 1, se = FALSE, colour = "blue4") +
  
  #plot theme - title, axis lables, gridlines
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.x = element_text(size = 18, face = "bold", vjust = 0.5)) +
  theme(axis.title.y = element_text(size = 18, face = "bold", vjust = 1.1)) +
  theme(axis.text.x=element_text(colour = "black", size = 12, angle = 60, vjust = 0.7)) +
  theme(axis.text.y=element_text(colour = "black", size = 12)) +
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  theme(panel.grid.minor.y=element_blank()) +
  
  #X & Y axis scaling & lables
  labs(x="Month", y="BDI score") +
  scale_x_continuous(breaks=c(0,30,60,90,120,150,180,210,240,270,300,330),
                     labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep","Oct","Nov","Dec")) +
  coord_cartesian(ylim = seq(0,45)) +
  scale_y_continuous(breaks=seq(0,45,5))

#add line y=20 to indicate clinical depression level startint point
g <- g + geom_hline(yintercept = 20, linetype = "dashed")

#add annotations
label <- grobTree(textGrob("Clinical depression", x=0.9,  y=0.47))

#view plot
g <- g + annotation_custom(label)
g
