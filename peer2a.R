# read 
library(gridExtra)
library(grid)   # impute function
library(ggplot2) #  use ggplot2 for plotting figures
#library(plyr)
prop <- read.csv("C:/Users/mom/Documents/RepData_PeerAssessment2/prop.csv")
head(prop)
y <- prop
y$PROPDMGVAL <- y$PROPDMGVAL/(10^9)  
y
y$EVTYPE <- factor(y$EVTYPE, levels = y$EVTYPE)
q <- qplot(EVTYPE,PROPDMGVAL,data = y, 
      main="Property Damage by Eventype", 
      ylab="Damage in billions",xlab="Eventype") +
      geom_bar(stat = "identity",fill="orange") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
q
#
#
crop <- read.csv("C:/Users/mom/Documents/RepData_PeerAssessment2/crop.csv")
head(crop)
q1 <- qplot(EVTYPE,CROPDMGVAL/(10^9),data=crop, 
           main="Crop Damage by Eventype", 
           ylab="Damage in billions",xlab="Eventype") +
  geom_bar(stat = "identity",fill="orange") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
pushViewport(viewport(layout = grid.layout(1, 2)))
print(q, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(q1,vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
