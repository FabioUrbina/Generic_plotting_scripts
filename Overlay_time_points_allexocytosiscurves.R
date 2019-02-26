#load tidyverse, which has dplayr and ggplot2
require(tidyverse)

#load reshape2 to melt/cast the data
require(reshape2)

require(ggplot2)
#load in the data curves
Curves <- read.csv("D:/Fabio/WT_fluor_curves_3level.csv",header = TRUE)
Curves <-read.csv("D:/Fabio/WT_fluor_curves_norm_split_minus_staygone.csv",header = TRUE)
#normalize each row

#First, get the background average

stand_curves <- t(apply(Curves[3:53], 1, function(x)(x-min(x))/(max(x)-min(x))))
stand_curves_BG <- t(apply(Curves[3:52], 1, function(x)(x-mean(x[1:22]))/sd(x[1:22])))
stand_curves_combined <- data.frame(Curves[1:2],stand_curves)
stand_curves_combined_2 <- data.frame(Curves[1:2], stand_curves_BG)

write.csv(stand_curves_combined_2,file = "D:/Fabio/WT_fluor_curves_norm.csv")
#melt the data into long form
curves2 <- melt(stand_curves_combined)
curves2_BG <- melt(stand_curves_combined_2)


curves2_BG <- melt(Curves)

#Standardize the data curves, with mean 0 and standard deviation 1
#plot the new long format

p <- ggplot(curves2, aes(x = variable, y = value, color = Cell_number, group = Cell_number))+ stat_summary(fun.data = "mean_cl_boot", geom = "smooth")+ theme(legend.position = "none") + ylim(c(-0.2,1.5))

p

p2 <- ggplot(curves2, aes(x = variable, y = value, color = Cell_number, group = Cell_number)) + stat_summary(fun.data = "mean_cl_boot", geom = "smooth") + theme(legend.position="none")
p2

p3 <- ggplot(curves2, aes(x = variable, y = value,color = Cell_av, group = Cell_av)) + stat_summary(fun.data ="mean_cl_boot", geom = "smooth",size = 2, color = "black") + theme(legend.position="none")
p3 <- geom_line(data=curves2,aes(x=variable,y=value,color=Cell_number,group=Cell_number),alpha = 0.5, color = "gray")
p3

p4 <-ggplot(curves2, aes(x = variable, y = value,color = Cell_av, group = Cell_av)) + geom_line(data=curves2,aes(x=variable,y=value,color=Cell_number,group=Cell_number),alpha = 0.5, color = "gray")
p4 +  stat_summary(fun.data ="mean_cl_boot", geom = "smooth",size = 1, color = "black") + theme(legend.position="none")

#connect the line points; do the average possibly?
