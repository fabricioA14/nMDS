# nMDS + Envfit + plot

install.packages("vegan")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tibble")

data <- read.table("amb.txt", h=T, sep="\t", dec=".") #Read environmental data

library(dplyr)
library(vegan)

env<-data[ ,3:8] %>% decostand(method="standardize") #Exclude no numeric collumns and standardize enviromental variables

cor(env) #Correlation for verify colinearity

env$TDS <- NULL  # Exclude variables with more than 70% colinearity
env$Width <- NULL

data1<-read.table("bio.txt", h=T, sep="\t", dec=".") #Read biological data

bio<-data1[ ,2:51] %>% vegdist(data1, method="bray", binary=FALSE) #Exclude non numeric columns and make a distance matrix with bray curtis

sol <- metaMDS(bio) #run nMDS

NMDS = data.frame(MDS1 = sol$points[,1], MDS2 = sol$points[,2]) #Extract NMDS points

NMDS$Location<-data1$Species #Insert labels

vec.sp<-envfit(sol$points,env, perm=999) #Run envfit
vec.sp #Summary envfit

library(tibble)

vec.sp.df<-as.data.frame(vec.sp$vectors$arrows*sqrt(vec.sp$vectors$r)) %>% 
  add_column(Environmental = rownames(vec.sp.df)) #Extract arrows for plot

vec<-vec.sp.df[3:4,] #Exclude non-significant variables

library(ggplot2)

ggplot(data = NMDS, aes(MDS1, MDS2))+  #plot
  xlim(-0.70, 1.4)+
  ylim(-0.4, 0.6)+
  geom_point(data = NMDS,aes(group= Location, colour = Location), size=3)+
  scale_color_manual(values = c("#00AFBB", "#0B7A38", "#FF4B2B", "#1454CC", "#2DDB0F"))+
  geom_polygon(data=NMDS, aes(x=MDS1, y=MDS2,fill= NULL, colour=Location), alpha=0.0, size=1, linetype=1)+
  geom_segment(data=vec,aes(x=0,xend=MDS1,y=0,yend=MDS2),
               arrow = arrow(length = unit(1.5, "mm")),colour="#142A3D")+
  geom_text(data=vec,aes(x=MDS1,y=MDS2,label=Environmental), size=5, hjust=0.1, nudge_y = 0.1, nudge_x = 0.0)+
  coord_fixed()+
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))+
  theme(
    legend.position = c(0.85, 1),
    legend.justification = c("left", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )
