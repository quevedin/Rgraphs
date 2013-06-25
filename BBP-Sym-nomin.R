library(ggplot2 )
library(grid) # for unit
library("RColorBrewer")

source('C:/Users/Kib/Downloads/documents-export-2013-01-24/geom_point.R')

setwd("C:/Users/Kib/Downloads/documents-export-2013-01-24/")

#AuAgBBP<-BondList
#AuAgBBP<-BondList

# Import BBP Data ---------------------------------------------------------
AuAgBBP <- read.csv("C:/Users/Kib/Downloads/documents-export-2013-01-24/BondList.csv")
AuAgBBP_order<-AuAgBBP[order(AuAgBBP$N_Ag,AuAgBBP$E_Free),]
#sorted by number of Ag atoms and then Energies

uniqueDataAuAgBBP<-unique(AuAgBBP$N_Ag)

###minimaBBP = AuAgIco_order[!duplicated(AuAgIco_order$N_Ag),]
#minimaBBP
minimaBBP<-AuAgBBP[0,]
for(i in uniqueDataAuAgBBP){
  subset<-AuAgBBP[which(AuAgBBP$N_Ag==i,arr.ind=TRUE),]
  subset2<-subset[which.min(subset$EEFinal),]
  #minima <- data.frame(minima,subset2)
  #print(subset2)
  minimaBBP<-rbind(minimaBBP,subset2)
}
#minimaBBP$First=FALSE
minimaBBP$First="E_Final"
#print(minimaBBP)

minimaBBP1st<-AuAgBBP[0,]
for(i in uniqueDataAuAgBBP){
  subset<-AuAgBBP[which(AuAgBBP$N_Ag==i,arr.ind=TRUE),]
  subset2<-subset[which.min(subset$E_KS..1st.),]
  #minima <- data.frame(minima,subset2)
  #print(subset2)
  minimaBBP1st<-rbind(minimaBBP1st,subset2)
}
#minimaBBP1st$First=TRUE
minimaBBP1st$First="E_1st"
#print(minimaBBP1st)

####


######
######

AuAgBBP_SYM=subset(AuAgBBP,(Schoenflies.symbol!="NO"))
AuAgBBP_SYM=AuAgBBP_SYM[order(AuAgBBP_SYM$N_Ag,AuAgBBP_SYM$E_Free),]
uniqueDataAuAgBBP_SYM<-unique(AuAgBBP$N_Ag)
minimaBBP_SYM<-AuAgBBP_SYM[0,]
for(i in uniqueDataAuAgBBP_SYM){
  subset<-AuAgBBP_SYM[which(AuAgBBP_SYM$N_Ag==i,arr.ind=TRUE),]
  subset2<-subset[which.min(subset$EEFinal),]
  #minima <- data.frame(minima,subset2)
  #print(subset2)
  minimaBBP_SYM<-rbind(minimaBBP_SYM,subset2)
}
#minimaBBP1st$First=TRUE
minimaBBP_SYM$First="E_Final"
#print(minimaBBP1st)

minSYMBBP=data.frame(minimaBBP_SYM$N_Ag, minimaBBP_SYM$NAME,
                      minimaBBP_SYM$Schoenflies.symbol, minimaBBP_SYM$EEFinal-minimaBBP$EEFinal)
colnames(minSYMBBP)=c("N_Ag","Names","Schoenflies.symbol","Difference")

minSYMBBPJoin=data.frame(minimaBBP$N_Ag, minimaBBP$NAME,
                          minimaBBP$Schoenflies.symbol, minimaBBP$Schoenflies.symbol.T , minimaBBP$EEFinal,
                          minimaBBP_SYM$N_Ag, minimaBBP_SYM$NAME,
                          minimaBBP_SYM$Schoenflies.symbol, minimaBBP_SYM$Schoenflies.symbol.T, minimaBBP_SYM$EEFinal,
                          minimaBBP_SYM$EEFinal-minimaBBP$EEFinal)

colnames(minSYMBBPJoin)=c("N_Ag_GS","Names_GS","Schoenflies.symbol_GS","Schoenflies.symbol_GS_T","EEFinal_GS",
                           "N_Ag_SYM","Names_SYM","Schoenflies.symbol_SYM","Schoenflies.symbol_SYM_T","EEFinal_SYM",
                           "Difference")


########


# Import Ico Data ---------------------------------------------------------
AuAgIco <- read.csv("C:/Users/Kib/Downloads/documents-export-2013-01-24/BondListIco.csv")
AuAgIco_order<-AuAgIco[order(AuAgIco$N_Ag,AuAgIco$E_Free),]
#sorted by number of Ag atoms and then Energies

uniqueDataAuAgIco<-unique(AuAgIco_order$N_Ag)
minimaIco<-AuAgIco[0,]
for(i in uniqueDataAuAgIco){
  subset<-AuAgIco[which(AuAgIco$N_Ag==i,arr.ind=TRUE),]
  subset2<-subset[which.min(subset$EEFinal),]
  #minima <- data.frame(minima,subset2)
  #print(subset2)
  minimaIco<-rbind(minimaIco,subset2)
}
#minimaIco$First=FALSE
minimaIco$First="E_Final"
#print(minimaIco)
#

#
minimaIco1st<-AuAgIco[0,]
for(i in uniqueDataAuAgIco){
  subset<-AuAgIco[which(AuAgIco$N_Ag==i,arr.ind=TRUE),]
  subset2<-subset[which.min(subset$E_KS..1st.),]
  #minima <- data.frame(minima,subset2)
  #print(subset2)
  minimaIco1st<-rbind(minimaIco1st,subset2)
}
#minimaIco1st$First=TRUE
minimaIco1st$First="E_1st"
#print(minimaIco1st)


######
######

AuAgIco_SYM=subset(AuAgIco,(Schoenflies.symbol!="NO"))
AuAgIco_SYM=AuAgIco_SYM[order(AuAgIco_SYM$N_Ag,AuAgIco_SYM$E_Free),]
uniqueDataAuAgIco_SYM<-unique(AuAgIco_SYM$N_Ag)
minimaIco_SYM<-AuAgIco_SYM[0,]
for(i in uniqueDataAuAgIco_SYM){
  subset<-AuAgIco_SYM[which(AuAgIco_SYM$N_Ag==i,arr.ind=TRUE),]
  subset2<-subset[which.min(subset$EEFinal),]
  #minima <- data.frame(minima,subset2)
  #print(subset2)
  minimaIco_SYM<-rbind(minimaIco_SYM,subset2)
}
#minimaIco1st$First=TRUE
minimaIco_SYM$First="E_Final"
#print(minimaIco1st)

# minSYMIco=data.frame(minimaIco_SYM$N_Ag, minimaIco_SYM$NAME, minimaIco_SYM$Schoenflies.symbol, 
#                      minimaIco_SYM$EEFinal-minimaIco$EEFinal)
# colnames(minSYMIco)=c("N_Ag","Name", "Schoenflies.symbol","Difference")
# 
# minSYMIcoJoin=data.frame(minimaIco$N_Ag, minimaIco$NAME,
#                           minimaIco$Schoenflies.symbol, minimaIco$EEFinal,
#                           minimaIco_SYM$N_Ag, minimaIco_SYM$NAME,
#                           minimaIco_SYM$Schoenflies.symbol, minimaIco_SYM$EEFinal,
#                           minimaIco_SYM$EEFinal-minimaIco$EEFinal)
# 
# colnames(minSYMIcoJoin)=c("N_Ag_GS","Names_GS","Schoenflies.symbol_GS","EEFinal_GS",
#                            "N_Ag_SYM","Names_SYM","Schoenflies.symbol_SYM","EEFinal_SYM",
#                            "Difference")

#
minSYMIco=data.frame(minimaIco_SYM$N_Ag, minimaIco_SYM$NAME,
                     minimaIco_SYM$Schoenflies.symbol, minimaIco_SYM$EEFinal-minimaIco$EEFinal)
colnames(minSYMIco)=c("N_Ag","Names","Schoenflies.symbol","Difference")

minSYMIcoJoin=data.frame(minimaIco$N_Ag, minimaIco$NAME,
                         minimaIco$Schoenflies.symbol, minimaIco$Schoenflies.symbol.T , minimaIco$EEFinal,
                         minimaIco_SYM$N_Ag, minimaIco_SYM$NAME,
                         minimaIco_SYM$Schoenflies.symbol, minimaIco_SYM$Schoenflies.symbol.T, minimaIco_SYM$EEFinal,
                         minimaIco_SYM$EEFinal-minimaIco$EEFinal)

colnames(minSYMIcoJoin)=c("N_Ag_GS","Names_GS","Schoenflies.symbol_GS","Schoenflies.symbol_GS_T","EEFinal_GS",
                          "N_Ag_SYM","Names_SYM","Schoenflies.symbol_SYM","Schoenflies.symbol_SYM_T","EEFinal_SYM",
                          "Difference")


########




# Import Cubo Data ---------------------------------------------------------
AuAgCubo <- read.csv("C:/Users/Kib/Downloads/documents-export-2013-01-24/BondListCubo.csv")
colnames(AuAgCubo)[13]="DE_Bulk_peratom"

AuAgCubo_order<-AuAgCubo[order(AuAgCubo$N_Ag,AuAgCubo$E_Free),]
#sorted by number of Ag atoms and then Energies

uniqueDataAuAgCubo<-unique(AuAgCubo$N_Ag)
minimaCubo<-AuAgCubo[0,]
for(i in uniqueDataAuAgCubo){
  subset<-AuAgCubo[which(AuAgCubo$N_Ag==i,arr.ind=TRUE),]
  subset2<-subset[which.min(subset$EEFinal),]
  #minima <- data.frame(minima,subset2)
  #print(subset2)
  minimaCubo<-rbind(minimaCubo,subset2)
}
#minimaCubo$First=FALSE
minimaCubo$First="E_Final"
#print(minimaCubo)
#

#
minimaCubo1st<-AuAgCubo[0,]
for(i in uniqueDataAuAgCubo){
  subset<-AuAgCubo[which(AuAgCubo$N_Ag==i,arr.ind=TRUE),]
  subset2<-subset[which.min(subset$E_KS..1st.),]
  #minima <- data.frame(minima,subset2)
  #print(subset2)
  minimaCubo1st<-rbind(minimaCubo1st,subset2)
}
#minimaCubo1st$First=TRUE
minimaCubo1st$First="E_1st"
#print(minimaCubo1st)



######
######

AuAgCubo_SYM=subset(AuAgCubo,(Schoenflies.symbol!="NO"))
AuAgCubo_SYM=AuAgCubo_SYM[order(AuAgCubo_SYM$N_Ag,AuAgCubo_SYM$E_Free),]
uniqueDataAuAgCubo_SYM<-unique(AuAgCubo$N_Ag)
minimaCubo_SYM<-AuAgCubo_SYM[0,]
for(i in uniqueDataAuAgCubo_SYM){
  subset<-AuAgCubo_SYM[which(AuAgCubo_SYM$N_Ag==i,arr.ind=TRUE),]
  subset2<-subset[which.min(subset$EEFinal),]
  #minima <- data.frame(minima,subset2)
  #print(subset2)
  minimaCubo_SYM<-rbind(minimaCubo_SYM,subset2)
}
#minimaCubo1st$First=TRUE
minimaCubo_SYM$First="E_Final"
#print(minimaCubo1st)

# minSYMCubo=data.frame(minimaCubo_SYM$N_Ag, minimaCubo_SYM$NAME,
#                       minimaCubo_SYM$Schoenflies.symbol, minimaCubo_SYM$EEFinal-minimaCubo$EEFinal)
# colnames(minSYMCubo)=c("N_Ag","Names","Schoenflies.symbol","Difference")
# 
# minSYMCuboJoin=data.frame(minimaCubo$N_Ag, minimaCubo$NAME,
#                           minimaCubo$Schoenflies.symbol, minimaCubo$EEFinal,
#                           minimaCubo_SYM$N_Ag, minimaCubo_SYM$NAME,
#                           minimaCubo_SYM$Schoenflies.symbol, minimaCubo_SYM$EEFinal,
#                           minimaCubo_SYM$EEFinal-minimaCubo$EEFinal)
# 
# colnames(minSYMCuboJoin)=c("N_Ag_GS","Names_GS","Schoenflies.symbol_GS","EEFinal_GS",
#                            "N_Ag_SYM","Names_SYM","Schoenflies.symbol_SYM","EEFinal_SYM",
#                            "Difference")
# #
minSYMCubo=data.frame(minimaCubo_SYM$N_Ag, minimaCubo_SYM$NAME,
                     minimaCubo_SYM$Schoenflies.symbol, minimaCubo_SYM$EEFinal-minimaCubo$EEFinal)
colnames(minSYMCubo)=c("N_Ag","Names","Schoenflies.symbol","Difference")

minSYMCuboJoin=data.frame(minimaCubo$N_Ag, minimaCubo$NAME,
                         minimaCubo$Schoenflies.symbol, minimaCubo$Schoenflies.symbol.T , minimaCubo$EEFinal,
                         minimaCubo_SYM$N_Ag, minimaCubo_SYM$NAME,
                         minimaCubo_SYM$Schoenflies.symbol, minimaCubo_SYM$Schoenflies.symbol.T, minimaCubo_SYM$EEFinal,
                         minimaCubo_SYM$EEFinal-minimaCubo$EEFinal)

colnames(minSYMCuboJoin)=c("N_Ag_GS","Names_GS","Schoenflies.symbol_GS","Schoenflies.symbol_GS_T","EEFinal_GS",
                          "N_Ag_SYM","Names_SYM","Schoenflies.symbol_SYM","Schoenflies.symbol_SYM_T","EEFinal_SYM",
                          "Difference")
########




# Import Deca Data ---------------------------------------------------------
AuAgDeca <- read.csv("C:/Users/Kib/Downloads/documents-export-2013-01-24/BondListDeca_Sym.csv")

AuAgDeca$N_Ag=as.numeric(AuAgDeca$N_Ag)
AuAgDeca$N_Au=as.numeric(AuAgDeca$N_Au)
AuAgDeca$DE1stStep=as.numeric(AuAgDeca$DE1stStep)

AuAgDeca=subset(AuAgDeca,DE1stStep >= -0.20*13)

AuAgDeca_order<-AuAgDeca[order(AuAgDeca$N_Ag,AuAgDeca$E_Free),]
#sorted by number of Ag atoms and then Energies



uniqueDataAuAgDeca<-unique(AuAgDeca$N_Ag)
minimaDeca<-AuAgDeca[0,]
for(i in uniqueDataAuAgDeca){
  subset<-AuAgDeca[which(AuAgDeca$N_Ag==i,arr.ind=TRUE),]
  subset2<-subset[which.min(subset$EEFinal),]
  #minima <- data.frame(minima,subset2)
  #print(subset2)
  minimaDeca<-rbind(minimaDeca,subset2)
}
#minimaDeca$First=FALSE
minimaDeca$First="E_Final"
#print(minimaDeca)
#

#
minimaDeca1st<-AuAgDeca[0,]
for(i in uniqueDataAuAgDeca){
  subset<-AuAgDeca[which(AuAgDeca$N_Ag==i,arr.ind=TRUE),]
  subset2<-subset[which.min(subset$E_KS..1st.),]
  #minima <- data.frame(minima,subset2)
  #print(subset2)
  minimaDeca1st<-rbind(minimaDeca1st,subset2)
}
#minimaDeca1st$First=TRUE
minimaDeca1st$First="E_1st"
#print(minimaDeca1st)

######
######

AuAgDeca_SYM=subset(AuAgDeca,(Schoenflies.symbol!="NO"))
AuAgDeca_SYM=AuAgDeca_SYM[order(AuAgDeca_SYM$N_Ag,AuAgDeca_SYM$E_Free),]
uniqueDataAuAgDeca_SYM<-unique(AuAgDeca$N_Ag)
minimaDeca_SYM<-AuAgDeca_SYM[0,]
for(i in uniqueDataAuAgDeca_SYM){
  subset<-AuAgDeca_SYM[which(AuAgDeca_SYM$N_Ag==i,arr.ind=TRUE),]
  subset2<-subset[which.min(subset$EEFinal),]
  #minima <- data.frame(minima,subset2)
  #print(subset2)
  minimaDeca_SYM<-rbind(minimaDeca_SYM,subset2)
}
#minimaDeca1st$First=TRUE
minimaDeca_SYM$First="E_Final"
#print(minimaDeca1st)

# minSYMDeca=data.frame(minimaDeca_SYM$N_Ag, minimaDeca_SYM$NAME,
#                       minimaDeca_SYM$Schoenflies.symbol, minimaDeca_SYM$EEFinal-minimaDeca$EEFinal)
# colnames(minSYMDeca)=c("N_Ag","Names","Schoenflies.symbol","Difference")
# 
# minSYMDecaJoin=data.frame(minimaDeca$N_Ag, minimaDeca$NAME,
#                           minimaDeca$Schoenflies.symbol, minimaDeca$EEFinal,
#                           minimaDeca_SYM$N_Ag, minimaDeca_SYM$NAME,
#                           minimaDeca_SYM$Schoenflies.symbol, minimaDeca_SYM$EEFinal,
#                           minimaDeca_SYM$EEFinal-minimaDeca$EEFinal)
# 
# colnames(minSYMDecaJoin)=c("N_Ag_GS","Names_GS","Schoenflies.symbol_GS","EEFinal_GS",
#                            "N_Ag_SYM","Names_SYM","Schoenflies.symbol_SYM","EEFinal_SYM",
#                            "Difference")
#
minSYMDeca=data.frame(minimaDeca_SYM$N_Ag, minimaDeca_SYM$NAME,
                      minimaDeca_SYM$Schoenflies.symbol, minimaDeca_SYM$EEFinal-minimaDeca$EEFinal)
colnames(minSYMDeca)=c("N_Ag","Names","Schoenflies.symbol","Difference")

minSYMDecaJoin=data.frame(minimaDeca$N_Ag, minimaDeca$NAME,
                          minimaDeca$Schoenflies.symbol, minimaDeca$Schoenflies.symbol.T , minimaDeca$EEFinal,
                          minimaDeca_SYM$N_Ag, minimaDeca_SYM$NAME,
                          minimaDeca_SYM$Schoenflies.symbol, minimaDeca_SYM$Schoenflies.symbol.T, minimaDeca_SYM$EEFinal,
                          minimaDeca_SYM$EEFinal-minimaDeca$EEFinal)

colnames(minSYMDecaJoin)=c("N_Ag_GS","Names_GS","Schoenflies.symbol_GS","Schoenflies.symbol_GS_T","EEFinal_GS",
                           "N_Ag_SYM","Names_SYM","Schoenflies.symbol_SYM","Schoenflies.symbol_SYM_T","EEFinal_SYM",
                           "Difference")
########

print(minSYMBBPJoin, row.names = FALSE)
print(minSYMCuboJoin,row.names = FALSE)
print(minSYMDecaJoin,row.names = FALSE)
print(minSYMIcoJoin, row.names = FALSE)

write.table(minSYMBBPJoin, file = "foo_nomin.csv", sep = ",", 
            col.names = NA, 
            append=TRUE, qmethod = "double")
write.table(minSYMCuboJoin, file = "foo_nomin.csv", sep = ",", 
            col.names = NA, 
            append=TRUE, qmethod = "double")
write.table(minSYMDecaJoin, file = "foo_nomin.csv", sep = ",", 
            col.names = NA, 
            append=TRUE, qmethod = "double")
write.table(minSYMIcoJoin, file = "foo_nomin.csv", sep = ",", 
            col.names = NA, 
            append=TRUE, qmethod = "double")

# Merge Data ---------------------------------------------------------

#colnames(AuAgCubo)[13]="DE_Bulk_peratom"

AuAg<-rbind(AuAgBBP,AuAgIco)
AuAg<-rbind(AuAg,AuAgDeca)
AuAg<-rbind(AuAg,AuAgCubo)
#AuAg<-rbind(AuAg,AuAgCubo)

#minima<-rbind(minimaBBP,minimaBBP1st)

#minima<-rbind(minima,minimaIco)

#minimaCubo



#minima<-rbind(minima,minimaIco1st)
###

### Calculate Excess Energy vs BBP
E_Au_BBP=minimaBBP$E_Free[minima$N_Ag==0][1]
E_Ag_BBP=minimaBBP$E_Free[minima$N_Ag==13][1]

(E_Au_BBP-E_Ag_BBP)/13.0


# Plot Data ---------------------------------------------------------
windows()

ggplot() + theme_bw() + 
  geom_point(data = subset(AuAgDeca,(Schoenflies.symbol=="NO")),
             # AuAgDeca
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol),width = I(3),lwd=3,alpha = I(0.75),size=2.5,shape=21) + 
  geom_point(data = subset(AuAgDeca,(Schoenflies.symbol=="D5h")),
             # AuAgDeca
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol),alpha = I(1.0),size=5,shape=23,
             width = I(3),lwd=3) +
 geom_point(data = subset(AuAgDeca,(Schoenflies.symbol!="NO" & Schoenflies.symbol!="D5h")),
            # AuAgDeca
            aes(x = N_Ag, y = EEFinal), #colour=Gap, # size = abs(Spin),
                colour="black",alpha = I(0.85),size=1,shape=0,
            width = I(3),lwd=3) +       
  #scale_color_manual(values=brewer.pal(12,"Paired")) +
  scale_color_manual(values=brewer.pal(8,"Accent")) +
  #scale_size_continuous()+
  geom_line(data = subset(AuAgDeca,(Schoenflies.symbol=="D5h")),
            aes(x = N_Ag, y = EEFinal),
                               #linetype="3313",
                               # %sprintf("%s_%s",minima$Type,minima$First) ) , 
                               alpha=I(0.5) , size=1 ) + 
  geom_line(data = minimaDeca, aes(x = N_Ag, y =EEFinal),
            linetype="dashed",
            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
            alpha=I(0.5) , size=1)
#   geom_point(data = subset(AuAgDeca,(Schoenflies.symbol=="D5h")),
#              # AuAgDeca
#              aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
#                  colour=Schoenflies.symbol),alpha = I(1.0),size=5,shape=23,
#              width = I(3),lwd=3)
##  labs(shape="Symmetry",colour="Symmetry")+
  theme(plot.title = element_text(size = rel(2), face="bold"),
        axis.line = element_line(size = 1)) +
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  #scale_x_continuous(breaks=0:14)+ 
  geom_hline(yintercept=0,size=1) +
  theme(plot.title = element_text(size = rel(2), 
                                  face="bold", colour="black"),
        #        axis.line = element_line(size = 1),
        axis.text=element_text( size=rel(2),
                                face="bold", ) ) + 
  theme(axis.title = element_text(size =  rel(2),face="bold", colour="black") ) +
  #  scale_x_continuous(expand=c(0,0))+ scale_y_continuous(expand=c(0,0)) +
  theme(legend.title = element_text(size=19,face="bold" ), 
        legend.text = element_text( size = 18, face = "bold"),
        plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y=element_text(vjust=0.025),
        axis.title.x=element_text(vjust=-.75)) +
  #scale_colour_manual(values=c("blue4","red4","green4","black")  ,  
  #                    breaks = c("Minima","Minima 1st","green4","black")  , 
  #                    labels=c("Minima","Minima 1st","BBP","BBP") ) 
  
  #scale_shape_manual(values=c(25,24,23,22,21,20,19,18,17,16,15,14))  + 
  #scale_size_continuous(range=c(3,9))+
  xlab("Number of Ag Atoms") + ylab("Excess Energy (eV)") +
  #labs(colour="Gap",size="Spin")+ 
  labs(
#    title = "Energy Excess vs Silver Atoms (Deca)" , #colour="Gap",size="Spin",size="Spin",
                                       #alpha="",shape="Seed"
       # ) + #theme(plot.title = element_text(size = rel(2), 
                                           #face="bold",),
                 axis.line = element_line(size = 1) ) + scale_linetype_manual(guide = FALSE) +
  guides(colour = guide_legend(override.aes = list(
    shape= c(
      #22,22,22,22,22,
      23,21),
    size=c(
      #4,4,4,4,4,
      5,2.5), 
    width = c(
      #3,3,3,3,3,
      3,3),
    lwd=c(
      #3,3,3,3,3,
      3,3) ) ) ) +
  scale_size_manual(values = c(3, 5), guide = FALSE) +
  theme(legend.key = element_blank(),
        legend.position = c(.9, .27),
        legend.background=element_blank(),
        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
        #panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 20, face = "bold"),
        panel.border = element_rect(colour = "black",size = 1) ) +
#  guides(colour = guide_legend(override.aes = list(shape=c(16,17,18,15)))
#  ) +
#  theme(legend.key = element_blank(),legend.position = c(.525, .775),legend.background=element_blank(),
#        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
#        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
#        panel.grid.minor.x = element_blank(),
#        legend.text = element_text(size = 20, face = "bold"),
#        panel.border = element_rect(colour = "black",size = 1) )
  #theme(axis.title= element_text(size = rel(1),face="bold" #, colour="steelblue")
  #                               ) #+
  #labs(linetype="")
ggsave(file = "EnergyExcessDecaSym_nomin.pdf", width = 10, height = 7 )
dev.off()
##################################################################################



ggplot() + theme_bw() + 
  geom_point(data = subset(AuAgDeca,(Schoenflies.symbol=="NO")),
             # AuAgDeca
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol),width = I(3),lwd=3,alpha = I(0.75),size=2.5,shape=21) + 
  geom_point(data = subset(AuAgDeca,(Schoenflies.symbol=="D5h")),
             # AuAgDeca
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol),alpha = I(1.0),size=5,shape=23,
             width = I(3),lwd=3) +
  geom_point(data = subset(AuAgDeca,(Schoenflies.symbol!="NO" & Schoenflies.symbol!="D5h")),
             # AuAgDeca
             aes(x = N_Ag, y = EEFinal), #colour=Gap, # size = abs(Spin),
             colour="black",alpha = I(0.85),size=1,shape=0,
             width = I(3),lwd=3) +       
  #scale_color_manual(values=brewer.pal(12,"Paired")) +
  scale_color_manual(values=brewer.pal(8,"Accent")) +
  #scale_size_continuous()+
  geom_line(data = subset(AuAgDeca,(Schoenflies.symbol=="D5h")),
            aes(x = N_Ag, y = EEFinal),
            #linetype="3313",
            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
            alpha=I(0.5) , size=1 ) + 
  geom_line(data = minimaDeca, aes(x = N_Ag, y =EEFinal),
            linetype="dashed",
            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
            alpha=I(0.5) , size=1)
#   geom_point(data = subset(AuAgDeca,(Schoenflies.symbol=="D5h")),
#              # AuAgDeca
#              aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
#                  colour=Schoenflies.symbol),alpha = I(1.0),size=5,shape=23,
#              width = I(3),lwd=3)
##  labs(shape="Symmetry",colour="Symmetry")+
theme(plot.title = element_text(size = rel(2), face="bold"),
      axis.line = element_line(size = 1)) +
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  #scale_x_continuous(breaks=0:14)+ 
  geom_hline(yintercept=0,size=1) +
  theme(plot.title = element_text(size = rel(2), 
                                  face="bold", colour="black"),
        #        axis.line = element_line(size = 1),
        axis.text=element_text( size=rel(2),
                                face="bold", ) ) + 
  theme(axis.title = element_text(size =  rel(2),face="bold", colour="black") ) +
  #  scale_x_continuous(expand=c(0,0))+ scale_y_continuous(expand=c(0,0)) +
  theme(legend.title = element_text(size=19,face="bold" ), 
        legend.text = element_text( size = 18, face = "bold"),
        plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y=element_text(vjust=0.025),
        axis.title.x=element_text(vjust=-.75)) +
  #scale_colour_manual(values=c("blue4","red4","green4","black")  ,  
  #                    breaks = c("Minima","Minima 1st","green4","black")  , 
  #                    labels=c("Minima","Minima 1st","BBP","BBP") ) 
  
  #scale_shape_manual(values=c(25,24,23,22,21,20,19,18,17,16,15,14))  + 
  #scale_size_continuous(range=c(3,9))+
  xlab("Number of Ag Atoms") + ylab("Excess Energy (eV)") +
  #labs(colour="Gap",size="Spin")+ 
  labs(
    #    title = "Energy Excess vs Silver Atoms (Deca)" , #colour="Gap",size="Spin",size="Spin",
    #alpha="",shape="Seed"
    # ) + #theme(plot.title = element_text(size = rel(2), 
    #face="bold",),
    axis.line = element_line(size = 1) ) + scale_linetype_manual(guide = FALSE) +
  guides(colour = guide_legend(override.aes = list(
    shape= c(
      #22,22,22,22,22,
      23,21),
    size=c(
      #4,4,4,4,4,
      5,2.5), 
    width = c(
      #3,3,3,3,3,
      3,3),
    lwd=c(
      #3,3,3,3,3,
      3,3) ) ) ) +
  scale_size_manual(values = c(3, 5), guide = FALSE) +
  theme(legend.key = element_blank(),
        legend.position = c(.9, .27),
        legend.background=element_blank(),
        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
        #panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 20, face = "bold"),
        panel.border = element_rect(colour = "black",size = 1) ) +
  #  guides(colour = guide_legend(override.aes = list(shape=c(16,17,18,15)))
  #  ) +
  #  theme(legend.key = element_blank(),legend.position = c(.525, .775),legend.background=element_blank(),
  #        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
  #        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
  #        panel.grid.minor.x = element_blank(),
  #        legend.text = element_text(size = 20, face = "bold"),
  #        panel.border = element_rect(colour = "black",size = 1) )
  #theme(axis.title= element_text(size = rel(1),face="bold" #, colour="steelblue")
  #                               ) #+
  #labs(linetype="")
ggsave(file = "EnergyExcessDecaSym_nomin.pdf", width = 10, height = 7 )
dev.off()
##################################################################################


ggplot() + theme_bw() + 
  geom_point(data = subset(AuAgDeca,(Schoenflies.symbol=="NO")),
             # AuAgDeca
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol),width = I(3),lwd=3,alpha = I(0.75),size=2.5,shape=21) + 
  geom_point(data = subset(AuAgDeca,(Schoenflies.symbol=="D5h")),
             # AuAgDeca
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol),alpha = I(1.0),size=5,shape=23,
             width = I(3),lwd=3) +
  geom_point(data = subset(AuAgDeca,(Schoenflies.symbol!="NO" & Schoenflies.symbol!="D5h")),
             # AuAgDeca
             aes(x = N_Ag, y = EEFinal), #colour=Gap, # size = abs(Spin),
             colour="black",alpha = I(0.85),size=1,shape=0,
             width = I(3),lwd=3) +       
  #scale_color_manual(values=brewer.pal(12,"Paired")) +
  scale_color_manual(values=brewer.pal(8,"Accent")) +
  #scale_size_continuous()+
  geom_line(data = subset(AuAgDeca,(Schoenflies.symbol=="D5h")),
            aes(x = N_Ag, y = EEFinal),
            #linetype="3313",
            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
            alpha=I(0.5) , size=1 ) + 
  geom_line(data = minimaDeca, aes(x = N_Ag, y =EEFinal),
            linetype="dashed",
            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
            alpha=I(0.5) , size=1)
#   geom_point(data = subset(AuAgDeca,(Schoenflies.symbol=="D5h")),
#              # AuAgDeca
#              aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
#                  colour=Schoenflies.symbol),alpha = I(1.0),size=5,shape=23,
#              width = I(3),lwd=3)
##  labs(shape="Symmetry",colour="Symmetry")+
theme(plot.title = element_text(size = rel(2), face="bold"),
      axis.line = element_line(size = 1)) +
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  #scale_x_continuous(breaks=0:14)+ 
  geom_hline(yintercept=0,size=1) +
  theme(plot.title = element_text(size = rel(2), 
                                  face="bold", colour="black"),
        #        axis.line = element_line(size = 1),
        axis.text=element_text( size=rel(2),
                                face="bold", ) ) + 
  theme(axis.title = element_text(size =  rel(2),face="bold", colour="black") ) +
  #  scale_x_continuous(expand=c(0,0))+ scale_y_continuous(expand=c(0,0)) +
  theme(legend.title = element_text(size=19,face="bold" ), 
        legend.text = element_text( size = 18, face = "bold"),
        plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y=element_text(vjust=0.025),
        axis.title.x=element_text(vjust=-.75)) +
  #scale_colour_manual(values=c("blue4","red4","green4","black")  ,  
  #                    breaks = c("Minima","Minima 1st","green4","black")  , 
  #                    labels=c("Minima","Minima 1st","BBP","BBP") ) 
  
  #scale_shape_manual(values=c(25,24,23,22,21,20,19,18,17,16,15,14))  + 
  #scale_size_continuous(range=c(3,9))+
  xlab("Number of Ag Atoms") + ylab("Excess Energy (eV)") +
  #labs(colour="Gap",size="Spin")+ 
  labs(
    #    title = "Energy Excess vs Silver Atoms (Deca)" , #colour="Gap",size="Spin",size="Spin",
    #alpha="",shape="Seed"
    # ) + #theme(plot.title = element_text(size = rel(2), 
    #face="bold",),
    axis.line = element_line(size = 1) ) + scale_linetype_manual(guide = FALSE) +
  guides(colour = guide_legend(override.aes = list(
    shape= c(
      #22,22,22,22,22,
      23,21),
    size=c(
      #4,4,4,4,4,
      5,2.5), 
    width = c(
      #3,3,3,3,3,
      3,3),
    lwd=c(
      #3,3,3,3,3,
      3,3) ) ) ) +
  scale_size_manual(values = c(3, 5), guide = FALSE) +
  theme(legend.key = element_blank(),
        legend.position = c(.9, .27),
        legend.background=element_blank(),
        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
        #panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 20, face = "bold"),
        panel.border = element_rect(colour = "black",size = 1) ) +
  #  guides(colour = guide_legend(override.aes = list(shape=c(16,17,18,15)))
  #  ) +
  #  theme(legend.key = element_blank(),legend.position = c(.525, .775),legend.background=element_blank(),
  #        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
  #        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
  #        panel.grid.minor.x = element_blank(),
  #        legend.text = element_text(size = 20, face = "bold"),
  #        panel.border = element_rect(colour = "black",size = 1) )
  #theme(axis.title= element_text(size = rel(1),face="bold" #, colour="steelblue")
  #                               ) #+
  #labs(linetype="")
ggsave(file = "EnergyExcessDecaSym_nomin.pdf", width = 10, height = 7 )
dev.off()
##################################################################################


##################################################################################
min_BBP_C2v=subset(AuAgBBP , (Schoenflies.symbol=="C2v") )

# one liner
min_BBP_C2v_min2 = min_BBP_C2v[order(min_BBP_C2v$N_Ag,min_BBP_C2v$EEFinal),]
min_BBP_C2v_min2 = min_BBP_C2v_min2[!duplicated(min_BBP_C2v_min2$N_Ag),]
# min_BBP_C2v_min3 = with(min_BBP_C2v,!duplicated(min_BBP_C2v[with(min_BBP_C2v, {order(min_BBP_C2v$N_Ag,min_BBP_C2v$EEFinal)} ) ,]$N_Ag),]
min_BBP_ALL_min2=AuAgBBP[order(AuAgBBP$N_Ag,AuAgBBP$EEFinal),]
min_BBP_ALL_min2 = min_BBP_ALL_min2[!duplicated(min_BBP_ALL_min2$N_Ag ),]

# unique(subset(min_BBP_C2v, var==ave(N_Ag, EEFinal, FUN=min)))
# data.frame(id=sort(unique(min_BBP_C2v$EEFinal)),max=tapply(min_BBP_C2v,min_BBP_C2v$N_Ag,min))
# mat <- mat[sort(rownames(mat)), ] 

ggplot() + theme_bw() + 
  geom_point(data = subset(AuAgBBP,(Schoenflies.symbol=="NO")),
             # AuAgBBP
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol),width = I(3),lwd=3,alpha = I(0.75),size=2.5,shape=21) + 
  geom_point(data = subset(AuAgBBP,(Schoenflies.symbol=="C2v")),
             # AuAgBBP
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol),alpha = I(1.0),size=5,shape=23,
             width = I(3),lwd=3) +
  geom_point(data = subset(AuAgBBP,(Schoenflies.symbol!="NO" & Schoenflies.symbol!="C2v")),
             # AuAgBBP
             aes(x = N_Ag, y = EEFinal,colour="Other"), #colour=Gap, # size = abs(Spin),
             alpha = I(0.85),size=1,shape=0,
             width = I(3),lwd=3) +       
  #scale_color_manual(values=brewer.pal(12,"Paired")) +
  scale_color_manual(values=brewer.pal(8,"Accent")) +
  #scale_size_continuous()+
  geom_line(data = min_BBP_C2v_min2,
            aes(x = N_Ag, y = EEFinal),
            linetype="dashed", color=brewer.pal(8,"Accent")[1],
            #linetype="3313",
            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
            alpha=I(1.0) , size=1 ) + 
  #  geom_line(data = subset(AuAgBBP,(Schoenflies.symbol=="C2v")),
  #            aes(x = N_Ag, y = EEFinal),
  #            #linetype="3313",
  #            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
  #            alpha=I(0.5) , size=1 ) + 
  geom_line(data = minimaBBP, aes(x = N_Ag, y =EEFinal), 
            linetype="3313", color=brewer.pal(8,"Accent")[2],
            #linetype="dashed",
            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
            alpha=I(1.0) , size=1) + 
  #   geom_point(data = subset(AuAgBBP,(Schoenflies.symbol=="C2v")),
  #              # AuAgBBP
  #              aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
  #                  colour=Schoenflies.symbol),alpha = I(1.0),size=5,shape=23,
  #              width = I(3),lwd=3)
  ##  labs(shape="Symmetry",colour="Symmetry")+
  theme(plot.title = element_text(size = rel(2), face="bold"),
        axis.line = element_line(size = 1)) +
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:14) + 
  geom_hline(yintercept=0,size=1) +
  theme(plot.title = element_text(size = rel(2), 
                                  face="bold", colour="black"),
        #        axis.line = element_line(size = 1),
        axis.text=element_text( size=rel(2),
                                face="bold", ) ) + 
  theme(axis.title = element_text(size =  rel(2),face="bold", colour="black") ) +
  #  scale_x_continuous(expand=c(0,0))+ scale_y_continuous(expand=c(0,0)) +
  theme(legend.title = element_text(size=19,face="bold" ), 
        legend.text = element_text( size = 18, face = "bold"),
        plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y=element_text(vjust=0.025),
        axis.title.x=element_text(vjust=-.75)) +
  #scale_colour_manual(values=c("blue4","red4","green4","black")  ,  
  #                    breaks = c("Minima","Minima 1st","green4","black")  , 
  #                    labels=c("Minima","Minima 1st","BBP","BBP") ) 
  
  #scale_shape_manual(values=c(25,24,23,22,21,20,19,18,17,16,15,14))  + 
  #scale_size_continuous(range=c(3,9))+
  xlab("Number of Ag Atoms") + ylab("Excess Energy (eV)") +
  #labs(colour="Gap",size="Spin")+ 
  labs(
    #    title = "Energy Excess vs Silver Atoms (BBP)" , #colour="Gap",size="Spin",size="Spin",
    #alpha="",shape="Seed"
    # ) + #theme(plot.title = element_text(size = rel(2), 
    #face="bold",),
    axis.line = element_line(size = 1) ) + scale_linetype_manual(guide = FALSE) +
  guides(colour = guide_legend(override.aes = list(
    shape= c(
      #22,22,22,22,22,
      23,21, 22),
    size=c(
      #4,4,4,4,4,
      5,2.5, 4), 
    width = c(
      #3,3,3,3,3,
      3,3, 3),
    lwd=c(
      #3,3,3,3,3,
      3,3,3) ) ) ) +
  scale_size_manual(values = c(3, 5), guide = FALSE) +
  theme(legend.key = element_blank(),
        legend.position = c(.9, .27),
        legend.background=element_blank(),
        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
        #panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 20, face = "bold"),
        panel.border = element_rect(colour = "black",size = 1) ) +
  #  guides(colour = guide_legend(override.aes = list(shape=c(16,17,18,15)))
  #  ) +
  #  theme(legend.key = element_blank(),legend.position = c(.525, .775),legend.background=element_blank(),
  #        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
  #        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
  #        panel.grid.minor.x = element_blank(),
  #        legend.text = element_text(size = 20, face = "bold"),
  #        panel.border = element_rect(colour = "black",size = 1) )
  #theme(axis.title= element_text(size = rel(1),face="bold" #, colour="steelblue")
  #                               ) #+
  #labs(linetype="")
ggsave(file = "EnergyExcessBBPSym-T_nomin.pdf", width = 10, height = 7 )
dev.off()
##################################################################################





##################################################################################
min_Deca_D5h=subset(AuAgDeca , (Schoenflies.symbol=="D5h") )

# one liner
min_Deca_D5h_min2 = min_Deca_D5h[order(min_Deca_D5h$N_Ag,min_Deca_D5h$EEFinal),]
min_Deca_D5h_min2 = min_Deca_D5h_min2[!duplicated(min_Deca_D5h_min2$N_Ag),]
# min_Deca_D5h_min3 = with(min_Deca_D5h,!duplicated(min_Deca_D5h[with(min_Deca_D5h, {order(min_Deca_D5h$N_Ag,min_Deca_D5h$EEFinal)} ) ,]$N_Ag),]
min_Deca_ALL_min2=AuAgDeca[order(AuAgDeca$N_Ag,AuAgDeca$EEFinal),]
min_Deca_ALL_min2 = min_Deca_ALL_min2[!duplicated(min_Deca_ALL_min2$N_Ag ),]

# unique(subset(min_Deca_D5h, var==ave(N_Ag, EEFinal, FUN=min)))
# data.frame(id=sort(unique(min_Deca_D5h$EEFinal)),max=tapply(min_Deca_D5h,min_Deca_D5h$N_Ag,min))
# mat <- mat[sort(rownames(mat)), ] 

ggplot() + theme_bw() + 
  geom_point(data = subset(AuAgDeca,(Schoenflies.symbol=="NO")),
             # AuAgDeca
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol),width = I(3),lwd=3,alpha = I(0.75),size=2.5,shape=21) + 
  geom_point(data = subset(AuAgDeca,(Schoenflies.symbol=="D5h")),
             # AuAgDeca
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol),alpha = I(1.0),size=5,shape=23,
             width = I(3),lwd=3) +
  geom_point(data = subset(AuAgDeca,(Schoenflies.symbol!="NO" & Schoenflies.symbol!="D5h")),
             # AuAgDeca
             aes(x = N_Ag, y = EEFinal,colour="Other"), #colour=Gap, # size = abs(Spin),
             alpha = I(0.85),size=1,shape=0,
             width = I(3),lwd=3) +       
  #scale_color_manual(values=brewer.pal(12,"Paired")) +
  scale_color_manual(values=brewer.pal(8,"Accent")) +
  #scale_size_continuous()+
  geom_line(data = min_Deca_D5h_min2,
            aes(x = N_Ag, y = EEFinal),
            linetype="dashed", color=brewer.pal(8,"Accent")[1],
            #linetype="3313",
            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
            alpha=I(1.0) , size=1 ) + 
#  geom_line(data = subset(AuAgDeca,(Schoenflies.symbol=="D5h")),
#            aes(x = N_Ag, y = EEFinal),
#            #linetype="3313",
#            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
#            alpha=I(0.5) , size=1 ) + 
  geom_line(data = minimaDeca, aes(x = N_Ag, y =EEFinal), 
            linetype="3313", color=brewer.pal(8,"Accent")[2],
            #linetype="dashed",
            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
            alpha=I(1.0) , size=1) + 
#   geom_point(data = subset(AuAgDeca,(Schoenflies.symbol=="D5h")),
#              # AuAgDeca
#              aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
#                  colour=Schoenflies.symbol),alpha = I(1.0),size=5,shape=23,
#              width = I(3),lwd=3)
##  labs(shape="Symmetry",colour="Symmetry")+
theme(plot.title = element_text(size = rel(2), face="bold"),
      axis.line = element_line(size = 1)) +
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:14) + 
  geom_hline(yintercept=0,size=1) +
  theme(plot.title = element_text(size = rel(2), 
                                  face="bold", colour="black"),
        #        axis.line = element_line(size = 1),
        axis.text=element_text( size=rel(2),
                                face="bold", ) ) + 
  theme(axis.title = element_text(size =  rel(2),face="bold", colour="black") ) +
  #  scale_x_continuous(expand=c(0,0))+ scale_y_continuous(expand=c(0,0)) +
  theme(legend.title = element_text(size=19,face="bold" ), 
        legend.text = element_text( size = 18, face = "bold"),
        plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y=element_text(vjust=0.025),
        axis.title.x=element_text(vjust=-.75)) +
  #scale_colour_manual(values=c("blue4","red4","green4","black")  ,  
  #                    breaks = c("Minima","Minima 1st","green4","black")  , 
  #                    labels=c("Minima","Minima 1st","Deca","Deca") ) 
  
  #scale_shape_manual(values=c(25,24,23,22,21,20,19,18,17,16,15,14))  + 
  #scale_size_continuous(range=c(3,9))+
  xlab("Number of Ag Atoms") + ylab("Excess Energy (eV)") +
  #labs(colour="Gap",size="Spin")+ 
  labs(
    #    title = "Energy Excess vs Silver Atoms (Deca)" , #colour="Gap",size="Spin",size="Spin",
    #alpha="",shape="Seed"
    # ) + #theme(plot.title = element_text(size = rel(2), 
    #face="bold",),
    axis.line = element_line(size = 1) ) + scale_linetype_manual(guide = FALSE) +
  guides(colour = guide_legend(override.aes = list(
    shape= c(
      #22,22,22,22,22,
      23,21, 22),
    size=c(
      #4,4,4,4,4,
      5,2.5, 4), 
    width = c(
      #3,3,3,3,3,
      3,3, 3),
    lwd=c(
      #3,3,3,3,3,
      3,3,3) ) ) ) +
  scale_size_manual(values = c(3, 5), guide = FALSE) +
  theme(legend.key = element_blank(),
        legend.position = c(.9, .27),
        legend.background=element_blank(),
        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
        #panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 20, face = "bold"),
        panel.border = element_rect(colour = "black",size = 1) ) +
  #  guides(colour = guide_legend(override.aes = list(shape=c(16,17,18,15)))
  #  ) +
  #  theme(legend.key = element_blank(),legend.position = c(.525, .775),legend.background=element_blank(),
  #        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
  #        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
  #        panel.grid.minor.x = element_blank(),
  #        legend.text = element_text(size = 20, face = "bold"),
  #        panel.border = element_rect(colour = "black",size = 1) )
  #theme(axis.title= element_text(size = rel(1),face="bold" #, colour="steelblue")
  #                               ) #+
  #labs(linetype="")
ggsave(file = "EnergyExcessDecaSym-T_nomin.pdf", width = 10, height = 7 )
dev.off()
##################################################################################



###############################################################################
min_Cubo_Oh=subset(AuAgCubo , (Schoenflies.symbol.T=="Oh") )

# one liner
min_Cubo_Oh_min2 = min_Cubo_Oh[order(min_Cubo_Oh$N_Ag,min_Cubo_Oh$EEFinal),]
min_Cubo_Oh_min2 = min_Cubo_Oh_min2[!duplicated(min_Cubo_Oh_min2$N_Ag),]
# min_Cubo_Oh_min3 = with(min_Cubo_Oh,!duplicated(min_Cubo_Oh[with(min_Cubo_Oh, {order(min_Cubo_Oh$N_Ag,min_Cubo_Oh$EEFinal)} ) ,]$N_Ag),]
min_Cubo_ALL_min2=AuAgCubo[order(AuAgCubo$N_Ag,AuAgCubo$EEFinal),]
min_Cubo_ALL_min2 = min_Cubo_ALL_min2[!duplicated(min_Cubo_ALL_min2$N_Ag ),]

# unique(subset(min_Cubo_Oh, var==ave(N_Ag, EEFinal, FUN=min)))
# data.frame(id=sort(unique(min_Cubo_Oh$EEFinal)),max=tapply(min_Cubo_Oh,min_Cubo_Oh$N_Ag,min))
# mat <- mat[sort(rownames(mat)), ] 



ggplot() + theme_bw() + 
  geom_point(data = subset(AuAgCubo,(Schoenflies.symbol.T=="NO")),
             # AuAgCubo
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol.T),width = I(3),lwd=3,alpha = I(0.75),size=2.5,shape=21) + 
  geom_point(data = subset(AuAgCubo,(Schoenflies.symbol.T=="Oh")),
             # AuAgCubo
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol.T),alpha = I(1.0),size=5,shape=23,
             width = I(3),lwd=3) +
  geom_point(data = subset(AuAgCubo,(Schoenflies.symbol.T!="NO" & Schoenflies.symbol.T!="Oh")),
             # AuAgCubo
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour="Other"),alpha = I(0.85),size=2,shape=0,
             width = I(3),lwd=3) +   
  #scale_color_manual(values=brewer.pal(12,"Paired")) +
  geom_line(data = min_Cubo_Oh_min2,
            aes(x = N_Ag, y = EEFinal),
            linetype="dashed", color=brewer.pal(8,"Accent")[2],
            #linetype="3313",
            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
            alpha=I(1.0) , size=1 ) + 
  #  geom_line(data = subset(AuAgCubo,(Schoenflies.symbol=="Oh")),
  #            aes(x = N_Ag, y = EEFinal),
  #            #linetype="3313",
  #            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
  #            alpha=I(0.5) , size=1 ) + 
  geom_line(data = min_Cubo_ALL_min2 , aes(x = N_Ag, y =EEFinal), 
            linetype="3313", color=brewer.pal(8,"Accent")[8],
            #linetype="dashed",
            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
            alpha=I(1.0) , size=1) + 
  scale_color_manual(values=c(brewer.pal(8,"Accent")) ,
                     #brewer.pal(12,"Set3") 
                     labels = c(
                       expression( "Other" ),
                       expression( O[~h] ),
                       expression( "No Sym" )
                     )
  ) +
  theme(plot.title = element_text(size = rel(2), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:14)+ geom_hline(yintercept=0,size=1) +
  theme(plot.title = element_text(size = rel(2), 
                                  face="bold", colour="black"),
        #        axis.line = element_line(size = 1),
        axis.text=element_text( size=rel(2),
                                face="bold", ) ) + 
  theme(axis.title = element_text(size =  rel(2),face="bold", colour="black") ) +
  #  scale_x_continuous(expand=c(0,0))+ scale_y_continuous(expand=c(0,0)) +
  theme(legend.title = element_text(size=19,face="bold" ), 
        legend.text = element_text( size = 18, face = "bold"),
        plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y=element_text(vjust=0.025),
        axis.title.x=element_text(vjust=-.75)) +
  #scale_colour_manual(values=c("blue4","red4","green4","black")  ,  
  #                    breaks = c("Minima","Minima 1st","green4","black")  , 
  #                    labels=c("Minima","Minima 1st","BBP","BBP") ) 
  
  #scale_shape_manual(values=c(25,24,23,22,21,20,19,18,17,16,15,14))  + 
  #scale_size_continuous(range=c(3,9))+
  xlab("Number of Ag Atoms") + ylab("Excess Energy (eV)") +
  #labs(colour="Gap",size="Spin")+ 
  labs(
    #    title = "Energy Excess vs Silver Atoms (Cubo)" , #colour="Gap",size="Spin",size="Spin",
    #alpha="",shape="Seed"
    # ) + #theme(plot.title = element_text(size = rel(2), 
    #face="bold",),
    axis.line = element_line(size = 1) ) + scale_linetype_manual(guide = FALSE) +
  guides(colour = guide_legend(override.aes = list(
    shape = c( 22,23,21 ),
    size  = c( 4 , 5,2.5 ),
    width = c( 3, 3,  3 ),
    lwd   = c( 3, 3,  3 ) 
    ) ) ) +
  # shape = c( rep( 22,length( levels(minimaCubo$Schoenflies.symbol.T) ) - 2 ),23,21 ),
  # size  = c(rep(4,length(levels(minimaCubo$Schoenflies.symbol.T))-2),5,2.5 ),
  # width = c(rep(3,length(levels(minimaCubo$Schoenflies.symbol.T)) ) ),
  # lwd   = c(rep(3,length(levels(minimaCubo$Schoenflies.symbol.T)) ) ) ) ) )+
  scale_size_manual(values = c(3, 5), guide = FALSE) +
  theme(legend.key = element_blank(),
        legend.position = c(.9, .3),
        legend.background=element_blank(),
        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
        #panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 20, face = "bold"),
        panel.border = element_rect(colour = "black",size = 1) 
#         labels = c(
#           expression( "Other" ),
#           expression( "No" ),
#           expression( O[~h] )
#         )
        ) +
  #  guides(colour = guide_legend(override.aes = list(shape=c(16,17,18,15)))
  #  ) +
  #  theme(legend.key = element_blank(),legend.position = c(.525, .775),legend.background=element_blank(),
  #        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
  #        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
  #        panel.grid.minor.x = element_blank(),
  #        legend.text = element_text(size = 20, face = "bold"),
  #        panel.border = element_rect(colour = "black",size = 1) )
  #theme(axis.title= element_text(size = rel(1),face="bold" #, colour="steelblue")
  #                               ) #+
  #labs(linetype="")
ggsave(file = "EnergyExcessCuboSym-T_nomin.pdf", width = 10, height = 7 )
#






###############################################################################
min_Ico_Ih=subset(AuAgIco , (Schoenflies.symbol.T=="Ih") )

# one liner
min_Ico_Ih_min2 = min_Ico_Ih[order(min_Ico_Ih$N_Ag,min_Ico_Ih$EEFinal),]
min_Ico_Ih_min2 = min_Ico_Ih_min2[!duplicated(min_Ico_Ih_min2$N_Ag),]
# min_Ico_Ih_min3 = with(min_Ico_Ih,!duplicated(min_Ico_Ih[with(min_Ico_Ih, {order(min_Ico_Ih$N_Ag,min_Ico_Ih$EEFinal)} ) ,]$N_Ag),]
min_Ico_ALL_min2=AuAgIco[order(AuAgIco$N_Ag,AuAgIco$EEFinal),]
min_Ico_ALL_min2 = min_Ico_ALL_min2[!duplicated(min_Ico_ALL_min2$N_Ag ),]

# unique(subset(min_Ico_Ih, var==ave(N_Ag, EEFinal, FUN=min)))
# data.frame(id=sort(unique(min_Ico_Ih$EEFinal)),max=tapply(min_Ico_Ih,min_Ico_Ih$N_Ag,min))
# mat <- mat[sort(rownames(mat)), ] 



ggplot() + theme_bw() + 
  geom_point(data = subset(AuAgIco,(Schoenflies.symbol.T=="NO")),
             # AuAgIco
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol.T),width = I(3),lwd=3,alpha = I(0.75),size=2.5,shape=21) + 
  geom_point(data = subset(AuAgIco,(Schoenflies.symbol.T=="Ih")),
             # AuAgIco
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol.T),alpha = I(1.0),size=5,shape=23,
             width = I(3),lwd=3) +
  geom_point(data = subset(AuAgIco,(Schoenflies.symbol.T!="NO" & Schoenflies.symbol.T!="Ih")),
             # AuAgIco
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour="Other"),alpha = I(0.85),size=2,shape=0,
             width = I(3),lwd=3) +   
  #scale_color_manual(values=brewer.pal(12,"Paired")) +
  geom_line(data = min_Ico_Ih_min2,
            aes(x = N_Ag, y = EEFinal),
            linetype="dashed", color=brewer.pal(8,"Accent")[1],
            #linetype="3313",
            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
            alpha=I(1.0) , size=1 ) + 
  #  geom_line(data = subset(AuAgIco,(Schoenflies.symbol=="Ih")),
  #            aes(x = N_Ag, y = EEFinal),
  #            #linetype="3313",
  #            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
  #            alpha=I(0.5) , size=1 ) + 
  geom_line(data = min_Ico_ALL_min2 , aes(x = N_Ag, y =EEFinal), 
            linetype="3313", color=brewer.pal(8,"Accent")[3],
            #linetype="dashed",
            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
            alpha=I(1.0) , size=1) + 
  scale_color_manual(values=c(brewer.pal(8,"Accent"),
                              c("#445632","#494066","#E0C39E","#D1940C","#C1665A")
                              #brewer.pal(12,"Set3") 
  ),   labels = c(
    expression( "Other" ),
    expression( I[~h] ),
    expression( "No Sym" ) 
  ) ) +
  #scale_size_continuous()+
  theme(plot.title = element_text(size = rel(2), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:14)+ geom_hline(yintercept=0,size=1) +
  theme(plot.title = element_text(size = rel(2), 
                                  face="bold", colour="black"),
        #        axis.line = element_line(size = 1),
        axis.text=element_text( size=rel(2),
                                face="bold", ) ) + 
  theme(axis.title = element_text(size =  rel(2),face="bold", colour="black") ) +
  #  scale_x_continuous(expand=c(0,0))+ scale_y_continuous(expand=c(0,0)) +
  theme(legend.title = element_text(size=19,face="bold" ), 
        legend.text = element_text( size = 18, face = "bold"),
        plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y=element_text(vjust=0.025),
        axis.title.x=element_text(vjust=-.75)) +
  #scale_colour_manual(values=c("blue4","red4","green4","black")  ,  
  #                    breaks = c("Minima","Minima 1st","green4","black")  , 
  #                    labels=c("Minima","Minima 1st","BBP","BBP") ) 
  
  #scale_shape_manual(values=c(25,24,23,22,21,20,19,18,17,16,15,14))  + 
  #scale_size_continuous(range=c(3,9))+
  xlab("Number of Ag Atoms") + ylab("Excess Energy (eV)") +
  #labs(colour="Gap",size="Spin")+ 
  labs(
    #    title = "Energy Excess vs Silver Atoms (Ico)" , #colour="Gap",size="Spin",size="Spin",
    #alpha="",shape="Seed"
    # ) + #theme(plot.title = element_text(size = rel(2), 
    #face="bold",),
    axis.line = element_line(size = 1) ) + scale_linetype_manual(guide = FALSE) +
  guides(colour = guide_legend(override.aes = list(
    shape = c( 22,23,21 ),
    size  = c( 4 , 5,2.5 ),
    width = c( 3, 3,  3 ),
    lwd   = c( 3, 3,  3 ) ) ) ) +
# shape = c( rep( 22,length( levels(minimaIco$Schoenflies.symbol.T) ) - 2 ),23,21 ),
# size  = c(rep(4,length(levels(minimaIco$Schoenflies.symbol.T))-2),5,2.5 ),
# width = c(rep(3,length(levels(minimaIco$Schoenflies.symbol.T)) ) ),
# lwd   = c(rep(3,length(levels(minimaIco$Schoenflies.symbol.T)) ) ) ) ) )+
  scale_size_manual(values = c(3, 5), guide = FALSE) +
  theme(legend.key = element_blank(),
        legend.position = c(.9, .3),
        legend.background=element_blank(),
        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
        #panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 20, face = "bold"),
        panel.border = element_rect(colour = "black",size = 1) ) +
  #  guides(colour = guide_legend(override.aes = list(shape=c(16,17,18,15)))
  #  ) +
  #  theme(legend.key = element_blank(),legend.position = c(.525, .775),legend.background=element_blank(),
  #        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
  #        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
  #        panel.grid.minor.x = element_blank(),
  #        legend.text = element_text(size = 20, face = "bold"),
  #        panel.border = element_rect(colour = "black",size = 1) )
  #theme(axis.title= element_text(size = rel(1),face="bold" #, colour="steelblue")
  #                               ) #+
  #labs(linetype="")
ggsave(file = "EnergyExcessIcoSym-T_nomin.pdf", width = 10, height = 7 )
##################################################################################


##################################################################################
ggplot() + theme_bw() + 
  geom_point(data = subset(AuAgIco,(Schoenflies.symbol=="NO")),
             # AuAgIco
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol),width = I(3),lwd=3,alpha = I(0.75),size=2.5,shape=21) + 
  geom_point(data = subset(AuAgIco,(Schoenflies.symbol=="Ih")),
             # AuAgIco
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol),alpha = I(1.0),size=5,shape=23,
             width = I(3),lwd=3) +
  geom_point(data = subset(AuAgIco,(Schoenflies.symbol!="NO" & Schoenflies.symbol!="Ih")),
             # AuAgIco
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol),alpha = I(0.85),size=4,shape=0,
             width = I(3),lwd=3) +       
  scale_color_manual(values=brewer.pal(12,"Paired")) +
  #scale_size_continuous()+
  geom_line(data = minimaIco, aes(x = N_Ag, y =EEFinal),
            linetype="dashed",
            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
            alpha=I(0.5) , size=1) + labs(shape="Symmetry",colour="Symmetry")+
  theme(plot.title = element_text(size = rel(2), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:14)+ geom_hline(yintercept=0,size=1) +
  theme(plot.title = element_text(size = rel(2), 
                                  face="bold", colour="black"),
        #        axis.line = element_line(size = 1),
        axis.text=element_text( size=rel(2),
                                face="bold", ) ) + 
  theme(axis.title = element_text(size =  rel(2),face="bold", colour="black") ) +
  #  scale_x_continuous(expand=c(0,0))+ scale_y_continuous(expand=c(0,0)) +
  theme(legend.title = element_text(size=19,face="bold" ), 
        legend.text = element_text( size = 18, face = "bold"),
        plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y=element_text(vjust=0.025),
        axis.title.x=element_text(vjust=-.75)) +
  #scale_colour_manual(values=c("blue4","red4","green4","black")  ,  
  #                    breaks = c("Minima","Minima 1st","green4","black")  , 
  #                    labels=c("Minima","Minima 1st","BBP","BBP") ) 
  
  #scale_shape_manual(values=c(25,24,23,22,21,20,19,18,17,16,15,14))  + 
  #scale_size_continuous(range=c(3,9))+
  xlab("Number of Ag Atoms") + ylab("Excess Energy (eV)") +
  #labs(colour="Gap",size="Spin")+ 
  labs(
    #    title = "Energy Excess vs Silver Atoms (Ico)" , #colour="Gap",size="Spin",size="Spin",
    #alpha="",shape="Seed"
    # ) + #theme(plot.title = element_text(size = rel(2), 
    #face="bold",),
    axis.line = element_line(size = 1) ) + scale_linetype_manual(guide = FALSE) +
  guides(colour = guide_legend(override.aes = list(shape= c(22,22,22,22,22,22,22,22,23,21),
                                                   size=c(4,4,4,4,4,4,4,4,5,2.5), width = c(3,3,3,3,3,3,3,3,3,3),lwd=c(3,3,3,3,3,3,3) )))+
  scale_size_manual(values = c(3, 5), guide = FALSE) +
  theme(legend.key = element_blank(),
        legend.position = c(.9, .27),
        legend.background=element_blank(),
        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
        #panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 20, face = "bold"),
        panel.border = element_rect(colour = "black",size = 1) ) +
  #  guides(colour = guide_legend(override.aes = list(shape=c(16,17,18,15)))
  #  ) +
  #  theme(legend.key = element_blank(),legend.position = c(.525, .775),legend.background=element_blank(),
  #        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
  #        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
  #        panel.grid.minor.x = element_blank(),
  #        legend.text = element_text(size = 20, face = "bold"),
  #        panel.border = element_rect(colour = "black",size = 1) )
  #theme(axis.title= element_text(size = rel(1),face="bold" #, colour="steelblue")
  #                               ) #+
  #labs(linetype="")
ggsave(file = "EnergyExcessIcoSYM_nomin.pdf", width = 10, height = 7 )
###############################################################


##################################################################################
ggplot() + theme_bw() + 
  geom_point(data = subset(AuAgIco,(Schoenflies.symbol.T=="NO")),
             # AuAgIco
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol.T),width = I(3),lwd=3,alpha = I(0.65),size=2.5,shape=21) + 
  geom_point(data = subset(AuAgIco,(Schoenflies.symbol.T=="Ih")),
             # AuAgIco
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol.T),alpha = I(0.9),size=5,shape=23,
             width = I(3),lwd=3) +
  geom_point(data = subset(AuAgIco,(Schoenflies.symbol.T!="NO" & Schoenflies.symbol.T!="Ih")),
             # AuAgIco
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol.T),alpha = I(0.75),size=4,shape=0,
             width = I(3),lwd=3) +
  scale_color_manual(values=c(brewer.pal(12,"Paired"),
                              c("#445632","#494066","#E0C39E","#D1940C","#C1665A")
                              #brewer.pal(12,"Set3") 
  ) ) +
  #scale_color_manual(values=brewer.pal(12,"Paired")) +
  #scale_size_continuous()+
  geom_line(data = minimaIco, aes(x = N_Ag, y =EEFinal),
            linetype="dashed",
            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
            alpha=I(0.5) , size=1) + labs(shape="Symmetry",colour="Symmetry")+
  theme(plot.title = element_text(size = rel(2), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:14)+ geom_hline(yintercept=0,size=1) +
  theme(plot.title = element_text(size = rel(2), 
                                  face="bold", colour="black"),
        #        axis.line = element_line(size = 1),
        axis.text=element_text( size=rel(2),
                                face="bold", ) ) + 
  theme(axis.title = element_text(size =  rel(2),face="bold", colour="black") ) +
  #  scale_x_continuous(expand=c(0,0))+ scale_y_continuous(expand=c(0,0)) +
  theme(legend.title = element_text(size=19,face="bold" ), 
        legend.text = element_text( size = 18, face = "bold"),
        plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y=element_text(vjust=0.025),
        axis.title.x=element_text(vjust=-.75)) +
  #scale_colour_manual(values=c("blue4","red4","green4","black")  ,  
  #                    breaks = c("Minima","Minima 1st","green4","black")  , 
  #                    labels=c("Minima","Minima 1st","BBP","BBP") ) 
  
  #scale_shape_manual(values=c(25,24,23,22,21,20,19,18,17,16,15,14))  + 
  #scale_size_continuous(range=c(3,9))+
  xlab("Number of Ag Atoms") + ylab("Excess Energy (eV)") +
  #labs(colour="Gap",size="Spin")+ 
  labs(
    #    title = "Energy Excess vs Silver Atoms (Ico)" , #colour="Gap",size="Spin",size="Spin",
    #alpha="",shape="Seed"
    # ) + #theme(plot.title = element_text(size = rel(2), 
    #face="bold",),
    axis.line = element_line(size = 1) ) + scale_linetype_manual(guide = FALSE) +
  guides(colour = guide_legend(override.aes = list(
    shape = c( rep( 22,length( levels(minimaDeca$Schoenflies.symbol.T) ) - 5 ), 23,21,22,22,22 ),
    size  = c(rep(4,length(levels(minimaDeca$Schoenflies.symbol.T))-5), 5, 2.5, 4, 4, 4 ),
    width = c(rep(3,length(levels(minimaDeca$Schoenflies.symbol.T)) ) ),
    lwd   = c(rep(3,length(levels(minimaDeca$Schoenflies.symbol.T)) ) ) ) ) )+
  scale_size_manual(values = c(3, 5), guide = FALSE) +
  theme(legend.key = element_blank(),
        legend.position = c(.935, .4),
        legend.background=element_blank(),
        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
        #panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 20, face = "bold"),
        panel.border = element_rect(colour = "black",size = 1) ) +
  #  guides(colour = guide_legend(override.aes = list(shape=c(16,17,18,15)))
  #  ) +
  #  theme(legend.key = element_blank(),legend.position = c(.525, .775),legend.background=element_blank(),
  #        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
  #        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
  #        panel.grid.minor.x = element_blank(),
  #        legend.text = element_text(size = 20, face = "bold"),
  #        panel.border = element_rect(colour = "black",size = 1) )
  #theme(axis.title= element_text(size = rel(1),face="bold" #, colour="steelblue")
  #                               ) #+
  #labs(linetype="")
ggsave(file = "EnergyExcessIcoSYM-T_nomin.pdf", width = 10, height = 7 )
###############################################################




#############################################
ggplot() + theme_bw() + 
  geom_point(data = subset(AuAgCubo,(Schoenflies.symbol=="NO")),
             # AuAgCubo
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol),width = I(3),lwd=3,alpha = I(0.75),size=2.5,shape=21) + 
  geom_point(data = subset(AuAgCubo,(Schoenflies.symbol=="Oh")),
             # AuAgCubo
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol),alpha = I(1.0),size=5,shape=23,
             width = I(3),lwd=3) +
  geom_point(data = subset(AuAgCubo,(Schoenflies.symbol!="NO" & Schoenflies.symbol!="Oh")),
             # AuAgCubo
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol),alpha = I(0.85),size=4,shape=0,
             width = I(3),lwd=3) +       
  #scale_color_manual(values=brewer.pal(12,"Paired")) +
  #scale_size_continuous()+
  geom_line(data = minimaCubo, aes(x = N_Ag, y =EEFinal),
            linetype="dashed",
            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
            alpha=I(0.5) , size=1) + labs(shape="Symmetry",colour="Symmetry")+
  theme(plot.title = element_text(size = rel(2), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:14)+ geom_hline(yintercept=0,size=1) +
  theme(plot.title = element_text(size = rel(2), 
                                  face="bold", colour="black"),
        #        axis.line = element_line(size = 1),
        axis.text=element_text( size=rel(2),
                                face="bold", ) ) + 
  theme(axis.title = element_text(size =  rel(2),face="bold", colour="black") ) +
  #  scale_x_continuous(expand=c(0,0))+ scale_y_continuous(expand=c(0,0)) +
  theme(legend.title = element_text(size=19,face="bold" ), 
        legend.text = element_text( size = 18, face = "bold"),
        plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y=element_text(vjust=0.025),
        axis.title.x=element_text(vjust=-.75)) +
  #scale_colour_manual(values=c("blue4","red4","green4","black")  ,  
  #                    breaks = c("Minima","Minima 1st","green4","black")  , 
  #                    labels=c("Minima","Minima 1st","BBP","BBP") ) 
  
  #scale_shape_manual(values=c(25,24,23,22,21,20,19,18,17,16,15,14))  + 
  #scale_size_continuous(range=c(3,9))+
  xlab("Number of Ag Atoms") + ylab("Excess Energy (eV)") +
  #labs(colour="Gap",size="Spin")+ 
  labs(
    #    title = "Energy Excess vs Silver Atoms (Cubo)" , #colour="Gap",size="Spin",size="Spin",
    #alpha="",shape="Seed"
    # ) + #theme(plot.title = element_text(size = rel(2), 
    #face="bold",),
    axis.line = element_line(size = 1) ) + scale_linetype_manual(guide = FALSE) +
  guides(colour = guide_legend(
    override.aes = list(shape= c(22,22,22,22,22,22,22,22,22,22,22,22,22,21,23),
                        size=c(4,4,4,4,4,4,4,4,4,4,4,4,4,2.5,5),
                        width = rep( 3, length(levels(AuAgCubo$Schoenflies.symbol) ) ),
                        lwd=rep( 3, length(levels(AuAgCubo$Schoenflies.symbol) ) ) ))) +
  scale_size_manual(values = c(3, 5), guide = FALSE) +
  theme(legend.key = element_blank(),
        legend.position = c(.9, .37),
        legend.background=element_blank(),
        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
        #panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 20, face = "bold"),
        panel.border = element_rect(colour = "black",size = 1) ) +
  #  guides(colour = guide_legend(override.aes = list(shape=c(16,17,18,15)))
  #  ) +
  #  theme(legend.key = element_blank(),legend.position = c(.525, .775),legend.background=element_blank(),
  #        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
  #        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
  #        panel.grid.minor.x = element_blank(),
  #        legend.text = element_text(size = 20, face = "bold"),
  #        panel.border = element_rect(colour = "black",size = 1) )
  #theme(axis.title= element_text(size = rel(1),face="bold" #, colour="steelblue")
  #                               ) #+
  #labs(linetype="")
ggsave(file = "EnergyExcessCuboSYM_nomin.pdf", width = 10, height = 7 )
#############################################



#############################################
ggplot() + theme_bw() + 
  geom_point(data = subset(AuAgCubo,(Schoenflies.symbol.T=="NO")),
             # AuAgCubo
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol.T),width = I(3),lwd=3,alpha = I(0.75),size=2.5,shape=21) + 
  geom_point(data = subset(AuAgCubo,(Schoenflies.symbol.T=="Oh")),
             # AuAgCubo
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol.T),alpha = I(1.0),size=5,shape=23,
             width = I(3),lwd=3) +
  geom_point(data = subset(AuAgCubo,(Schoenflies.symbol.T!="NO" & Schoenflies.symbol.T!="Oh")),
             # AuAgCubo
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol.T),alpha = I(0.85),size=4,shape=0,
             width = I(3),lwd=3) +       
  #scale_color_manual(values=brewer.pal(12,"Paired")) +
  #scale_size_continuous()+
  geom_line(data = minimaCubo, aes(x = N_Ag, y =EEFinal),
            linetype="dashed",
            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
            alpha=I(0.5) , size=1) + labs(shape="Symmetry",colour="Symmetry")+
  theme(plot.title = element_text(size = rel(2), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:14)+ geom_hline(yintercept=0,size=1) +
  theme(plot.title = element_text(size = rel(2), 
                                  face="bold", colour="black"),
        #        axis.line = element_line(size = 1),
        axis.text=element_text( size=rel(2),
                                face="bold", ) ) + 
  theme(axis.title = element_text(size =  rel(2),face="bold", colour="black") ) +
  #  scale_x_continuous(expand=c(0,0))+ scale_y_continuous(expand=c(0,0)) +
  theme(legend.title = element_text(size=19,face="bold" ), 
        legend.text = element_text( size = 18, face = "bold"),
        plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y=element_text(vjust=0.025),
        axis.title.x=element_text(vjust=-.75)) +
  #scale_colour_manual(values=c("blue4","red4","green4","black")  ,  
  #                    breaks = c("Minima","Minima 1st","green4","black")  , 
  #                    labels=c("Minima","Minima 1st","BBP","BBP") ) 
  
  #scale_shape_manual(values=c(25,24,23,22,21,20,19,18,17,16,15,14))  + 
  #scale_size_continuous(range=c(3,9))+
  xlab("Number of Ag Atoms") + ylab("Excess Energy (eV)") +
  #labs(colour="Gap",size="Spin")+ 
  labs(
    #    title = "Energy Excess vs Silver Atoms (Cubo)" , #colour="Gap",size="Spin",size="Spin",
    #alpha="",shape="Seed"
    # ) + #theme(plot.title = element_text(size = rel(2), 
    #face="bold",),
    axis.line = element_line(size = 1) ) + scale_linetype_manual(guide = FALSE) +
  guides(colour = guide_legend(
    override.aes = list(shape= c(22,22,22,22,21,23),
                        size=c(4,4,4,4,2.5,5),
                        width = rep( 3, length(levels(AuAgCubo$Schoenflies.symbol.T) ) ),
                        lwd=rep( 3, length(levels(AuAgCubo$Schoenflies.symbol.T) ) ) ))) +
  scale_size_manual(values = c(3, 5), guide = FALSE) +
  theme(legend.key = element_blank(),
        legend.position = c(.9, .37),
        legend.background=element_blank(),
        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
        #panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 20, face = "bold"),
        panel.border = element_rect(colour = "black",size = 1) ) +
  #  guides(colour = guide_legend(override.aes = list(shape=c(16,17,18,15)))
  #  ) +
  #  theme(legend.key = element_blank(),legend.position = c(.525, .775),legend.background=element_blank(),
  #        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
  #        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
  #        panel.grid.minor.x = element_blank(),
  #        legend.text = element_text(size = 20, face = "bold"),
  #        panel.border = element_rect(colour = "black",size = 1) )
  #theme(axis.title= element_text(size = rel(1),face="bold" #, colour="steelblue")
  #                               ) #+
  #labs(linetype="")
ggsave(file = "EnergyExcessCuboSYM-T_nomin.pdf", width = 10, height = 7 )
#############################################






#############################################

ggplot() + theme_bw() + 
  geom_point(data = subset(AuAgBBP,(Schoenflies.symbol=="NO")),
             # AuAgBBP
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol),width = I(3),lwd=3,alpha = I(0.65),size=2.5,shape=21) + 
  geom_point(data = subset(AuAgBBP,(Schoenflies.symbol=="C2v")),
             # AuAgBBP
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol),alpha = I(1.0),size=5,shape=23,
             width = I(3),lwd=3) +
  geom_point(data = subset(AuAgBBP,(Schoenflies.symbol!="NO" & Schoenflies.symbol!="C2v")),
             # AuAgBBP
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol),alpha = I(0.75),size=4,shape=0,
             width = I(3),lwd=3) +       
  #scale_color_manual(values=brewer.pal(12,"Paired")) +
  #scale_size_continuous()+
  geom_line(data = minimaBBP, aes(x = N_Ag, y =EEFinal),
            linetype="dashed",
            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
            alpha=I(0.5) , size=1) + labs(shape="Symmetry",colour="Symmetry")+
  theme(plot.title = element_text(size = rel(2), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:14)+ geom_hline(yintercept=0,size=1) +
  theme(plot.title = element_text(size = rel(2), 
                                  face="bold", colour="black"),
        #        axis.line = element_line(size = 1),
        axis.text=element_text( size=rel(2),
                                face="bold", ) ) + 
  theme(axis.title = element_text(size =  rel(2),face="bold", colour="black") ) +
  #  scale_x_continuous(expand=c(0,0))+ scale_y_continuous(expand=c(0,0)) +
  theme(legend.title = element_text(size=19,face="bold" ), 
        legend.text = element_text( size = 18, face = "bold"),
        plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y=element_text(vjust=0.025),
        axis.title.x=element_text(vjust=-.75)) +
  #scale_colour_manual(values=c("blue4","red4","green4","black")  ,  
  #                    breaks = c("Minima","Minima 1st","green4","black")  , 
  #                    labels=c("Minima","Minima 1st","BBP","BBP") ) 
  
  #scale_shape_manual(values=c(25,24,23,22,21,20,19,18,17,16,15,14))  + 
  #scale_size_continuous(range=c(3,9))+
  xlab("Number of Ag Atoms") + ylab("Excess Energy (eV)") +
  #labs(colour="Gap",size="Spin")+ 
  labs(
    #    title = "Energy Excess vs Silver Atoms (BBP)" , #colour="Gap",size="Spin",size="Spin",
    #alpha="",shape="Seed"
    # ) + #theme(plot.title = element_text(size = rel(2), 
    #face="bold",),
    axis.line = element_line(size = 1) ) + scale_linetype_manual(guide = FALSE) +
  guides(colour = guide_legend(
    override.aes = list(shape= c(22,23,22,21),
                        size=c(4,5,4,2.5),
                        width = rep( 3, length(levels(AuAgBBP$Schoenflies.symbol) ) ),
                        lwd=rep( 3, length(levels(AuAgBBP$Schoenflies.symbol) ) ) ))) +
  scale_size_manual(values = c(2, 6), guide = FALSE) +
  theme(legend.key = element_blank(),
        legend.position = c(.9, .37),
        legend.background=element_blank(),
        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
        #panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 20, face = "bold"),
        panel.border = element_rect(colour = "black",size = 1) ) +
  #  guides(colour = guide_legend(override.aes = list(shape=c(16,17,18,15)))
  #  ) +
  #  theme(legend.key = element_blank(),legend.position = c(.525, .775),legend.background=element_blank(),
  #        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
  #        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
  #        panel.grid.minor.x = element_blank(),
  #        legend.text = element_text(size = 20, face = "bold"),
  #        panel.border = element_rect(colour = "black",size = 1) )
  #theme(axis.title= element_text(size = rel(1),face="bold" #, colour="steelblue")
  #                               ) #+
  #labs(linetype="")
ggsave(file = "EnergyExcessBBPSYM_nomin.pdf", width = 10, height = 7 )


#############################################


#############################################

ggplot() + theme_bw() + 
  geom_point(data = subset(AuAgBBP,(Schoenflies.symbol.T=="NO")),
             # AuAgBBP
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol.T),width = I(3),lwd=3,alpha = I(0.65),size=2.5,shape=21) + 
  geom_point(data = subset(AuAgBBP,(Schoenflies.symbol.T=="C2v")),
             # AuAgBBP
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol.T),alpha = I(.90),size=5,shape=23,
             width = I(3),lwd=3) +
  geom_point(data = subset(AuAgBBP,(Schoenflies.symbol.T!="NO" & Schoenflies.symbol.T!="C2v")),
             # AuAgBBP
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol.T),alpha = I(0.75),size=4,shape=0,
             width = I(3),lwd=3) +       
  #scale_color_manual(values=brewer.pal(12,"Paired")) +
  #scale_size_continuous()+
  geom_line(data = minimaBBP, aes(x = N_Ag, y =EEFinal),
            linetype="dashed",
            # %sprintf("%s_%s",minima$Type,minima$First) ) , 
            alpha=I(0.5) , size=1) + labs(shape="Symmetry",colour="Symmetry")+
  theme(plot.title = element_text(size = rel(2), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:14)+ geom_hline(yintercept=0,size=1) +
  theme(plot.title = element_text(size = rel(2), 
                                  face="bold", colour="black"),
        #        axis.line = element_line(size = 1),
        axis.text=element_text( size=rel(2),
                                face="bold", ) ) + 
  theme(axis.title = element_text(size =  rel(2),face="bold", colour="black") ) +
  #  scale_x_continuous(expand=c(0,0))+ scale_y_continuous(expand=c(0,0)) +
  theme(legend.title = element_text(size=19,face="bold" ), 
        legend.text = element_text( size = 18, face = "bold"),
        plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y=element_text(vjust=0.025),
        axis.title.x=element_text(vjust=-.75)) +
  #scale_colour_manual(values=c("blue4","red4","green4","black")  ,  
  #                    breaks = c("Minima","Minima 1st","green4","black")  , 
  #                    labels=c("Minima","Minima 1st","BBP","BBP") ) 
  
  #scale_shape_manual(values=c(25,24,23,22,21,20,19,18,17,16,15,14))  + 
  #scale_size_continuous(range=c(3,9))+
  xlab("Number of Ag Atoms") + ylab("Excess Energy (eV)") +
  #labs(colour="Gap",size="Spin")+ 
  labs(
    #    title = "Energy Excess vs Silver Atoms (BBP)" , #colour="Gap",size="Spin",size="Spin",
    #alpha="",shape="Seed"
    # ) + #theme(plot.title = element_text(size = rel(2), 
    #face="bold",),
    axis.line = element_line(size = 1) ) + scale_linetype_manual(guide = FALSE) +
  guides(colour = guide_legend(
    override.aes = list(shape= c(22,22,23,22,21),
                        size=c(4,4,5,4,2.5),
                        width = rep( 3, length(levels(AuAgBBP$Schoenflies.symbol.T) ) ),
                        lwd=rep( 3, length(levels(AuAgBBP$Schoenflies.symbol.T) ) ) ))) +
  scale_size_manual(values = c(2, 6), guide = FALSE) +
  theme(legend.key = element_blank(),
        legend.position = c(.9, .37),
        legend.background=element_blank(),
        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
        #panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 20, face = "bold"),
        panel.border = element_rect(colour = "black",size = 1) ) +
  #  guides(colour = guide_legend(override.aes = list(shape=c(16,17,18,15)))
  #  ) +
  #  theme(legend.key = element_blank(),legend.position = c(.525, .775),legend.background=element_blank(),
  #        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
  #        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
  #        panel.grid.minor.x = element_blank(),
  #        legend.text = element_text(size = 20, face = "bold"),
  #        panel.border = element_rect(colour = "black",size = 1) )
  #theme(axis.title= element_text(size = rel(1),face="bold" #, colour="steelblue")
  #                               ) #+
  #labs(linetype="")
ggsave(file = "EnergyExcessBBPSYM-T_nomin.pdf", width = 10, height = 7 )

#############################################
















windows()

ggplot() + theme_bw() + 
  scale_x_continuous(breaks=0:14)+ geom_hline(yintercept=0,size=1) + 
  geom_point(data = AuAg,aes(x = N_Ag, y = EEFinal,colour=Gap,size = abs(Spin),
                             shape=Type),alpha = I(0.45)) + 
  #scale_size_continuous(range=c(3,9)) +
  xlab("Number of Ag Atoms") + ylab("Energy Excess BBP (eV)") +
  labs(colour="Gap",size="Spin")+ labs(title = "Energy Excess vs Silver Atoms",colour="Gap",size="Spin",size="Spin",
                                       alpha="",shape="Seed") + theme(plot.title = element_text(size = rel(2), face="bold", colour="steelblue"),
                                                                      axis.line = element_line(size = 1))+theme(axis.title= element_text(size = rel(1),face="bold", colour="steelblue")) +
  geom_line(data = minima, aes(x = minima$N_Ag, y = minima$EEFinal,
                               linetype=sprintf("%s_%s",minima$Type,minima$First) ) , alpha=I(0.5) , size=1) +
  labs(linetype="") #+
  
  
  
  
#  geom_line(data = minimaBBP, aes(x = minimaBBP$N_Ag, y = minimaBBP$EEFinal,linetype = "Minima E BBP"),
#            colour = "darkred",size = 1) + 
#  geom_line(data = minimaBBP1st, aes(x = minimaBBP1st$N_Ag, y = minimaBBP1st$EEFinal,
#                                     linetype = "Minima E1st BBP"), colour = "darkred", size = 1) + 
#  #  scale_linetype_discrete('Minima') + 
#  geom_line(data = minimaIco, aes(x = minimaIco$N_Ag, y = minimaIco$EEFinal,linetype = "Minima E BBP"),
#            colour = "darkred",size = 1) + 
#  geom_line(data = minimaBBP1st, aes(x = minimaBBP1st$N_Ag, y = minimaBBP1st$EEFinal,
#                                     linetype = "Minima E1st BBP"), colour = "darkred", size = 1) + 
#  scale_linetype_discrete('Minima') + 
#  
#  geom_line(data = minima, aes(x = minima$N_Ag, y = minima$EEFinal),
#            colour = "darkred",size = 1,linetype=sprintf("%s%s",minima$Type,minima$First)) #+ 
  
#  minima

#colour = "darkred",size = 1,)

#sprintf("%s%s",minima$Type,minima$First)) + 
  
  
  
  #pdf(file = "EnergyExcessBBP_nomin.pdf", width = 6, height = 4)
  ggsave(file = "EnergyExcessBBPIco_nomin.pdf", width = 10, height = 7 )
#dev.off()
#png(file = "EnergyExcessBBP.png", bg="transparent")
  ggsave(file = "EnergyExcessBBPIco.png", bg="transparent",width = 10, height = 7)
#dev.off()

windows()

# Plot BBP Data - Energy Excess ---------------------------------------------------------
ggplot() + theme_bw() + geom_line(data = minimaBBP, 
                                  aes(x = minimaBBP$N_Ag, y = minimaBBP$EEFinal,linetype = "Minima E BBP"),colour = "darkred",size = 1) +
  geom_line(data = minimaBBP1st, aes(x = minimaBBP1st$N_Ag, y = minimaBBP1st$EEFinal,linetype = "Minima E1st BBP"),
            colour = "darkred", size = 1) + scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgBBP, aes(x = AuAgBBP$N_Ag, y = AuAgBBP$EEFinal,colour=AuAgBBP$Gap,size = abs(AuAgBBP$Spin),
                                 shape=AuAgBBP$Type),alpha = I(0.45))+ xlab("Number of Ag Atoms") + ylab("Energy Excess BBP (eV)")+
  labs(colour="Gap",size="Spin")+labs(title = "Energy Excess vs Silver Atoms",colour="Gap",size="Spin",size="Spin",
                                      alpha="",shape="Seed")+
  theme(plot.title = element_text(size = rel(2), face="bold", colour="steelblue"),
  axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold", colour="steelblue")) +
  scale_x_continuous(breaks=0:14) #+ geom_hline(yintercept=0,size=1)

#pdf(file = "EnergyExcessBBP_nomin.pdf", width = 6, height = 4)
ggsave(file = "EnergyExcessBBP_nomin.pdf", width = 10, height = 7 )
#dev.off()
#png(file = "EnergyExcessBBP.png", bg="transparent")
ggsave(file = "EnergyExcessBBP.png", bg="transparent",width = 10, height = 7)
#dev.off()

windows()
###

# Plot BBP Data - Spin ---------------------------------------------------------
ggplot() + theme_bw() + geom_line(data = minimaBBP, aes(x = minimaBBP$N_Ag, y = minimaBBP$Spin,
                                                        linetype = "Minima E BBP"),colour = "darkred",size = 1) + geom_line(data = minimaBBP1st, aes(x = minimaBBP1st$N_Ag, 
                                                                                                                                                     y = minimaBBP1st$Spin,linetype = "Minima E1st BBP"), colour = "darkred", size = 1) + 
  scale_linetype_discrete('Minima') + geom_point(data = AuAgBBP, aes(x = AuAgBBP$N_Ag, y = abs(AuAgBBP$Spin),
                                                                     colour=AuAgBBP$Gap,size=2*exp(-1.0*AuAgBBP$EEFinal),shape=AuAgBBP$Type),alpha = I(0.45)) + 
  xlab("Number of Ag Atoms") + ylab("Spin (Bohr Magneton)")+labs(colour="Gap",size="Spin") +
  labs(title = "Spin vs Silver Atoms",colour="Gap",size="R=exp(-(E-E_gs))",size="Spin",alpha="",shape="Seed")+
  theme(plot.title = element_text(size = rel(2), face="bold", colour="steelblue"),axis.line = element_line(size = 1)) +
  theme(axis.title= element_text(size = rel(1),face="bold", colour="steelblue"))+scale_x_continuous(breaks=0:14)#+ 
geom_hline(yintercept=0,size=1) # +
# scale_y_continuous(limits=c(0.95,1.05))

ggsave(file = "SpinBBP_nomin.pdf", width = 10, height = 7 )
#dev.off()
#png(file = "EnergyExcessBBP.png", bg="transparent")
ggsave(file = "SpinBBP.png", bg="transparent",width = 10, height = 7)
#dev.off()

windows()

# Plot BBP Data - Spin ---------------------------------------------------------
ggplot() + theme_bw() + geom_line(data = minimaBBP, aes(x = minimaBBP$N_Ag, y = minimaBBP$Spin,linetype = 
                                                          "Minima E BBP"),colour = "darkred",size = 1) + geom_line(data = minimaBBP1st, aes(x = minimaBBP1st$N_Ag, y = 
                                                                                                                                              minimaBBP1st$Spin,linetype = "Minima E1st BBP"),colour = "darkred", size = 1) + scale_linetype_discrete('Minima') +
  geom_point(data = AuAgBBP, aes(x = AuAgBBP$N_Ag, y = abs(AuAgBBP$Spin), colour = AuAgBBP$Gap , 
                                 size = 2*exp(-1.0*AuAgBBP$EEFinal) , shape=AuAgBBP$Type) , alpha = I(0.45)) + xlab("Number of Ag Atoms") +
  ylab("Spin (Bohr Magneton)")+labs(colour="Gap",size="Spin")+labs(title = "Spin vs Silver Atoms" , colour="Gap" , 
                                                                   size="R=exp(-(E-E_gs))" , size="Spin" , alpha="" , shape="Seed") + theme(plot.title = element_text(size = rel(2) ,
                                                                                                                                                                      face="bold", colour="steelblue"), axis.line = element_line(size = 1)) + theme(axis.title= element_text(size = rel(1),
                                                                                                                                                                                                                                                                             face="bold", colour="steelblue"))+scale_x_continuous(breaks=0:14) + geom_hline(yintercept=0,size=1) + 
  scale_y_continuous(limits=c(0.95,1.05))

ggsave(file = "SpinBBP_nomin.pdf", width = 10, height = 7 )
#dev.off()
#png(file = "EnergyExcessBBP.png", bg="transparent")
ggsave(file = "SpinBBP.png", bg="transparent",width = 10, height = 7)
#dev.off()

windows()
###
# Plot BBP Data - Gap ---------------------------------------------------------
ggplot() + theme_bw() + geom_line(data = minimaBBP, aes(x = minimaBBP$N_Ag, y = minimaBBP$Gap,linetype = 
                                                          "Minima E BBP"),colour = "darkred",size = 1) + geom_line(data = minimaBBP1st, aes(x = minimaBBP1st$N_Ag, y = 
                                                                                                                                              minimaBBP1st$Gap,linetype = "Minima E1st BBP"),colour = "darkred", size = 1) + scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgBBP, aes(x = AuAgBBP$N_Ag, y = abs(AuAgBBP$Gap),colour=abs(AuAgBBP$Spin),
                                 size=2*exp(-1.0*AuAgBBP$EEFinal),shape=AuAgBBP$Type),alpha = I(0.45))+ xlab("Number of Ag Atoms") + 
  ylab("Gap (eV)")+labs(colour="Gap",size="Spin")+labs(title = "Gap vs Silver Atoms", colour="Spin", 
                                                       size="R=exp(-(E-E_gs))",alpha="",shape="Seed")+theme(plot.title = element_text(size = rel(2), face="bold", 
                                                                                                                                      colour="steelblue"),axis.line = element_line(size = 1))+theme(axis.title= element_text(size = rel(1),face="bold", 
                                                                                                                                                                                                                             colour="steelblue"))+scale_x_continuous(breaks=0:14) + 
  scale_y_continuous(limits=c(min(AuAgBBP$Gap),max(AuAgBBP$Gap)))

ggsave(file = "GapBBP_nomin.pdf", width = 10, height = 7 )
ggsave(file = "GapBBP.png", bg="transparent",width = 10, height = 7)
#dev.off()

windows()
###
# Plot BBP Data - Spin-vs-EE ---------------------------------------------------------
ggplot() + theme_bw() + 
  geom_line(data = minimaBBP, aes(x = minimaBBP$EEFinal, y = minimaBBP$Spin,linetype = "Minima E BBP"), 
            colour = "darkred",size = 1) + geom_line(data = minimaBBP1st, aes(x = minimaBBP1st$EEFinal, y = minimaBBP1st$Spin,
                                                                              linetype = "Minima E1st BBP"), colour = "darkred", size = 1) + 
  scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgBBP, aes(x = AuAgBBP$EEFinal, y = abs(AuAgBBP$Spin),colour=abs(AuAgBBP$Gap),
                                 size=2*exp(-1.0*AuAgBBP$EEFinal),shape=AuAgBBP$Type),alpha = I(0.45))+ 
  xlab("Number of Ag Atoms") + ylab("Spin (Bohr Magneton)")+labs(colour="Gap",size="")+
  labs(title = "Spin vs Silver Atoms",colour="Gap",size="R=exp(-(E-E_gs))",alpha="",shape="Seed")+
  theme(plot.title = element_text(size =rel(2), face="bold", colour="steelblue"),
        axis.line = element_line(size = 1))+theme(axis.title= element_text(size =rel(1),face="bold", colour="steelblue"))

ggsave(file = "EESpinnBBP_nomin.pdf", width = 10, height = 7 )
#dev.off()
#png(file = "EnergyExcessBBP.png", bg="transparent")
ggsave(file = "EESpinBBP.png", bg="transparent",width = 10, height = 7)
#dev.off()

windows()

###

# Plot BBP Data - Gap-vs-EE ---------------------------------------------------------
ggplot() + theme_bw() + 
  geom_line(data = minimaBBP, aes(x = minimaBBP$EEFinal, y = minimaBBP$Gap, linetype = "Minima E BBP"),
            colour = "darkred",size = 1) + 
  geom_line(data = minimaBBP1st, aes(x = minimaBBP1st$EEFinal, y = minimaBBP1st$Gap, linetype = "Minima E1st BBP"),
            colour = "darkred", size = 1) + scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgBBP, aes(x =AuAgBBP$EEFinal, y = abs(AuAgBBP$Gap),colour=abs(AuAgBBP$Spin),
                                 size=2*exp(-1.0*AuAgBBP$EEFinal),shape=AuAgBBP$Type),alpha = I(0.45)) + 
  xlab("Excess Energy (eV)") + ylab("Gap (eV)")+labs(colour="Gap",size="Spin")+labs(title = "Gap vs Excess Energy",
                                                                                    colour="Spin",size="R=exp(-(E-E_gs))",alpha="",shape="Seed")+
  theme(plot.title = element_text(size =rel(2), face="bold", colour="steelblue"),axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size =rel(1),face="bold", colour="steelblue"))

ggsave(file = "EEGapBBP_nomin.pdf", width = 10, height = 7 )
#dev.off()
#png(file = "EnergyExcessBBP.png", bg="transparent")
ggsave(file = "EEGapBBP.png", bg="transparent",width = 10, height = 7)
#dev.off()

###

# Plot Ico Data - EE ---------------------------------------------------------
ggplot() + theme_bw() + 
  geom_line(data = minimaIco, aes(x = minimaIco$N_Ag, y = minimaIco$EEFinal,linetype = "Minima E Ico"),
            colour = "darkred",size = 1) + 
  geom_line(data = minimaIco1st, aes(x = minimaIco1st$N_Ag, y = minimaIco1st$EEFinal,
                                     linetype = "Minima E1st Ico"),colour = "darkred", size = 1) + 
  scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgIco, aes(x = AuAgIco$N_Ag, y = AuAgIco$EEFinal,colour=AuAgIco$Gap,
                                 size = 2+0*abs(AuAgIco$Spin)),alpha = I(0.45))+ xlab("Number of Ag Atoms") + 
  ylab("Energy Excess Ico (eV)")+labs(colour="Gap",size="Spin")+
  labs(title = "Energy Excess vs Silver Atoms",colour="Gap",size="Spin",size="Spin",alpha="",shape="Seed")+
  theme(plot.title = element_text(size = rel(2), face="bold", colour="steelblue"),axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold", colour="steelblue"))+
  scale_x_continuous(breaks=0:14)+ geom_hline(yintercept=0,size=1)

ggsave(file = "EnergyExcessIco_nomin.pdf", width = 10, height = 7 )
ggsave(file = "EnergyExcessIco.png", bg="transparent",width = 10, height = 7)


windows()
###
# Plot Ico Data - Spin ---------------------------------------------------------
ggplot() + theme_bw() + 
  geom_line(data = minimaIco, aes(x = minimaIco$N_Ag, y = minimaIco$Spin,linetype = "Minima E AuAgIco"),
            colour = "darkred",size = 1) + 
  geom_line(data = minimaIco1st, aes(x = minimaIco1st$N_Ag, y = minimaIco1st$Spin,linetype = "Minima E1st AuAgIco"),
            colour = "darkred", size = 1) + 
  scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgIco, aes(x = AuAgIco$N_Ag, y = abs(AuAgIco$Spin),colour=AuAgIco$Gap,size=2+0*
                                   exp(-.0050*AuAgIco$EEFinal),shape=AuAgIco$Type),alpha = I(0.45)) + 
  xlab("Number of Ag Atoms") + ylab("Spin (Bohr Magneton)")+labs(colour="Gap",size="Spin")+
  labs(title = "Spin vs Silver Atoms",colour="Gap",size="R=exp(-(E-E_gs))",size="Spin",alpha="",shape="Seed")+
  theme(plot.title = element_text(size = rel(2), face="bold", colour="steelblue"),axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold", colour="steelblue"))+scale_x_continuous(breaks=0:14)+ 
  geom_hline(yintercept=0,size=1)+scale_y_continuous(limits=c(0.95,5.05))

ggsave(file = "SpinIco_nomin.pdf", width = 10, height = 7 )
#dev.off()
#png(file = "EnergyExcessBBP.png", bg="transparent")
ggsave(file = "SpinIco.png", bg="transparent",width = 10, height = 7)
#dev.off()

windows()
###

# Plot Ico Data - Gap ---------------------------------------------------------
ggplot() + theme_bw() + geom_line(data = minimaIco, aes(x = minimaIco$N_Ag, y = minimaIco$Gap,linetype = 
  "Minima E Ico"),colour = "darkred",size = 1) + geom_line(data = minimaIco1st, aes(x = minimaIco1st$N_Ag, y = 
  minimaIco1st$Gap,linetype = "Minima E1st Ico"),colour = "darkred", size = 1) + scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgIco, aes(x = AuAgIco$N_Ag, y = abs(AuAgIco$Gap),colour=abs(AuAgIco$Spin),
  size=2*exp(-1.0*AuAgIco$EEFinal),shape=AuAgIco$Type),alpha = I(0.45))+ xlab("Number of Ag Atoms") + 
  ylab("Gap (eV)")+labs(colour="Gap",size="Spin")+labs(title = "Gap vs Silver Atoms", colour="Spin", 
  size="R=exp(-(E-E_gs))",alpha="",shape="Seed")+theme(plot.title = element_text(size = rel(2), face="bold", 
  colour="steelblue"),axis.line = element_line(size = 1))+theme(axis.title= element_text(size = rel(1),face="bold", 
  colour="steelblue"))+scale_x_continuous(breaks=0:14) + 
  scale_y_continuous(limits=c(min(AuAgIco$Gap),max(AuAgIco$Gap)))

ggsave(file = "GapIco_nomin.pdf", width = 10, height = 7 )
ggsave(file = "GapIco.png", bg="transparent",width = 10, height = 7)
#dev.off()

windows()
###
# Plot Ico Data - Spin-vs-EE ---------------------------------------------------------
ggplot() + theme_bw() + 
  geom_line(data = minimaIco, aes(x = minimaIco$EEFinal, y = minimaIco$Spin,linetype = "Minima E Ico"), 
            colour = "darkred",size = 1) + geom_line(data = minimaIco1st, aes(x = minimaIco1st$EEFinal, y = minimaIco1st$Spin,
                                                                              linetype = "Minima E1st Ico"), colour = "darkred", size = 1) + 
  scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgIco, aes(x = AuAgIco$EEFinal, y = abs(AuAgIco$Spin),colour=abs(AuAgIco$Gap),
                                 size=2*exp(-1.0*AuAgIco$EEFinal),shape=AuAgIco$Type),alpha = I(0.45))+ 
  xlab("Number of Ag Atoms") + ylab("Spin (Bohr Magneton)")+labs(colour="Gap",size="")+
  labs(title = "Spin vs Silver Atoms",colour="Gap",size="R=exp(-(E-E_gs))",alpha="",shape="Seed")+
  theme(plot.title = element_text(size =rel(2), face="bold", colour="steelblue"),
        axis.line = element_line(size = 1))+theme(axis.title= element_text(size =rel(1),face="bold", colour="steelblue"))

ggsave(file = "EESpinnIco_nomin.pdf", width = 10, height = 7 )
#dev.off()
#png(file = "EnergyExcessIco.png", bg="transparent")
ggsave(file = "EESpinIco.png", bg="transparent",width = 10, height = 7)
#dev.off()

windows()

###

# Plot Ico Data - Gap-vs-EE ---------------------------------------------------------
ggplot() + theme_bw() + 
  geom_line(data = minimaIco, aes(x = minimaIco$EEFinal, y = minimaIco$Gap, linetype = "Minima E Ico"),
            colour = "darkred",size = 1) + 
  geom_line(data = minimaIco1st, aes(x = minimaIco1st$EEFinal, y = minimaIco1st$Gap, linetype = "Minima E1st Ico"),
            colour = "darkred", size = 1) + scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgIco, aes(x =AuAgIco$EEFinal, y = abs(AuAgIco$Gap),colour=abs(AuAgIco$Spin),
                                 size=2*exp(-1.0*AuAgIco$EEFinal),shape=AuAgIco$Type),alpha = I(0.45)) + 
  xlab("Excess Energy (eV)") + ylab("Gap (eV)")+labs(colour="Gap",size="Spin")+labs(title = "Gap vs Excess Energy",
                                                                                    colour="Spin",size="R=exp(-(E-E_gs))",alpha="",shape="Seed")+
  theme(plot.title = element_text(size =rel(2), face="bold", colour="steelblue"),axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size =rel(1),face="bold", colour="steelblue"))

ggsave(file = "EEGapIco_nomin.pdf", width = 10, height = 7 )
#dev.off()
#png(file = "EnergyExcessIco.png", bg="transparent")
ggsave(file = "EEGapIco.png", bg="transparent",width = 10, height = 7)
#dev.off()






# Plot Cubo Data - EE ---------------------------------------------------------
ggplot() + theme_bw() + 
  geom_line(data = minimaCubo, aes(x = minimaCubo$N_Ag, y = minimaCubo$EEFinal,linetype = "Minima E Cubo"),
            colour = "darkred",size = 1) + 
  geom_line(data = minimaCubo1st, aes(x = minimaCubo1st$N_Ag, y = minimaCubo1st$EEFinal,
                                      linetype = "Minima E1st Cubo"),colour = "darkred", size = 1) + 
  scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgCubo, aes(x = AuAgCubo$N_Ag, y = AuAgCubo$EEFinal,colour=AuAgCubo$Gap,
                                  size = 2+0*abs(AuAgCubo$Spin)),alpha = I(0.45))+ xlab("Number of Ag Atoms") + 
  ylab("Energy Excess Cubo (eV)")+labs(colour="Gap",size="Spin")+
  labs(title = "Energy Excess vs Silver Atoms",colour="Gap",size="Spin",size="Spin",alpha="",shape="Seed")+
  theme(plot.title = element_text(size = rel(2), face="bold", colour="steelblue"),axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold", colour="steelblue"))+
  scale_x_continuous(breaks=0:14)+ geom_hline(yintercept=0,size=1)

ggsave(file = "EnergyExcessCubo_nomin.pdf", width = 10, height = 7 )
ggsave(file = "EnergyExcessCubo.png", bg="transparent",width = 10, height = 7)


windows()
###
# Plot Cubo Data - Spin ---------------------------------------------------------
ggplot() + theme_bw() + 
  geom_line(data = minimaCubo, aes(x = minimaCubo$N_Ag, y = minimaCubo$Spin,linetype = "Minima E AuAgCubo"),
            colour = "darkred",size = 1) + 
  geom_line(data = minimaCubo1st, aes(x = minimaCubo1st$N_Ag, y = minimaCubo1st$Spin,linetype = "Minima E1st AuAgCubo"),
            colour = "darkred", size = 1) + 
  scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgCubo, aes(x = AuAgCubo$N_Ag, y = abs(AuAgCubo$Spin),colour=AuAgCubo$Gap,size=2+0*
                                    exp(-.0050*AuAgCubo$EEFinal),shape=AuAgCubo$Type),alpha = I(0.45)) + 
  xlab("Number of Ag Atoms") + ylab("Spin (Bohr Magneton)")+labs(colour="Gap",size="Spin")+
  labs(title = "Spin vs Silver Atoms",colour="Gap",size="R=exp(-(E-E_gs))",size="Spin",alpha="",shape="Seed")+
  theme(plot.title = element_text(size = rel(2), face="bold", colour="steelblue"),axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold", colour="steelblue"))+scale_x_continuous(breaks=0:14)+ 
  geom_hline(yintercept=0,size=1)+scale_y_continuous(limits=c(0.95,5.05))

ggsave(file = "SpinCubo_nomin.pdf", width = 10, height = 7 )
#dev.off()
#png(file = "EnergyExcessCubo.png", bg="transparent")
ggsave(file = "SpinCubo.png", bg="transparent",width = 10, height = 7)
#dev.off()

windows()
###

# Plot Cubo Data - Gap ---------------------------------------------------------
ggplot() + theme_bw() + geom_line(data = minimaCubo, aes(x = minimaCubo$N_Ag, y = minimaCubo$Gap,linetype = 
                                                           "Minima E Cubo"),colour = "darkred",size = 1) + geom_line(data = minimaCubo1st, aes(x = minimaCubo1st$N_Ag, y = 
                                                                                                                                                 minimaCubo1st$Gap,linetype = "Minima E1st Cubo"),colour = "darkred", size = 1) + scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgCubo, aes(x = AuAgCubo$N_Ag, y = abs(AuAgCubo$Gap),colour=abs(AuAgCubo$Spin),
                                  size=2*exp(-1.0*AuAgCubo$EEFinal),shape=AuAgCubo$Type),alpha = I(0.45))+ xlab("Number of Ag Atoms") + 
  ylab("Gap (eV)")+labs(colour="Gap",size="Spin")+labs(title = "Gap vs Silver Atoms", colour="Spin", 
                                                       size="R=exp(-(E-E_gs))",alpha="",shape="Seed")+theme(plot.title = element_text(size = rel(2), face="bold", 
                                                                                                                                      colour="steelblue"),axis.line = element_line(size = 1))+theme(axis.title= element_text(size = rel(1),face="bold", 
                                                                                                                                                                                                                             colour="steelblue"))+scale_x_continuous(breaks=0:14) + 
  scale_y_continuous(limits=c(min(AuAgCubo$Gap),max(AuAgCubo$Gap)))

ggsave(file = "GapCubo_nomin.pdf", width = 10, height = 7 )
ggsave(file = "GapCubo.png", bg="transparent",width = 10, height = 7)
#dev.off()

windows()
###
# Plot Cubo Data - Spin-vs-EE ---------------------------------------------------------
ggplot() + theme_bw() + 
  geom_line(data = minimaCubo, aes(x = minimaCubo$EEFinal, y = minimaCubo$Spin,linetype = "Minima E Cubo"), 
            colour = "darkred",size = 1) + geom_line(data = minimaCubo1st, aes(x = minimaCubo1st$EEFinal, y = minimaCubo1st$Spin,
                                                                               linetype = "Minima E1st Cubo"), colour = "darkred", size = 1) + 
  scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgCubo, aes(x = AuAgCubo$EEFinal, y = abs(AuAgCubo$Spin),colour=abs(AuAgCubo$Gap),
                                  size=2*exp(-1.0*AuAgCubo$EEFinal),shape=AuAgCubo$Type),alpha = I(0.45))+ 
  xlab("Number of Ag Atoms") + ylab("Spin (Bohr Magneton)")+labs(colour="Gap",size="")+
  labs(title = "Spin vs Silver Atoms",colour="Gap",size="R=exp(-(E-E_gs))",alpha="",shape="Seed")+
  theme(plot.title = element_text(size =rel(2), face="bold", colour="steelblue"),
        axis.line = element_line(size = 1))+theme(axis.title= element_text(size =rel(1),face="bold", colour="steelblue"))

ggsave(file = "EESpinnCubo_nomin.pdf", width = 10, height = 7 )
#dev.off()
#png(file = "EnergyExcessCubo.png", bg="transparent")
ggsave(file = "EESpinCubo.png", bg="transparent",width = 10, height = 7)
#dev.off()

windows()

###

# Plot Cubo Data - Gap-vs-EE ---------------------------------------------------------
ggplot() + theme_bw() + 
  geom_line(data = minimaCubo, aes(x = minimaCubo$EEFinal, y = minimaCubo$Gap, linetype = "Minima E Cubo"),
            colour = "darkred",size = 1) + 
  geom_line(data = minimaCubo1st, aes(x = minimaCubo1st$EEFinal, y = minimaCubo1st$Gap, linetype = "Minima E1st Cubo"),
            colour = "darkred", size = 1) + scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgCubo, aes(x =AuAgCubo$EEFinal, y = abs(AuAgCubo$Gap),colour=abs(AuAgCubo$Spin),
                                  size=2*exp(-1.0*AuAgCubo$EEFinal),shape=AuAgCubo$Type),alpha = I(0.45)) + 
  xlab("Excess Energy (eV)") + ylab("Gap (eV)")+labs(colour="Gap",size="Spin")+labs(title = "Gap vs Excess Energy",
                                                                                    colour="Spin",size="R=exp(-(E-E_gs))",alpha="",shape="Seed")+
  theme(plot.title = element_text(size =rel(2), face="bold", colour="steelblue"),axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size =rel(1),face="bold", colour="steelblue"))

ggsave(file = "EEGapCubo_nomin.pdf", width = 10, height = 7 )
#dev.off()
#png(file = "EnergyExcessCubo.png", bg="transparent")
ggsave(file = "EEGapCubo.png", bg="transparent",width = 10, height = 7)
#dev.off()



# Plot Deca Data - EE ---------------------------------------------------------
ggplot() + theme_bw() + 
  geom_line(data = minimaDeca, aes(x = minimaDeca$N_Ag, y = minimaDeca$EEFinal,linetype = "Minima E Deca"),
            colour = "darkred",size = 1) + 
  geom_line(data = minimaDeca1st, aes(x = minimaDeca1st$N_Ag, y = minimaDeca1st$EEFinal,
                                      linetype = "Minima E1st Deca"),colour = "darkred", size = 1) + 
  scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgDeca, aes(x = AuAgDeca$N_Ag, y = AuAgDeca$EEFinal,colour=AuAgDeca$Gap,
                                  size = 2+0*abs(AuAgDeca$Spin)),alpha = I(0.45))+ xlab("Number of Ag Atoms") + 
  ylab("Energy Excess Deca (eV)")+labs(colour="Gap",size="Spin")+
  labs(title = "Energy Excess vs Silver Atoms",colour="Gap",size="Spin",size="Spin",alpha="",shape="Seed")+
  theme(plot.title = element_text(size = rel(2), face="bold", colour="steelblue"),axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold", colour="steelblue"))+
  scale_x_continuous(breaks=0:14)+ geom_hline(yintercept=0,size=1)

ggsave(file = "EnergyExcessDeca_nomin.pdf", width = 10, height = 7 )
ggsave(file = "EnergyExcessDeca.png", bg="transparent",width = 10, height = 7)


windows()
###
# Plot Deca Data - Spin ---------------------------------------------------------
ggplot() + theme_bw() + 
  geom_line(data = minimaDeca, aes(x = minimaDeca$N_Ag, y = minimaDeca$Spin,linetype = "Minima E AuAgDeca"),
            colour = "darkred",size = 1) + 
  geom_line(data = minimaDeca1st, aes(x = minimaDeca1st$N_Ag, y = minimaDeca1st$Spin,linetype = "Minima E1st AuAgDeca"),
            colour = "darkred", size = 1) + 
  scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgDeca, aes(x = AuAgDeca$N_Ag, y = abs(AuAgDeca$Spin),colour=AuAgDeca$Gap,size=2+0*
                                    exp(-.0050*AuAgDeca$EEFinal),shape=AuAgDeca$Type),alpha = I(0.45)) + 
  xlab("Number of Ag Atoms") + ylab("Spin (Bohr Magneton)")+labs(colour="Gap",size="Spin")+
  labs(title = "Spin vs Silver Atoms",colour="Gap",size="R=exp(-(E-E_gs))",size="Spin",alpha="",shape="Seed")+
  theme(plot.title = element_text(size = rel(2), face="bold", colour="steelblue"),axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold", colour="steelblue"))+scale_x_continuous(breaks=0:14)+ 
  geom_hline(yintercept=0,size=1)+scale_y_continuous(limits=c(0.95,5.05))

ggsave(file = "SpinDeca_nomin.pdf", width = 10, height = 7 )
#dev.off()
#png(file = "EnergyExcessDeca.png", bg="transparent")
ggsave(file = "SpinDeca.png", bg="transparent",width = 10, height = 7)
#dev.off()

#windows()
###

# Plot Deca Data - Gap ---------------------------------------------------------
ggplot() + theme_bw() + geom_line(data = minimaDeca, aes(x = minimaDeca$N_Ag, y = minimaDeca$Gap,linetype = 
                                                           "Minima E Deca"),colour = "darkred",size = 1) + geom_line(data = minimaDeca1st, aes(x = minimaDeca1st$N_Ag, y = 
                                                                                                                                                 minimaDeca1st$Gap,linetype = "Minima E1st Deca"),colour = "darkred", size = 1) + scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgDeca, aes(x = AuAgDeca$N_Ag, y = abs(AuAgDeca$Gap),colour=abs(AuAgDeca$Spin),
                                  size=2*exp(-1.0*AuAgDeca$EEFinal),shape=AuAgDeca$Type),alpha = I(0.45))+ xlab("Number of Ag Atoms") + 
  ylab("Gap (eV)")+labs(colour="Gap",size="Spin")+labs(title = "Gap vs Silver Atoms", colour="Spin", 
                                                       size="R=exp(-(E-E_gs))",alpha="",shape="Seed")+theme(plot.title = element_text(size = rel(2), face="bold", 
                                                                                                                                      colour="steelblue"),axis.line = element_line(size = 1))+theme(axis.title= element_text(size = rel(1),face="bold", 
                                                                                                                                                                                                                             colour="steelblue"))+scale_x_continuous(breaks=0:14) + 
  scale_y_continuous(limits=c(min(AuAgDeca$Gap),max(AuAgDeca$Gap)))

ggsave(file = "GapDeca_nomin.pdf", width = 10, height = 7 )
ggsave(file = "GapDeca.png", bg="transparent",width = 10, height = 7)
#dev.off()

windows()
###
# Plot Deca Data - Spin-vs-EE ---------------------------------------------------------
ggplot() + theme_bw() + 
  geom_line(data = minimaDeca, aes(x = minimaDeca$EEFinal, y = minimaDeca$Spin,linetype = "Minima E Deca"), 
            colour = "darkred",size = 1) + geom_line(data = minimaDeca1st, aes(x = minimaDeca1st$EEFinal, y = minimaDeca1st$Spin,
                                                                               linetype = "Minima E1st Deca"), colour = "darkred", size = 1) + 
  scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgDeca, aes(x = AuAgDeca$EEFinal, y = abs(AuAgDeca$Spin),colour=abs(AuAgDeca$Gap),
                                  size=2*exp(-1.0*AuAgDeca$EEFinal),shape=AuAgDeca$Type),alpha = I(0.45))+ 
  xlab("Number of Ag Atoms") + ylab("Spin (Bohr Magneton)")+labs(colour="Gap",size="")+
  labs(title = "Spin vs Silver Atoms",colour="Gap",size="R=exp(-(E-E_gs))",alpha="",shape="Seed")+
  theme(plot.title = element_text(size =rel(2), face="bold", colour="steelblue"),
        axis.line = element_line(size = 1))+theme(axis.title= element_text(size =rel(1),face="bold", colour="steelblue"))

ggsave(file = "EESpinnDeca_nomin.pdf", width = 10, height = 7 )
#dev.off()
#png(file = "EnergyExcessDeca.png", bg="transparent")
ggsave(file = "EESpinDeca.png", bg="transparent",width = 10, height = 7)
#dev.off()

windows()

###

# Plot Deca Data - Gap-vs-EE ---------------------------------------------------------
ggplot() + theme_bw() + 
  geom_line(data = minimaDeca, aes(x = minimaDeca$EEFinal, y = minimaDeca$Gap, linetype = "Minima E Deca"),
            colour = "darkred",size = 1) + 
  geom_line(data = minimaDeca1st, aes(x = minimaDeca1st$EEFinal, y = minimaDeca1st$Gap, linetype = "Minima E1st Deca"),
            colour = "darkred", size = 1) + scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgDeca, aes(x =AuAgDeca$EEFinal, y = abs(AuAgDeca$Gap),colour=abs(AuAgDeca$Spin),
                                  size=2*exp(-1.0*AuAgDeca$EEFinal),shape=AuAgDeca$Type),alpha = I(0.45)) + 
  xlab("Excess Energy (eV)") + ylab("Gap (eV)")+labs(colour="Gap",size="Spin")+labs(title = "Gap vs Excess Energy",
                                                                                    colour="Spin",size="R=exp(-(E-E_gs))",alpha="",shape="Seed")+
  theme(plot.title = element_text(size =rel(2), face="bold", colour="steelblue"),axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size =rel(1),face="bold", colour="steelblue"))

ggsave(file = "EEGapDeca_nomin.pdf", width = 10, height = 7 )
#dev.off()
#png(file = "EnergyExcessDeca.png", bg="transparent")
ggsave(file = "EEGapDeca.png", bg="transparent",width = 10, height = 7)
#dev.off()





AuAg<-rbind(AuAg,AuAgCubo)






nrow
c(
nrow ( subset(AuAgBBP,(Schoenflies.symbol.T!="NO" & Schoenflies.symbol.T!="C2v")) ) / (
nrow ( subset(AuAgBBP,(Schoenflies.symbol.T!="NO" & Schoenflies.symbol.T!="C2v")) ) +
  nrow ( subset(AuAgBBP,(Schoenflies.symbol.T=="NO")) ) +
  nrow ( subset(AuAgBBP,(Schoenflies.symbol.T=="C2v")) )
) * 100
,
nrow ( subset(AuAgBBP,(Schoenflies.symbol.T=="NO" )) ) / (
  nrow ( subset(AuAgBBP,(Schoenflies.symbol.T!="NO" & Schoenflies.symbol.T!="C2v")) ) +
    nrow ( subset(AuAgBBP,(Schoenflies.symbol.T=="NO")) ) +
    nrow ( subset(AuAgBBP,(Schoenflies.symbol.T=="C2v")) )
) * 100
,
nrow ( subset(AuAgBBP,(Schoenflies.symbol.T=="C2v" )) ) / (
  nrow ( subset(AuAgBBP,(Schoenflies.symbol.T!="NO" & Schoenflies.symbol.T!="C2v")) ) +
    nrow ( subset(AuAgBBP,(Schoenflies.symbol.T=="NO")) ) +
    nrow ( subset(AuAgBBP,(Schoenflies.symbol.T=="C2v")) )
) * 100
)



c(
nrow ( subset(AuAgIco,(Schoenflies.symbol.T!="NO" & Schoenflies.symbol.T!="Ih")) ) / (
  nrow ( subset(AuAgIco,(Schoenflies.symbol.T!="NO" & Schoenflies.symbol.T!="Ih")) ) +
    nrow ( subset(AuAgIco,(Schoenflies.symbol.T=="NO")) ) +
    nrow ( subset(AuAgIco,(Schoenflies.symbol.T=="Ih")) )
) * 100
,
nrow ( subset(AuAgIco,(Schoenflies.symbol.T=="NO" )) ) / (
  nrow ( subset(AuAgIco,(Schoenflies.symbol.T!="NO" & Schoenflies.symbol.T!="Ih")) ) +
    nrow ( subset(AuAgIco,(Schoenflies.symbol.T=="NO")) ) +
    nrow ( subset(AuAgIco,(Schoenflies.symbol.T=="Ih")) )
) * 100
,
nrow ( subset(AuAgIco,(Schoenflies.symbol.T=="Ih" )) ) / (
  nrow ( subset(AuAgIco,(Schoenflies.symbol.T!="NO" & Schoenflies.symbol.T!="Ih")) ) +
    nrow ( subset(AuAgIco,(Schoenflies.symbol.T=="NO")) ) +
    nrow ( subset(AuAgIco,(Schoenflies.symbol.T=="Ih")) )
) * 100
)



c(
nrow ( subset(AuAgCubo,(Schoenflies.symbol.T!="NO" & Schoenflies.symbol.T!="Oh")) ) / (
  nrow ( subset(AuAgCubo,(Schoenflies.symbol.T!="NO" & Schoenflies.symbol.T!="Oh")) ) +
    nrow ( subset(AuAgCubo,(Schoenflies.symbol.T=="NO")) ) +
    nrow ( subset(AuAgCubo,(Schoenflies.symbol.T=="Oh")) )
) * 100
,
nrow ( subset(AuAgCubo,(Schoenflies.symbol.T=="NO" )) ) / (
  nrow ( subset(AuAgCubo,(Schoenflies.symbol.T!="NO" & Schoenflies.symbol.T!="Oh")) ) +
    nrow ( subset(AuAgCubo,(Schoenflies.symbol.T=="NO")) ) +
    nrow ( subset(AuAgCubo,(Schoenflies.symbol.T=="Oh")) )
) * 100
,
nrow ( subset(AuAgCubo,(Schoenflies.symbol.T=="Oh" )) ) / (
  nrow ( subset(AuAgCubo,(Schoenflies.symbol.T!="NO" & Schoenflies.symbol.T!="Oh")) ) +
    nrow ( subset(AuAgCubo,(Schoenflies.symbol.T=="NO")) ) +
    nrow ( subset(AuAgCubo,(Schoenflies.symbol.T=="Oh")) )
) * 100
)




c(
nrow ( subset(AuAgDeca,(Schoenflies.symbol.T!="NO" & Schoenflies.symbol.T!="D5h")) ) / (
  nrow ( subset(AuAgDeca,(Schoenflies.symbol.T!="NO" & Schoenflies.symbol.T!="D5h")) ) +
    nrow ( subset(AuAgDeca,(Schoenflies.symbol.T=="NO")) ) +
    nrow ( subset(AuAgDeca,(Schoenflies.symbol.T=="D5h")) )
) * 100
,
nrow ( subset(AuAgDeca,(Schoenflies.symbol.T=="NO" )) ) / (
  nrow ( subset(AuAgDeca,(Schoenflies.symbol.T!="NO" & Schoenflies.symbol.T!="D5h")) ) +
    nrow ( subset(AuAgDeca,(Schoenflies.symbol.T=="NO")) ) +
    nrow ( subset(AuAgDeca,(Schoenflies.symbol.T=="D5h")) )
) * 100
,
nrow ( subset(AuAgDeca,(Schoenflies.symbol.T=="D5h" )) ) / (
  nrow ( subset(AuAgDeca,(Schoenflies.symbol.T!="NO" & Schoenflies.symbol.T!="D5h")) ) +
    nrow ( subset(AuAgDeca,(Schoenflies.symbol.T=="NO")) ) +
    nrow ( subset(AuAgDeca,(Schoenflies.symbol.T=="D5h")) )
) * 100
)