library(ggplot2 )

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

# Import Ico Data ---------------------------------------------------------
AuAgIco <- read.csv("C:/Users/Kib/Downloads/documents-export-2013-01-24/BondListIco.csv")
AuAgIco_order<-AuAgIco[order(AuAgIco$N_Ag,AuAgIco$E_Free),]
#sorted by number of Ag atoms and then Energies




uniqueDataAuAgIco<-unique(AuAgIco$N_Ag)
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



# Import Deca Data ---------------------------------------------------------
AuAgDeca <- read.csv("C:/Users/Kib/Downloads/documents-export-2013-01-24/BondListDeca.csv")

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


#

# Merge Data ---------------------------------------------------------

#colnames(AuAgCubo)[13]="DE_Bulk_peratom"

AuAg<-rbind(AuAgBBP,AuAgIco)
AuAg<-rbind(AuAg,AuAgDeca)
AuAg<-rbind(AuAg,AuAgCubo)
#AuAg<-rbind(AuAg,AuAgCubo)

#minima<-rbind(minimaBBP,minimaBBP1st)
minima<-rbind(minima,minimaIco)
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
  scale_x_continuous(breaks=0:14)+ geom_hline(yintercept=0,size=1) + 
  geom_point(data = AuAg,aes(x = N_Ag, y = EEFinal,colour=Gap,size = abs(Spin),
                             shape=Type),alpha = I(0.45)) + scale_size_continuous(range=c(3,9)) +
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
  
  
  
  #pdf(file = "EnergyExcessBBP.pdf", width = 6, height = 4)
  ggsave(file = "EnergyExcessBBPIco.pdf", width = 10, height = 7 )
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

#pdf(file = "EnergyExcessBBP.pdf", width = 6, height = 4)
ggsave(file = "EnergyExcessBBP.pdf", width = 10, height = 7 )
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

ggsave(file = "SpinBBP.pdf", width = 10, height = 7 )
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

ggsave(file = "SpinBBP.pdf", width = 10, height = 7 )
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

ggsave(file = "GapBBP.pdf", width = 10, height = 7 )
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

ggsave(file = "EESpinnBBP.pdf", width = 10, height = 7 )
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

ggsave(file = "EEGapBBP.pdf", width = 10, height = 7 )
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

ggsave(file = "EnergyExcessIco.pdf", width = 10, height = 7 )
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

ggsave(file = "SpinIco.pdf", width = 10, height = 7 )
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

ggsave(file = "GapIco.pdf", width = 10, height = 7 )
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

ggsave(file = "EESpinnIco.pdf", width = 10, height = 7 )
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

ggsave(file = "EEGapIco.pdf", width = 10, height = 7 )
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

ggsave(file = "EnergyExcessCubo.pdf", width = 10, height = 7 )
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

ggsave(file = "SpinCubo.pdf", width = 10, height = 7 )
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

ggsave(file = "GapCubo.pdf", width = 10, height = 7 )
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

ggsave(file = "EESpinnCubo.pdf", width = 10, height = 7 )
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

ggsave(file = "EEGapCubo.pdf", width = 10, height = 7 )
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

ggsave(file = "EnergyExcessDeca.pdf", width = 10, height = 7 )
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

ggsave(file = "SpinDeca.pdf", width = 10, height = 7 )
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

ggsave(file = "GapDeca.pdf", width = 10, height = 7 )
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

ggsave(file = "EESpinnDeca.pdf", width = 10, height = 7 )
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

ggsave(file = "EEGapDeca.pdf", width = 10, height = 7 )
#dev.off()
#png(file = "EnergyExcessDeca.png", bg="transparent")
ggsave(file = "EEGapDeca.png", bg="transparent",width = 10, height = 7)
#dev.off()





AuAg<-rbind(AuAg,AuAgCubo)
