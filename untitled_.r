BondList <- read.csv("C:/Users/Kib/Downloads/documents-export-2013-01-24/BondList.csv")

library("ggplot2", lib.loc="C:/Program Files/R/R-2.15.2/library")

AuAgBBP<-BondList
uniqueDataAuAgI<-unique(AuAgBBP$N_Ag)
minDEminAuAgI<-0.0*unique(AuAgBBP$N_Ag)
for(i in uniqueDataAuAgI){
  minDEminAuAgI[i+1]=min(AuAgBBP$EEFinal[which(AuAgBBP$N_Ag==i,arr.ind=TRUE)])
}

mydata <- data.frame(Ag=uniqueDataAuAgI, minDEminAuAgI)

MergedDataset<-BondList

windows()

#ggplot() + geom_line(data = mydata, aes(x = mydata$Ag, y = mydata$minDEmin),colour = "darkgreen", size = 1,linetype = 2) + geom_point(data = MergedDataset, aes(x = MergedDataset$N_Ag, y = MergedDataset$EEFinal,colour=MergedDataset$Gap,size = abs(MergedDataset$Spin),shape=MergedDataset$Type),alpha = I(0.45))
#ggplot() +theme_bw()+ geom_line(data = mydata, aes(x = mydata$Ag, y = mydata$minDEmin),colour = "darkgreen", size = 1,linetype = 2) + geom_point(data = MergedDataset, aes(x = MergedDataset$N_Ag, y = MergedDataset$EEFinal,colour=MergedDataset$Gap,size = abs(MergedDataset$Spin),shape=MergedDataset$Type),alpha = I(0.45))+ xlab("Number of Ag Atoms") + ylab("Energy Excess (eV)")+labs(colour="Gap",size="Spin")+labs(title = "Energy Excess vs Silver Atoms",colour="Gap",size="Spin",size="Spin",alpha="",shape="Seed")+theme(plot.title = element_text(size = rel(3), face="bold", colour="steelblue"),axis.line = element_line(size = 1))+theme(axis.title= element_text(size = rel(2),face="bold", colour="steelblue"))+scale_x_continuous(breaks=0:14)+ geom_hline(yintercept=0,size=1)

ggplot() +theme_bw()+ geom_line(data = minimaBBP, aes(x = minimaBBP$N_Ag, y = minimaBBP$EEFinal),colour = "darkgreen", size = 1,linetype = 2) + geom_point(data = MergedDataset, aes(x = MergedDataset$N_Ag, y = MergedDataset$EEFinal,colour=MergedDataset$Gap,size = abs(MergedDataset$Spin),shape=MergedDataset$Type),alpha = I(0.45))+ xlab("Number of Ag Atoms") + ylab("Energy Excess (eV)")+labs(colour="Gap",size="Spin")+labs(title = "Energy Excess vs Silver Atoms",colour="Gap",size="Spin",size="Spin",alpha="",shape="Seed")+theme(plot.title = element_text(size = rel(3), face="bold", colour="steelblue"),axis.line = element_line(size = 1))+theme(axis.title= element_text(size = rel(2),face="bold", colour="steelblue"))+scale_x_continuous(breaks=0:14)+ geom_hline(yintercept=0,size=1)
ggsave(file = "EnergyExcessBBP.pdf", width = 7, height = 7,bg="transparent")
#dev.off()
#png(file = "EnergyExcessIcoDeca.png", bg="transparent")
ggsave(file = "EnergyExcessBBP.png", bg="transparent")


EPJD.43.53.2007 <- read.table("C:/Users/Kib/Google Drive/EPJD-43-53-2007.txt", quote="\"")
colnames(EPJD.43.53.2007)=c("N_Au","E_Excess")
EPJD.43.53.2007$N_Au<-round(EPJD.43.53.2007$N_Au)
View(EPJD.43.53.2007)






ggplot() + theme_bw() + 
  geom_point(data = subset(AuAgDeca, #(DE1stStep/13+0.01*(7-N_Ag)) <= -0.16  ) ,
                           (DE1stStep/13+0.00*(7-N_Ag)) <= -0.15  ) ,
             aes(x = N_Ag, y = DE1stStep/13 #,
                                  #linetype = "Final"
                                  ),size =2,colour  = "red") +
  geom_point(data = subset(AuAgDeca, (DE1stStep/13+0.00*(7-N_Ag)) > -0.15) ,
             aes(x = N_Ag, y = DE1stStep/13                  
                 #linetype = "Final"
             ),colour  = "blue"
             #size =2
             ) + scale_y_continuous(breaks=(-10:10)/20)


  geom_line(data = minimaDeca, aes(x = minimaDeca$N_Ag, y = minimaDeca$EEFinal/13,
                                   colour  = "Minima",linetype = "Final"),size = 1) + 
  geom_line(data = minimaDeca1st, aes(x = minimaDeca1st$N_Ag, y = minimaDeca1st$EEFinal/13,
                                      colour  = "Minima 1st",linetype = "Final"), size = 1) + 
  geom_line(data = minimaDeca, aes(x = minimaDeca$N_Ag, y = minimaDeca$DE1stStep/13,
                                   colour  = "Minima",linetype = "First"),size = 1) +
  geom_line(data = minimaDeca1st, aes(x = minimaDeca1st$N_Ag, y = minimaDeca1st$DE1stStep/13,
                                      colour  = "Minima 1st",linetype = "First"), size = 1) +
  scale_linetype_discrete('Minima') + 
  ylab("Energy Excess Deca (eV)")+xlab("Number of Ag Atoms") +
  labs(title = "Energy Excess vs Silver Atoms",colour="Case",size="Spin",size="Spin",alpha="",
       shape="Seed",linetype="Case")+
  theme(plot.title = element_text(size = rel(1), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:14)+ geom_hline(yintercept=0,size=1) +
  scale_colour_manual(values=c("blue4","red4","green4","black")  ,  
                      breaks = c("Minima","Minima 1st","green4","black")  , 
                      labels=c("Minima","Minima 1st","Deca","Deca") ) 