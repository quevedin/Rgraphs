pIco <- 
  ggplot() + theme_bw() + 
     geom_line(data = minimaIco, aes(x = minimaIco$N_Ag, y = minimaIco$EEFinal/13,
                                     colour  = "Minima",linetype = "Final"),size = 1) + 
     geom_line(data = minimaIco1st, aes(x = minimaIco1st$N_Ag, y = minimaIco1st$EEFinal/13,
                                        colour  = "Minima 1st",linetype = "Final"), size = 1) + 
  geom_line(data = minimaIco, aes(x = minimaIco$N_Ag, y = DE1stStep/13,
                                  colour  = "Minima",linetype = "First"),size = 1) +
  geom_line(data = minimaIco1st, aes(x = minimaIco1st$N_Ag, y = minimaIco1st$DE1stStep/13,
                                     colour  = "Minima 1st",linetype = "First"), size = 1) +
     scale_linetype_discrete('Minima') + 
     ylab("Energy Excess Ico (eV)")+xlab("Number of Ag Atoms") +
     labs(title = "Energy Excess vs Silver Atoms",colour="Case",size="Spin",size="Spin",alpha="",
          shape="Seed",linetype="Case")+
     theme(plot.title = element_text(size = rel(1), face="bold"),
           axis.line = element_line(size = 1))+
     theme(axis.title= element_text(size = rel(1),face="bold"))+
     scale_x_continuous(breaks=0:14)+ geom_hline(yintercept=0,size=1) +
   scale_colour_manual(values=c("blue4","red4","green4","black")  ,  
                      breaks = c("Minima","Minima 1st","green4","black")  , 
                   labels=c("Minima","Minima 1st","Deca","Ico") ) 

# EE1stTotDeca

##### Plot Deca Data - Energy Excess ---------------------------------------------------------
EE1stDeca= ( minimaDeca$E_KS..1st. - 
               ( minimaDeca$E_KS..1st.[1]/13.0*minimaDeca$N_Au + 
                   minimaDeca$E_KS..1st.[14]/13.0*minimaDeca$N_Ag ) ) 
#/ 13.0
EE1st_1stDeca= ( minimaDeca1st$E_KS..1st. - 
                   ( minimaDeca1st$E_KS..1st.[1]/13.0*minimaDeca1st$N_Au + 
                       minimaDeca1st$E_KS..1st.[14]/13.0*minimaDeca1st$N_Ag ) ) 
#/ 13.0
EE1stTotDeca= ( AuAgDeca$E_KS..1st. - 
                  ( minimaDeca$E_KS..1st.[1]/13.0*AuAgDeca$N_Au + 
                      minimaDeca$E_KS..1st.[14]/13.0*AuAgDeca$N_Ag ) ) 
#/ 13.0
AuAgDeca$EE1stTotDeca=EE1stTotDeca
AuAgDeca_order1st<-AuAgDeca[order(AuAgDeca$N_Ag,AuAgDeca$E_KS..1st.),]
subDeca=subset(AuAgDeca_order1st,EE1stTotDeca>-1.92 & EE1stTotDeca<0.2)
subDeca2=subset(AuAgDeca_order1st,EE1stTotDeca<-1.92 | EE1stTotDeca>0.2)
subDeca2$Diff=subDeca2$E_Free-subDeca2$E_KS..1st.
subDeca2B=subset(subDeca2,(Diff > -.52 | Diff < -2.52) )
subDeca2C=subset(subDeca2,( Diff < -2.52) )
subDeca2B[order(subDeca2B$N_Ag,-subDeca2B$Diff),]

minsubDeca=subDeca[!duplicated(subDeca$N_Ag),]


ggplot() + theme_bw() + 
#  geom_line(data = minsubDeca, aes(x = minsubDeca$N_Ag, y = minsubDeca$EE1stTotDeca,
#                                   colour  = "Minima 1st Tot subDeca",linetype = "1st Final subDeca"), 
#            size = 4,alpha=I(0.5) ) +
  geom_point(data=subDeca2B,aes(x = N_Ag,
#                               y = EE1stTotDeca, 
                               y = Diff, 
                 colour  = "Minima",linetype = "Final"),size = 3) +
  scale_y_continuous(limits=c(-3.25,0.1)) +   scale_linetype_discrete('Minima')


ggplot() + theme_bw() + 
  geom_line(data = minsubDeca, aes(x = minsubDeca$N_Ag, y = minsubDeca$EE1stTotDeca,
                                colour  = "Minima 1st Tot subDeca",linetype = "1st Final subDeca"), 
            size = 4,alpha=I(0.5) ) +
  geom_point(aes(x = AuAgDeca$N_Ag, y = EE1stTotDeca, 
                                   colour  = "Minima",linetype = "Final"),size = 3) +
  geom_line(data = minimaDeca1st, aes(x = minimaDeca1st$N_Ag, y = EE1st_1stDeca,
                                      colour  = "Minima 1st",linetype = "Final"), size = 2.5,alpha=I(0.5)) + 
  geom_line(data = minimaDeca1st, aes(x = minimaDeca1st$N_Ag, y = EE1stDeca,
                                      colour  = "Minima 1st Tot",linetype = "1st Final"), 
            size = 2,alpha=I(0.5)) +
  scale_y_continuous(limits=c(-3.25,0.1)) +   scale_linetype_discrete('Minima')
  
pDeca <- 
ggplot() + theme_bw() + 
  geom_line(data = minimaDeca, aes(x = minimaDeca$N_Ag, y = minimaDeca$EEFinal/13,
                                  colour  = "Minima",linetype = "Final"),size = 1) + 
  geom_line(data = minimaDeca1st, aes(x = minimaDeca1st$N_Ag, y = minimaDeca1st$EEFinal/13,
                                     colour  = "Minima 1st",linetype = "Final"), size = 1) + 
  geom_line(data = minimaDeca, aes(x = minimaDeca$N_Ag, y = minimaDeca$DE1stStep/13,
                                  colour  = "Minima",linetype = "First"),size = 1) +
  geom_line(data = 
              minimaDeca1st
              , 
            aes(x = N_Ag, y = DE1stStep/13,
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

pCubo <- 
  ggplot() + theme_bw() + 
  geom_line(data = minimaCubo, aes(x = minimaCubo$N_Ag, y = minimaCubo$EEFinal/13,
                                   colour  = "Minima",linetype = "Final"),size = 1) + 
  geom_line(data = 
              minimaCubo1st,
                        aes(x = minimaCubo1st$N_Ag, y = minimaCubo1st$EEFinal/13,
                                      colour  = "Minima 1st",linetype = "Final"), size = 1) + 
  geom_line(data = minimaCubo, aes(x = minimaCubo$N_Ag, y = minimaCubo$DE1stStep/13,
                                   colour  = "Minima",linetype = "First"),size = 1) +
  geom_line(data = minimaCubo1st, 
            aes(x = minimaCubo1st$N_Ag, y = minimaCubo1st$DE1stStep/13,
                                      colour  = "Minima 1st",linetype = "First"), size = 1) +
  scale_linetype_discrete('Minima') + 
  ylab("Energy Excess Cubo (eV)")+xlab("Number of Ag Atoms") +
  labs(title = "Energy Excess vs Silver Atoms",colour="Case",size="Spin",size="Spin",alpha="",
       shape="Seed",linetype="Case")+
  theme(plot.title = element_text(size = rel(1), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:14)+ geom_hline(yintercept=0,size=1) +
  scale_colour_manual(values=c("blue4","red4","green4","black")  ,  
                      breaks = c("Minima","Minima 1st","green4","black")  , 
                      labels=c("Minima","Minima 1st","Cubo","Cubo") ) 


pBBP <- 
  ggplot() + theme_bw() + 
  geom_line(data = minimaBBP, aes(x = minimaBBP$N_Ag, y = minimaBBP$EEFinal/13,
                                   colour  = "Minima",linetype = "Final"),size = 1) + 
  geom_line(data = minimaBBP1st, aes(x = minimaBBP1st$N_Ag, y = minimaBBP1st$EEFinal/13,
                                      colour  = "Minima 1st",linetype = "Final"), size = 1) + 
  geom_line(data = minimaBBP, aes(x = minimaBBP$N_Ag, y = minimaBBP$DE1stStep/13,
                                   colour  = "Minima",linetype = "First"),size = 1) +
  geom_line(data = minimaBBP1st, aes(x = minimaBBP1st$N_Ag, y = minimaBBP1st$DE1stStep/13,
                                      colour  = "Minima 1st",linetype = "First"), size = 1) +
  scale_linetype_discrete('Minima') + 
  ylab("Energy Excess BBP (eV)")+xlab("Number of Ag Atoms") +
  labs(title = "Energy Excess vs Silver Atoms",colour="Case",size="Spin",size="Spin",alpha="",
       shape="Seed",linetype="Case")+
  theme(plot.title = element_text(size = rel(1), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:14)+ geom_hline(yintercept=0,size=1) +
  scale_colour_manual(values=c("blue4","red4","green4","black")  ,  
                      breaks = c("Minima","Minima 1st","green4","black")  , 
                      labels=c("Minima","Minima 1st","BBP","BBP") ) 

library(gridExtra)

pdf(file = "4panel.pdf", width = 10, height = 7 )

grid.arrange(pIco, pCubo, pDeca, pBBP, ncol=2)

#ggsave(file = "4panel.pdf", width = 10, height = 7 )

dev.off()