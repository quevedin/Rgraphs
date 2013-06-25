# Plot BBP Data - Energy Excess ---------------------------------------------------------
EE1st= ( minimaBBP$E_KS..1st. - 
       ( minimaBBP$E_KS..1st.[1]/13.0*minimaBBP$N_Au + 
         minimaBBP$E_KS..1st.[14]/13.0*minimaBBP$N_Ag ) ) 
  #/ 13.0
EE1st_1st= ( minimaBBP1st$E_KS..1st. - 
           ( minimaBBP1st$E_KS..1st.[1]/13.0*minimaBBP1st$N_Au + 
             minimaBBP1st$E_KS..1st.[14]/13.0*minimaBBP1st$N_Ag ) ) 
  #/ 13.0
EE1stTot= ( AuAgBBP$E_KS..1st. - 
          ( minimaBBP$E_KS..1st.[1]/13.0*AuAgBBP$N_Au + 
            minimaBBP$E_KS..1st.[14]/13.0*AuAgBBP$N_Ag ) ) 
  #/ 13.0

ggplot() + theme_bw() + geom_line(data = minimaBBP, 
                                  aes(x = minimaBBP$N_Ag, y = EE1st,
                                      linetype = "Minima E BBP"),colour = "darkred",size = 1) +
  geom_line(data = minimaBBP1st, aes(x = minimaBBP1st$N_Ag, y = EE1st_1st,
                                     linetype = "Minima E1st BBP"),
            colour = "darkred", size = 1) + scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgBBP, aes(x = AuAgBBP$N_Ag, y = EE1stTot,colour=AuAgBBP$Gap,
                                 size = abs(AuAgBBP$Spin),shape=AuAgBBP$Type),alpha = I(0.45)) + 
  xlab("Number of Ag Atoms") + ylab("Energy Excess 1st step BBP (eV)") +
  labs(colour="Gap",size="Spin") + 
  labs(title = "Energy Excess 1st step vs Silver Atoms", colour="Gap",size="Spin",size="Spin", 
       alpha="", shape="Seed") + 
  theme(plot.title = element_text(size = rel(2), face="bold", 
                                  #colour="steelblue"
                                  colour="black"), 
        axis.line = element_line(size = 1)) + 
  theme(axis.title= element_text(size = rel(1),face="bold", colour="steelblue")) +
  scale_y_continuous(limits=c(min(EE1st_1st),0.045))
  #+
##scale_x_continuous(breaks=0:14)+ geom_hline(yintercept=0,size=1)

#pdf(file = "EnergyExcessBBP1st.pdf", width = 6, height = 4)
ggsave(file = "EnergyExcessBBP1st.pdf", width = 10, height = 7 )
#dev.off()
#png(file = "EnergyExcessBBP1st.png", bg="transparent")
ggsave(file = "EnergyExcessBBP1st.png", bg="transparent",width = 10, height = 7)
#dev.off()

############################## Deca

# Plot Deca Data - Energy Excess ---------------------------------------------------------
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

subDeca=subset(AuAgDeca_order1st,EE1stTot>-2.2 & EE1stTot<2.2)

ggplot() + theme_bw() + 
  geom_line(data = minimaDeca, 
            aes(x = minimaDeca$N_Ag, y = minimaDeca$EEFinal,linetype = "EE Minima E Deca"),
            colour = "darkred",size = 1) +
  geom_line(data = minimaDeca1st, aes(x = minimaDeca1st$N_Ag, y = minimaDeca1st$EEFinal,
                                     linetype = "EE Minima E1st Deca"),
            colour = "darkred", size = 1) + 
  geom_line(data = minimaDeca, aes(x = minimaDeca$N_Ag, y = EE1st,
                                  linetype = "EE1st Minima E Deca"),colour = "darkblue",size = 1) +
  geom_line(data = minimaDeca1st, aes(x = minimaDeca1st$N_Ag, y = EE1st_1st,
                                     linetype = "EE1st Minima E1st Deca"),
            colour = "darkblue", size = 1) + scale_linetype_discrete('Minima') +  
  xlab("Number of Ag Atoms") + ylab("Energy Excess 1st step Deca (eV)") +
  labs(colour="Gap",size="Spin") + 
  labs(title = "Energy Excess 1st step vs Silver Atoms", colour="Gap",size="Spin",size="Spin", 
       alpha="", shape="Seed") + 
  theme(plot.title = element_text(size = rel(2), face="bold", 
                                  #colour="steelblue"
                                  colour="black"), 
        axis.line = element_line(size = 1)) + 
  theme(axis.title= element_text(size = rel(1),face="bold", colour="steelblue")) +
  scale_y_continuous(limits=c(min(minimaDeca$EEFinal),0.045),
                     breaks=seq(from=-3.25,
                                  #min(minimaDeca$EEFinal),
                                  to=0.0,length.out = 10))
#  scale_y_continuous(limits=c(min(minimaDeca$EEFinal),0.045)) + 
  
  #pdf(file = "EnergyExcessDeca1st.pdf", width = 6, height = 4)
  ggsave(file = "EnergyExcessDeca1stC.pdf", width = 10, height = 7 )
  #dev.off()
  #png(file = "EnergyExcessDeca1st.png", bg="transparent")
  ggsave(file = "EnergyExcessDeca1stC.png", bg="transparent",width = 10, height = 7)
  #dev.off()