minEbop <- read.table("M:/RANGER/work/Newconfs/minEbop.txt", header=T, quote="\"")
View(minEbop)

minEbop$Ag=c(0:13)

minEbop$Excess=minEbop$energy-(minEbop$energy[1]*(13-minEbop$Ag)/13+
                                 minEbop$energy[14]*(minEbop$Ag)/13)

pairs(minEbop)




AuAgIco_order<-df3[order(df3$N_Ag,df3$E_Free),]
#sorted by number of Ag atoms and then Energies

uniqueDataAuAgIco<-unique(df3$N_Ag)

minimaIco = AuAgIco_order[!duplicated(AuAgIco_order$N_Ag),]

AuAgIco_order1st<-df3[order(df3$N_Ag,df3$E_KS..1st.),]
uniqueDataAuAgIco<-unique(df3$N_Ag)
minimaIco1st = AuAgIco_order1st[!duplicated(AuAgIco_order1st$N_Ag),]

windows()

ggplot()+geom_point(aes(x=df3$NAGAU_ini,y=df3$DE1stSteppa))+
  geom_line(aes(x=minimaIco$NAGAU_ini,y=minimaIco$DE1stSteppa))+
  geom_line(aes(x=minimaIco1st$NAGAU_ini,y=minimaIco1st$DE1stSteppa),linetype="dashed",width=2)

windows()

ggplot()+geom_point(aes(x=df3$NAGAU_ini/df3$NTOT,y=df3$DE1stSteppa))+
  geom_line(aes(x=minimaIco$NAGAU_ini/minimaIco$NTOT,y=minimaIco$DE1stSteppa))+
  geom_line(aes(x=minimaIco1st$NAGAU_ini/minimaIco1st$NTOT,y=minimaIco1st$DE1stSteppa),
            linetype="dashed",width=2)

windows()

ggplot()+geom_point(aes(x=df3$NAUAU_ini,y=df3$DE1stSteppa))+
  geom_line(aes(x=minimaIco$NAUAU_ini,y=minimaIco$DE1stSteppa),linetype="dashed",width=4)+
  geom_line(aes(x=minimaIco1st$NAUAU_ini,y=minimaIco1st$DE1stSteppa),linetype="dashed",width=4)

windows()

ggplot()+geom_point(aes(x=df3$NAUAU_ini/df3$NTOT,y=df3$DE1stSteppa))+
  geom_line(aes(x=minimaIco$NAUAU_ini/minimaIco$NTOT,y=minimaIco$DE1stSteppa))+
  geom_line(aes(x=minimaIco1st$NAUAU_ini/minimaIco1st$NTOT,y=minimaIco1st$DE1stSteppa),
            linetype="dashed",width=2)

windows()

ggplot()+geom_point(aes(x=df3$NAGAG_ini,y=df3$DE1stSteppa))+
  geom_line(aes(x=minimaIco$NAGAG_ini,y=minimaIco$DE1stSteppa))+
  geom_line(aes(x=minimaIco1st$NAGAG_ini,y=minimaIco1st$DE1stSteppa),
            linetype="dashed",width=2)


windows()

df3$diffE=df3$E_Free-df3$E_KS..1st.
minimaIco$diffE=minimaIco$E_Free-minimaIco$E_KS..1st.
minimaIco1st$diffE=minimaIco1st$E_Free-minimaIco1st$E_KS..1st.

ggplot()+geom_point(aes(x=df3$NAGAG_ini,y=df3$diff))+
  geom_line(aes(x=minimaIco$NAGAG_ini,y=minimaIco$diff))+
  geom_line(aes(x=minimaIco1st$NAGAG_ini,y=minimaIco1st$diff),
            linetype="dashed",width=2)


windows()

ggplot()+geom_point(aes(x=df3$N_Ag,y=df3$diff))+
  geom_line(aes(x=minimaIco$N_Ag,y=minimaIco$diff))+
  geom_line(aes(x=minimaIco1st$N_Ag,y=minimaIco1st$diff),
            linetype="dashed",width=2)





ggplot() + theme_bw() + 
  geom_line(data = minimaIco, 
            aes(x = minimaIco$N_Ag, y = minimaIco$EEFinal,linetype = "EE Minima E Ico"),
            colour = "darkred",size = 1) +
  geom_line(data = minimaIco1st, aes(x = minimaIco1st$N_Ag, y = minimaIco1st$EEFinal,
                                      linetype = "EE Minima E1st Ico"),
            colour = "darkred", size = 1) + 
  geom_line(data = minimaIco, aes(x = minimaIco$N_Ag, y = EE1st,
                                   linetype = "EE1st Minima E Ico"),colour = "darkblue",size = 1) +
  geom_line(data = minimaIco1st, aes(x = minimaIco1st$N_Ag, y = EE1st_1st,
                                      linetype = "EE1st Minima E1st Ico"),
            colour = "darkblue", size = 1) + scale_linetype_discrete('Minima') +  
  xlab("Number of Ag Atoms") + ylab("Energy Excess 1st step Ico (eV)") +
  labs(colour="Gap",size="Spin") + 
  labs(title = "Energy Excess 1st step vs Silver Atoms", colour="Gap",size="Spin",size="Spin", 
       alpha="", shape="Seed") + 
  theme(plot.title = element_text(size = rel(2), face="bold", 
                                  #colour="steelblue"
                                  colour="black"), 
        axis.line = element_line(size = 1)) + 
  theme(axis.title= element_text(size = rel(1),face="bold", colour="steelblue")) +
  scale_y_continuous(limits=c(min(minimaIco$EEFinal),0.045),
                     breaks=seq(from=-3.25,
                                #min(minimaIco$EEFinal),
                                to=0.0,length.out = 10))
#  scale_y_continuous(limits=c(min(minimaIco$EEFinal),0.045)) + 

#pdf(file = "EnergyExcessIco1st.pdf", width = 6, height = 4)
ggsave(file = "EnergyExcessIco1stC.pdf", width = 10, height = 7 )
#dev.off()
#png(file = "EnergyExcessIco1st.png", bg="transparent")
ggsave(file = "EnergyExcessIco1stC.png", bg="transparent",width = 10, height = 7)
#dev.off()