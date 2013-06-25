library(grid) # for unit
ggplot() + theme_bw() + 
  geom_point(data = minimaIco, aes(x = minimaIco$N_Ag+0.25, 
                                   y = abs(minimaIco$Spin),colour=minimaIco$Type),
             shape = 15,
             #             colour = "#56B4E9", 
             size=3.5,lwd=3) + 
  geom_point(data = AuAgIco, aes(x = AuAgIco$N_Ag+0.25, y = abs(AuAgIco$Spin),colour= AuAgIco$Type),
             shape=0,size=3.5, 
             #             colour="#56B4E9",
             lwd=3,width = I(3)) +
  #annotate("text", x = 8, y = 0.7, label = "Ico",colour="#56B4E9") +
  #
  #Deca
  geom_point(data = minimaDeca, aes(x = minimaDeca$N_Ag-0.25, 
                                    y = abs(minimaDeca$Spin),
                                    color=minimaDeca$Type),
             shape = 18,
             #colour = "#D55E00", 
             size=5,lwd=3) + 
  geom_point(data = AuAgDeca, aes(x = AuAgDeca$N_Ag-0.25, y = abs(AuAgDeca$Spin),color=AuAgDeca$Type),
             size=3.5, 
             #colour="#D55E00",
             shape=5,width = I(3),lwd=3 ) +
  geom_point(data = minimaCubo, aes(x = minimaCubo$N_Ag-0.0833, 
                                    y = abs(minimaCubo$Spin),color=minimaCubo$Type),
             shape = 17,
             #             colour = "#009E73",
             size=3.5,lwd=3) + 
  scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgCubo, aes(x = AuAgCubo$N_Ag-0.0833, y = abs(AuAgCubo$Spin),colour=AuAgCubo$Type),
             size=3.5 , #) + #, 
             #             colour="#009E73",
             shape=2,width = I(3) ) +
  #  ) + # % position = "jitter") + 
  #annotate("text", x = 6, y = 0.7, label = "Cubo",colour="#009E73") +
  #Ico
  
  #  annotate("text", x = 10, y = 0.7, label = "Deca",colour="#D55E00") +
  #  
  geom_point(data = minimaBBP, aes(x = minimaBBP$N_Ag+0.0833, 
                                   y = abs(minimaBBP$Spin),color=minimaBBP$Type),
             shape = 16,width = I(0.6),
             size=3.5,show_guide = FALSE) + 
  geom_point(data = AuAgBBP, aes(x = AuAgBBP$N_Ag+0.0833, y = abs(AuAgBBP$Spin), color=AuAgBBP$Type ),
             size=3.5, width = I(3),
             #             colour="black",
             shape=21) +
  geom_line(data = minimaIco, aes(x = minimaIco$N_Ag+0.25, y = abs(minimaIco$Spin),colour=minimaIco$Type),
            linetype = "solid",
            # "Minima E Ico"),
            #colour = "#56B4E9",
            size = 1) + 
  geom_line(data = minimaDeca, aes(x = minimaDeca$N_Ag-0.25, y = abs(minimaDeca$Spin),
                                   color=minimaDeca$Type),linetype = "solid",
            # "Minima E Deca"),
            #colour = "#D55E00",
            size = 1) + 
  geom_line(data = minimaCubo, aes(x = minimaCubo$N_Ag-0.0833, y = abs(minimaCubo$Spin),
                                   color=minimaCubo$Type),
            linetype = "solid", #"Minima E Cubo"),
            #colour = "#009E73", 
            size = 1) + 
  geom_line(data = minimaBBP, aes(x = minimaBBP$N_Ag+0.0833, y = abs(minimaBBP$Spin),
                                  color=minimaBBP$Type), linetype = "solid",
            # "Minima E BBP"),
            #colour = "black",
            size = 1) + 
  #annotate("text", x = 4, y = 0.7, label = "BBP",colour="black")+
  xlab("Number of Ag Atoms") + ylab("Spin (eV)")+
  #labs(colour="Spin",size="Spin") +
  #labs(title = "Spin vs Silver Atoms", colour="Spin" ) + 
  theme(plot.title = element_text(size = rel(2), 
                                  face="bold", colour="black"),
        #        axis.line = element_line(size = 1),
        axis.text=element_text( size=rel(2),
                                face="bold", ) ) + 
  theme(axis.title = element_text(size =  rel(2),face="bold", colour="black") ) +
  #  scale_x_continuous(expand=c(0,0))+ scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(breaks=0:14,expand=c(0.01,0.01)) + 
  scale_y_continuous(limits=c(0.9,5.1),expand=c(0.0,0.01),
                     breaks=seq(1, 5, 1)) + 
  theme(legend.title = element_text(size=15,face="bold" ), 
        legend.text = element_text( size = 20, face = "bold"),
        plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y=element_text(vjust=0.025),
        axis.title.x=element_text(vjust=-.75)) +
  #guides(fill = guide_legend(override.aes = list(shape = 21)))+
  #
  scale_color_manual(values=c("black","green4","red4","blue4"), 
                     #name="Configuration",
                     name="",
                     labels=c("BBP","Cubo","Deca","Ico") #, 
                     #                    breaks = c("blue4","red4","green4","black") 
  ) +
  #,                    breaks = c("ICO","Deca","Cubo","BBP") ) +
  guides(colour = guide_legend(override.aes = list(shape=c(16,17,18,15)))
         ) +
  theme(legend.key = element_blank(),legend.position = c(.525, .775),legend.background=element_blank(),
        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
        panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 20, face = "bold"),
        panel.border = element_rect(colour = "black",size = 1) )
#
#  scale_shape_manual(values=c(15,18, 17, 16), name="Configuration-Shape",
#                     labels=c("Ico","Deca","Cubo","BBP") ,
#                     breaks = c("Ico","Deca","Cubo","BBP"))
#                    #breaks = c("ICO","Deca","Cubo","BBP"))
#,
#                     breaks=c("BBP"=18, 15, 17,16))
#,
#breaks = c("blue4","red4","green4","black"))
#scale_shape_manual(values=c(16, 17, 15,18), 
#                                            name="Configuration",
#                                            labels=c("BBP", "Cubo", "ICO", "Deca"))

#  "#009E73"  
ggsave(file = "SpinComparado.pdf", width = 10, height = 7 )
#dev.off()
#png(file = "EnergyExcessDeca.png", bg="transparent")
ggsave(file = "SpinComparado.png", bg="transparent",width = 10, height = 7)
