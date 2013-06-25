library(colorspace)
library(RColorBrewer)
library(Hmisc)
library(ggplot2)

graphOptions<-  ylab("Energy Excess Ico (eV)") +  
  theme(plot.title = element_text(size = rel(1), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:14)
  
  
  ylab("Gap (eV)") +
  labs(colour="Gap",size="Spin")+
  labs(title = "Gap vs Excess Energy",
       colour="Spin",size="R=exp(-(E-E_gs))",alpha="",shape="Seed")+
       #  theme(plot.title = element_text(size =rel(2), face="bold", colour="steelblue"),axis.line = element_line(size = 1))+
       #  theme(axis.title= element_text(size =rel(1),face="bold", colour="steelblue"))


lm_eqn = function(df){
  m = lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}


pal <- function(col, border = "light gray", ...)
   {
     n <- length(col)
     plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
            axes = FALSE, xlab = "", ylab = "", ...)
     rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
     }

#brewer.pal(8,"Dark2")
#"#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E" "#E6AB02" "#A6761D" "#666666"
myPal=rainbow(14, alpha = 1)
#p=
ggplot() + theme_bw() + 
#  geom_line(data = minimaDeca, aes(x = minimaDeca$EEFinal, y = minimaDeca$Gap, linetype = "Minima E Deca"),
#            colour = "darkred",size = 1) + 
#  geom_line(data = minimaDeca1st, aes(x = minimaDeca1st$EEFinal, y = minimaDeca1st$Gap, linetype = "Minima E1st Deca"),
#            colour = "darkred", size = 1) + scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgBBP, 
             aes(y =EEFinal, x = NAUAUBONDS,colour=as.factor(N_Ag),
                 #size=2*exp(-1.0*EEFinal),
                 #shape=N_Au),
                 shape=as.factor(N_Ag)),
             ,alpha = I(0.75),size=3 ,position = position_jitter(width = .15) ) +
  geom_path(data = minimaBBP, 
             aes(y =EEFinal, x = as.numeric(NAUAUBONDS) #,
                 #,colour= as.factor(N_Ag)
                 #
                 ##,
                 ##size=2*exp(-1.0*EEFinal),
                 ##shape=N_Au+1
                 ),alpha = 0.25,
            size=1) +
  stat_smooth(data = AuAgBBP,method=lm , 
              aes(y = EEFinal, x = NAUAUBONDS, 
                  #fill=as.factor(N_Ag), 
                  colour=as.factor(N_Ag), shape=as.factor(N_Ag)) , size=I(1.25), alpha = I(0.125)) +
  #scale_colour_gradientn(colours=brewer.pal(8,"Dark2"))
  scale_shape_manual(values=rep(21:24,4)) +
  scale_color_manual(values=rep(brewer.pal(4,"Set1"),4))
#scale_colour_brewer(palette="Dark2")
#scale_shape_identity() +

#  scale_colour_gradientn(colours=rainbow(7))
#  xlab("Excess Energy (eV)") + ylab("Gap (eV)")+labs(colour="Gap",size="Spin")+labs(title = "Gap vs Excess Energy",
#                                                                                    colour="Spin",size="R=exp(-(E-E_gs))",alpha="",shape="Seed")+
#  theme(plot.title = element_text(size =rel(2), face="bold", colour="steelblue"),axis.line = element_line(size = 1))+
#  theme(axis.title= element_text(size =rel(1),face="bold", colour="steelblue"))

ggsave(file = "NAUAU.pdf", width = 10, height = 7 )
#dev.off()
#png(file = "EnergyExcessDeca.png", bg="transparent")
#ggsave(file = "EEGapDeca.png", bg="transparent",width = 10, height = 7)

#####


ggplot() + theme_bw() + 
  #  geom_line(data = minimaDeca, aes(x = minimaDeca$EEFinal, y = minimaDeca$Gap, linetype = "Minima E Deca"),
  #            colour = "darkred",size = 1) + 
  #  geom_line(data = minimaDeca1st, aes(x = minimaDeca1st$EEFinal, y = minimaDeca1st$Gap, linetype = "Minima E1st Deca"),
  #            colour = "darkred", size = 1) + scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgBBP, 
             aes(y =EEFinal, x = NAGAUBONDS,colour=as.factor(N_Ag),
                 #size=2*exp(-1.0*EEFinal),
                 #shape=N_Au),
                 shape=as.factor(N_Ag)),
             ,alpha = I(0.75),size=3 ,position = position_jitter(width = .15) ) +
  geom_path(data = minimaBBP, 
            aes(y =EEFinal, x = as.numeric(NAGAUBONDS),
                #,colour= as.factor(N_Ag)
                alpha = 0.25
                ##,
                ##size=2*exp(-1.0*EEFinal),
                ##shape=N_Au+1
            ),
            size=1) +
  stat_smooth(data = AuAgBBP,method=lm , 
              aes(y = EEFinal, x = NAGAUBONDS, 
                  #fill=as.factor(N_Ag), 
                  colour=as.factor(N_Ag), shape=as.factor(N_Ag)) , size=I(1.25), alpha = I(0.125)) +
  #scale_colour_gradientn(colours=brewer.pal(8,"Dark2"))
  scale_shape_manual(values=rep(21:24,4)) +
  scale_color_manual(values=rep(brewer.pal(4,"Set1"),4))
#scale_colour_brewer(palette="Dark2")
#scale_shape_identity() +

#  scale_colour_gradientn(colours=rainbow(7))
#  xlab("Excess Energy (eV)") + ylab("Gap (eV)")+labs(colour="Gap",size="Spin")+labs(title = "Gap vs Excess Energy",
#                                                                                    colour="Spin",size="R=exp(-(E-E_gs))",alpha="",shape="Seed")+
#  theme(plot.title = element_text(size =rel(2), face="bold", colour="steelblue"),axis.line = element_line(size = 1))+
#  theme(axis.title= element_text(size =rel(1),face="bold", colour="steelblue"))

ggsave(file = "NAGAU.pdf", width = 10, height = 7 )
#dev.off()
#png(file = "EnergyExcessDeca.png", bg="transparent")
#ggsave(file = "EEGapDeca.png", bg="transparent",width = 10, height = 7)

#####


#########
ggplot() + theme_bw() + 
  #  geom_line(data = minimaDeca, aes(x = minimaDeca$EEFinal, y = minimaDeca$Gap, linetype = "Minima E Deca"),
  #            colour = "darkred",size = 1) + 
  #  geom_line(data = minimaDeca1st, aes(x = minimaDeca1st$EEFinal, y = minimaDeca1st$Gap, linetype = "Minima E1st Deca"),
  #            colour = "darkred", size = 1) + scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgBBP, 
             aes(y =EEFinal, x = NAGAGBONDS,colour=as.factor(N_Ag),
                 #size=2*exp(-1.0*EEFinal),
                 #shape=N_Au),
                 shape=as.factor(N_Ag)),
             ,alpha = I(0.75),size=3 ,position = position_jitter(width = .15) ) +
  geom_path(data = minimaBBP, 
            aes(y =EEFinal, x = as.numeric(NAGAGBONDS),
                #,colour= as.factor(N_Ag)
                alpha = 0.25
                ##,
                ##size=2*exp(-1.0*EEFinal),
                ##shape=N_Au+1
            ),
            size=1) +
  stat_smooth(data = AuAgBBP,method=lm , 
              aes(y = EEFinal, x = NAGAGBONDS, 
                  #fill=as.factor(N_Ag), 
                  colour=as.factor(N_Ag), shape=as.factor(N_Ag)) , size=I(1.25), alpha = I(0.125)) +
  #scale_colour_gradientn(colours=brewer.pal(8,"Dark2"))
  scale_shape_manual(values=rep(21:24,4)) +
  scale_color_manual(values=rep(brewer.pal(4,"Set1"),4))
#scale_colour_brewer(palette="Dark2")
#scale_shape_identity() +

#  scale_colour_gradientn(colours=rainbow(7))
#  xlab("Excess Energy (eV)") + ylab("Gap (eV)")+labs(colour="Gap",size="Spin")+labs(title = "Gap vs Excess Energy",
#                                                                                    colour="Spin",size="R=exp(-(E-E_gs))",alpha="",shape="Seed")+
#  theme(plot.title = element_text(size =rel(2), face="bold", colour="steelblue"),axis.line = element_line(size = 1))+
#  theme(axis.title= element_text(size =rel(1),face="bold", colour="steelblue"))

ggsave(file = "NAGAG.pdf", width = 10, height = 7 )





# ggplot() + theme_bw() + 
#   #  geom_line(data = minimaDeca, aes(x = minimaDeca$EEFinal, y = minimaDeca$Gap, linetype = "Minima E Deca"),
#   #            colour = "darkred",size = 1) + 
#   #  geom_line(data = minimaDeca1st, aes(x = minimaDeca1st$EEFinal, y = minimaDeca1st$Gap, linetype = "Minima E1st Deca"),
#   #            colour = "darkred", size = 1) + scale_linetype_discrete('Minima') + 
#   geom_point(data = AuAgBBP, 
#              aes(y =EEFinal, x = NAGAUBONDS+0.4*N_Ag,colour=N_Ag,
#                  #size=2*exp(-1.0*EEFinal),
#                  shape=N_Au),
#              alpha = I(0.5)
#              ) + scale_shape_identity() +
#   geom_line(data = minimaBBP, 
#             aes(y =EEFinal, x = NAGAUBONDS+0.4*N_Ag,colour=N_Ag
#                 #,
#                 #size=2*exp(-1.0*EEFinal),
#                 #shape=N_Au+1
#             ),
#             alpha = I(0.5),
#             size=1) +
#   scale_colour_gradientn(colours=brewer.pal(8,"Dark2"))
#   #scale_colour_gradientn(colours=rainbow(7))
# 
#   #scale_colour_gradientn(colours = terrain.colors(10))
#   #scale_colour_gradientn(colours=topo.colors(10))
#   #scale_fill_brewer(palette="Set1")
# #  xlab("Excess Energy (eV)") + ylab("Gap (eV)")+labs(colour="Gap",size="Spin")+labs(title = "Gap vs Excess Energy",
# #                                                                                    colour="Spin",size="R=exp(-(E-E_gs))",alpha="",shape="Seed")+
# #  theme(plot.title = element_text(size =rel(2), face="bold", colour="steelblue"),axis.line = element_line(size = 1))+
# #  theme(axis.title= element_text(size =rel(1),face="bold", colour="steelblue"))
# 
# ggsave(file = "NAGAU.pdf", width = 10, height = 7 )





ggplot() + theme_bw() + 
  #  geom_line(data = minimaDeca, aes(x = minimaDeca$EEFinal, y = minimaDeca$Gap, linetype = "Minima E Deca"),
  #            colour = "darkred",size = 1) + 
  #  geom_line(data = minimaDeca1st, aes(x = minimaDeca1st$EEFinal, y = minimaDeca1st$Gap, linetype = "Minima E1st Deca"),
  #            colour = "darkred", size = 1) + scale_linetype_discrete('Minima') + 
  geom_point(data = AuAgBBP, 
             aes(y =EEFinal, x = NAUAUBONDS , colour=N_Ag,
                 #size=2*exp(-1.0*EEFinal),
                 shape=(N_Au+1)
              ),alpha = I(0.75),size=3 ,position = position_jitter(width = .15) ) +
  scale_shape_identity() +
#  geom_line(data = minimaBBP, 
#            aes(y =EEFinal, x = NAUAUBONDS,colour=N_Ag
#                #,
#                #size=2*exp(-1.0*EEFinal),
#                #shape=N_Au+1
#            ),
#            alpha = I(0.5),
#            size=1) +
  geom_path(data = minimaBBP, 
            aes(y =EEFinal, x = NAUAUBONDS,colour=N_Ag
                #size=2*exp(-1.0*EEFinal),
                #shape=N_Au+1
            ),
            alpha = I(0.25),
            size=1.5) +
  stat_smooth(data = AuAgBBP,method=lm , 
              aes(y = EEFinal, x = NAUAUBONDS, fill=factor(N_Ag), 
                  colour=N_Ag, shape=factor(N_Ag)) , size=I(0.75), alpha = I(0.175)) +
  #scale_colour_gradientn(colours=rainbow_hcl(14, start = 00, end = 330))+
  scale_colour_gradientn(colours=myPal)+
    #colours=brewer.pal(8,"Dark2")) +
  #scale_fill_manual(values=rainbow_hcl(14, start = 00, end = 330))
  scale_fill_manual(values=myPal)

#scale_colour_gradientn(colours=rainbow(7))

#scale_colour_gradientn(colours = terrain.colors(10))
#scale_colour_gradientn(colours=topo.colors(10))
#scale_fill_brewer(palette="Set1")
#  xlab("Excess Energy (eV)") + ylab("Gap (eV)")+labs(colour="Gap",size="Spin")+
#labs(title = "Gap vs Excess Energy",
#                                                                                    colour="Spin",size="R=exp(-(E-E_gs))",alpha="",shape="Seed")+
#  theme(plot.title = element_text(size =rel(2), face="bold", colour="steelblue"),axis.line = element_line(size = 1))+
#  theme(axis.title= element_text(size =rel(1),face="bold", colour="steelblue"))

ggsave(file = "NAUAU.pdf", width = 10, height = 7 )


# ggplot() + theme_bw() +   
#   geom_path(data = minimaBBP, 
#             aes(y =EEFinal, x = NAUAUBONDS,colour=N_Ag
#                 #,
#                 #size=2*exp(-1.0*EEFinal),
#                 #shape=N_Au+1
#             ),
#             alpha = I(0.5),
#             size=1) +




ggplot() + theme_bw() + 
  geom_point(data = AuAgBBP, 
             aes(y =EEFinal, x = NAGAUBONDS , colour=N_Ag,
                 #size=2*exp(-1.0*EEFinal),
                 shape=(N_Au+1)
             ),alpha = I(0.75),size=3 ,position = position_jitter(width = .15) ) +
  scale_shape_identity() +
  geom_path(data = minimaBBP, 
            aes(y =EEFinal, x = NAGAUBONDS,colour=N_Ag
                #size=2*exp(-1.0*EEFinal),
                #shape=N_Au+1
            ),
            alpha = I(0.25),
            size=1.5) +
  stat_smooth(data = AuAgBBP,method=lm , 
              aes(y = EEFinal, x = NAGAUBONDS, fill=factor(N_Ag), 
                  colour=N_Ag, shape=factor(N_Ag)) , size=I(0.75), alpha = I(0.175)) +
  #scale_colour_gradientn(colours=rainbow_hcl(14, start = 00, end = 330))+
  scale_colour_gradientn(colours=myPal)+
  #colours=brewer.pal(8,"Dark2")) +
  #scale_fill_manual(values=rainbow_hcl(14, start = 00, end = 330))
  scale_fill_manual(values=myPal)


ggsave(file = "NAGAU.pdf", width = 10, height = 7 )

ggplot() + theme_bw() + 
  geom_point(data = AuAgBBP, 
             aes(y =E_Free, x = NAGAUBONDS , colour=N_Ag,
                 #size=2*exp(-1.0*EEFinal),
                 shape=(N_Au+1)
             ),alpha = I(0.75),size=3 #,position = position_jitter(width = .15) 
             ) +
  scale_shape_identity() +
  geom_path(data = minimaBBP, 
            aes(y =E_Free, x = NAGAUBONDS,colour=N_Ag
                #size=2*exp(-1.0*EEFinal),
                #shape=N_Au+1
            ),
            alpha = I(0.25),
            size=1.5) +
  stat_smooth(data = AuAgBBP,method=lm , 
              aes(y = E_Free, x = NAGAUBONDS, fill=factor(N_Ag), 
                  colour=N_Ag, shape=factor(N_Ag)) , size=I(0.75), alpha = I(0.175)) +
  #scale_colour_gradientn(colours=rainbow_hcl(14, start = 00, end = 330))+
  scale_colour_gradientn(colours=myPal)+
  #colours=brewer.pal(8,"Dark2")) +
  #scale_fill_manual(values=rainbow_hcl(14, start = 00, end = 330))
  scale_fill_manual(values=myPal)

ggsave(file = "NAGAU2.pdf", width = 10, height = 7 )





ggplot() + theme_bw() + 
  geom_point(data = AuAgBBP, 
             aes(y =E_Free, x = NAUAUBONDS , colour=N_Ag,
                 #size=2*exp(-1.0*EEFinal),
                 shape=(N_Au+1)
             ),alpha = I(0.75),size=3 #,position = position_jitter(width = .15) 
  ) +
  scale_shape_identity() +
  geom_path(data = minimaBBP, 
            aes(y =E_Free, x = NAUAUBONDS,colour=N_Ag
                #size=2*exp(-1.0*EEFinal),
                #shape=N_Au+1
            ),
            alpha = I(0.25),
            size=1.5) +
  stat_smooth(data = AuAgBBP,method=lm , 
              aes(y = E_Free, x = NAUAUBONDS, fill=factor(N_Ag), 
                  colour=N_Ag, shape=factor(N_Ag)) , size=I(0.75), alpha = I(0.175)) +
  #scale_colour_gradientn(colours=rainbow_hcl(14, start = 00, end = 330))+
  scale_colour_gradientn(colours=myPal)+
  #colours=brewer.pal(8,"Dark2")) +
  #scale_fill_manual(values=rainbow_hcl(14, start = 00, end = 330))
  scale_fill_manual(values=myPal)

ggsave(file = "NAUAU2.pdf", width = 10, height = 7 )





ggplot() + theme_bw() + 
  geom_point(data = AuAgBBP, 
             aes(y =E_Free, x = NAUAUBONDS , colour=N_Ag,
                 #size=2*exp(-1.0*EEFinal),
                 shape=(N_Au+1)
             ),alpha = I(0.75),size=3 #,position = position_jitter(width = .15) 
  ) +
  scale_shape_identity() +
  geom_path(data = minimaBBP, 
            aes(y =E_Free, x = NAUAUBONDS,colour=N_Ag
                #size=2*exp(-1.0*EEFinal),
                #shape=N_Au+1
            ),
            alpha = I(0.25),
            size=1.5) +
  stat_smooth(data = AuAgBBP,method=lm , 
              aes(y = E_Free, x = NAUAUBONDS, fill=factor(N_Ag), 
                  colour=N_Ag, shape=factor(N_Ag)) , size=I(0.75), alpha = I(0.175)) +
  #scale_colour_gradientn(colours=rainbow_hcl(14, start = 00, end = 330))+
  scale_colour_gradientn(colours=myPal)+
  #colours=brewer.pal(8,"Dark2")) +
  #scale_fill_manual(values=rainbow_hcl(14, start = 00, end = 330))
  scale_fill_manual(values=myPal)

ggsave(file = "NAUAU2.pdf", width = 10, height = 7 )




Ag10=subset(AuAgBBP, N_Ag =="10")
Ag09=subset(AuAgBBP, N_Ag =="9")
Ag08=subset(AuAgBBP, N_Ag =="8")
Ag07=subset(AuAgBBP, N_Ag =="7")
Ag06=subset(AuAgBBP, N_Ag =="6")
Ag05=subset(AuAgBBP, N_Ag =="5")
ggplot() + theme_bw() + 
  geom_point(data = Ag10, 
             aes(y =E_Free-min(E_Free), x = NAUAUBONDS +0.2, colour=N_Ag  #,
                 #size=2*exp(-1.0*EEFinal),
                 #  shape=(N_Au+1)
             ),alpha = I(0.75),size=3 #,position = position_jitter(width = .15) 
  ) + stat_smooth(data = Ag10,method=lm , 
                  aes(y = E_Free-min(E_Free), x = NAUAUBONDS, fill=factor(N_Ag), 
                      colour=N_Ag, shape=factor(N_Ag)) , size=I(0.75), alpha = I(0.175)) +
  geom_point(data = Ag09, 
             aes(y =E_Free-min(E_Free), x = NAUAUBONDS + 0.1, colour=N_Ag  #,
                 #size=2*exp(-1.0*EEFinal),
                 #  shape=(N_Au+1)
             ),alpha = I(0.75),size=3 #,position = position_jitter(width = .15) 
  ) + stat_smooth(data = Ag09,method=lm , 
                  aes(y = E_Free-min(E_Free), x = NAUAUBONDS, fill=factor(N_Ag), 
                      colour=N_Ag, shape=factor(N_Ag)) , size=I(0.75), alpha = I(0.175)) +
  geom_point(data = Ag08, 
             aes(y =E_Free-min(E_Free), x = NAUAUBONDS , colour=N_Ag  #,
                 #size=2*exp(-1.0*EEFinal),
                 #  shape=(N_Au+1)
             ),alpha = I(0.75),size=3 #,position = position_jitter(width = .15) 
  ) + stat_smooth(data = Ag08,method=lm , 
                  aes(y = E_Free-min(E_Free), x = NAUAUBONDS, fill=factor(N_Ag), 
                      colour=N_Ag, shape=factor(N_Ag)) , size=I(0.75), alpha = I(0.175)) +
  geom_point(data = Ag07, 
             aes(y =E_Free-min(E_Free), x = NAUAUBONDS -0.1, colour=N_Ag  #,
                 #size=2*exp(-1.0*EEFinal),
                 #  shape=(N_Au+1)
             ),alpha = I(0.75),size=3 #,position = position_jitter(width = .15) 
  ) + stat_smooth(data = Ag07,method=lm , 
                  aes(y = E_Free-min(E_Free), x = NAUAUBONDS, fill=factor(N_Ag), 
                      colour=N_Ag, shape=factor(N_Ag)) , size=I(0.75), alpha = I(0.175)) +
  geom_point(data = Ag06, 
             aes(y =E_Free-min(E_Free), x = NAUAUBONDS -0.2, colour=N_Ag  #,
                 #size=2*exp(-1.0*EEFinal),
                 #  shape=(N_Au+1)
             ),alpha = I(0.75),size=3 #,position = position_jitter(width = .15) 
  ) + stat_smooth(data = Ag06,method=lm , 
                  aes(y = E_Free-min(E_Free), x = NAUAUBONDS, fill=factor(N_Ag), 
                      colour=N_Ag, shape=factor(N_Ag)) , size=I(0.75), alpha = I(0.175)) +
  geom_point(data = Ag05, 
             aes(y =E_Free-min(E_Free), x = NAUAUBONDS -0.2, colour=N_Ag  #,
                 #size=2*exp(-1.0*EEFinal),
                 #  shape=(N_Au+1)
             ),alpha = I(0.75),size=3 #,position = position_jitter(width = .15) 
  ) + stat_smooth(data = Ag05,method=lm , 
                  aes(y = E_Free-min(E_Free), x = NAUAUBONDS, fill=factor(N_Ag), 
                      colour=N_Ag, shape=factor(N_Ag)) , size=I(0.75), alpha = I(0.175)) +
  scale_colour_gradientn(colours=myPal)




########################################################################
#### models NAUAUBONDS

modelsNAUAU <- vector(mode="list", length=13)

for (i in 1:13){
  modelsNAUAU[[i]]=lm(EEFinal ~ NAUAUBONDS, subset(AuAgBBP, N_Ag ==i))
  #print(coef(modelsNAUAU[[i]])[2])
}



coeffNAUAU <- 1:13 # vector(mode="numeric", 14)
coeffNAUAU=rep(NA,13)
errcoeffNAUAU <- 1:13
errcoeffNAUAU=rep(NA,13)
#for (i in 1:13){
for (i in 1:13){  
  #print(coef(modelsNAUAU[[i]])[2])
  coeffNAUAU[i]=modelsNAUAU[[i]]$coefficients[2][[1]]
}
for (i in 2:11){
  errcoeffNAUAU[i]=coef(summary(modelsNAUAU[[i]]))[2, "Std. Error"]
}


coeffDFNAUAU=as.data.frame(matrix( c( 1:length(coeffNAUAU), 
                                      coeffNAUAU , errcoeffNAUAU), ncol=3 ))

colnames(coeffDFNAUAU)=c("i","coeffNAUAU","errcoeffNAUAU")
coeffDFNAUAU$i=as.double(as.vector(coeffDFNAUAU$i))

coeffDFNAUAU$coeffNAUAU=as.double(as.vector(coeffDFNAUAU$coeffNAUAU))
coeffDFNAUAU$errcoeffNAUAU=as.double(as.vector(coeffDFNAUAU$errcoeffNAUAU))

coeffDFNAUAU=coeffDFNAUAU[complete.cases(coeffDFNAUAU),]

#errcoeffNAUAU[is.na(errcoeffNAUAU)] <- 0
ggplot(coeffDFNAUAU, aes(x=i, y=coeffNAUAU, colour=factor(i),group = 1,
                         shape=factor(i),fill=factor(i) ) ) + 
  geom_errorbar(aes(ymin=coeffNAUAU-errcoeffNAUAU, ymax=coeffNAUAU+errcoeffNAUAU), width=.15,
                size=1) +
  geom_line(size=1) +
  geom_point(size=5) + 
  stat_smooth(method=lm, fullrange = TRUE, size=1.25, alpha = 0.35, linetype = "3313") +
  scale_shape_manual(values=rep(21:24,4)[3:16]) +
  scale_color_manual(values=rep(brewer.pal(4,"Set1"),4)[3:16],guide = 'none') +
  scale_fill_manual(values=rep(brewer.pal(4,"Set1"),4)[3:16]) +
  xlab("Number of Ag - Ag Bonds ") + ylab("EE (eV)") +
  labs(fill="Number of Ag atoms",shape="Number of Ag atoms") +
  labs(title = "Slope vs Number of Bonds" #, 
       # colour="Spin",
       #size="R=exp(-(E-E_gs))",alpha="",shape="Seed"
  ) +
  scale_x_continuous(breaks=0:14) + #+ geom_hline(yintercept=0,size=1)
  ggsave(file = "SlopeAUAU.pdf", width = 10, height = 7 ) 


plot(coeffNAUAU)

xtmp=1:length(coeffNAUAU)

xtmp=xtmp*1.0

errbar( xtmp, coeffNAUAU, coeffNAUAU+errcoeffNAUAU, coeffNAUAU-errcoeffNAUAU )

modelSlopeNAUAU=lm(coeffNAUAU ~ xtmp)
z <- nls(coeffNAUAU ~ a * xtmp + b, start=list(a=1, b=1))
summary(z)

summary(modelSlopeNAUAU)
abline(modelSlopeNAUAU,col='red')

lines(predict(z),col='blue')

#ggsave(file = "SlopeAUAU.pdf", width = 10, height = 7 )

dev.off()


####################################################################





########################################################################
#### models NAGAUBONDS

modelsNAGAU <- vector(mode="list", length=13)

for (i in 1:13){
  modelsNAGAU[[i]]=lm(EEFinal ~ NAGAUBONDS, subset(AuAgBBP, N_Ag ==i))
  #print(coef(modelsNAGAU[[i]])[2])
}

coeffNAGAU <- 1:13 # vector(mode="numeric", 14)
coeffNAGAU=rep(NA,13)
errcoeffNAGAU <- 1:13
errcoeffNAGAU=rep(NA,13)
#for (i in 1:13){
for (i in 1:13){  
  #print(coef(modelsNAGAU[[i]])[2])
  coeffNAGAU[i]=modelsNAGAU[[i]]$coefficients[2][[1]]
}
for (i in 2:12){
  errcoeffNAGAU[i]=coef(summary(modelsNAGAU[[i]]))[2, "Std. Error"]
}
# plot(coeffNAGAU)

coeffDFNAGAU=as.data.frame(matrix( c( 1:length(coeffNAGAU), 
                                      coeffNAGAU , errcoeffNAGAU), ncol=3 ))

colnames(coeffDFNAGAU)=c("i","coeffNAGAU","errcoeffNAGAU")
coeffDFNAGAU$i=as.double(as.vector(coeffDFNAGAU$i))

coeffDFNAGAU$coeffNAGAU=as.double(as.vector(coeffDFNAGAU$coeffNAGAU))
coeffDFNAGAU$errcoeffNAGAU=as.double(as.vector(coeffDFNAGAU$errcoeffNAGAU))

coeffDFNAGAU=coeffDFNAGAU[complete.cases(coeffDFNAGAU),]

#errcoeffNAGAU[is.na(errcoeffNAGAU)] <- 0
ggplot(coeffDFNAGAU, aes(x=i, y=coeffNAGAU, colour=factor(i),group = 1,
                         shape=factor(i),fill=factor(i) ) ) + 
  geom_errorbar(aes(ymin=coeffNAGAU-errcoeffNAGAU, ymax=coeffNAGAU+errcoeffNAGAU), width=.15,
                size=1) +
  geom_line(size=1) +
  geom_point(size=5) + 
  stat_smooth(method=lm, fullrange = TRUE, size=1.25, alpha = 0.35, linetype = "3313") +
  scale_shape_manual(values=rep(21:24,4)[3:16]) +
  scale_color_manual(values=rep(brewer.pal(4,"Set1"),4)[3:16],guide = 'none') +
  scale_fill_manual(values=rep(brewer.pal(4,"Set1"),4)[3:16]) +
  xlab("Number of Ag - Ag Bonds ") + ylab("EE (eV)") +
  labs(fill="Number of Ag atoms",shape="Number of Ag atoms") +
  labs(title = "Slope vs Number of Bonds" #, 
       # colour="Spin",
       #size="R=exp(-(E-E_gs))",alpha="",shape="Seed"
  ) +
  scale_x_continuous(breaks=0:14) + #+ geom_hline(yintercept=0,size=1)
  ggsave(file = "SlopeAGAU.pdf", width = 10, height = 7 ) 


plot(coeffNAGAU)


xtmp=1:length(coeffNAGAU)

xtmp=xtmp*1.0

errbar( xtmp, coeffNAGAU, coeffNAGAU+errcoeffNAGAU, coeffNAGAU-errcoeffNAGAU )

modelSlopeNAGAU=lm(coeffNAGAU ~ xtmp)
z <- nls(coeffNAGAU ~ a * xtmp + b, start=list(a=1, b=1))
summary(z)

summary(modelSlopeNAGAU)
abline(modelSlopeNAGAU,col='red')

lines(predict(z),col='blue')

ggsave(file = "SlopeAGAU.pdf", width = 10, height = 7 )
####################################################################



########################################################################
#### models NAGAGBONDS

modelsNAGAG <- vector(mode="list", length=13)

for (i in 1:13){
  modelsNAGAG[[i]]=lm(EEFinal ~ NAGAGBONDS, subset(AuAgBBP, N_Ag ==i))
  #print(coef(modelsNAGAG[[i]])[2])
}

#coeffNAGAG <- 1:13 # vector(mode="numeric", 14)
coeffNAGAG=rep(NA,13)
#errcoeffNAGAG <- 1:13
errcoeffNAGAG=rep(NA,13)
#for (i in 1:13){
for (i in 1:13){  
  #print(coef(modelsNAGAG[[i]])[2])
  coeffNAGAG[i]=modelsNAGAG[[i]]$coefficients[2][[1]]
}
for (i in 2:11){  
  #print(coef(modelsNAGAG[[i]])[2])
  errcoeffNAGAG[i]=coef(summary(modelsNAGAG[[i]]))[2, "Std. Error"]
}

coeffDFNAGAG=as.data.frame(matrix( c( 1:length(coeffNAGAG), 
                                      coeffNAGAG , errcoeffNAGAG), ncol=3 ))

colnames(coeffDFNAGAG)=c("i","coeffNAGAG","errcoeffNAGAG")
coeffDFNAGAG$i=as.double(as.vector(coeffDFNAGAG$i))

coeffDFNAGAG$x=coeffDFNAGAG$i
coeffDFNAGAG$y=coeffDFNAGAG$coeffNAGAG

coeffDFNAGAG$coeffNAGAG=as.double(as.vector(coeffDFNAGAG$coeffNAGAG))
coeffDFNAGAG$errcoeffNAGAG=as.double(as.vector(coeffDFNAGAG$errcoeffNAGAG))

coeffDFNAGAG=coeffDFNAGAG[complete.cases(coeffDFNAGAG),]

#errcoeffNAGAG[is.na(errcoeffNAGAG)] <- 0
ggplot(coeffDFNAGAG, aes(x=i, y=coeffNAGAG, colour=factor(i),group = 1,
                         shape=factor(i),fill=factor(i) ) ) + 
  geom_errorbar(aes(ymin=coeffNAGAG-errcoeffNAGAG, ymax=coeffNAGAG+errcoeffNAGAG), width=.15,
                size=1) +
  geom_line(size=1) +
  geom_point(size=5) + 
  stat_smooth(method=lm, fullrange = TRUE, size=1.25, alpha = 0.35, linetype = "3313") +
  scale_shape_manual(values=rep(21:24,4)[3:16]) +
  scale_color_manual(values=rep(brewer.pal(4,"Set1"),4)[3:16],guide = 'none') +
  scale_fill_manual(values=rep(brewer.pal(4,"Set1"),4)[3:16]) +
  xlab("Number of Ag Atoms ") + ylab("EE (eV)") +
  labs(fill="Number of Ag atoms",shape="Number of Ag atoms") +
  labs(title = "Slope vs Number of Ag atoms" #, 
       # colour="Spin",
       #size="R=exp(-(E-E_gs))",alpha="",shape="Seed"
       ) + 
  scale_x_continuous(breaks=0:14) + #+ geom_hline(yintercept=0,size=1)
  geom_text(aes(x = 7, y = 0.040, label = lm_eqn(coeffDFNAGAG)), parse = TRUE,colour="steelblue")
ggsave(file = "SlopeAGAG.pdf", width = 10, height = 7 ) 


#plot( coeffDFNAGAG$i, coeffDFNAGAG$coeffNAGAG)

#errcoeffNAGAGminus = as.numeric(coeffNAGAG) - as.numeric(errcoeffNAGAG)
#errcoeffNAGAGplus = as.numeric(coeffNAGAG) + as.numeric(errcoeffNAGAG)

#xtmp=1:length(coeffNAGAG)
#xtmp=xtmp*1.0

#errbar( coeffDFNAGAG$i, coeffDFNAGAG$coeffNAGAG , 
#        coeffDFNAGAG$coeffNAGAG + coeffDFNAGAG$errcoeffNAGAG ,
#        coeffDFNAGAG$coeffNAGAG - coeffDFNAGAG$errcoeffNAGAG )


modelSlopeNAGAG=lm( coeffDFNAGAG$coeffNAGAG ~ coeffDFNAGAG$i )
summary(modelSlopeNAGAG)
abline(modelSlopeNAGAG,col='red')


z <- nls(coeffNAGAG ~ a*i+b , start=list(a=1, b=1), data=coeffDFNAGAG )
summary(z)
lines(coeffDFNAGAG$i,predict(z),col='blue')


####################################################################



#modelsNAUAGAU <- vector(mode="list", length=13)
#for (i in 1:13){
#  print(i)
#  modelsNAUAGAU[[i]]=lm(EEFinal ~ NAUAUBONDS + NAGAGBONDS + NAGAUBONDS, subset(AuAgBBP, N_Ag ==i))
#  print( summary(modelsNAUAGAU[[i]]) )
  #print(coef(modelsNAUAU[[i]])[2])
#}



##############################################################

ggplot() + theme_bw() + 
  geom_path(data = minimaIco, 
            aes(y =EEFinal, x = as.numeric(AUAURATIO) #,
            ),alpha = 0.25,
            size=1) +
  geom_point(data = AuAgIco, 
             aes(y =EEFinal, x = AUAURATIO,colour=as.factor(N_Au)  ,
                 shape=as.factor(N_Au)
                 ),
             alpha = I(0.75),size=3 ,position = position_jitter(width = .0000015) ) +
  stat_smooth(data = subset(AuAgIco, 1 < N_Ag & N_Ag < 13),
              method=lm, 
              aes(y = EEFinal, x = AUAURATIO  ,
                  colour=as.factor(N_Au) ,
                  shape=as.factor(N_Au)
                  ) , size=I(1.25), alpha = I(0.125)) +
  scale_shape_manual(values=rep(21:24,4)[1:16]) +
  scale_color_manual(values=rep(brewer.pal(4,"Set1"),4)[1:16])
ggsave(file = "NAUAU-Ico.pdf", width = 10, height = 7 )

ggplot() + theme_bw() + 
  geom_path(data = minimaIco, 
            aes(y =EEFinal, x = as.numeric(AGAURATIO) #,
            ),alpha = 0.25,
            size=1) +
  geom_point(data = AuAgIco, 
             aes(y =EEFinal, x = AGAURATIO,colour=as.factor(N_Au)  ,
                 shape=as.factor(N_Au)
             ),
             alpha = I(0.75),size=3 ,position = position_jitter(width = .0000015) ) +
  stat_smooth(data = subset(AuAgIco, 1 < N_Ag & N_Ag < 13),
              method=lm, 
              aes(y = EEFinal, x = AGAURATIO  ,
                  colour=as.factor(N_Au) ,
                  shape=as.factor(N_Au)
              ) , size=I(1.25), alpha = I(0.125)) +
  scale_shape_manual(values=rep(21:24,4)[1:16]) +
  scale_color_manual(values=rep(brewer.pal(4,"Set1"),4)[1:16])
ggsave(file = "NAGAU-Ico.pdf", width = 10, height = 7 )

ggplot() + theme_bw() + 
  geom_path(data = minimaIco, 
            aes(y =EEFinal, x = as.numeric(AGAGRATIO) #,
            ),alpha = 0.25,
            size=1) +
  geom_point(data = AuAgIco, 
             aes(y =EEFinal, x = AGAGRATIO,colour=as.factor(N_Au)  ,
                 shape=as.factor(N_Au)
             ),
             alpha = I(0.75),size=3 ,position = position_jitter(width = .0000015) ) +
  stat_smooth(data = subset(AuAgIco, 0 < N_Ag & N_Ag < 13),
              method=lm, 
              aes(y = EEFinal, x = AGAGRATIO  ,
                  colour=as.factor(N_Au) ,
                  shape=as.factor(N_Au)
              ) , size=I(1.25), alpha = I(0.125)) +
  scale_shape_manual(values=rep(21:24,4)[1:16]) +
  scale_color_manual(values=rep(brewer.pal(4,"Set1"),4)[1:16])
ggsave(file = "NAGAG-Ico.pdf", width = 10, height = 7 )

##############################################################
# Deca
ggplot() + theme_bw() + 
  geom_path(data = minimaDeca, 
            aes(y =EEFinal, x = as.numeric(AUAURATIO) #,
            ),alpha = 0.25,
            size=1) +
  geom_point(data = AuAgDeca, 
             aes(y =EEFinal, x = AUAURATIO,colour=as.factor(N_Au)  ,
                 shape=as.factor(N_Au)
             ),
             alpha = I(0.75),size=3 ,position = position_jitter(width = .0000015) ) +
  stat_smooth(data = subset(AuAgDeca, 1 < N_Ag & N_Ag < 13),
              method=lm, 
              aes(y = EEFinal, x = AUAURATIO  ,
                  colour=as.factor(N_Au) ,
                  shape=as.factor(N_Au)
              ) , size=I(1.25), alpha = I(0.125)) +
  scale_shape_manual(values=rep(21:24,4)[1:16]) +
  scale_color_manual(values=rep(brewer.pal(4,"Set1"),4)[1:16])
ggsave(file = "NAUAU-Deca.pdf", width = 10, height = 7 )

ggplot() + theme_bw() + 
  geom_path(data = minimaDeca, 
            aes(y =EEFinal, x = as.numeric(AGAURATIO) #,
            ),alpha = 0.25,
            size=1) +
  geom_point(data = AuAgDeca, 
             aes(y =EEFinal, x = AGAURATIO,colour=as.factor(N_Au)  ,
                 shape=as.factor(N_Au)
             ),
             alpha = I(0.75),size=3 ,position = position_jitter(width = .0000015) ) +
  stat_smooth(data = subset(AuAgDeca, 1 < N_Ag & N_Ag < 13),
              method=lm, 
              aes(y = EEFinal, x = AGAURATIO  ,
                  colour=as.factor(N_Au) ,
                  shape=as.factor(N_Au)
              ) , size=I(1.25), alpha = I(0.125)) +
  scale_shape_manual(values=rep(21:24,4)[1:16]) +
  scale_color_manual(values=rep(brewer.pal(4,"Set1"),4)[1:16])
ggsave(file = "NAGAU-Deca.pdf", width = 10, height = 7 )

####

ggplot() + theme_bw() + 
  geom_path(data = minimaDeca, 
            aes(y =EEFinal, x = as.numeric(AGAGRATIO) #,
            ),alpha = 0.25,
            size=1) +
  geom_point(data = AuAgDeca, 
             aes(y =EEFinal, x = AGAGRATIO, colour=as.factor(N_Ag)  ,
                 shape=as.factor(N_Ag)      ),    width = I(3) ,
             alpha = I(0.75),size=3 ,position = position_jitter(width = .0000015) ) +
  stat_smooth(data = subset(AuAgDeca, 1 < N_Ag & N_Ag < 12),
              method=lm, 
              aes(y = EEFinal, x = AGAGRATIO  ,
                  colour=as.factor(N_Ag) # ,
                  # shape=as.factor(N_Au)
              ) , size=I(1.25), alpha = I(0.125), show_guide=FALSE) +
  scale_shape_manual(values=rep(21:24,4)[1:16]) +
  scale_color_manual(values=rep(brewer.pal(4,"Set1"),4)[1:16]) +
  xlab("Ratio of AgAg Bonds") +
  ylab("Excess (eV)") +
  ylab("Energy Excess Deca (eV)") +  
  theme(plot.title = element_text(size = rel(1), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:10/10) +
  scale_y_continuous(
#    breaks=seq(      round(min(minimaDeca$EEFinal),digits=3), 
#                     round(max(minimaDeca$EEFinal),digits=3),
#                     length.out=10) ,
    limits=c(round(min(minimaDeca$EEFinal),digits=2),
             round(max(minimaDeca$EEFinal),digits=2) )
     ) +
labs(title = "Energy Excess vs Silver Atoms" ,
     colour="Case", shape="Case", linewidth="Case"
     # size="Spin", 
     # alpha="",
      # ,
     # linetype="Case" 
     ) + # guides( colour = FALSE ) +
####  guides(shape = guide_legend(override.aes = list(linewidth = 0) ) ) +
####  guides(shape = guide_legend(override.aes = list(width = I(3) ) ) )  +
  guides(colour=guide_legend(override.aes = list(size = 4), linewidth = I(3))) +
  ggsave(file = "NAGAG-Deca.pdf", width = 10, height = 7 )

###############
#BBP
#AGAG
ggplot() + theme_bw() + 
  geom_path(data = minimaDeca, 
            aes(y =EEFinal, x = as.numeric(AGAGRATIO) #,
            ),alpha = 0.25,
            size=1) +
  geom_point(data = AuAgDeca, 
             aes(y =EEFinal, x = AGAGRATIO, colour=as.factor(N_Ag)  ,
                 shape=as.factor(N_Ag)      ),    width = I(3) ,
             alpha = I(0.75),size=3 ,position = position_jitter(width = .0000015) ) +
  stat_smooth(data = subset(AuAgDeca, 1 < N_Ag & N_Ag < 12),
              method=lm, 
              aes(y = EEFinal, x = AGAGRATIO  ,
                  colour=as.factor(N_Ag) # ,
                  # shape=as.factor(N_Au)
              ) , size=I(1.25), alpha = I(0.125), show_guide=FALSE) +
  scale_shape_manual(values=rep(21:24,4)[1:16]) +
  scale_color_manual(values=rep(brewer.pal(4,"Set1"),4)[1:16]) +
  xlab("Ratio of AgAg Bonds") +
  ylab("Excess (eV)") +
  ylab("Energy Excess Deca (eV)") +  
  theme(plot.title = element_text(size = rel(1), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:10/10) +
  scale_y_continuous(
    #    breaks=seq(      round(min(minimaDeca$EEFinal),digits=3), 
    #                     round(max(minimaDeca$EEFinal),digits=3),
    #                     length.out=10) ,
    limits=c(round(min(minimaDeca$EEFinal),digits=2),
             round(max(minimaDeca$EEFinal),digits=2) )
  ) +
  labs(title = "Energy Excess vs Silver Atoms" ,
       colour="Case", shape="Case", linewidth="Case"
       # size="Spin", 
       # alpha="",
       # ,
       # linetype="Case" 
  ) + # guides( colour = FALSE ) +
  ####  guides(shape = guide_legend(override.aes = list(linewidth = 0) ) ) +
  ####  guides(shape = guide_legend(override.aes = list(width = I(3) ) ) )  +
  guides(colour=guide_legend(override.aes = list(size = 4), linewidth = I(3))) +
  ggsave(file = "NAGAG-Deca.pdf", width = 10, height = 7 )

