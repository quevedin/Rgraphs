library("ggplot2")
require(stats)

SEEDS=c("BBP","Deca","Cubo","Ico")
paste("AuAg",SEEDS,sep = "")

require(gridExtra)
# grid.table(head(iris))




eq <- function(x,b0,b1) {
  y = 1 - x
  b0*x*log(x +!x )+b1* y *log(y + !y )
}
XX=seq(0,1,by=0.01)
AA=3.0
tmp <- data.frame(x=XX, y=eq(XX,AA,AA-1.0))


# p <- qplot(x, y, data=tmp, xlab="X-axis", ylab="Y-axis")
# c <- stat_function(fun=eq,arg = list(a = 3))
# print(p + c)
 

# ggplot(aes(x,y), data=tmp) +
#   stat_function(fun=eq,arg = list(a = c(0.001,0.01,0.1,0.25,0.5,1.0)))

rm(min_Deca_D5h_min2,min_Deca_D5h,min_BBP_ALL_min2,min_BBP_C2v,min_BBP_C2v_min2)


min_Deca_D5h=subset(AuAgDeca , (Schoenflies.symbol=="D5h") )

# min_Deca_D5h_b=min_Deca_D5h[order(min_Deca_D5h$N_Ag,min_Deca_D5h$E_Free),]

#######min_Deca_D5hN_Ag<-unique(min_Deca_D5h$N_Ag)

# min_Deca_D5h_min = min_Deca_D5h_b[!duplicated(min_Deca_D5h_b$N_Ag),]

# one liner
min_Deca_D5h_min2 = min_Deca_D5h[order(min_Deca_D5h$N_Ag,min_Deca_D5h$EEFinal),]
min_Deca_D5h_min2 = min_Deca_D5h_min2[!duplicated(min_Deca_D5h_min2$N_Ag),]
#min_Deca_D5h_min2 = min_Deca_D5h[!duplicated( min_Deca_D5h[order(min_Deca_D5h$N_Ag,min_Deca_D5h$E_Free),]$N_Ag ),]

min_Deca_ALL_min2=AuAgDeca[order(AuAgDeca$N_Ag,AuAgDeca$EEFinal),]
min_Deca_ALL_min2 = min_Deca_ALL_min2[!duplicated(min_Deca_ALL_min2$N_Ag ),]

#min_Deca_D5h_min==min_Deca_D5h_min2 #always true

myEq <- function(x,b0,b1) {
  x=x/13
    b0*x*log(x +!x ) + b1 * (1-x) * log( (1-x)+!(1-x) )
}

fm3DNase1 <- nls(EEFinal ~ myEq(N_Ag,b0,b1) ,
                 data = min_Deca_D5h_min2,
                 start = list(b0 = 3, b1 = 1) , trace = T)
summary(fm3DNase1)

fm3DNase1ALL <- nls(EEFinal ~ myEq(N_Ag,b0,b1) ,
                 data = min_Deca_ALL_min2,
                 start = list(b0 = 3, b1 = 1) , trace = T)
summary(fm3DNase1ALL)



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
             colour="black",alpha = I(0.5),size=1,shape=0,
             width = I(3),lwd=3) +
  geom_line(data=min_Deca_ALL_min2,aes(x = N_Ag, y = EEFinal),linetype="dashed") + 
  stat_function(fun=myEq, data=min_Deca_D5h_min2, 
                arg = list(
                           b0 = coef(summary(fm3DNase1))[1,1], 
                           b1 = coef(summary(fm3DNase1))[2,1] ) ) +
  stat_function(fun=myEq, data=min_Deca_ALL_min2, 
                arg = list(
                  b0 = coef(summary(fm3DNase1ALL))[1,1], 
                  b1 = coef(summary(fm3DNase1ALL))[2,1] ) ) 


#### BBP ##########################################################

min_BBP_C2v=subset(AuAgBBP , (Schoenflies.symbol=="C2v") )

# min_BBP_C2v_b=min_BBP_C2v[order(min_BBP_C2v$N_Ag,min_BBP_C2v$E_Free),]

#######min_BBP_C2vN_Ag<-unique(min_BBP_C2v$N_Ag)

# min_BBP_C2v_min = min_BBP_C2v_b[!duplicated(min_BBP_C2v_b$N_Ag),]

# one liner
min_BBP_C2v_min2 = min_BBP_C2v[order(min_BBP_C2v$N_Ag,min_BBP_C2v$EEFinal),]
min_BBP_C2v_min2 = min_BBP_C2v_min2[!duplicated(min_BBP_C2v_min2$N_Ag),]
min_BBP_C2v_min3 = min_BBP_C2v[!duplicated( min_BBP_C2v[order(min_BBP_C2v$N_Ag,min_BBP_C2v$E_Free),]$N_Ag ),]

min_BBP_ALL_min2=AuAgBBP[order(AuAgBBP$N_Ag,AuAgBBP$EEFinal),]
min_BBP_ALL_min2 = min_BBP_ALL_min2[!duplicated(min_BBP_ALL_min2$N_Ag ),]

#min_BBP_C2v_min==min_BBP_C2v_min2 #always true

myEq <- function(x,b0,b1) {
  x=x/13
  b0*x*log(x +!x ) + b1 * (1-x) * log( (1-x)+!(1-x) )
}

fm3DNase1 <- nls(EEFinal ~ myEq(N_Ag,b0,b1) ,
                 data = min_BBP_C2v_min2, 
                 weights = exp(-2.0*min_BBP_C2v_min2$EEFinal),
                 start = list(b0 = 3, b1 = 1) , trace = T)
fm3DNase1 <- nls(EEFinal ~ myEq(N_Ag,b0,b1) ,
                 data = min_BBP_C2v_min2, 
                 weights = exp( -5.0* ( min_BBP_C2v_min2$EEFinal - predict(fm3DNase1 ) ) ),
                 start = coef(fm3DNase1), trace = T)
summary(fm3DNase1)

fm3DNase1ALL <- nls(EEFinal ~ myEq(N_Ag,b0,b1) ,
                    data = min_BBP_ALL_min2,
                    weights = exp(-2.0*min_BBP_ALL_min2$EEFinal),
                    start = list(b0 = 3, b1 = 1) , trace = T)
fm3DNase1ALL <- nls(EEFinal ~ myEq(N_Ag,b0,b1) ,
                    data = min_BBP_ALL_min2,
                    weights = exp( -5.0* ( min_BBP_ALL_min2$EEFinal - predict(fm3DNase1ALL ) ) ),
                    start = coef(fm3DNase1ALL) , trace = T)
summary(fm3DNase1ALL)



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
             aes(x = N_Ag, y = EEFinal), #colour=Gap, # size = abs(Spin),
             colour="black",alpha = I(0.5),size=1,shape=0,
             width = I(3),lwd=3) +
  geom_line(data=min_BBP_ALL_min2,aes(x = N_Ag, y = EEFinal),linetype="dashed") + 
  geom_line(data=min_BBP_C2v_min2,aes(x = N_Ag, y = EEFinal),linetype="dashed") + 
  stat_function(fun=myEq, data=min_BBP_C2v_min2, 
                arg = list(
                  b0 = coef(summary(fm3DNase1))[1,1], 
                  b1 = coef(summary(fm3DNase1))[2,1] ) ) +
  stat_function(fun=myEq, data=min_BBP_ALL_min2, 
                arg = list(
                  b0 = coef(summary(fm3DNase1ALL))[1,1], 
                  b1 = coef(summary(fm3DNase1ALL))[2,1] ) ) 


### End BBP ###
# fm3DNase1 <- nls(EEFinal ~ myEq(N_Ag,b0,b1) ,
#                  data = min_BBP_C2v_min2, 
#                  weights = exp(-2.0*min_BBP_C2v_min2$EEFinal),
#                  start = list(b0 = 3, b1 = 1) , trace = T)
fm3DNase1 <- nls(EEFinal ~ myEq(N_Ag,b0,b1) ,
                 data = min_BBP_C2v_min2, 
                 weights = exp( -10.0* ( min_BBP_C2v_min2$EEFinal - predict(fm3DNase1 ) ) ),
                 start = 
                   list(b0=2.0, b1=-0.0614932),
#                   coef(fm3DNase1), 
                 trace = T)

summary(fm3DNase1)

( min_BBP_C2v_min2$EEFinal - predict(fm3DNase1 ) )

ggplot() + theme_bw() + 
  geom_point(data = subset(AuAgBBP,(Schoenflies.symbol=="C2v")),
             # AuAgBBP
             aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
                 colour=Schoenflies.symbol),alpha = I(1.0),size=5,shape=23,
             width = I(3),lwd=3) +
  stat_function(fun=myEq, data=min_BBP_C2v_min2, 
                arg = list(
                  #                b0 = 1.8, 
                  #                b1 = 1.7 
                  b0 = coef(summary(fm3DNase1))[1,1], 
                  b1 = coef(summary(fm3DNase1))[2,1] 
                ) )   +
  stat_function(fun=myEq, data=min_BBP_C2v_min2, 
                arg = list(
                  b0 = 12.9, 
                  b1 = -4.5 
#                   b0 = coef(summary(fm3DNase1))[1,1], 
#                   b1 = coef(summary(fm3DNase1))[2,1] 
                ) )



  
#   
# 
# 
# 
# ggplot() + theme_bw() + 
#   geom_point(data = subset(AuAgDeca,(Schoenflies.symbol=="NO")),
#              # AuAgDeca
#              aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
#                  colour=Schoenflies.symbol),width = I(3),lwd=3,alpha = I(0.75),size=2.5,shape=21) + 
#   geom_point(data = subset(AuAgDeca,(Schoenflies.symbol=="D5h")),
#              # AuAgDeca
#              aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
#                  colour=Schoenflies.symbol),alpha = I(1.0),size=5,shape=23,
#              width = I(3),lwd=3) +
#   geom_point(data = subset(AuAgDeca,(Schoenflies.symbol!="NO" & Schoenflies.symbol!="D5h")),
#              # AuAgDeca
#              aes(x = N_Ag, y = EEFinal), #colour=Gap, # size = abs(Spin),
#              colour="black",alpha = I(0.85),size=1,shape=0,
#              width = I(3),lwd=3) +       
#   #scale_color_manual(values=brewer.pal(12,"Paired")) +
#   scale_color_manual(values=brewer.pal(8,"Accent")) +
#   #scale_size_continuous()+
#   geom_line(data = subset(AuAgDeca,(Schoenflies.symbol=="D5h")),
#             aes(x = N_Ag, y = EEFinal),
#             #linetype="3313",
#             # %sprintf("%s_%s",minima$Type,minima$First) ) , 
#             alpha=I(0.5) , size=1 ) + 
#   geom_line(data = minimaDeca, aes(x = N_Ag, y =EEFinal),
#             linetype="dashed",
#             # %sprintf("%s_%s",minima$Type,minima$First) ) , 
#             alpha=I(0.5) , size=1)
# #   geom_point(data = subset(AuAgDeca,(Schoenflies.symbol=="D5h")),
# #              # AuAgDeca
# #              aes(x = N_Ag, y = EEFinal, #colour=Gap, # size = abs(Spin),
# #                  colour=Schoenflies.symbol),alpha = I(1.0),size=5,shape=23,
# #              width = I(3),lwd=3)
# ##  labs(shape="Symmetry",colour="Symmetry")+
# theme(plot.title = element_text(size = rel(2), face="bold"),
#       axis.line = element_line(size = 1)) +
#   theme(axis.title= element_text(size = rel(1),face="bold"))+
#   #scale_x_continuous(breaks=0:14)+ 
#   geom_hline(yintercept=0,size=1) +
#   theme(plot.title = element_text(size = rel(2), 
#                                   face="bold", colour="black"),
#         #        axis.line = element_line(size = 1),
#         axis.text=element_text( size=rel(2),
#                                 face="bold", ) ) + 
#   theme(axis.title = element_text(size =  rel(2),face="bold", colour="black") ) +
#   #  scale_x_continuous(expand=c(0,0))+ scale_y_continuous(expand=c(0,0)) +
#   theme(legend.title = element_text(size=19,face="bold" ), 
#         legend.text = element_text( size = 18, face = "bold"),
#         plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y=element_text(vjust=0.025),
#         axis.title.x=element_text(vjust=-.75)) +
#   #scale_colour_manual(values=c("blue4","red4","green4","black")  ,  
#   #                    breaks = c("Minima","Minima 1st","green4","black")  , 
#   #                    labels=c("Minima","Minima 1st","BBP","BBP") ) 
#   
#   #scale_shape_manual(values=c(25,24,23,22,21,20,19,18,17,16,15,14))  + 
#   #scale_size_continuous(range=c(3,9))+
#   xlab("Number of Ag Atoms") + ylab("Excess Energy (eV)") +
#   #labs(colour="Gap",size="Spin")+ 
#   labs(
#     #    title = "Energy Excess vs Silver Atoms (Deca)" , #colour="Gap",size="Spin",size="Spin",
#     #alpha="",shape="Seed"
#     # ) + #theme(plot.title = element_text(size = rel(2), 
#     #face="bold",),
#     axis.line = element_line(size = 1) ) + scale_linetype_manual(guide = FALSE) +
#   guides(colour = guide_legend(override.aes = list(
#     shape= c(
#       #22,22,22,22,22,
#       23,21),
#     size=c(
#       #4,4,4,4,4,
#       5,2.5), 
#     width = c(
#       #3,3,3,3,3,
#       3,3),
#     lwd=c(
#       #3,3,3,3,3,
#       3,3) ) ) ) +
#   scale_size_manual(values = c(3, 5), guide = FALSE) +
#   theme(legend.key = element_blank(),
#         legend.position = c(.9, .27),
#         legend.background=element_blank(),
#         panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
#         panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
#         #panel.grid.minor.x = element_blank(),
#         legend.text = element_text(size = 20, face = "bold"),
#         panel.border = element_rect(colour = "black",size = 1) ) +
#   #  guides(colour = guide_legend(override.aes = list(shape=c(16,17,18,15)))
#   #  ) +
#   #  theme(legend.key = element_blank(),legend.position = c(.525, .775),legend.background=element_blank(),
#   #        panel.grid.major = element_line(size = 0.25, colour="#858585",linetype = "longdash"),
#   #        panel.grid.minor.y = element_line(size = 0.25, colour="#858585",linetype = "dotted"),
#   #        panel.grid.minor.x = element_blank(),
#   #        legend.text = element_text(size = 20, face = "bold"),
#   #        panel.border = element_rect(colour = "black",size = 1) )
#   #theme(axis.title= element_text(size = rel(1),face="bold" #, colour="steelblue")
#   #                               ) #+
#   #labs(linetype="")
# ggsave(file = "EnergyExcessDecaSym_nomin.pdf", width = 10, height = 7 )
# 
# 
# # eq = function(x){x*x}
# # y = eq(x)                                                               
# # 
# # 
# # x=seq(0,1,by=0.01)
# # y=x*log(x)+(1-x)*log(1-x)
# # plot(x,y,type='l')
