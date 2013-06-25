library(colorspace)
library(RColorBrewer)
library(Hmisc)
library(ggplot2)
library(stats)

#lm_eqn = function(df){
#  m = lm(y ~ x, df);
#  eq <- substitute(italic(y) == a~b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                   list(a = formatC(coef(m)[1], digits=3,flag="+", format="f"),
#                        b = formatC(coef(m)[2], digits=3,flag="+", format="f"),
#                        r2 = format(summary(m)$r.squared, digits = 3)))
#  as.character(as.expression(eq));                 
#}

lm_eqn = function(m) {
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  as.character(as.expression(eq));                 
}


setwd("C:/Users/Kib/Downloads/documents-export-2013-01-24/")

if(dev.cur() == 1) dev.new()
# Cubo --------------------------------------------------------------------

## AGAG Cubo ====================================================================
ggplot() + theme_bw() + 
  geom_path(data = minimaCubo, 
            aes(y =DE1stStep, x = as.numeric(AGAGRATIO) #,
            ),alpha = 0.25,
            size=1) +
  geom_point(data = AuAgCubo, 
             aes(y =DE1stStep, x = AGAGRATIO, colour=as.factor(N_Ag)  ,
                 shape=as.factor(N_Ag)      ),    width = I(3) ,
             alpha = I(0.75),size=3 ,position = position_jitter(width = .0000015) ) +
  stat_smooth(data = subset(AuAgCubo, 1 < N_Ag & N_Ag < 12),
              method=lm, 
              aes(y = DE1stStep, x = AGAGRATIO  ,
                  colour=as.factor(N_Ag) # ,
                  # shape=as.factor(N_Au)
              ) , size=I(1.25), alpha = I(0.125), show_guide=FALSE) +
  scale_shape_manual(values=rep(21:24,4)[1:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[1:16]) +
  xlab("Ratio of AgAg Bonds") +
  ylab("Excess (eV)") +
  ylab("Energy Excess 1st Cubo (eV)") +  
  theme(plot.title = element_text(size = rel(1), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:10/10) +
  scale_y_continuous(
    #    breaks=seq(      round(min(minimaCubo$DE1stStep),digits=3), 
    #                     round(max(minimaCubo$DE1stStep),digits=3),
    #                     length.out=10) ,
    limits=c(round(min(minimaCubo$DE1stStep),digits=2)-1E-2,
             round(max(minimaCubo$DE1stStep),digits=2)+1E-2 )
  ) +
  labs(title = "Energy Excess 1st vs Silver Atoms - Cubo" ,
       colour="#Ag", shape="#Ag", linewidth="Case"
       # size="Spin", 
       # alpha="",
       # ,
       # linetype="Case" 
  ) + # guides( colour = FALSE ) +
  ####  guides(shape = guide_legend(override.aes = list(linewidth = 0) ) ) +
  ####  guides(shape = guide_legend(override.aes = list(width = I(3) ) ) )  +
  guides(colour=guide_legend(override.aes = list(size = 4), linewidth = I(3))) +
  ggsave(file = "NAGAG-Cubo-1st_nomin.pdf", width = 10, height = 7 )
dev.off()

## AGAU Cubo ====================================================================
ggplot() + theme_bw() + 
  geom_path(data = minimaCubo, 
            aes(y =DE1stStep, x = as.numeric(AGAURATIO) #,
            ),alpha = 0.25,
            size=1) +
  geom_point(data = AuAgCubo, 
             aes(y =DE1stStep, x = AGAURATIO, colour=as.factor(N_Ag)  ,
                 shape=as.factor(N_Ag)      ),    width = I(3) ,
             alpha = I(0.75),size=3 ,position = position_jitter(width = .0000015) ) +
  stat_smooth(data = subset(AuAgCubo, 1 < N_Ag & N_Ag < 12),
              method=lm, 
              aes(y = DE1stStep, x = AGAURATIO  ,
                  colour=as.factor(N_Ag) # ,
                  # shape=as.factor(N_Au)
              ) , size=I(1.25), alpha = I(0.125), show_guide=FALSE) +
  scale_shape_manual(values=rep(21:24,4)[1:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[1:16]) +
  xlab("Ratio of AgAu Bonds") +
  ylab("Excess (eV)") +
  ylab("Energy Excess 1st Cubo (eV)") +  
  theme(plot.title = element_text(size = rel(1), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:10/10) +
  scale_y_continuous(
    #    breaks=seq(      round(min(minimaCubo$DE1stStep),digits=3), 
    #                     round(max(minimaCubo$DE1stStep),digits=3),
    #                     length.out=10) ,
    limits=c(round(min(minimaCubo$DE1stStep),digits=2)-1E-2,
             round(max(minimaCubo$DE1stStep),digits=2)+1E-2 )
  ) +
  labs(title = "Energy Excess 1st vs Silver Atoms - Cubo" ,
       colour="#Ag", shape="#Ag", linewidth="Case"
       # size="Spin", 
       # alpha="",
       # ,
       # linetype="Case" 
  ) + # guides( colour = FALSE ) +
  ####  guides(shape = guide_legend(override.aes = list(linewidth = 0) ) ) +
  ####  guides(shape = guide_legend(override.aes = list(width = I(3) ) ) )  +
  guides(colour=guide_legend(override.aes = list(size = 4), linewidth = I(3))) +
  ggsave(file = "NAGAU-Cubo-1st_nomin.pdf", width = 10, height = 7 )
dev.off()
#

## AUAU Cubo ====================================================================
ggplot() + theme_bw() + 
  geom_path(data = minimaCubo, 
            aes(y =DE1stStep, x = as.numeric(AUAURATIO) #,
            ),alpha = 0.25,
            size=1) +
  geom_point(data = AuAgCubo, 
             aes(y =DE1stStep, x = AUAURATIO, colour=as.factor(N_Au)  ,
                 shape=as.factor(N_Au)      ),    width = I(3) ,
             alpha = I(0.75),size=3 ,position = position_jitter(width = .0000015) ) +
  stat_smooth(data = subset(AuAgCubo, 1 < N_Ag & N_Ag < 12),
              method=lm, 
              aes(y = DE1stStep, x = AUAURATIO  ,
                  colour=as.factor(N_Au) # ,
                  # shape=as.factor(N_Au)
              ) , size=I(1.25), alpha = I(0.125), show_guide=FALSE) +
  scale_shape_manual(values=rep(21:24,4)[1:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[1:16]) +
  xlab("Ratio of AuAu Bonds") +
  ylab("Excess (eV)") +
  ylab("Energy Excess 1st Cubo (eV)") +  
  theme(plot.title = element_text(size = rel(1), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:10/10) +
  scale_y_continuous(
    #    breaks=seq(      round(min(minimaCubo$DE1stStep),digits=3), 
    #                     round(max(minimaCubo$DE1stStep),digits=3),
    #                     length.out=10) ,
    limits=c(round(min(minimaCubo$DE1stStep),digits=2)-1E-2,
             round(max(minimaCubo$DE1stStep),digits=2)+1E-2 )
  ) +
  labs(title = "Energy Excess 1st vs Silver Atoms - Cubo" ,
       colour="#Au", shape="#Au", linewidth="Case"
       # size="Spin", 
       # alpha="",
       # ,
       # linetype="Case" 
  ) + # guides( colour = FALSE ) +
  ####  guides(shape = guide_legend(override.aes = list(linewidth = 0) ) ) +
  ####  guides(shape = guide_legend(override.aes = list(width = I(3) ) ) )  +
  guides(colour=guide_legend(override.aes = list(size = 4), linewidth = I(3))) +
  ggsave(file = "NAUAU-Cubo-1st_nomin.pdf", width = 10, height = 7 )
dev.off()

# Deca --------------------------------------------------------------------

## AGAG Deca ====================================================================
ggplot() + theme_bw() + 
  geom_path(data = minimaDeca, 
            aes(y =DE1stStep, x = as.numeric(AGAGRATIO) #,
            ),alpha = 0.25,
            size=1) +
  geom_point(data = AuAgDeca, 
             aes(y =DE1stStep, x = AGAGRATIO, colour=as.factor(N_Ag)  ,
                 shape=as.factor(N_Ag)      ),    width = I(3) ,
             alpha = I(0.75),size=3 ,position = position_jitter(width = .0000015) ) +
  stat_smooth(data = subset(AuAgDeca, 1 < N_Ag & N_Ag < 12),
              method=lm, 
              aes(y = DE1stStep, x = AGAGRATIO  ,
                  colour=as.factor(N_Ag) # ,
                  # shape=as.factor(N_Au)
              ) , size=I(1.25), alpha = I(0.125), show_guide=FALSE) +
  scale_shape_manual(values=rep(21:24,4)[1:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[1:16]) +
  xlab("Ratio of AgAg Bonds") +
  ylab("Excess (eV)") +
  ylab("Energy Excess 1st Deca (eV)") +  
  theme(plot.title = element_text(size = rel(1), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:10/10) +
  scale_y_continuous(
    #    breaks=seq(      round(min(minimaDeca$DE1stStep),digits=3), 
    #                     round(max(minimaDeca$DE1stStep),digits=3),
    #                     length.out=10) ,
    limits=c(round(min(minimaDeca$DE1stStep),digits=2)-1E-2,
             round(max(minimaDeca$DE1stStep),digits=2)+1E-2 )
  ) +
  labs(title = "Energy Excess 1st vs Silver Atoms - Deca" ,
       colour="#Ag", shape="#Ag", linewidth="Case"
       # size="Spin", 
       # alpha="",
       # ,
       # linetype="Case" 
  ) + # guides( colour = FALSE ) +
  ####  guides(shape = guide_legend(override.aes = list(linewidth = 0) ) ) +
  ####  guides(shape = guide_legend(override.aes = list(width = I(3) ) ) )  +
  guides(colour=guide_legend(override.aes = list(size = 4), linewidth = I(3))) +
  ggsave(file = "NAGAG-Deca-1st_nomin.pdf", width = 10, height = 7 )
dev.off()

## AGAU Deca ====================================================================
ggplot() + theme_bw() + 
  geom_path(data = minimaDeca, 
            aes(y =DE1stStep, x = as.numeric(AGAURATIO) #,
            ),alpha = 0.25,
            size=1) +
  geom_point(data = AuAgDeca, 
             aes(y =DE1stStep, x = AGAURATIO, colour=as.factor(N_Ag)  ,
                 shape=as.factor(N_Ag)      ),    width = I(3) ,
             alpha = I(0.75),size=3 ,position = position_jitter(width = .0000015) ) +
  stat_smooth(data = subset(AuAgDeca, 1 < N_Ag & N_Ag < 12),
              method=lm, 
              aes(y = DE1stStep, x = AGAURATIO  ,
                  colour=as.factor(N_Ag) # ,
                  # shape=as.factor(N_Au)
              ) , size=I(1.25), alpha = I(0.125), show_guide=FALSE) +
  scale_shape_manual(values=rep(21:24,4)[1:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[1:16]) +
  xlab("Ratio of AgAu Bonds") +
  ylab("Excess (eV)") +
  ylab("Energy Excess 1st Deca (eV)") +  
  theme(plot.title = element_text(size = rel(1), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:10/10) +
  scale_y_continuous(
    #    breaks=seq(      round(min(minimaDeca$DE1stStep),digits=3), 
    #                     round(max(minimaDeca$DE1stStep),digits=3),
    #                     length.out=10) ,
    limits=c(round(min(minimaDeca$DE1stStep),digits=2)-1E-2,
             round(max(minimaDeca$DE1stStep),digits=2)+1E-2 )
  ) +
  labs(title = "Energy Excess 1st vs Silver Atoms - Deca" ,
       colour="#Ag", shape="#Ag", linewidth="Case"
       # size="Spin", 
       # alpha="",
       # ,
       # linetype="Case" 
  ) + # guides( colour = FALSE ) +
  ####  guides(shape = guide_legend(override.aes = list(linewidth = 0) ) ) +
  ####  guides(shape = guide_legend(override.aes = list(width = I(3) ) ) )  +
  guides(colour=guide_legend(override.aes = list(size = 4), linewidth = I(3))) +
  ggsave(file = "NAGAU-Deca-1st_nomin.pdf", width = 10, height = 7 )
dev.off()
#AGAU

## AUAU Deca ====================================================================
ggplot() + theme_bw() + 
  geom_path(data = minimaDeca, 
            aes(y =DE1stStep, x = as.numeric(AUAURATIO) #,
            ),alpha = 0.25,
            size=1) +
  geom_point(data = AuAgDeca, 
             aes(y =DE1stStep, x = AUAURATIO, colour=as.factor(N_Au)  ,
                 shape=as.factor(N_Au)      ),    width = I(3) ,
             alpha = I(0.75),size=3 ,position = position_jitter(width = .0000015) ) +
  stat_smooth(data = subset(AuAgDeca, 1 < N_Ag & N_Ag < 12),
              method=lm, 
              aes(y = DE1stStep, x = AUAURATIO  ,
                  colour=as.factor(N_Au) # ,
                  # shape=as.factor(N_Au)
              ) , size=I(1.25), alpha = I(0.125), show_guide=FALSE) +
  scale_shape_manual(values=rep(21:24,4)[1:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[1:16]) +
  xlab("Ratio of AuAu Bonds") +
  ylab("Excess (eV)") +
  ylab("Energy Excess 1st Deca (eV)") +  
  theme(plot.title = element_text(size = rel(1), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:10/10) +
  scale_y_continuous(
    #    breaks=seq(      round(min(minimaDeca$DE1stStep),digits=3), 
    #                     round(max(minimaDeca$DE1stStep),digits=3),
    #                     length.out=10) ,
    limits=c(round(min(minimaDeca$DE1stStep),digits=2)-1E-2,
             round(max(minimaDeca$DE1stStep),digits=2)+1E-2 )
  ) +
  labs(title = "Energy Excess 1st vs Silver Atoms - Deca" ,
       colour="#Au", shape="#Au", linewidth="Case"
       # size="Spin", 
       # alpha="",
       # ,
       # linetype="Case" 
  ) + # guides( colour = FALSE ) +
  ####  guides(shape = guide_legend(override.aes = list(linewidth = 0) ) ) +
  ####  guides(shape = guide_legend(override.aes = list(width = I(3) ) ) )  +
  guides(colour=guide_legend(override.aes = list(size = 4), linewidth = I(3))) +
  ggsave(file = "NAUAU-Deca-1st_nomin.pdf", width = 10, height = 7 )
dev.off()

# Ico --------------------------------------------------------------------

## AGAG Ico ====================================================================
ggplot() + theme_bw() + 
  geom_path(data = minimaIco, 
            aes(y =DE1stStep, x = as.numeric(AGAGRATIO) #,
            ),alpha = 0.25,
            size=1) +
  geom_point(data = AuAgIco, 
             aes(y =DE1stStep, x = AGAGRATIO, colour=as.factor(N_Ag)  ,
                 shape=as.factor(N_Ag)      ),    width = I(3) ,
             alpha = I(0.75),size=3 ,position = position_jitter(width = .0000015) ) +
  stat_smooth(data = subset(AuAgIco, 1 < N_Ag & N_Ag < 12),
              method=lm, 
              aes(y = DE1stStep, x = AGAGRATIO  ,
                  colour=as.factor(N_Ag) # ,
                  # shape=as.factor(N_Au)
              ) , size=I(1.25), alpha = I(0.125), show_guide=FALSE) +
  scale_shape_manual(values=rep(21:24,4)[1:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[1:16]) +
  xlab("Ratio of AgAg Bonds") +
  ylab("Excess (eV)") +
  ylab("Energy Excess 1st Ico (eV)") +  
  theme(plot.title = element_text(size = rel(1), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:10/10) +
  scale_y_continuous(
    #    breaks=seq(      round(min(minimaIco$DE1stStep),digits=3), 
    #                     round(max(minimaIco$DE1stStep),digits=3),
    #                     length.out=10) ,
    limits=c(round(min(minimaIco$DE1stStep),digits=2)-1E-2,
             round(max(minimaIco$DE1stStep),digits=2)+1E-2 )
  ) +
  labs(title = "Energy Excess 1st vs Silver Atoms - Ico" ,
       colour="#Ag", shape="#Ag", linewidth="Case"
       # size="Spin", 
       # alpha="",
       # ,
       # linetype="Case" 
  ) + # guides( colour = FALSE ) +
  ####  guides(shape = guide_legend(override.aes = list(linewidth = 0) ) ) +
  ####  guides(shape = guide_legend(override.aes = list(width = I(3) ) ) )  +
  guides(colour=guide_legend(override.aes = list(size = 4), linewidth = I(3))) +
  ggsave(file = "NAGAG-Ico-1st_nomin.pdf", width = 10, height = 7 )
dev.off()

## AGAU Ico ====================================================================
ggplot() + theme_bw() + 
  geom_path(data = minimaIco, 
            aes(y =DE1stStep, x = as.numeric(AGAURATIO) #,
            ),alpha = 0.25,
            size=1) +
  geom_point(data = AuAgIco, 
             aes(y =DE1stStep, x = AGAURATIO, colour=as.factor(N_Ag)  ,
                 shape=as.factor(N_Ag)      ),    width = I(3) ,
             alpha = I(0.75),size=3 ,position = position_jitter(width = .0000015) ) +
  stat_smooth(data = subset(AuAgIco, 1 < N_Ag & N_Ag < 12),
              method=lm, 
              aes(y = DE1stStep, x = AGAURATIO  ,
                  colour=as.factor(N_Ag) # ,
                  # shape=as.factor(N_Au)
              ) , size=I(1.25), alpha = I(0.125), show_guide=FALSE) +
  scale_shape_manual(values=rep(21:24,4)[1:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[1:16]) +
  xlab("Ratio of AgAu Bonds") +
  ylab("Excess (eV)") +
  ylab("Energy Excess 1st Ico (eV)") +  
  theme(plot.title = element_text(size = rel(1), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:10/10) +
  scale_y_continuous(
    #    breaks=seq(      round(min(minimaIco$DE1stStep),digits=3), 
    #                     round(max(minimaIco$DE1stStep),digits=3),
    #                     length.out=10) ,
    limits=c(round(min(minimaIco$DE1stStep),digits=2)-1E-2,
             round(max(minimaIco$DE1stStep),digits=2)+1E-2 )
  ) +
  labs(title = "Energy Excess 1st vs Silver Atoms - Ico" ,
       colour="#Ag", shape="#Ag", linewidth="Case"
       # size="Spin", 
       # alpha="",
       # ,
       # linetype="Case" 
  ) + # guides( colour = FALSE ) +
  ####  guides(shape = guide_legend(override.aes = list(linewidth = 0) ) ) +
  ####  guides(shape = guide_legend(override.aes = list(width = I(3) ) ) )  +
  guides(colour=guide_legend(override.aes = list(size = 4), linewidth = I(3))) +
  ggsave(file = "NAGAU-Ico-1st_nomin.pdf", width = 10, height = 7 )
dev.off()

## AUAU Ico ====================================================================
ggplot() + theme_bw() + 
  geom_path(data = minimaIco, 
            aes(y =DE1stStep, x = as.numeric(AUAURATIO) #,
            ),alpha = 0.25,
            size=1) +
  geom_point(data = AuAgIco, 
             aes(y =DE1stStep, x = AUAURATIO, colour=as.factor(N_Au)  ,
                 shape=as.factor(N_Au)      ),    width = I(3) ,
             alpha = I(0.75),size=3 ,position = position_jitter(width = .0000015) ) +
  stat_smooth(data = subset(AuAgIco, 1 < N_Ag & N_Ag < 12),
              method=lm, 
              aes(y = DE1stStep, x = AUAURATIO  ,
                  colour=as.factor(N_Au) # ,
                  # shape=as.factor(N_Au)
              ) , size=I(1.25), alpha = I(0.125), show_guide=FALSE) +
  scale_shape_manual(values=rep(21:24,4)[1:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[1:16]) +
  xlab("Ratio of AuAu Bonds") +
  ylab("Excess (eV)") +
  ylab("Energy Excess 1st Ico (eV)") +  
  theme(plot.title = element_text(size = rel(1), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:10/10) +
  scale_y_continuous(
    #    breaks=seq(      round(min(minimaIco$DE1stStep),digits=3), 
    #                     round(max(minimaIco$DE1stStep),digits=3),
    #                     length.out=10) ,
    limits=c(round(min(minimaIco$DE1stStep),digits=2)-1E-2,
             round(max(minimaIco$DE1stStep),digits=2)+1E-2 )
  ) +
  labs(title = "Energy Excess 1st vs Silver Atoms - Ico" ,
       colour="#Au", shape="#Au", linewidth="Case"
       # size="Spin", 
       # alpha="",
       # ,
       # linetype="Case" 
  ) + # guides( colour = FALSE ) +
  ####  guides(shape = guide_legend(override.aes = list(linewidth = 0) ) ) +
  ####  guides(shape = guide_legend(override.aes = list(width = I(3) ) ) )  +
  guides(colour=guide_legend(override.aes = list(size = 4), linewidth = I(3))) +
  ggsave(file = "NAUAU-Ico-1st_nomin.pdf", width = 10, height = 7 )
dev.off()

# BBP --------------------------------------------------------------------

## AGAG BBP ====================================================================
ggplot() + theme_bw() + 
  geom_path(data = minimaBBP, 
            aes(y =DE1stStep, x = as.numeric(AGAGRATIO) #,
            ),alpha = 0.25,
            size=1) +
  geom_point(data = AuAgBBP, 
             aes(y =DE1stStep, x = AGAGRATIO, colour=as.factor(N_Ag)  ,
                 shape=as.factor(N_Ag)      ),    width = I(3) ,
             alpha = I(0.75),size=3 ,position = position_jitter(width = .0000015) ) +
  stat_smooth(data = subset(AuAgBBP, 1 < N_Ag & N_Ag < 12),
              method=lm, 
              aes(y = DE1stStep, x = AGAGRATIO  ,
                  colour=as.factor(N_Ag) # ,
                  # shape=as.factor(N_Au)
              ) , size=I(1.25), alpha = I(0.125), show_guide=FALSE) +
  scale_shape_manual(values=rep(21:24,4)[1:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[1:16]) +
  xlab("Ratio of AgAg Bonds") +
  ylab("Excess (eV)") +
  ylab("Energy Excess 1st BBP (eV)") +  
  theme(plot.title = element_text(size = rel(1), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:10/10) +
  scale_y_continuous(
    #    breaks=seq(      round(min(minimaBBP$DE1stStep),digits=3), 
    #                     round(max(minimaBBP$DE1stStep),digits=3),
    #                     length.out=10) ,
    limits=c(round(min(minimaBBP$DE1stStep),digits=2)-1E-2,
             round(max(minimaBBP$DE1stStep),digits=2)+1E-2 )
  ) +
  labs(title = "Energy Excess 1st vs Silver Atoms - BBP" ,
       colour="#Ag", shape="#Ag", linewidth="Case"
       # size="Spin", 
       # alpha="",
       # ,
       # linetype="Case" 
  ) + # guides( colour = FALSE ) +
  ####  guides(shape = guide_legend(override.aes = list(linewidth = 0) ) ) +
  ####  guides(shape = guide_legend(override.aes = list(width = I(3) ) ) )  +
  guides(colour=guide_legend(override.aes = list(size = 4), linewidth = I(3))) +
  ggsave(file = "NAGAG-BBP-1st_nomin.pdf", width = 10, height = 7 )
dev.off()

## AGAU BBP ====================================================================
ggplot() + theme_bw() + 
  geom_path(data = minimaBBP, 
            aes(y =DE1stStep, x = as.numeric(AGAURATIO) #,
            ),alpha = 0.25,
            size=1) +
  geom_point(data = AuAgBBP, 
             aes(y =DE1stStep, x = AGAURATIO, colour=as.factor(N_Ag)  ,
                 shape=as.factor(N_Ag)      ),    width = I(3) ,
             alpha = I(0.75),size=3 ,position = position_jitter(width = .0000015) ) +
  stat_smooth(data = subset(AuAgBBP, 1 < N_Ag & N_Ag < 12),
              method=lm, 
              aes(y = DE1stStep, x = AGAURATIO  ,
                  colour=as.factor(N_Ag) # ,
                  # shape=as.factor(N_Au)
              ) , size=I(1.25), alpha = I(0.125), show_guide=FALSE) +
  scale_shape_manual(values=rep(21:24,4)[1:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[1:16]) +
  xlab("Ratio of AgAu Bonds") +
  ylab("Excess (eV)") +
  ylab("Energy Excess 1st BBP (eV)") +  
  theme(plot.title = element_text(size = rel(1), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:10/10) +
  scale_y_continuous(
    #    breaks=seq(      round(min(minimaBBP$DE1stStep),digits=3), 
    #                     round(max(minimaBBP$DE1stStep),digits=3),
    #                     length.out=10) ,
    limits=c(round(min(minimaBBP$DE1stStep),digits=2)-1E-2,
             round(max(minimaBBP$DE1stStep),digits=2)+1E-2 )
  ) +
  labs(title = "Energy Excess 1st vs Silver Atoms - BBP" ,
       colour="#Ag", shape="#Ag", linewidth="Case"
       # size="Spin", 
       # alpha="",
       # ,
       # linetype="Case" 
  ) + # guides( colour = FALSE ) +
  ####  guides(shape = guide_legend(override.aes = list(linewidth = 0) ) ) +
  ####  guides(shape = guide_legend(override.aes = list(width = I(3) ) ) )  +
  guides(colour=guide_legend(override.aes = list(size = 4), linewidth = I(3))) +
  ggsave(file = "NAGAU-BBP-1st_nomin.pdf", width = 10, height = 7 )
dev.off()
#AGAU

## AUAU BBP ====================================================================
ggplot() + theme_bw() + 
  geom_path(data = minimaBBP, 
            aes(y =DE1stStep, x = as.numeric(AUAURATIO) #,
            ),alpha = 0.25,
            size=1) +
  geom_point(data = AuAgBBP, 
             aes(y =DE1stStep, x = AUAURATIO, colour=as.factor(N_Au)  ,
                 shape=as.factor(N_Au)      ),    width = I(3) ,
             alpha = I(0.75),size=3 ,position = position_jitter(width = .0000015) ) +
  stat_smooth(data = subset(AuAgBBP, 1 < N_Ag & N_Ag < 12),
              method=lm, 
              aes(y = DE1stStep, x = AUAURATIO  ,
                  colour=as.factor(N_Au) # ,
                  # shape=as.factor(N_Au)
              ) , size=I(1.25), alpha = I(0.125), show_guide=FALSE) +
  scale_shape_manual(values=rep(21:24,4)[1:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[1:16]) +
  xlab("Ratio of AuAu Bonds") +
  ylab("Excess (eV)") +
  ylab("Energy Excess 1st BBP (eV)") +  
  theme(plot.title = element_text(size = rel(1), face="bold"),
        axis.line = element_line(size = 1))+
  theme(axis.title= element_text(size = rel(1),face="bold"))+
  scale_x_continuous(breaks=0:10/10) +
  scale_y_continuous(
    #    breaks=seq(      round(min(minimaBBP$DE1stStep),digits=3), 
    #                     round(max(minimaBBP$DE1stStep),digits=3),
    #                     length.out=10) ,
    limits=c(round(min(minimaBBP$DE1stStep),digits=2)-1E-2,
             round(max(minimaBBP$DE1stStep),digits=2)+1E-2 )
  ) +
  labs(title = "Energy Excess 1st vs Silver Atoms - BBP" ,
       colour="#Au", shape="#Au", linewidth="Case"
       # size="Spin", 
       # alpha="",
       # ,
       # linetype="Case" 
  ) + # guides( colour = FALSE ) +
  ####  guides(shape = guide_legend(override.aes = list(linewidth = 0) ) ) +
  ####  guides(shape = guide_legend(override.aes = list(width = I(3) ) ) )  +
  guides(colour=guide_legend(override.aes = list(size = 4), linewidth = I(3))) +
  ggsave(file = "NAUAU-BBP-1st_nomin.pdf", width = 10, height = 7 )
dev.off()









# Models -----------------------------------------------------------------

#models=data.frame(unique(AuAgBBP$N_Ag))
#models = matrix(c(unique(AuAg$N_Ag)*length(unique(AuAg$Type)) ),
#                nrow=length(unique(AuAg$N_Ag)) , ncol = length(unique(AuAg$Type)))

#colnames(models)=unique(AuAg$Type)
#rownames(models)=unique(AuAg$N_Ag)


#modelsNAUAU <- vector(mode="list", length=13)





########################################################################
#### modelsBBP NAUAUBONDS

#for(i in unique(AuAg$Type)) {
#  subset(AuAg, Type == i)
#}

modelsBBPNAUAU <- vector(mode="list", length=13)

for (i in 1:13){
  modelsBBPNAUAU[[i]]=lm(DE1stStep ~ NAUAUBONDS, subset(AuAgBBP, N_Ag ==i))
  #print(coef(modelsBBPNAUAU[[i]])[2])
}

coeffBBPNAUAU <- 1:13 # vector(mode="numeric", 14)
coeffBBPNAUAU=rep(NA,13)
errcoeffBBPNAUAU <- 1:13
errcoeffBBPNAUAU=rep(NA,13)
#for (i in 1:13){
for (i in 1:13){  
  #print(coef(modelsBBPNAUAU[[i]])[2])
  coeffBBPNAUAU[i]=modelsBBPNAUAU[[i]]$coefficients[2][[1]]
}
for (i in 2:11){
  errcoeffBBPNAUAU[i]=coef(summary(modelsBBPNAUAU[[i]]))[2, "Std. Error"]
}


coeffBBPDFNAUAU=as.data.frame(matrix( c( 1:length(coeffBBPNAUAU), 
                                         coeffBBPNAUAU , errcoeffBBPNAUAU), ncol=3 ))

colnames(coeffBBPDFNAUAU)=c("i","coeffBBPNAUAU","errcoeffBBPNAUAU")
coeffBBPDFNAUAU$i=as.double(as.vector(coeffBBPDFNAUAU$i))

coeffBBPDFNAUAU$coeffBBPNAUAU=as.double(as.vector(coeffBBPDFNAUAU$coeffBBPNAUAU))
coeffBBPDFNAUAU$errcoeffBBPNAUAU=as.double(as.vector(coeffBBPDFNAUAU$errcoeffBBPNAUAU))

coeffBBPDFNAUAU=coeffBBPDFNAUAU[complete.cases(coeffBBPDFNAUAU),]

#errcoeffBBPNAUAU[is.na(errcoeffBBPNAUAU)] <- 0
ggplot(coeffBBPDFNAUAU, aes(x=i, y=coeffBBPNAUAU, colour=factor(i),group = 1,
                            shape=factor(i),fill=factor(i) ) ) + 
  geom_errorbar(aes(ymin=coeffBBPNAUAU-errcoeffBBPNAUAU, ymax=coeffBBPNAUAU+errcoeffBBPNAUAU), width=.15,
                size=1) +
  geom_line(size=1) +
  geom_point(size=5) + 
  stat_smooth(method=lm, fullrange = TRUE, size=1.25, alpha = 0.35, linetype = "3313") +
  scale_shape_manual(values=rep(21:24,4)[3:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16],guide = 'none') +
  scale_fill_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16]) +
  xlab("Number of Au-Au Bonds") + ylab("EE1st(eV)") +
  labs(fill="Number of Au-Au Bonds",shape="Number of Au-Au Bonds") +
  labs(title = "Slope vs Number of Bonds" ) +
  scale_x_continuous(breaks=0:14) + #+ geom_hline(yintercept=0,size=1)
  geom_text(data=coeffBBPDFNAUAU, aes(x = 9, y = max(coeffBBPNAUAU+errcoeffBBPNAUAU) , 
                                      label = lm_eqn(lm(coeffBBPNAUAU~i,coeffBBPDFNAUAU)) ),
            colour="black", parse = TRUE) +
  ggsave(file = "SlopeAUAUBBP-1st_nomin.pdf", width = 10, height = 7 ) 
dev.off()
####################################################################



########################################################################
#### modelsBBP NAGAUBONDS
modelsBBPNAGAU <- vector(mode="list", length=13)

for (i in 1:13){
  modelsBBPNAGAU[[i]]=lm(DE1stStep ~ NAGAUBONDS, subset(AuAgBBP, N_Ag ==i))
  #print(coef(modelsBBPNAGAU[[i]])[2])
}

coeffBBPNAGAU <- 1:13 # vector(mode="numeric", 14)
coeffBBPNAGAU=rep(NA,13)
errcoeffBBPNAGAU <- 1:13
errcoeffBBPNAGAU=rep(NA,13)
#for (i in 1:13){
for (i in 1:13){  
  #print(coef(modelsBBPNAGAU[[i]])[2])
  coeffBBPNAGAU[i]=modelsBBPNAGAU[[i]]$coefficients[2][[1]]
}
for (i in 2:12){
  errcoeffBBPNAGAU[i]=coef(summary(modelsBBPNAGAU[[i]]))[2, "Std. Error"]
}
# plot(coeffBBPNAGAU)

coeffBBPDFNAGAU=as.data.frame(matrix( c( 1:length(coeffBBPNAGAU), 
                                         coeffBBPNAGAU , errcoeffBBPNAGAU), ncol=3 ))

colnames(coeffBBPDFNAGAU)=c("i","coeffBBPNAGAU","errcoeffBBPNAGAU")
coeffBBPDFNAGAU$i=as.double(as.vector(coeffBBPDFNAGAU$i))

coeffBBPDFNAGAU$coeffBBPNAGAU=as.double(as.vector(coeffBBPDFNAGAU$coeffBBPNAGAU))
coeffBBPDFNAGAU$errcoeffBBPNAGAU=as.double(as.vector(coeffBBPDFNAGAU$errcoeffBBPNAGAU))

coeffBBPDFNAGAU=coeffBBPDFNAGAU[complete.cases(coeffBBPDFNAGAU),]

#errcoeffBBPNAGAU[is.na(errcoeffBBPNAGAU)] <- 0
ggplot(coeffBBPDFNAGAU, aes(x=i, y=coeffBBPNAGAU, colour=factor(i),group = 1,
                            shape=factor(i),fill=factor(i) ) ) + 
  geom_errorbar(aes(ymin=coeffBBPNAGAU-errcoeffBBPNAGAU, ymax=coeffBBPNAGAU+errcoeffBBPNAGAU), width=.15,
                size=1) +
  geom_line(size=1) +
  geom_point(size=5) + 
  stat_smooth(method=lm, fullrange = TRUE, size=1.25, alpha = 0.35, linetype = "3313") +
  scale_shape_manual(values=rep(21:24,4)[3:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16],guide = 'none') +
  scale_fill_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16]) +
  xlab("Number of Ag-Au Bonds") + ylab("EE1st(eV)") +
  labs(fill="Number of Ag-Au Bonds",shape="Number of Ag-Au Bonds") +
  labs(title = "Slope vs Number of Ag-Au Bonds" #, 
       # colour="Spin",
       #size="R=exp(-(E-E_gs))",alpha="",shape="Seed"
  ) +
  scale_x_continuous(breaks=0:14) + #+ geom_hline(yintercept=0,size=1)
  geom_text(data=coeffBBPDFNAGAU, aes(x = 9, y = max(coeffBBPNAGAU+errcoeffBBPNAGAU) , 
                                      label = lm_eqn(lm(coeffBBPNAGAU~i,coeffBBPDFNAGAU)) ),
            colour="black", parse = TRUE) +
  ggsave(file = "SlopeAGAUBBP-1st_nomin.pdf", width = 10, height = 7 ) 
dev.off()
####################################################################


########################################################################
#### modelsBBP NAGAGBONDS
modelsBBPNAGAG <- vector(mode="list", length=13)

for (i in 1:13){
  modelsBBPNAGAG[[i]]=lm(DE1stStep ~ NAGAGBONDS, subset(AuAgBBP, N_Ag ==i))
  #print(coef(modelsBBPNAGAG[[i]])[2])
}

#coeffBBPNAGAG <- 1:13 # vector(mode="numeric", 14)
coeffBBPNAGAG=rep(NA,13)
#errcoeffBBPNAGAG <- 1:13
errcoeffBBPNAGAG=rep(NA,13)
#for (i in 1:13){
for (i in 1:13){  
  #print(coef(modelsBBPNAGAG[[i]])[2])
  coeffBBPNAGAG[i]=modelsBBPNAGAG[[i]]$coefficients[2][[1]]
}
for (i in 2:11){  
  #print(coef(modelsBBPNAGAG[[i]])[2])
  errcoeffBBPNAGAG[i]=coef(summary(modelsBBPNAGAG[[i]]))[2, "Std. Error"]
}

coeffBBPDFNAGAG=as.data.frame(matrix( c( 1:length(coeffBBPNAGAG), 
                                         coeffBBPNAGAG , errcoeffBBPNAGAG), ncol=3 ))

colnames(coeffBBPDFNAGAG)=c("i","coeffBBPNAGAG","errcoeffBBPNAGAG")
coeffBBPDFNAGAG$i=as.double(as.vector(coeffBBPDFNAGAG$i))

coeffBBPDFNAGAG$x=coeffBBPDFNAGAG$i
coeffBBPDFNAGAG$y=coeffBBPDFNAGAG$coeffBBPNAGAG

coeffBBPDFNAGAG$coeffBBPNAGAG=as.double(as.vector(coeffBBPDFNAGAG$coeffBBPNAGAG))
coeffBBPDFNAGAG$errcoeffBBPNAGAG=as.double(as.vector(coeffBBPDFNAGAG$errcoeffBBPNAGAG))

coeffBBPDFNAGAG=coeffBBPDFNAGAG[complete.cases(coeffBBPDFNAGAG),]

#errcoeffBBPNAGAG[is.na(errcoeffBBPNAGAG)] <- 0
ggplot(coeffBBPDFNAGAG, aes(x=i, y=coeffBBPNAGAG, colour=factor(i),group = 1,
                            shape=factor(i),fill=factor(i) ) ) + 
  geom_errorbar(aes(ymin=coeffBBPNAGAG-errcoeffBBPNAGAG, ymax=coeffBBPNAGAG+errcoeffBBPNAGAG), width=.15,
                size=1) +
  geom_line(size=1) +
  geom_point(size=5) + 
  stat_smooth(method=lm, fullrange = TRUE, size=1.25, alpha = 0.35, linetype = "3313") +
  scale_shape_manual(values=rep(21:24,4)[3:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16],guide = 'none') +
  scale_fill_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16]) +
  xlab("Number of Ag-Ag Bonds") + ylab("EE1st(eV)") +
  labs(fill="Number of Ag-Ag Bonds",shape="Number of Ag-Ag Bonds") +
  labs(title = "Slope vs Number of Number of Ag-Ag Bonds" #, 
       # colour="Spin",
       #size="R=exp(-(E-E_gs))",alpha="",shape="Seed"
  ) + 
  scale_x_continuous(breaks=0:14) + #+ geom_hline(yintercept=0,size=1)
  geom_text(data=coeffBBPDFNAGAG, aes(x = 9, y = max(coeffBBPNAGAG+errcoeffBBPNAGAG) , 
                                      label = lm_eqn(lm(coeffBBPNAGAG~i,coeffBBPDFNAGAG)) ),
            colour="black", parse = TRUE) +
  ggsave(file = "SlopeAGAGBBP-1st_nomin.pdf", width = 10, height = 7 ) 
dev.off()
####################################################################



#dev.off()
#plot( coeffBBPDFNAGAG$i, coeffBBPDFNAGAG$coeffBBPNAGAG)

#errcoeffBBPNAGAGminus = as.numeric(coeffBBPNAGAG) - as.numeric(errcoeffBBPNAGAG)
#errcoeffBBPNAGAGplus = as.numeric(coeffBBPNAGAG) + as.numeric(errcoeffBBPNAGAG)

#xtmp=1:length(coeffBBPNAGAG)
#xtmp=xtmp*1.0

#errbar( coeffBBPDFNAGAG$i, coeffBBPDFNAGAG$coeffBBPNAGAG , 
#        coeffBBPDFNAGAG$coeffBBPNAGAG + coeffBBPDFNAGAG$errcoeffBBPNAGAG ,
#        coeffBBPDFNAGAG$coeffBBPNAGAG - coeffBBPDFNAGAG$errcoeffBBPNAGAG )

#modelsBBPSlopeNAGAG=lm( coeffBBPDFNAGAG$coeffBBPNAGAG ~ coeffBBPDFNAGAG$i )
#summary(modelsBBPSlopeNAGAG)
#abline(modelsBBPSlopeNAGAG,col='red')

#z <- nls(coeffBBPNAGAG ~ a*i+b , start=list(a=1, b=1), data=coeffBBPDFNAGAG )
#summary(z)
#lines(coeffBBPDFNAGAG$i,predict(z),col='blue')

####################################################################













########################################################################
#### modelsIco NAUAUBONDS

#for(i in unique(AuAg$Type)) {
#  subset(AuAg, Type == i)
#}

modelsIcoNAUAU <- vector(mode="list", length=13)

for (i in 1:13){
  modelsIcoNAUAU[[i]]=lm(DE1stStep ~ NAUAUBONDS, subset(AuAgIco, N_Ag ==i))
  #print(coef(modelsIcoNAUAU[[i]])[2])
}

coeffIcoNAUAU <- 1:13 # vector(mode="numeric", 14)
coeffIcoNAUAU=rep(NA,13)
errcoeffIcoNAUAU <- 1:13
errcoeffIcoNAUAU=rep(NA,13)
#for (i in 1:13){
for (i in 1:13){  
  #print(coef(modelsIcoNAUAU[[i]])[2])
  coeffIcoNAUAU[i]=modelsIcoNAUAU[[i]]$coefficients[2][[1]]
}
for (i in 2:11){
  errcoeffIcoNAUAU[i]=coef(summary(modelsIcoNAUAU[[i]]))[2, "Std. Error"]
}


coeffIcoDFNAUAU=as.data.frame(matrix( c( 1:length(coeffIcoNAUAU), 
                                         coeffIcoNAUAU , errcoeffIcoNAUAU), ncol=3 ))

colnames(coeffIcoDFNAUAU)=c("i","coeffIcoNAUAU","errcoeffIcoNAUAU")
coeffIcoDFNAUAU$i=as.double(as.vector(coeffIcoDFNAUAU$i))

coeffIcoDFNAUAU$coeffIcoNAUAU=as.double(as.vector(coeffIcoDFNAUAU$coeffIcoNAUAU))
coeffIcoDFNAUAU$errcoeffIcoNAUAU=as.double(as.vector(coeffIcoDFNAUAU$errcoeffIcoNAUAU))

coeffIcoDFNAUAU=coeffIcoDFNAUAU[complete.cases(coeffIcoDFNAUAU),]

#errcoeffIcoNAUAU[is.na(errcoeffIcoNAUAU)] <- 0
ggplot(coeffIcoDFNAUAU, aes(x=i, y=coeffIcoNAUAU, colour=factor(i),group = 1,
                            shape=factor(i),fill=factor(i) ) ) + 
  geom_errorbar(aes(ymin=coeffIcoNAUAU-errcoeffIcoNAUAU, ymax=coeffIcoNAUAU+errcoeffIcoNAUAU), width=.15,
                size=1) +
  geom_line(size=1) +
  geom_point(size=5) + 
  stat_smooth(method=lm, fullrange = TRUE, size=1.25, alpha = 0.35, linetype = "3313") +
  scale_shape_manual(values=rep(21:24,4)[3:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16],guide = 'none') +
  scale_fill_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16]) +
  xlab("Number of Au-Au Bonds") + ylab("EE1st(eV)") +
  labs(fill="Number of Au-Au Bonds",shape="Number of Au-Au Bonds") +
  labs(title = "Slope vs Number of Bonds" #, 
       # colour="Spin",
       #size="R=exp(-(E-E_gs))",alpha="",shape="Seed"
  ) + 
  scale_x_continuous(breaks=0:14) + #+ geom_hline(yintercept=0,size=1)
  geom_text(data=coeffIcoDFNAUAU, aes(x = 9, y = max(coeffIcoNAUAU+errcoeffIcoNAUAU) , 
                                      label = lm_eqn(lm(coeffIcoNAUAU~i,coeffIcoDFNAUAU)) ),
            colour="black", parse = TRUE) +
  ggsave(file = "SlopeAUAUIco-1st_nomin.pdf", width = 10, height = 7 ) 
dev.off()
####################################################################



########################################################################
# modelsBBP NAUAUBONDS
modelsBBPNAUAU <- vector(mode="list", length=13)

for (i in 1:13){
  modelsBBPNAUAU[[i]]=lm(DE1stStep ~ NAUAUBONDS, subset(AuAgBBP, N_Ag ==i))
  #print(coef(modelsBBPNAUAU[[i]])[2])
}

coeffBBPNAUAU <- 1:13 # vector(mode="numeric", 14)
coeffBBPNAUAU=rep(NA,13)
errcoeffBBPNAUAU <- 1:13
errcoeffBBPNAUAU=rep(NA,13)
#for (i in 1:13){
for (i in 1:13){  
  #print(coef(modelsBBPNAUAU[[i]])[2])
  coeffBBPNAUAU[i]=modelsBBPNAUAU[[i]]$coefficients[2][[1]]
}
for (i in 2:11){
  errcoeffBBPNAUAU[i]=coef(summary(modelsBBPNAUAU[[i]]))[2, "Std. Error"]
}

########################################################################
#### modelsIco NAGAUBONDS

coeffIcoDFNAGAU=as.data.frame(matrix( c( 1:length(coeffIcoNAGAU), 
                                         coeffIcoNAGAU , errcoeffIcoNAGAU), ncol=3 ))

colnames(coeffIcoDFNAGAU)=c("i","coeffIcoNAGAU","errcoeffIcoNAGAU")
coeffIcoDFNAGAU$i=as.double(as.vector(coeffIcoDFNAGAU$i))

coeffIcoDFNAGAU$coeffIcoNAGAU=as.double(as.vector(coeffIcoDFNAGAU$coeffIcoNAGAU))
coeffIcoDFNAGAU$errcoeffIcoNAGAU=as.double(as.vector(coeffIcoDFNAGAU$errcoeffIcoNAGAU))

coeffIcoDFNAGAU=coeffIcoDFNAGAU[complete.cases(coeffIcoDFNAGAU),]

#errcoeffIcoNAGAU[is.na(errcoeffIcoNAGAU)] <- 0
ggplot(coeffIcoDFNAGAU, aes(x=i, y=coeffIcoNAGAU, colour=factor(i),group = 1,
                            shape=factor(i),fill=factor(i) ) ) + 
  geom_errorbar(aes(ymin=coeffIcoNAGAU-errcoeffIcoNAGAU, ymax=coeffIcoNAGAU+errcoeffIcoNAGAU), width=.15,
                size=1) +
  geom_line(size=1) +
  geom_point(size=5) + 
  stat_smooth(method=lm, fullrange = TRUE, size=1.25, alpha = 0.35, linetype = "3313") +
  scale_shape_manual(values=rep(21:24,4)[3:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16],guide = 'none') +
  scale_fill_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16]) +
  xlab("Number of Ag-Au Bonds") + ylab("EE1st(eV)") +
  labs(fill="Number of Ag-Au Bonds",shape="Number of Ag-Au Bonds") +
  labs(title = "Slope vs Number of Bonds" ) +
  scale_x_continuous(breaks=0:14) +
  geom_text(data=coeffIcoDFNAGAU, aes(x = 9, y = max(coeffIcoNAGAU+errcoeffIcoNAGAU) , 
                                      label = lm_eqn(lm(coeffIcoNAGAU~i,coeffIcoDFNAGAU)) ),
            colour="black", parse = TRUE) +
  ggsave(file = "SlopeAGAUIco-1st_nomin.pdf", width = 10, height = 7 ) 
dev.off()
####################################################################


#plot(coeffIcoNAGAU)

#xtmp=1:length(coeffIcoNAGAU)

#xtmp=xtmp*1.0

#errbar( xtmp, coeffIcoNAGAU, coeffIcoNAGAU+errcoeffIcoNAGAU, coeffIcoNAGAU-errcoeffIcoNAGAU )

#modelsIcoSlopeNAGAU=lm(coeffIcoNAGAU ~ xtmp)
#z <- nls(coeffIcoNAGAU ~ a * xtmp + b, start=list(a=1, b=1))
#summary(z)

#summary(modelsIcoSlopeNAGAU)
#abline(modelsIcoSlopeNAGAU,col='red')

#lines(predict(z),col='blue')

#ggsave(file = "SlopeAGAUIco-1st_nomin.pdf", width = 10, height = 7 )
####################################################################


########################################################################
#### modelsIco NAGAGBONDS
modelsIcoNAGAG <- vector(mode="list", length=13)

for (i in 1:13){
  modelsIcoNAGAG[[i]]=lm(DE1stStep ~ NAGAGBONDS, subset(AuAgIco, N_Ag ==i))
  #print(coef(modelsIcoNAGAG[[i]])[2])
}

#coeffIcoNAGAG <- 1:13 # vector(mode="numeric", 14)
coeffIcoNAGAG=rep(NA,13)
#errcoeffIcoNAGAG <- 1:13
errcoeffIcoNAGAG=rep(NA,13)
#for (i in 1:13){
for (i in 1:13){  
  #print(coef(modelsIcoNAGAG[[i]])[2])
  coeffIcoNAGAG[i]=modelsIcoNAGAG[[i]]$coefficients[2][[1]]
}
for (i in 2:11){  
  #print(coef(modelsIcoNAGAG[[i]])[2])
  errcoeffIcoNAGAG[i]=coef(summary(modelsIcoNAGAG[[i]]))[2, "Std. Error"]
}

coeffIcoDFNAGAG=as.data.frame(matrix( c( 1:length(coeffIcoNAGAG), 
                                         coeffIcoNAGAG , errcoeffIcoNAGAG), ncol=3 ))

colnames(coeffIcoDFNAGAG)=c("i","coeffIcoNAGAG","errcoeffIcoNAGAG")
coeffIcoDFNAGAG$i=as.double(as.vector(coeffIcoDFNAGAG$i))

coeffIcoDFNAGAG$x=coeffIcoDFNAGAG$i
coeffIcoDFNAGAG$y=coeffIcoDFNAGAG$coeffIcoNAGAG

coeffIcoDFNAGAG$coeffIcoNAGAG=as.double(as.vector(coeffIcoDFNAGAG$coeffIcoNAGAG))
coeffIcoDFNAGAG$errcoeffIcoNAGAG=as.double(as.vector(coeffIcoDFNAGAG$errcoeffIcoNAGAG))

coeffIcoDFNAGAG=coeffIcoDFNAGAG[complete.cases(coeffIcoDFNAGAG),]

#errcoeffIcoNAGAG[is.na(errcoeffIcoNAGAG)] <- 0
ggplot(coeffIcoDFNAGAG, aes(x=i, y=coeffIcoNAGAG, colour=factor(i),group = 1,
                            shape=factor(i),fill=factor(i) ) ) + 
  geom_errorbar(aes(ymin=coeffIcoNAGAG-errcoeffIcoNAGAG, ymax=coeffIcoNAGAG+errcoeffIcoNAGAG), width=.15,
                size=1) +
  geom_line(size=1) +
  geom_point(size=5) + 
  stat_smooth(method=lm, fullrange = TRUE, size=1.25, alpha = 0.35, linetype = "3313") +
  scale_shape_manual(values=rep(21:24,4)[3:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16],guide = 'none') +
  scale_fill_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16]) +
  xlab("Number of Ag-Ag Bonds") + ylab("EE1st(eV)") +
  labs(fill="Number of Ag-Ag Bonds",shape="Number of Ag-Ag Bonds") +
  labs(title = "Slope vs Number of Ag atoms" #, 
       # colour="Spin",
       #size="R=exp(-(E-E_gs))",alpha="",shape="Seed"
  ) + 
  scale_x_continuous(breaks=0:14) + #+ geom_hline(yintercept=0,size=1)
  geom_text(data=coeffIcoDFNAGAG, aes(x = 9, y = max(coeffIcoNAGAG+errcoeffIcoNAGAG) , 
                                      label = lm_eqn(lm(coeffIcoNAGAG~i,coeffIcoDFNAGAG)) ),
            colour="black", parse = TRUE) +
  
  ggsave(file = "SlopeAGAGIco-1st_nomin.pdf", width = 10, height = 7 ) 
dev.off()

#plot( coeffIcoDFNAGAG$i, coeffIcoDFNAGAG$coeffIcoNAGAG)

#errcoeffIcoNAGAGminus = as.numeric(coeffIcoNAGAG) - as.numeric(errcoeffIcoNAGAG)
#errcoeffIcoNAGAGplus = as.numeric(coeffIcoNAGAG) + as.numeric(errcoeffIcoNAGAG)

#xtmp=1:length(coeffIcoNAGAG)
#xtmp=xtmp*1.0

#errbar( coeffIcoDFNAGAG$i, coeffIcoDFNAGAG$coeffIcoNAGAG , 
#        coeffIcoDFNAGAG$coeffIcoNAGAG + coeffIcoDFNAGAG$errcoeffIcoNAGAG ,
#        coeffIcoDFNAGAG$coeffIcoNAGAG - coeffIcoDFNAGAG$errcoeffIcoNAGAG )

#modelsIcoSlopeNAGAG=lm( coeffIcoDFNAGAG$coeffIcoNAGAG ~ coeffIcoDFNAGAG$i )
#summary(modelsIcoSlopeNAGAG)
#abline(modelsIcoSlopeNAGAG,col='red')

#z <- nls(coeffIcoNAGAG ~ a*i+b , start=list(a=1, b=1), data=coeffIcoDFNAGAG )
#summary(z)
#lines(coeffIcoDFNAGAG$i,predict(z),col='blue')

####################################################################













########################################################################
#### modelsCubo NAUAUBONDS

#for(i in unique(AuAg$Type)) {
#  subset(AuAg, Type == i)
#}

modelsCuboNAUAU <- vector(mode="list", length=13)

for (i in 1:13){
  modelsCuboNAUAU[[i]]=lm(DE1stStep ~ NAUAUBONDS, subset(AuAgCubo, N_Ag ==i))
  #print(coef(modelsCuboNAUAU[[i]])[2])
}

coeffCuboNAUAU <- 1:13 # vector(mode="numeric", 14)
coeffCuboNAUAU=rep(NA,13)
errcoeffCuboNAUAU <- 1:13
errcoeffCuboNAUAU=rep(NA,13)
#for (i in 1:13){
for (i in 1:13){  
  #print(coef(modelsCuboNAUAU[[i]])[2])
  coeffCuboNAUAU[i]=modelsCuboNAUAU[[i]]$coefficients[2][[1]]
}
for (i in 2:11){
  errcoeffCuboNAUAU[i]=coef(summary(modelsCuboNAUAU[[i]]))[2, "Std. Error"]
}


coeffCuboDFNAUAU=as.data.frame(matrix( c( 1:length(coeffCuboNAUAU), 
                                          coeffCuboNAUAU , errcoeffCuboNAUAU), ncol=3 ))

colnames(coeffCuboDFNAUAU)=c("i","coeffCuboNAUAU","errcoeffCuboNAUAU")
coeffCuboDFNAUAU$i=as.double(as.vector(coeffCuboDFNAUAU$i))

coeffCuboDFNAUAU$coeffCuboNAUAU=as.double(as.vector(coeffCuboDFNAUAU$coeffCuboNAUAU))
coeffCuboDFNAUAU$errcoeffCuboNAUAU=as.double(as.vector(coeffCuboDFNAUAU$errcoeffCuboNAUAU))

coeffCuboDFNAUAU=coeffCuboDFNAUAU[complete.cases(coeffCuboDFNAUAU),]

#errcoeffCuboNAUAU[is.na(errcoeffCuboNAUAU)] <- 0
ggplot(coeffCuboDFNAUAU, aes(x=i, y=coeffCuboNAUAU, colour=factor(i),group = 1,
                             shape=factor(i),fill=factor(i) ) ) + 
  geom_errorbar(aes(ymin=coeffCuboNAUAU-errcoeffCuboNAUAU, ymax=coeffCuboNAUAU+errcoeffCuboNAUAU), width=.15,
                size=1) +
  geom_line(size=1) +
  geom_point(size=5) + 
  stat_smooth(method=lm, fullrange = TRUE, size=1.25, alpha = 0.35, linetype = "3313") +
  scale_shape_manual(values=rep(21:24,4)[3:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16],guide = 'none') +
  scale_fill_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16]) +
  xlab("Number of Au-Au Bonds") + ylab("EE1st(eV)") +
  labs(fill="Number of Ag atoms",shape="Number of Ag atoms") +
  labs(title = "Slope vs Number of Bonds" #, 
       # colour="Spin",
       #size="R=exp(-(E-E_gs))",alpha="",shape="Seed"
  ) +
  scale_x_continuous(breaks=0:14) + #+ geom_hline(yintercept=0,size=1)
  geom_text(data=coeffCuboDFNAUAU, aes(x = 9, y = max(coeffCuboNAUAU+errcoeffCuboNAUAU) , 
                                       label = lm_eqn(lm(coeffCuboNAUAU~i,coeffCuboDFNAUAU)) ),
            colour="black", parse = TRUE) +
  ggsave(file = "SlopeAUAUCubo-1st_nomin.pdf", width = 10, height = 7 ) 
dev.off()
####################################################################



########################################################################
#### modelsCubo NAGAUBONDS
modelsCuboNAGAU <- vector(mode="list", length=13)

for (i in 1:13){
  modelsCuboNAGAU[[i]]=lm(DE1stStep ~ NAGAUBONDS, subset(AuAgCubo, N_Ag ==i))
  #print(coef(modelsCuboNAGAU[[i]])[2])
}

coeffCuboNAGAU <- 1:13 # vector(mode="numeric", 14)
coeffCuboNAGAU=rep(NA,13)
errcoeffCuboNAGAU <- 1:13
errcoeffCuboNAGAU=rep(NA,13)
#for (i in 1:13){
for (i in 1:13){  
  #print(coef(modelsCuboNAGAU[[i]])[2])
  coeffCuboNAGAU[i]=modelsCuboNAGAU[[i]]$coefficients[2][[1]]
}
for (i in 2:12){
  errcoeffCuboNAGAU[i]=coef(summary(modelsCuboNAGAU[[i]]))[2, "Std. Error"]
}
# plot(coeffCuboNAGAU)

coeffCuboDFNAGAU=as.data.frame(matrix( c( 1:length(coeffCuboNAGAU), 
                                          coeffCuboNAGAU , errcoeffCuboNAGAU), ncol=3 ))

colnames(coeffCuboDFNAGAU)=c("i","coeffCuboNAGAU","errcoeffCuboNAGAU")
coeffCuboDFNAGAU$i=as.double(as.vector(coeffCuboDFNAGAU$i))

coeffCuboDFNAGAU$coeffCuboNAGAU=as.double(as.vector(coeffCuboDFNAGAU$coeffCuboNAGAU))
coeffCuboDFNAGAU$errcoeffCuboNAGAU=as.double(as.vector(coeffCuboDFNAGAU$errcoeffCuboNAGAU))

coeffCuboDFNAGAU=coeffCuboDFNAGAU[complete.cases(coeffCuboDFNAGAU),]

#errcoeffCuboNAGAU[is.na(errcoeffCuboNAGAU)] <- 0
ggplot(coeffCuboDFNAGAU, aes(x=i, y=coeffCuboNAGAU, colour=factor(i),group = 1,
                             shape=factor(i),fill=factor(i) ) ) + 
  geom_errorbar(aes(ymin=coeffCuboNAGAU-errcoeffCuboNAGAU, ymax=coeffCuboNAGAU+errcoeffCuboNAGAU), width=.15,
                size=1) +
  geom_line(size=1) +
  geom_point(size=5) + 
  stat_smooth(method=lm, fullrange = TRUE, size=1.25, alpha = 0.35, linetype = "3313") +
  scale_shape_manual(values=rep(21:24,4)[3:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16],guide = 'none') +
  scale_fill_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16]) +
  xlab("Number of Ag-Ag Bonds ") + ylab("EE1st(eV)") +
  labs(fill="Number of Ag atoms",shape="Number of Ag atoms") +
  labs(title = "Slope vs Number of Bonds" #, 
       # colour="Spin",
       #size="R=exp(-(E-E_gs))",alpha="",shape="Seed"
  ) +
  scale_x_continuous(breaks=0:14) + #+ geom_hline(yintercept=0,size=1)
  geom_text(data=coeffCuboDFNAGAU, aes(x = 9, y = max(coeffCuboNAGAU+errcoeffCuboNAGAU) , 
                                       label = lm_eqn(lm(coeffCuboNAGAU~i,coeffCuboDFNAGAU)) ),
            colour="black", parse = TRUE) +
  ggsave(file = "SlopeAGAUCubo-1st_nomin.pdf", width = 10, height = 7 ) 
dev.off()

# plot(coeffCuboNAGAU)
# 
# xtmp=1:length(coeffCuboNAGAU)
# 
# xtmp=xtmp*1.0
# 
# errbar( xtmp, coeffCuboNAGAU, coeffCuboNAGAU+errcoeffCuboNAGAU, coeffCuboNAGAU-errcoeffCuboNAGAU )
# 
# modelsCuboSlopeNAGAU=lm(coeffCuboNAGAU ~ xtmp)
# z <- nls(coeffCuboNAGAU ~ a * xtmp + b, start=list(a=1, b=1))
# summary(z)
# 
# summary(modelsCuboSlopeNAGAU)
# abline(modelsCuboSlopeNAGAU,col='red')
# 
# lines(predict(z),col='blue')
# 
# ggsave(file = "SlopeAGAUCubo-1st_nomin.pdf", width = 10, height = 7 )
####################################################################


########################################################################
#### modelsCubo NAGAGBONDS
modelsCuboNAGAG <- vector(mode="list", length=13)

for (i in 1:13){
  modelsCuboNAGAG[[i]]=lm(DE1stStep ~ NAGAGBONDS, subset(AuAgCubo, N_Ag ==i))
  #print(coef(modelsCuboNAGAG[[i]])[2])
}

#coeffCuboNAGAG <- 1:13 # vector(mode="numeric", 14)
coeffCuboNAGAG=rep(NA,13)
#errcoeffCuboNAGAG <- 1:13
errcoeffCuboNAGAG=rep(NA,13)
#for (i in 1:13){
for (i in 1:13){  
  #print(coef(modelsCuboNAGAG[[i]])[2])
  coeffCuboNAGAG[i]=modelsCuboNAGAG[[i]]$coefficients[2][[1]]
}
for (i in 2:11){  
  #print(coef(modelsCuboNAGAG[[i]])[2])
  errcoeffCuboNAGAG[i]=coef(summary(modelsCuboNAGAG[[i]]))[2, "Std. Error"]
}

coeffCuboDFNAGAG=as.data.frame(matrix( c( 1:length(coeffCuboNAGAG), 
                                          coeffCuboNAGAG , errcoeffCuboNAGAG), ncol=3 ))

colnames(coeffCuboDFNAGAG)=c("i","coeffCuboNAGAG","errcoeffCuboNAGAG")
coeffCuboDFNAGAG$i=as.double(as.vector(coeffCuboDFNAGAG$i))

coeffCuboDFNAGAG$x=coeffCuboDFNAGAG$i
coeffCuboDFNAGAG$y=coeffCuboDFNAGAG$coeffCuboNAGAG

coeffCuboDFNAGAG$coeffCuboNAGAG=as.double(as.vector(coeffCuboDFNAGAG$coeffCuboNAGAG))
coeffCuboDFNAGAG$errcoeffCuboNAGAG=as.double(as.vector(coeffCuboDFNAGAG$errcoeffCuboNAGAG))

coeffCuboDFNAGAG=coeffCuboDFNAGAG[complete.cases(coeffCuboDFNAGAG),]

#errcoeffCuboNAGAG[is.na(errcoeffCuboNAGAG)] <- 0
ggplot(coeffCuboDFNAGAG, aes(x=i, y=coeffCuboNAGAG, colour=factor(i),group = 1,
                             shape=factor(i),fill=factor(i) ) ) + 
  geom_errorbar(aes(ymin=coeffCuboNAGAG-errcoeffCuboNAGAG, ymax=coeffCuboNAGAG+errcoeffCuboNAGAG), width=.15,
                size=1) +
  geom_line(size=1) +
  geom_point(size=5) + 
  stat_smooth(method=lm, fullrange = TRUE, size=1.25, alpha = 0.35, linetype = "3313") +
  scale_shape_manual(values=rep(21:24,4)[3:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16],guide = 'none') +
  scale_fill_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16]) +
  xlab("Number of Ag-Ag Bonds") + ylab("EE1st(eV)") +
  labs(fill="Number of Ag-Ag Bonds",shape="Number of Ag-Ag Bonds") +
  labs(title = "Slope vs Number of Ag atoms" #, 
       # colour="Spin",
       #size="R=exp(-(E-E_gs))",alpha="",shape="Seed"
  ) + 
  scale_x_continuous(breaks=0:14) + #+ geom_hline(yintercept=0,size=1)
  geom_text(data=coeffCuboDFNAGAG, aes(x = 9, y = max(coeffCuboNAGAG+errcoeffCuboNAGAG) , 
                                       label = lm_eqn(lm(coeffCuboNAGAG~i,coeffCuboDFNAGAG)) ),
            colour="black", parse = TRUE) +
  ggsave(file = "SlopeAGAGCubo-1st_nomin.pdf", width = 10, height = 7 ) 
dev.off()

#plot( coeffCuboDFNAGAG$i, coeffCuboDFNAGAG$coeffCuboNAGAG)

#errcoeffCuboNAGAGminus = as.numeric(coeffCuboNAGAG) - as.numeric(errcoeffCuboNAGAG)
#errcoeffCuboNAGAGplus = as.numeric(coeffCuboNAGAG) + as.numeric(errcoeffCuboNAGAG)

#xtmp=1:length(coeffCuboNAGAG)
#xtmp=xtmp*1.0

#errbar( coeffCuboDFNAGAG$i, coeffCuboDFNAGAG$coeffCuboNAGAG , 
#        coeffCuboDFNAGAG$coeffCuboNAGAG + coeffCuboDFNAGAG$errcoeffCuboNAGAG ,
#        coeffCuboDFNAGAG$coeffCuboNAGAG-coeffCuboDFNAGAG$errcoeffCuboNAGAG )

#modelsCuboSlopeNAGAG=lm( coeffCuboDFNAGAG$coeffCuboNAGAG ~ coeffCuboDFNAGAG$i )
#summary(modelsCuboSlopeNAGAG)
#abline(modelsCuboSlopeNAGAG,col='red')

#z <- nls(coeffCuboNAGAG ~ a*i+b , start=list(a=1, b=1), data=coeffCuboDFNAGAG )
#summary(z)
#lines(coeffCuboDFNAGAG$i,predict(z),col='blue')

####################################################################










########################################################################
#### modelsCubo NAUAUBONDS

#for(i in unique(AuAg$Type)) {
#  subset(AuAg, Type == i)
#}

modelsCuboNAUAU <- vector(mode="list", length=13)

for (i in 1:13){
  modelsCuboNAUAU[[i]]=lm(DE1stStep ~ NAUAUBONDS, subset(AuAgCubo, N_Ag ==i))
  #print(coef(modelsCuboNAUAU[[i]])[2])
}

coeffCuboNAUAU <- 1:13 # vector(mode="numeric", 14)
coeffCuboNAUAU=rep(NA,13)
errcoeffCuboNAUAU <- 1:13
errcoeffCuboNAUAU=rep(NA,13)
#for (i in 1:13){
for (i in 1:13){  
  #print(coef(modelsCuboNAUAU[[i]])[2])
  coeffCuboNAUAU[i]=modelsCuboNAUAU[[i]]$coefficients[2][[1]]
}
for (i in 2:11){
  errcoeffCuboNAUAU[i]=coef(summary(modelsCuboNAUAU[[i]]))[2, "Std. Error"]
}


coeffCuboDFNAUAU=as.data.frame(matrix( c( 1:length(coeffCuboNAUAU), 
                                          coeffCuboNAUAU , errcoeffCuboNAUAU), ncol=3 ))

colnames(coeffCuboDFNAUAU)=c("i","coeffCuboNAUAU","errcoeffCuboNAUAU")
coeffCuboDFNAUAU$i=as.double(as.vector(coeffCuboDFNAUAU$i))

coeffCuboDFNAUAU$coeffCuboNAUAU=as.double(as.vector(coeffCuboDFNAUAU$coeffCuboNAUAU))
coeffCuboDFNAUAU$errcoeffCuboNAUAU=as.double(as.vector(coeffCuboDFNAUAU$errcoeffCuboNAUAU))

coeffCuboDFNAUAU=coeffCuboDFNAUAU[complete.cases(coeffCuboDFNAUAU),]

#errcoeffCuboNAUAU[is.na(errcoeffCuboNAUAU)] <- 0
ggplot(coeffCuboDFNAUAU, aes(x=i, y=coeffCuboNAUAU, colour=factor(i),group = 1,
                             shape=factor(i),fill=factor(i) ) ) + 
  geom_errorbar(aes(ymin=coeffCuboNAUAU-errcoeffCuboNAUAU, ymax=coeffCuboNAUAU+errcoeffCuboNAUAU), width=.15,
                size=1) +
  geom_line(size=1) +
  geom_point(size=5) + 
  stat_smooth(method=lm, fullrange = TRUE, size=1.25, alpha = 0.35, linetype = "3313") +
  scale_shape_manual(values=rep(21:24,4)[3:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16],guide = 'none') +
  scale_fill_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16]) +
  xlab("Number of Au-Au Bonds") + ylab("EE1st(eV)") +
  labs(fill="Number of Au-Au Bonds",shape="Number of Au-Au Bonds") +
  labs(title = "Slope vs Number of Bonds" #, 
       # colour="Spin",
       #size="R=exp(-(E-E_gs))",alpha="",shape="Seed"
  ) +
  scale_x_continuous(breaks=0:14) + #+ geom_hline(yintercept=0,size=1)
  geom_text(data=coeffCuboDFNAUAU, aes(x = 9, y = max(coeffCuboNAUAU+errcoeffCuboNAUAU) , 
                                       label = lm_eqn(lm(coeffCuboNAUAU~i,coeffCuboDFNAUAU)) ),
            colour="black", parse = TRUE) +
  ggsave(file = "SlopeAUAUCubo-1st_nomin.pdf", width = 10, height = 7 ) 
dev.off()
####################################################################



########################################################################
#### modelsCubo NAGAUBONDS
modelsCuboNAGAU <- vector(mode="list", length=13)

for (i in 1:13){
  modelsCuboNAGAU[[i]]=lm(DE1stStep ~ NAGAUBONDS, subset(AuAgCubo, N_Ag ==i))
  #print(coef(modelsCuboNAGAU[[i]])[2])
}

coeffCuboNAGAU <- 1:13 # vector(mode="numeric", 14)
coeffCuboNAGAU=rep(NA,13)
errcoeffCuboNAGAU <- 1:13
errcoeffCuboNAGAU=rep(NA,13)
#for (i in 1:13){
for (i in 1:13){  
  #print(coef(modelsCuboNAGAU[[i]])[2])
  coeffCuboNAGAU[i]=modelsCuboNAGAU[[i]]$coefficients[2][[1]]
}
for (i in 2:12){
  errcoeffCuboNAGAU[i]=coef(summary(modelsCuboNAGAU[[i]]))[2, "Std. Error"]
}
# plot(coeffCuboNAGAU)

coeffCuboDFNAGAU=as.data.frame(matrix( c( 1:length(coeffCuboNAGAU), 
                                          coeffCuboNAGAU , errcoeffCuboNAGAU), ncol=3 ))

colnames(coeffCuboDFNAGAU)=c("i","coeffCuboNAGAU","errcoeffCuboNAGAU")
coeffCuboDFNAGAU$i=as.double(as.vector(coeffCuboDFNAGAU$i))

coeffCuboDFNAGAU$coeffCuboNAGAU=as.double(as.vector(coeffCuboDFNAGAU$coeffCuboNAGAU))
coeffCuboDFNAGAU$errcoeffCuboNAGAU=as.double(as.vector(coeffCuboDFNAGAU$errcoeffCuboNAGAU))

coeffCuboDFNAGAU=coeffCuboDFNAGAU[complete.cases(coeffCuboDFNAGAU),]

#errcoeffCuboNAGAU[is.na(errcoeffCuboNAGAU)] <- 0
ggplot(coeffCuboDFNAGAU, aes(x=i, y=coeffCuboNAGAU, colour=factor(i),group = 1,
                             shape=factor(i),fill=factor(i) ) ) + 
  geom_errorbar(aes(ymin=coeffCuboNAGAU-errcoeffCuboNAGAU, ymax=coeffCuboNAGAU+errcoeffCuboNAGAU), width=.15,
                size=1) +
  geom_line(size=1) +
  geom_point(size=5) + 
  stat_smooth(method=lm, fullrange = TRUE, size=1.25, alpha = 0.35, linetype = "3313") +
  scale_shape_manual(values=rep(21:24,4)[3:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16],guide = 'none') +
  scale_fill_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16]) +
  xlab("Number of Ag-Au Bonds") + ylab("EE1st(eV)") +
  labs(fill="Number of Ag-Au Bonds",shape="Number of Ag-Au Bonds") +
  labs(title = "Slope vs Number of Bonds" #, 
       # colour="Spin",
       #size="R=exp(-(E-E_gs))",alpha="",shape="Seed"
  ) +
  scale_x_continuous(breaks=0:14) + #+ geom_hline(yintercept=0,size=1)
  geom_text(data=coeffCuboDFNAGAU, aes(x = 9, y = max(coeffCuboNAGAU+errcoeffCuboNAGAU) , 
                                       label = lm_eqn(lm(coeffCuboNAGAU~i,coeffCuboDFNAGAU)) ),
            colour="black", parse = TRUE) +
  ggsave(file = "SlopeAGAUCubo-1st_nomin.pdf", width = 10, height = 7 ) 
dev.off()
####################################################################


########################################################################
#### modelsCubo NAGAGBONDS
modelsCuboNAGAG <- vector(mode="list", length=13)

for (i in 1:13){
  modelsCuboNAGAG[[i]]=lm(DE1stStep ~ NAGAGBONDS, subset(AuAgCubo, N_Ag ==i))
  #print(coef(modelsCuboNAGAG[[i]])[2])
}

#coeffCuboNAGAG <- 1:13 # vector(mode="numeric", 14)
coeffCuboNAGAG=rep(NA,13)
#errcoeffCuboNAGAG <- 1:13
errcoeffCuboNAGAG=rep(NA,13)
#for (i in 1:13){
for (i in 1:13){  
  #print(coef(modelsCuboNAGAG[[i]])[2])
  coeffCuboNAGAG[i]=modelsCuboNAGAG[[i]]$coefficients[2][[1]]
}
for (i in 2:11){  
  #print(coef(modelsCuboNAGAG[[i]])[2])
  errcoeffCuboNAGAG[i]=coef(summary(modelsCuboNAGAG[[i]]))[2, "Std. Error"]
}

coeffCuboDFNAGAG=as.data.frame(matrix( c( 1:length(coeffCuboNAGAG), 
                                          coeffCuboNAGAG , errcoeffCuboNAGAG), ncol=3 ))

colnames(coeffCuboDFNAGAG)=c("i","coeffCuboNAGAG","errcoeffCuboNAGAG")
coeffCuboDFNAGAG$i=as.double(as.vector(coeffCuboDFNAGAG$i))

coeffCuboDFNAGAG$x=coeffCuboDFNAGAG$i
coeffCuboDFNAGAG$y=coeffCuboDFNAGAG$coeffCuboNAGAG

coeffCuboDFNAGAG$coeffCuboNAGAG=as.double(as.vector(coeffCuboDFNAGAG$coeffCuboNAGAG))
coeffCuboDFNAGAG$errcoeffCuboNAGAG=as.double(as.vector(coeffCuboDFNAGAG$errcoeffCuboNAGAG))

coeffCuboDFNAGAG=coeffCuboDFNAGAG[complete.cases(coeffCuboDFNAGAG),]

#errcoeffCuboNAGAG[is.na(errcoeffCuboNAGAG)] <- 0
ggplot(coeffCuboDFNAGAG, aes(x=i, y=coeffCuboNAGAG, colour=factor(i),group = 1,
                             shape=factor(i),fill=factor(i) ) ) + 
  geom_errorbar(aes(ymin=coeffCuboNAGAG-errcoeffCuboNAGAG, ymax=coeffCuboNAGAG+errcoeffCuboNAGAG), width=.15,
                size=1) +
  geom_line(size=1) +
  geom_point(size=5) + 
  stat_smooth(method=lm, fullrange = TRUE, size=1.25, alpha = 0.35, linetype = "3313") +
  scale_shape_manual(values=rep(21:24,4)[3:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16],guide = 'none') +
  scale_fill_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16]) +
  xlab("Number of Ag-Ag Bonds") + ylab("EE1st(eV)") +
  labs(fill="Number of Ag-Ag Bonds",shape="Number of Ag-Ag Bonds") +
  labs(title = "Slope vs Number of Ag atoms" #, 
       # colour="Spin",
       #size="R=exp(-(E-E_gs))",alpha="",shape="Seed"
  ) + 
  scale_x_continuous(breaks=0:14) + #+ geom_hline(yintercept=0,size=1)
  geom_text(data=coeffCuboDFNAGAG, aes(x = 9, y = max(coeffCuboNAGAG+errcoeffCuboNAGAG) , 
                                       label = lm_eqn(lm(coeffCuboNAGAG~i,coeffCuboDFNAGAG)) ),
            colour="black", parse = TRUE) +
  ggsave(file = "SlopeAGAGCubo-1st_nomin.pdf", width = 10, height = 7 ) 
dev.off()
####################################################################








########################################################################
#### modelsDeca NAUAUBONDS

#for(i in unique(AuAg$Type)) {
#  subset(AuAg, Type == i)
#}

modelsDecaNAUAU <- vector(mode="list", length=13)

for (i in 1:13){
  modelsDecaNAUAU[[i]]=lm(DE1stStep ~ NAUAUBONDS, subset(AuAgDeca, N_Ag ==i))
  #print(coef(modelsDecaNAUAU[[i]])[2])
}

coeffDecaNAUAU <- 1:13 # vector(mode="numeric", 14)
coeffDecaNAUAU=rep(NA,13)
errcoeffDecaNAUAU <- 1:13
errcoeffDecaNAUAU=rep(NA,13)
#for (i in 1:13){
for (i in 1:13){  
  #print(coef(modelsDecaNAUAU[[i]])[2])
  coeffDecaNAUAU[i]=modelsDecaNAUAU[[i]]$coefficients[2][[1]]
}
for (i in 2:11){
  errcoeffDecaNAUAU[i]=coef(summary(modelsDecaNAUAU[[i]]))[2, "Std. Error"]
}


coeffDecaDFNAUAU=as.data.frame(matrix( c( 1:length(coeffDecaNAUAU), 
                                          coeffDecaNAUAU , errcoeffDecaNAUAU), ncol=3 ))

colnames(coeffDecaDFNAUAU)=c("i","coeffDecaNAUAU","errcoeffDecaNAUAU")
coeffDecaDFNAUAU$i=as.double(as.vector(coeffDecaDFNAUAU$i))

coeffDecaDFNAUAU$coeffDecaNAUAU=as.double(as.vector(coeffDecaDFNAUAU$coeffDecaNAUAU))
coeffDecaDFNAUAU$errcoeffDecaNAUAU=as.double(as.vector(coeffDecaDFNAUAU$errcoeffDecaNAUAU))

coeffDecaDFNAUAU=coeffDecaDFNAUAU[complete.cases(coeffDecaDFNAUAU),]

#errcoeffDecaNAUAU[is.na(errcoeffDecaNAUAU)] <- 0
ggplot(coeffDecaDFNAUAU, aes(x=i, y=coeffDecaNAUAU, colour=factor(i),group = 1,
                             shape=factor(i),fill=factor(i) ) ) + 
  geom_errorbar(aes(ymin=coeffDecaNAUAU-errcoeffDecaNAUAU, ymax=coeffDecaNAUAU+errcoeffDecaNAUAU), width=.15,
                size=1) +
  geom_line(size=1) +
  geom_point(size=5) + 
  stat_smooth(method=lm, fullrange = TRUE, size=1.25, alpha = 0.35, linetype = "3313") +
  scale_shape_manual(values=rep(21:24,4)[3:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16],guide = 'none') +
  scale_fill_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16]) +
  xlab("Number of Au-Au Bonds") + ylab("EE1st(eV)") +
  labs(fill="Number of Au-Au Bonds",shape="Number of Au-Au Bonds") +
  labs(title = "Slope vs Number of Bonds" #, 
       # colour="Spin",
       #size="R=exp(-(E-E_gs))",alpha="",shape="Seed"
  ) +
  scale_x_continuous(breaks=0:14) + #+ geom_hline(yintercept=0,size=1)
  geom_text(data=coeffDecaDFNAUAU, aes(x = 9, y = max(coeffDecaNAUAU+errcoeffDecaNAUAU) , 
                                       label = lm_eqn(lm(coeffDecaNAUAU~i,coeffDecaDFNAUAU)) ),
            colour="black", parse = TRUE) +
  ggsave(file = "SlopeAUAUDeca-1st_nomin.pdf", width = 10, height = 7 ) 
dev.off()

####################################################################



########################################################################
#### modelsDeca NAGAUBONDS
modelsDecaNAGAU <- vector(mode="list", length=13)

for (i in 1:13){
  modelsDecaNAGAU[[i]]=lm(DE1stStep ~ NAGAUBONDS, subset(AuAgDeca, N_Ag ==i))
  #print(coef(modelsDecaNAGAU[[i]])[2])
}

coeffDecaNAGAU <- 1:13 # vector(mode="numeric", 14)
coeffDecaNAGAU=rep(NA,13)
errcoeffDecaNAGAU <- 1:13
errcoeffDecaNAGAU=rep(NA,13)
#for (i in 1:13){
for (i in 1:13){  
  #print(coef(modelsDecaNAGAU[[i]])[2])
  coeffDecaNAGAU[i]=modelsDecaNAGAU[[i]]$coefficients[2][[1]]
}
for (i in 2:12){
  errcoeffDecaNAGAU[i]=coef(summary(modelsDecaNAGAU[[i]]))[2, "Std. Error"]
}
# plot(coeffDecaNAGAU)

coeffDecaDFNAGAU=as.data.frame(matrix( c( 1:length(coeffDecaNAGAU), 
                                          coeffDecaNAGAU , errcoeffDecaNAGAU), ncol=3 ))

colnames(coeffDecaDFNAGAU)=c("i","coeffDecaNAGAU","errcoeffDecaNAGAU")
coeffDecaDFNAGAU$i=as.double(as.vector(coeffDecaDFNAGAU$i))

coeffDecaDFNAGAU$coeffDecaNAGAU=as.double(as.vector(coeffDecaDFNAGAU$coeffDecaNAGAU))
coeffDecaDFNAGAU$errcoeffDecaNAGAU=as.double(as.vector(coeffDecaDFNAGAU$errcoeffDecaNAGAU))

coeffDecaDFNAGAU=coeffDecaDFNAGAU[complete.cases(coeffDecaDFNAGAU),]

#errcoeffDecaNAGAU[is.na(errcoeffDecaNAGAU)] <- 0
ggplot(coeffDecaDFNAGAU, aes(x=i, y=coeffDecaNAGAU, colour=factor(i),group = 1,
                             shape=factor(i),fill=factor(i) ) ) + 
  geom_errorbar(aes(ymin=coeffDecaNAGAU-errcoeffDecaNAGAU, ymax=coeffDecaNAGAU+errcoeffDecaNAGAU), width=.15,
                size=1) +
  geom_line(size=1) +
  geom_point(size=5) + 
  stat_smooth(method=lm, fullrange = TRUE, size=1.25, alpha = 0.35, linetype = "3313") +
  scale_shape_manual(values=rep(21:24,4)[3:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16],guide = 'none') +
  scale_fill_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16]) +
  xlab("Number of Ag-Au Bonds") + ylab("EE1st(eV)") +
  labs(fill="Number of Ag-Au Bonds",shape="Number of Ag-Au Bonds") +
  labs(title = "Slope vs Number of Bonds" #, 
       # colour="Spin",
       #size="R=exp(-(E-E_gs))",alpha="",shape="Seed"
  ) +
  scale_x_continuous(breaks=0:14) + #+ geom_hline(yintercept=0,size=1)
  geom_text(data=coeffDecaDFNAGAU, aes(x = 9, y = max(coeffDecaNAGAU+errcoeffDecaNAGAU) , 
                                       label = lm_eqn(lm(coeffDecaNAGAU~i,coeffDecaDFNAGAU)) ),
            colour="black", parse = TRUE) +
  ggsave(file = "SlopeAGAUDeca-1st_nomin.pdf", width = 10, height = 7 ) 
dev.off()
####################################################################


########################################################################
#### modelsDeca NAGAGBONDS
modelsDecaNAGAG <- vector(mode="list", length=13)

for (i in 1:13){
  modelsDecaNAGAG[[i]]=lm(DE1stStep ~ NAGAGBONDS, subset(AuAgDeca, N_Ag ==i))
  #print(coef(modelsDecaNAGAG[[i]])[2])
}

#coeffDecaNAGAG <- 1:13 # vector(mode="numeric", 14)
coeffDecaNAGAG=rep(NA,13)
#errcoeffDecaNAGAG <- 1:13
errcoeffDecaNAGAG=rep(NA,13)
#for (i in 1:13){
for (i in 1:13){  
  #print(coef(modelsDecaNAGAG[[i]])[2])
  coeffDecaNAGAG[i]=modelsDecaNAGAG[[i]]$coefficients[2][[1]]
}
for (i in 2:11){  
  #print(coef(modelsDecaNAGAG[[i]])[2])
  errcoeffDecaNAGAG[i]=coef(summary(modelsDecaNAGAG[[i]]))[2, "Std. Error"]
}

coeffDecaDFNAGAG=as.data.frame(matrix( c( 1:length(coeffDecaNAGAG), 
                                          coeffDecaNAGAG , errcoeffDecaNAGAG), ncol=3 ))

colnames(coeffDecaDFNAGAG)=c("i","coeffDecaNAGAG","errcoeffDecaNAGAG")
coeffDecaDFNAGAG$i=as.double(as.vector(coeffDecaDFNAGAG$i))

coeffDecaDFNAGAG$x=coeffDecaDFNAGAG$i
coeffDecaDFNAGAG$y=coeffDecaDFNAGAG$coeffDecaNAGAG

coeffDecaDFNAGAG$coeffDecaNAGAG=as.double(as.vector(coeffDecaDFNAGAG$coeffDecaNAGAG))
coeffDecaDFNAGAG$errcoeffDecaNAGAG=as.double(as.vector(coeffDecaDFNAGAG$errcoeffDecaNAGAG))

coeffDecaDFNAGAG=coeffDecaDFNAGAG[complete.cases(coeffDecaDFNAGAG),]

#errcoeffDecaNAGAG[is.na(errcoeffDecaNAGAG)] <- 0
ggplot(coeffDecaDFNAGAG, aes(x=i, y=coeffDecaNAGAG, colour=factor(i),group = 1,
                             shape=factor(i),fill=factor(i) ) ) + 
  geom_errorbar(aes(ymin=coeffDecaNAGAG-errcoeffDecaNAGAG, ymax=coeffDecaNAGAG+errcoeffDecaNAGAG), width=.15,
                size=1) +
  geom_line(size=1) +
  geom_point(size=5) + 
  stat_smooth(method=lm, fullrange = TRUE, size=1.25, alpha = 0.35, linetype = "3313") +
  scale_shape_manual(values=rep(21:24,4)[3:16]) +
  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16],guide = 'none') +
  scale_fill_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16]) +
  xlab("Number of Ag-Ag Bonds") + ylab("EE1st(eV)") +
  labs(fill="Number of Ag-Ag Bonds",shape="Number of Ag-Ag Bonds") +
  labs(title = "Slope vs Number of Bonds" ) +
  scale_x_continuous(breaks=0:14) + 
  geom_text(data=coeffDecaDFNAGAG, aes(x = 9, y = max(coeffDecaNAGAG+errcoeffDecaNAGAG) , 
                                       label = lm_eqn(lm(coeffDecaNAGAG~i,coeffDecaDFNAGAG)) ),
            colour="black", parse = TRUE) +
  ggsave(file = "SlopeAGAGDeca-1st_nomin.pdf", width = 10, height = 7 ) 
dev.off()

#plot( coeffDecaDFNAGAG$i, coeffDecaDFNAGAG$coeffDecaNAGAG)

#errcoeffDecaNAGAGminus = as.numeric(coeffDecaNAGAG) - as.numeric(errcoeffDecaNAGAG)
#errcoeffDecaNAGAGplus = as.numeric(coeffDecaNAGAG) + as.numeric(errcoeffDecaNAGAG)

#xtmp=1:length(coeffDecaNAGAG)
#xtmp=xtmp*1.0

#errbar( coeffDecaDFNAGAG$i, coeffDecaDFNAGAG$coeffDecaNAGAG , 
#        coeffDecaDFNAGAG$coeffDecaNAGAG + coeffDecaDFNAGAG$errcoeffDecaNAGAG ,
#        coeffDecaDFNAGAG$coeffDecaNAGAG - coeffDecaDFNAGAG$errcoeffDecaNAGAG )

#modelsDecaSlopeNAGAG=lm( coeffDecaDFNAGAG$coeffDecaNAGAG ~ coeffDecaDFNAGAG$i )
#summary(modelsDecaSlopeNAGAG)
#abline(modelsDecaSlopeNAGAG,col='red')

#z <- nls(coeffDecaNAGAG ~ a*i+b , start=list(a=1, b=1), data=coeffDecaDFNAGAG )
#summary(z)
#lines(coeffDecaDFNAGAG$i,predict(z),col='blue')

####################################################################










































# ggplot(coeffBBPDFNAGAG, aes(x=i, y=coeffBBPNAGAG, colour=factor(i),group = 1,
#                             shape=factor(i),fill=factor(i) ) ) + 
#   geom_errorbar(aes(ymin=coeffBBPNAGAG-errcoeffBBPNAGAG, ymax=coeffBBPNAGAG+errcoeffBBPNAGAG), width=.15,
#                 size=1) +
#   geom_line(size=1) +
#   geom_point(size=5) + 
#   stat_smooth(method=lm, fullrange = TRUE, size=1.25, alpha = 0.35, linetype = "3313") +
#   scale_shape_manual(values=rep(21:24,4)[3:16]) +
#   scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16],guide = 'none') +
#   scale_fill_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16]) +
#   xlab("Number of Ag Atoms ") + ylab("EE1st(eV)") +
#   labs(fill="Number of Ag atoms",shape="Number of Ag atoms") +
#   labs(title = "Slope vs Number of Ag atoms" #, 
#        # colour="Spin",
#        #size="R=exp(-(E-E_gs))",alpha="",shape="Seed"
#   ) + 
#   scale_x_continuous(breaks=0:14) + #+ geom_hline(yintercept=0,size=1)
#     geom_text(aes(x = 9, y = 0.440, label = lm_eqn(coeffIcoDFNAGAG)), parse = TRUE,colour="black") + +
# geom_point(data = coeffIcoDFNAGAG, aes(x=i, y=coeffIcoNAGAG, colour=factor(i),group = 1,
#                             shape=factor(i),fill=factor(i) ) ) + 
#   geom_errorbar(data = coeffIcoDFNAGAG,aes(ymin=coeffIcoNAGAG-errcoeffIcoNAGAG, ymax=coeffIcoNAGAG+errcoeffIcoNAGAG), width=.15,
#                 size=1) +
#   geom_line(size=1) +
#   geom_point(size=5) + 
#   stat_smooth(method=lm, fullrange = TRUE, size=1.25, alpha = 0.35, linetype = "3313") +
# #  scale_shape_manual(values=rep(21:24,4)[3:16]) +
# #  scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16],guide = 'none') +
# #  scale_fill_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16]) +
# #  xlab("Number of Ag Atoms ") + ylab("EE1st(eV)") +
# #  labs(fill="Number of Ag atoms",shape="Number of Ag atoms") +
# #  labs(title = "Slope vs Number of Ag atoms" #, 
# #       # colour="Spin",
# #       #size="R=exp(-(E-E_gs))",alpha="",shape="Seed"
# #  ) + 
# #  scale_x_continuous(breaks=0:14) + #+ geom_hline(yintercept=0,size=1)
# #  geom_text(aes(x = 9, y = 0.440, label = lm_eqn(coeffIcoDFNAGAG)), parse = TRUE,colour="black") + label = lm_eqn(coeffIcoDFNAGAG)), parse = TRUE,colour="black")
# +geom_point(coeffCuboDFNAGAG, aes(x=i, y=coeffCuboNAGAG, colour=factor(i),group = 1,
#                              shape=factor(i),fill=factor(i) ) ) + 
#   geom_errorbar(aes(ymin=coeffCuboNAGAG-errcoeffCuboNAGAG, ymax=coeffCuboNAGAG+errcoeffCuboNAGAG), width=.15,
#                 size=1) +
#   geom_line(size=1) +
#   geom_point(size=5) + 
#   stat_smooth(method=lm, fullrange = TRUE, size=1.25, alpha = 0.35, linetype = "3313") +
#   scale_shape_manual(values=rep(21:24,4)[3:16]) +
#   scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16],guide = 'none') +
#   scale_fill_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16]) +
#   xlab("Number of Ag Atoms ") + ylab("EE1st(eV)") +
#   labs(fill="Number of Ag atoms",shape="Number of Ag atoms") +
#   labs(title = "Slope vs Number of Ag atoms" #, 
#        # colour="Spin",
#        #size="R=exp(-(E-E_gs))",alpha="",shape="Seed"
#   ) + 
#   scale_x_continuous(breaks=0:14) + #+ geom_hline(yintercept=0,size=1)
#   geom_text(aes(x = 9, y = 0.440, label = lm_eqn(coeffIcoDFNAGAG)), parse = TRUE,colour="black") + label = lm_eqn(coeffCuboDFNAGAG)), parse = TRUE,colour="black")
# +geom_point(coeffDecaDFNAGAG, aes(x=i, y=coeffDecaNAGAG, colour=factor(i),group = 1,
#                              shape=factor(i),fill=factor(i) ) ) + 
#   geom_errorbar(aes(ymin=coeffDecaNAGAG-errcoeffDecaNAGAG, ymax=coeffDecaNAGAG+errcoeffDecaNAGAG), width=.15,
#                 size=1) +
#   geom_line(size=1) +
#   geom_point(size=5) + 
#   stat_smooth(method=lm, fullrange = TRUE, size=1.25, alpha = 0.35, linetype = "3313") +
#   scale_shape_manual(values=rep(21:24,4)[3:16]) +
#   scale_color_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16],guide = 'none') +
#   scale_fill_manual(values=rep(brewer.pal(5,"Set1"),4)[3:16]) +
#   xlab("Number of Ag Atoms ") + ylab("EE1st(eV)") +
#   labs(fill="Number of Ag atoms",shape="Number of Ag atoms") +
#   labs(title = "Slope vs Number of Ag atoms" #, 
#        # colour="Spin",
#        #size="R=exp(-(E-E_gs))",alpha="",shape="Seed"
#   ) + 
#   scale_x_continuous(breaks=0:14) + #+ geom_hline(yintercept=0,size=1)
#   geom_text(aes(x = 9, y = 0.440, label = lm_eqn(coeffIcoDFNAGAG)), parse = TRUE,colour="black") + label = lm_eqn(coeffDecaDFNAGAG)), parse = TRUE,colour="black")
# ggsave(file = "SlopeAGAGComb-1st_nomin.pdf", width = 10, height = 7 ) 
