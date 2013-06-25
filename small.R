library(colorspace)
library(RColorBrewer)
library(Hmisc)
library(ggplot2)
library(stats)

setwd("C:/Users/Kib/Downloads/documents-export-2013-01-24/")

if(dev.cur() == 1) dev.new()


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



#######

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
#### modelsBBP NAUAUBONDS

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
  xlab("Number of Au - Au Bonds") + ylab("EE1st(eV)") +
  labs(fill="Number of Au - Au Bonds",shape="Number of Au - Au Bonds") +
  labs(title = "Slope vs Number of Bonds" #, 
       # colour="Spin",
       #size="R=exp(-(E-E_gs))",alpha="",shape="Seed"
  ) +
  scale_x_continuous(breaks=0:14) + 
  geom_text(data=coeffBBPDFNAUAU, aes(x = 5, y = max(coeffBBPNAUAU+errcoeffBBPNAUAU) , 
                                      label = lm_eqn(lm(coeffBBPNAUAU~i,coeffBBPDFNAUAU)) ),
            colour="black", parse = TRUE) +
ggsave(file = "SlopeAUAUBBP-1st.pdf", width = 10, height = 7 ) 
dev.off()
####################################################################






paste( paste("models", c("BBP","ICO","CUBO","DECA"), sep = "_") , c("NAUAU","NAGAG","NAUAG"), sep = "")



















#######

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
#### modelsBBP NAUAUBONDS

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
  xlab("Number of Au - Au Bonds") + ylab("EE1st(eV)") +
  labs(fill="Number of Au - Au Bonds",shape="Number of Au - Au Bonds") +
  labs(title = "Slope vs Number of Bonds" #, 
       # colour="Spin",
       #size="R=exp(-(E-E_gs))",alpha="",shape="Seed"
  ) +
  scale_x_continuous(breaks=0:14) + 
  geom_text(data=coeffBBPDFNAUAU, aes(x = 5, y = max(coeffBBPNAUAU+errcoeffBBPNAUAU) , 
                                      label = lm_eqn(lm(coeffBBPNAUAU~i,coeffBBPDFNAUAU)) ),
            colour="black", parse = TRUE) +
  ggsave(file = "SlopeAUAUBBP-1st.pdf", width = 10, height = 7 ) 
dev.off()
####################################################################
