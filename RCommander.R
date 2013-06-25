
library(relimp, pos=4)
showData(AuAgBBP, placement='-20+200', font=getRcmdr('logFont'), 
  maxwidth=80, maxheight=10)
RegModel.1 <- lm(EEFinal~NAUAUBONDS, data=AuAgBBP)
summary(RegModel.1)
Anova(RegModel.1, type="II")
oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
plot(RegModel.1)
par(oldpar)

