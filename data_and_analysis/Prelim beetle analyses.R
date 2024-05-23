beetle.dat = read.table("U:/Datastore/CSCE/biology/users/jmoorad/Data/Beetle/AgeingExptDat.csv", header=TRUE, sep = ",")
head(beetle.dat)

parents = which(!is.na(beetle.dat$AgeR))

par(mfrow=c(1,3)) 
par(mar=c(5,6,4,1)+0.1,mgp=c(3,1,0))


age = beetle.dat$AgeR[parents]
parent = as.numeric(beetle.dat$Parent.flag[parents]) - mean(as.numeric(beetle.dat$Parent.flag[parents]))
mouse = beetle.dat$MouseSize[parents] - mean(beetle.dat$MouseSize[parents], na.rm = TRUE)
block = beetle.dat$Block[parents] - mean(beetle.dat$Block[parents])

total = beetle.dat$AdultNum[parents]
total[which(is.na(total))] = 0

model1 = lm( total ~ age + parent + mouse  +  block)
summary.model1 = summary(model1)
plot(total ~ age, bty = "n", ylab = "Adult production", xlab = "Female age (in days)", cex.lab = 2, cex.axis = 2, cex = 2, main = "A", cex.main = 2)
line1 = summary.model1$coef[1:2,1]

abline(a = line1[1], b = line1[2], lwd = 2)

#text( x = 60, y = 35, "Intercept = 15.1, Slope = -0.119")
#text(x = 60, y = 32, "n = 126, p = 0.0031")

library(survival)

sex = beetle.dat$Sex

sex = as.numeric(sex)-2

coxph(Surv(beetle.dat$Lifespan, event = abs(as.numeric(beetle.dat$Censored)-2),
           type = "right") ~ as.numeric(beetle.dat$Parent.flag)  +  sex)

####Lx by day
surv = Surv(beetle.dat$Lifespan, event = abs(as.numeric(beetle.dat$Censored)-2), type = "right")

fit = coxph(surv ~ 1)
plot(survfit(fit), cex.axis = 2, cex.lab = 2,cex =2, main = "B", cex.main = 2,
     xlab = "Adult age (in days)", ylab = "Survival", cex.axis = 2, bty= "n")


####mortality(by week)
surv = Surv(beetle.dat$week, event = abs(as.numeric(beetle.dat$Censored)-2), type = "right")
coxph(surv ~ 1)
fit = coxph(surv ~ 1)
summ = summary(survfit(fit))
x = summ$time
Lx = summ$surv

library(flexsurv)

Px = Lx[1:13]/c(1,Lx[1:12])
plot(log(-log(Px)), cex.axis = 2, xlab = "Adult age (in weeks)", ylab = "Mortality (Log-scaled)",  cex.lab = 2,cex =2,ylim = c(-5,1),main = "C", 
     cex.main = 2, bty = "n")

flexsurvreg(surv ~ 1, dist="exp")
flexsurvreg(surv ~ 1, dist="gompertz")

flexsurvreg(surv ~ 1, dist="gompertz")$coef

shape = flexsurvreg(surv ~ 1, dist="gompertz")$coef[1]
rate = flexsurvreg(surv ~ 1, dist="gompertz")$coef[2]

x = seq(1,13)
y = rate + x * shape
  
lines(x = x, y = y, lwd = 2) 
