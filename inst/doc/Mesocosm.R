### R code from vignette source 'Mesocosm.Rnw'

###################################################
### code chunk number 1: Mesocosm.Rnw:90-93
###################################################
library(QPress)
lake.a <- model.dia("Lake-a.dia")
lake.b <- model.dia("Lake-b.dia")


###################################################
### code chunk number 2: Mesocosm.Rnw:100-102 (eval = FALSE)
###################################################
## simlake.a <- system.simulate(1000,lake.a)
## impact.barplot(simlake.a)


###################################################
### code chunk number 3: Mesocosm.Rnw:115-121
###################################################
simlake.a <- system.simulate(1000,lake.a)
simlake.b <- system.simulate(1000,lake.b)
opar <- par(mfrow=c(1,2))
QPress:::impact.barplot.action(node.labels(lake.a),simlake.a$A,c(0,0,0,0,0,0,0,1),c(NA,NA,1,NA,1,NA,1,1),main="Model (a)")
QPress:::impact.barplot.action(node.labels(lake.b),simlake.b$A,c(0,0,0,0,0,0,0,1),c(NA,NA,1,NA,1,NA,1,1),main="Model (b)")
par(opar)


###################################################
### code chunk number 4: Mesocosm.Rnw:184-190
###################################################
## Enforce the ordering of nodes.
labels <- c("CO2","Fe","Macronutrients","Small phytoplankton",
            "Large phytoplankton","Heterotrophic nanoflagellates",
            "Microzooplankton","Bacteria","DOC")
antarctic.a <- model.dia("Antarctic-a.dia",labels=labels)
antarctic.a <- enforce.limitation(antarctic.a)


###################################################
### code chunk number 5: Mesocosm.Rnw:196-198 (eval = FALSE)
###################################################
## simantarctic.a <- system.simulate(1000,antarctic.a)
## impact.barplot(simantarctic.a)


###################################################
### code chunk number 6: Mesocosm.Rnw:210-212
###################################################
simantarctic.a <- system.simulate(1000,antarctic.a)
QPress:::impact.barplot.action(node.labels(antarctic.a),simantarctic.a$A,c(1,0,0,0,0,0,0,0,0),c(NA,NA,NA,1,-1,-1,NA,1,NA),main="Model (a)")


###################################################
### code chunk number 7: Mesocosm.Rnw:241-243
###################################################
antarctic.b <- model.dia("Antarctic-b.dia",labels=labels)
antarctic.b <- enforce.limitation(antarctic.b)


###################################################
### code chunk number 8: Mesocosm.Rnw:248-250 (eval = FALSE)
###################################################
## simantarctic.b <- system.simulate(1000,antarctic.b)
## impact.barplot(simantarctic.b)


###################################################
### code chunk number 9: Mesocosm.Rnw:264-266
###################################################
antarctic.c <- model.dia("Antarctic-c.dia",labels=labels)
antarctic.c <- enforce.limitation(antarctic.c)


###################################################
### code chunk number 10: Mesocosm.Rnw:273-275
###################################################
simantarctic.b <- system.simulate(1000,antarctic.b)
QPress:::impact.barplot.action(node.labels(antarctic.b),simantarctic.b$A,c(1,0,0,0,0,0,0,0,0),c(NA,NA,NA,1,-1,-1,NA,1,NA),main="Model (b)")


###################################################
### code chunk number 11: Mesocosm.Rnw:293-295
###################################################
simantarctic.c <- system.simulate(1000,antarctic.c)
QPress:::impact.barplot.action(node.labels(antarctic.c),simantarctic.c$A,c(1,0,0,0,0,0,0,0,0),c(NA,NA,NA,1,-1,-1,NA,1,NA),main="Model (c)")


