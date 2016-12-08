### R code from vignette source 'Snowshoe.Rnw'

###################################################
### code chunk number 1: Snowshoe.Rnw:136-139
###################################################
## Read a textual model description
library(QPress)
modelA <- parse.text(c("V-*V","P-*P","V*->H","H*->P"))


###################################################
### code chunk number 2: Snowshoe.Rnw:144-147
###################################################
## Write/read a model description to a text file
write.text(modelA,"modelA.txt")
modelA <- model.text("modelA.txt")


###################################################
### code chunk number 3: Snowshoe.Rnw:152-154
###################################################
## Model is represented as a set of paired directed edges
modelA


###################################################
### code chunk number 4: Snowshoe.Rnw:166-171
###################################################
## Define variant models
modelB <- parse.text(c("V-*V","P-*P","V*->H","H*->P","V->P"))
modelC <- parse.text(c("V-*V","H-*H","P-*P","V*->H","H*->P","V->P"))
modelD <- parse.text(c("V-*V","P-*P","V*->H","H*->P","V*->P"))
modelE <- parse.text(c("V-*V","H-*H","P-*P","V*->H","H*->P","V*->P"))


###################################################
### code chunk number 5: Snowshoe.Rnw:179-183
###################################################
## Model with uncertain edges
modelBCDE <- parse.text(c("V-*V","P-*P","V*->H","H*->P","V->P",
                          "H--*H","V*--P"))
modelBCDE


###################################################
### code chunk number 6: Snowshoe.Rnw:194-197
###################################################
## Add self-limitation to all nodes
modelC <- enforce.limitation(modelB)
modelE <- enforce.limitation(modelD)


###################################################
### code chunk number 7: Snowshoe.Rnw:202-204
###################################################
## Signed adjacency matrix representation
adjacency.matrix(modelA,labels=T)


###################################################
### code chunk number 8: Snowshoe.Rnw:234-236
###################################################
## Simulate 1000 community matrices from model A
simA <- system.simulate(1000,modelA)


###################################################
### code chunk number 9: Snowshoe.Rnw:239-241 (eval = FALSE)
###################################################
## ## Interactively explore
## impact.barplot(simA)


###################################################
### code chunk number 10: Snowshoe.Rnw:255-258 (eval = FALSE)
###################################################
## ## Simulate 1000 community matrices from model B
## simB <- system.simulate(1000,modelB)
## impact.barplot(simB)


###################################################
### code chunk number 11: Snowshoe.Rnw:272-275 (eval = FALSE)
###################################################
## ## Simulate 1000 community matrices from the uncertain model
## simBCDE <- system.simulate(1000,modelBCDE)
## impact.barplot(simBCDE,epsilon=0.05)


###################################################
### code chunk number 12: Snowshoe.Rnw:283-292
###################################################
## Create figure for vignette
opar <- par(mfrow=c(2,2))
QPress:::impact.barplot.action(node.labels(modelA),simA$A,c(0,0,1),c(NA,NA,NA),main="Model A")
simB <- system.simulate(1000,modelB)
QPress:::impact.barplot.action(node.labels(modelB),simB$A,c(0,0,1),c(NA,NA,NA),main="Model B")
simBCDE <- system.simulate(1000,modelBCDE)
QPress:::impact.barplot.action(node.labels(modelBCDE),simBCDE$A,c(0,0,1),c(NA,NA,NA),main="Model BCDE")
QPress:::impact.barplot.action(node.labels(modelBCDE),simBCDE$A,c(0,0,1),c(1,NA,NA),main="Model BCDE, Hares -")
par(opar)


###################################################
### code chunk number 13: Snowshoe.Rnw:304-306 (eval = FALSE)
###################################################
## ## Investigate relation between weights and outcomes
## weight.density(simBCDE)


###################################################
### code chunk number 14: Snowshoe.Rnw:312-314
###################################################
## Adjacency matrix of uncertain model
adjacency.matrix(modelBCDE,required.groups=0:1)


###################################################
### code chunk number 15: Snowshoe.Rnw:341-346
###################################################
## Create figure for vignette
colnames(simBCDE$w) <- paste(unclass(simBCDE$edges$To),unclass(simBCDE$edges$From),sep=":")
QPress:::weight.density.action(simBCDE$A,simBCDE$w,
                               c(0,0,1),c(-1,NA,NA),
                               edges=c(T,T,T,F,T,T,T,T,T),slider=1)


###################################################
### code chunk number 16: Snowshoe.Rnw:359-370
###################################################
## Simulate 1000 community matrices from the uncertain model, st
## 1. A positive perturbation of P leads to a reduction in H
## 2. A positive perturbation of V leads to an increase in V
simBCDE1 <- system.simulate(1000,modelBCDE,
                            validators=list(
                              press.validate(modelBCDE,
                                             perturb=c(P=1),
                                             monitor=c(H=-1)),
                              press.validate(modelBCDE,
                                             perturb=c(V=1),
                                             monitor=c(V=1))))


###################################################
### code chunk number 17: Snowshoe.Rnw:390-392
###################################################
## Define prior probability for the competing models
prior <- c(B=3/16,C=3/16,D=5/16,E=5/16)


###################################################
### code chunk number 18: Snowshoe.Rnw:397-414
###################################################
## Compute the likelihood for each model
simB <- system.simulate(
  1000,modelB,validators=list(
                press.validate(modelB,perturb=c(V=1),monitor=c(H=-1))))
simC <- system.simulate(
  1000,modelC,validators=list(
                press.validate(modelC,perturb=c(V=1),monitor=c(H=-1))))
simD <- system.simulate(
  1000,modelD,validators=list(
                press.validate(modelD,perturb=c(V=1),monitor=c(H=-1))))
simE <- system.simulate(
  1000,modelE,validators=list(
                press.validate(modelE,perturb=c(V=1),monitor=c(H=-1))))
likelihood <- c(B=simB$accepted/simB$total,
                C=simC$accepted/simC$total,
                D=simD$accepted/simD$total,
                E=simE$accepted/simE$total)


###################################################
### code chunk number 19: Snowshoe.Rnw:417-420
###################################################
## Calculate posterior probabilities
posterior <- (prior*likelihood)/sum(prior*likelihood)
posterior


###################################################
### code chunk number 20: Snowshoe.Rnw:435-437
###################################################
## Make simulators
s <- community.sampler(modelBCDE)


###################################################
### code chunk number 21: Snowshoe.Rnw:447-449
###################################################
## Select uncertain edges
s$select(p=0.5)


###################################################
### code chunk number 22: Snowshoe.Rnw:455-458
###################################################
## Generate a community matrix
W <- s$community()
W


###################################################
### code chunk number 23: Snowshoe.Rnw:462-464
###################################################
## Extract weights
s$weights(W)


###################################################
### code chunk number 24: Snowshoe.Rnw:468-471
###################################################
## Labels of non-zero edges and uncertain edges
s$weight.labels
s$uncertain.labels


###################################################
### code chunk number 25: Snowshoe.Rnw:476-478
###################################################
## Test if equilibrium would be stable
stable.community(W)


###################################################
### code chunk number 26: Snowshoe.Rnw:483-487
###################################################
## Evaluate impact of a perturbation to V
impact <- press.impact(modelBCDE,perturb=c(V=1))
impact(W)
signum(impact(W))


###################################################
### code chunk number 27: Snowshoe.Rnw:493-497
###################################################
## Validation
## a positive perturbation to V should produce a reduction in H
g <- press.validate(modelBCDE,perturb=c(V=1),monitor=c(H=-1))
g(W)


