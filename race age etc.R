## ----import, include=FALSE-----------------------------------------------
library(dplyr)
library(kableExtra)
knitr::opts_chunk$set(echo = TRUE)
smokeUrl = 'http://pbrown.ca/teaching/appliedstats/data/smoke.RData'
(smokeFile = tempfile(fileext='.RData'))
download.file(smokeUrl, smokeFile)
(load(smokeFile))
dim(smoke)
smokeSub = smoke[smoke$Age >= 10, ]
# exlcude NA
smokeAgg = smokeSub[complete.cases("chewing_tobacco_snuff_or", "Race","sex","age","RuralUrban"),]
# set Rural as baseline
smokeAgg$RuralUrban <- factor(smokeAgg$RuralUrban, levels=c("Rural","Urban"))
# centerize age at 15
smokeAgg$ageC = smokeAgg$Age - 15
smokeFit = glm(chewing_tobacco_snuff_or ~ ageC + Sex + Race + RuralUrban, 
	family=binomial(link='logit'), data=smokeAgg)

# coefficient table
parTable =  data.frame(summary(smokeFit)$coef[c(1,2,3,4,5,9),c(1,2,4)])
parTable$lower = parTable[,1] - 2*parTable[,2]
parTable$upper = parTable[,1] + 2*parTable[,2]
parTable[,c(1,2,4,5)] = round(parTable[,c(1,2,4,5)],3)
parTable$p_val = ifelse(parTable[,3]<0.05,"<0.05",">0.05")
parTable = parTable[,c(1,2,4,5,6)]
rownames(parTable) = c("Intercept","age","female","black","hispanic","Urban")
colnames(parTable) = c( "Estimate", "Std. Error", "p-value","lower","upper")

# OddsRatio table
smokeTable1 = as.data.frame(summary(smokeFit)$coef)
smokeTable1$lower = smokeTable1$Estimate - 2*smokeTable1$'Std. Error'
smokeTable1$upper = smokeTable1$Estimate + 2*smokeTable1$'Std. Error'
smokeOddsRatio1 = exp(smokeTable1[,c('Estimate','lower','upper')])
# focus only on interested variables
smokeOddsRatio1 = smokeOddsRatio1[c(1,2,3,4,5,9),]
rownames(smokeOddsRatio1) = c('baseline odds', 'age', 'female','black','hispanic','urban')
#smokeOddsRatio1[1,] = smokeOddsRatio1[1,]/(1+smokeOddsRatio1[,1]) # probability


## ----exclude9, echo=F----------------------------------------------------
table = table(smoke$Age, smoke$Tried_cigarette_smkg_even,exclude =NULL)[1:5,]
colnames(table) = c("Once","Twice","None")
knitr::kable(table, caption = "Age and Cigarette Times", booktabs = TRUE,longtable = T)


## ----hypoplot,message=F, echo=F, fig.height=3, fig.width=5, fig.cap='Proportion for regular use of chewing tobacco, snuff or dip for Rural Americans ',fig.align='center',fig.pos='H'----
smokeplotAgg = reshape2::dcast(smokeSub,
                               Age + Sex + Race + RuralUrban ~ chewing_tobacco_snuff_or,length)
smokeplotAgg = smokeplotAgg[complete.cases("chewing_tobacco_snuff_or", "Race","Sex","Age","RuralUrban"),]
smokeplotAgg = smokeplotAgg[which(smokeplotAgg$Race == 'white'|smokeplotAgg$Race == 'black'|smokeplotAgg$Race == 'hispanic'),]
smokeplotAgg$Race = factor(smokeplotAgg$Race)
smokeplotAgg$total = smokeplotAgg$"TRUE" + smokeplotAgg$"FALSE"
smokeplotAgg$prop = smokeplotAgg$"TRUE" / smokeplotAgg$total
smokeplotAgg = smokeplotAgg[which(smokeplotAgg$RuralUrban == "Rural" & smokeplotAgg$prop<1),]
#'
#' 
#+ smokeExplPlot
Spch = c('M' = 3, 'F'=1)
Scol = RColorBrewer::brewer.pal(nlevels(smokeplotAgg$Race), 'Set2')
names(Scol) = levels(smokeplotAgg$Race)
par(mar=c(2.5,2.5,0.1,0.1),
mgp=c(1.5, 0.5, 0), cex=0.8)
plot(smokeplotAgg$Age, smokeplotAgg$prop, pch = Spch[as.character(smokeplotAgg$Sex)],
     col = Scol[as.character(smokeplotAgg$Race)],
     xlab = "Age",
     ylab = "Proportion")
legend('topleft', fill=Scol, legend=names(Scol))
legend('left', pch=Spch, legend=names(Spch))


## ----hypoplot2,message=F, echo=F, fig.height=3, fig.width=5, fig.cap="Likelihood of having used a hookah or waterpipe on at least one occasion for White urban Americans ",fig.align='center',fig.pos='H'----
smokeplotAgg = reshape2::dcast(smokeSub,
                               Age + Sex + Race + RuralUrban ~ ever_tobacco_hookah_or_wa,length)
smokeplotAgg = smokeplotAgg[complete.cases("ever_tobacco_hookah_or_wa", "Race","Sex","Age","RuralUrban"),]
smokeplotAgg$total = smokeplotAgg$"TRUE" + smokeplotAgg$"FALSE"
smokeplotAgg$prop = smokeplotAgg$"TRUE" / smokeplotAgg$total
smokeplotAgg = smokeplotAgg[which(smokeplotAgg$RuralUrban == "Urban" & smokeplotAgg$Race=="white"),]
#'
#' 
#+ smokeExplPlot
Spch = c('M' = 3, 'F'=1)
par(mar=c(2.5,2.5,0.1,0.1),
mgp=c(1.5, 0.5, 0), cex=0.8)
plot(smokeplotAgg$Age, smokeplotAgg$prop, pch = Spch[as.character(smokeplotAgg$Sex)],
     xlab = "Age",
     ylab = "likelihood")
legend('left', pch=Spch, legend=names(Spch))


## ----Q2cof1,results='markup',echo=FALSE----------------------------------
kable(parTable, caption = "Parameter estimates for the binomial regression", booktabs = TRUE,longtable = T)


## ----Q2oddsratio1, results='markup',echo=FALSE---------------------------
knitr::kable(smokeOddsRatio1, digits=3, caption = " MLE’s of baseline odds and odds ratios, with 95 conidence intervals.", booktabs = TRUE,longtable = T)


## ----echo=F--------------------------------------------------------------
smokeAgg2 = smokeSub[complete.cases("ever_tobacco_hookah_or_wa", "Race","RuralUrban", "Age","Sex"),]
smokeAgg2$ageC = smokeAgg2$Age - 15

smokeFit2 = glm(ever_tobacco_hookah_or_wa ~ ageC + Sex + Race + RuralUrban, 
	family=binomial(link='logit'), data=smokeAgg2)


# coefficient table
parTable2 =  data.frame(summary(smokeFit2)$coef)
parTable2$lower = parTable2[,1] - 2*parTable2[,2]
parTable2$upper = parTable2[,1] + 2*parTable2[,2]
parTable2[,c(1,2,5,6)] = round(parTable2[,c(1,2,5,6)],3)
parTable2$p_val = ifelse(parTable2[,4]<0.05,"<0.05",">0.05")
parTable2 = parTable2[,c(1,2,5,6,7)]
rownames(parTable2) = c("intercept","age","female","black","hispanic","asian","native","pacific","Rural")
colnames(parTable2) = c( "Estimate", "Std. Error","lower","upper","p-value")


smokeTable2 = as.data.frame(summary(smokeFit2)$coef)
smokeTable2$lower = smokeTable2$Estimate - 2*smokeTable2$'Std. Error'
smokeTable2$upper = smokeTable2$Estimate + 2*smokeTable2$'Std. Error'
smokeOddsRatio2 = exp(smokeTable2[,c('Estimate','lower','upper')])
rownames(smokeOddsRatio2) = c('baseline odds',"age","female","black","hispanic","asian","native","pacific","Rural")
#smokeOddsRatio2[1,]/(1+smokeOddsRatio2[,1]) # probability


## ----Q2cof2,results='markup',echo=FALSE----------------------------------
kable(parTable2, caption = "Parameter estimates for the binomial regression", booktabs = TRUE,longtable = T)


## ----Q2oddsratio2, results='markup',echo=FALSE---------------------------
knitr::kable(smokeOddsRatio2, digits=3, caption = " MLE’s of baseline odds and odds ratios, with 95 conidence intervals.", booktabs = TRUE,longtable = T)

