identdata <- read.csv("identdata.csv")
#Separate dataframes per contrast, so I can plot
iIident<-subset(identdata, vowelcontrast=="i/I")
calcIi<-data.frame(tapply(iIident$correct, list(iIident$subject, iIident$correct), length))
calcIi[is.na(calcIi)]<- 0
propsIi<-c(calcIi$X1/18)
Iimean<-mean(propsIi)
se<- function(x) sd(x)/sqrt(length(x))
seIi<-se(propsIi)

uyident<-subset(ns, vowelcontrast=="y/u")
calcuy<-data.frame(tapply(uyident$correct, list(uyident$subject, uyident$correct), length))
calcuy[is.na(calcuy)]<- 0
propsuy<-c(calcuy$X1/18)
uymean<-mean(propsuy)
seuy<-se(propsuy)

uiident<-subset(ns, vowelcontrast=="i/u")
calcui<-data.frame(tapply(uiident$correct, list(uiident$subject, uiident$correct), length))
calcui[is.na(calcui)]<- 0
propsui<-c(calcui$X1/18)
uimean<-mean(propsui)
seui<-se(propsui)

yiident<-subset(ns, vowelcontrast=="y/i")
calcyi<-data.frame(tapply(yiident$correct, list(yiident$subject, yiident$correct), length))
calcyi[is.na(calcyi)]<- 0
propsyi<-c(calcyi$X1/18)
yimean<-mean(propsyi)
seyi<-se(propsyi)

contrast<-c("/i-\u026A/", "/u-i/", "/u-y/", "/y-i/")
allmeans<-c(Iimean, uimean, uymean, yimean)
allses<-c(seIi, seui, seuy, seyi)
margin2<-qt(0.975,df=29-1)*allses/sqrt(29)
margin<-allses*0.95
my_stuff<-data.frame(contrast, allmeans, allses, margin)

#plot
library(ggplot2)
library(plotrix)
ggplot(my_stuff) +
  geom_bar(aes(x=contrast, y=allmeans), stat="identity", fill="gray", color="black") + labs(x="Contrast", y = "Correct (prop.)")+ theme_classic(base_size = 15) +
  geom_errorbar(aes(x=contrast, ymin=allmeans-margin, ymax=allmeans+margin), width=0.08, colour="black", alpha=0.9, size=1)

#reg
identdata$correct<-as.factor(identdata$correct)
identdata$subject<-as.factor(identdata$subject)
identdata$pic_choices<-as.factor(identdata$pic_choices)
identdata$vowelcontrast<-as.factor(identdata$vowelcontrast)
identdata$vowelcontrast<-relevel(identdata$vowelcontrast, ref = "i/u")

thisreg<-glmer(correct  ~ vowelcontrast + (1|subject) + (1|pic_choices), family=binomial, data=identdata)
a<-emmeans(thisreg, list(pairwise ~ vowelcontrast))

#regression, rts ident task

identdata$rt <- as.numeric(identdata$rt)
identnoNA <- identdata[!is.na(identdata$rt),]
thatreg<-lmer(rt  ~ vowelcontrast + (1|subject) + (1|pic_choices), data=identnoNA)
b<-emmeans(thatreg, list(pairwise ~ vowelcontrast))

#RTplot
rtplot<-ggplot(identnoNA, aes(x=vowelcontrast, y=rt)) + geom_jitter(aes(x=vowelcontrast, y=rt), colour="grey", width=0.2) + geom_boxplot(outlier.shape = NA, alpha = 0.1) + theme_classic(base_size=15)
ggpar(rtplot, xlab = "Contrast", ylab = "Reaction time (ms)")

