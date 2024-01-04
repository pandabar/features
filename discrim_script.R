data <- read.csv("discrimdata.csv")
#exclude bilinguals
discrimdata<- subset(data, subject!="jw5c" & subject!="fnoe")
#subset by contrast
iI<-subset(discrimdata, tributesto=="i/I"| tributesto2=="i/I")
uy<-subset(discrimdata, tributesto=="u/y"| tributesto2=="u/y")
ui<-subset(discrimdata, tributesto=="u/i"| tributesto2=="u/i")
yi<-subset(discrimdata, tributesto=="y/i"| tributesto2=="y/i")

#split by id
iInsid<-split(iI, as.character(iI$subject))
uynsid<-split(uy, as.character(uy$subject))
uinsid<-split(ui, as.character(ui$subject))
yinsid<-split(yi, as.character(yi$subject))

#contingency tables: spanish (I hope I can find a more elegant way of doing this)
for (i in 1:length(iInsid)) {
  iInsid[[i]]<-table(as.character(iInsid[[i]]$vowelcontrast), as.character(iInsid[[i]]$RESP))
}
for (i in 1:length(uynsid)) {
  uynsid[[i]]<-table(as.character(uynsid[[i]]$vowelcontrast), as.character(uynsid[[i]]$RESP))
}

for (i in 1:length(uinsid)) {
  uinsid[[i]]<-table(as.character(uinsid[[i]]$vowelcontrast), as.character(uinsid[[i]]$RESP))
}
for (i in 1:length(yinsid)) {
  yinsid[[i]]<-table(as.character(yinsid[[i]]$vowelcontrast), as.character(yinsid[[i]]$RESP))
}

#this loop collapses signals with noises and adds row and column names
# spanish
for (i in 1:length(iInsid)) {
  iInsid[[i]]<-rbind(iInsid[[i]][1,] + iInsid[[i]][3,],  iInsid[[i]][2,])+0.5
  dimnames(iInsid[[i]]) = list(c("noiseiI", "signaliI"), c("rsame", "rdiff"))
} 

for (i in 1:length(uynsid)) {
  uynsid[[i]]<-rbind(uynsid[[i]][1,] + uynsid[[i]][3,],  uynsid[[i]][2,])+0.5
  dimnames(uynsid[[i]]) = list(c("noiseuy", "signaluy"), c("rsame", "rdiff"))
} 

for (i in 1:length(uinsid)) {
  uinsid[[i]]<-rbind(uinsid[[i]][1,] + uinsid[[i]][3,],  uinsid[[i]][2,])+0.5
  dimnames(uinsid[[i]]) = list(c("noiseui", "signalui"), c("rsame", "rdiff"))
} 

for (i in 1:length(yinsid)) {
  yinsid[[i]]<-rbind(yinsid[[i]][1,] + yinsid[[i]][3,],  yinsid[[i]][2,])+0.5
  dimnames(yinsid[[i]]) = list(c("noiseyi", "signalyi"), c("rsame", "rdiff"))
}

#we calculate dprime
library(mysensR)
nsiddiI<-vector("list", length(iInsid))
for (i in 1:length(iInsid)) {
  nsiddiI[[i]]<-as.matrix(summary(mysensR::samediff(iInsid[[i]][1,1], iInsid[[i]][1,2], iInsid[[i]][2,1], iInsid[[i]][2,2]))[[10]])
}

nsidduy<-vector("list", length(uynsid))
for (i in 1:length(uynsid)) {
  nsidduy[[i]]<-as.matrix(summary(mysensR::samediff(uynsid[[i]][1,1], uynsid[[i]][1,2],uynsid[[i]][2,1], uynsid[[i]][2,2]))[[10]])
}

nsiddui<-vector("list", length(uinsid))
for (i in 1:length(uinsid)) {
  nsiddui[[i]]<-as.matrix(summary(mysensR::samediff(uinsid[[i]][1,1], uinsid[[i]][1,2],uinsid[[i]][2,1], uinsid[[i]][2,2]))[[10]])
}

nsiddyi<-vector("list", length(yinsid))
for (i in 1:length(yinsid)) {
  nsiddyi[[i]]<-as.matrix(summary(mysensR::samediff(yinsid[[i]][1,1], yinsid[[i]][1,2],yinsid[[i]][2,1], yinsid[[i]][2,2]))[[10]])
}

#and this loop creates a vector of dprime values
#spanish
dprimensiI<-vector("numeric", length(nsiddiI))
for (i in 1:length(nsiddiI)) { 
  dprimensiI[i]<-nsiddiI[[i]][2] 
}
dprimensuy<-vector("numeric", length(nsidduy))
for (i in 1:length(nsidduy)) { 
  dprimensuy[i]<-nsidduy[[i]][2] 
}
dprimensui<-vector("numeric", length(nsiddui))
for (i in 1:length(nsiddui)) { 
  dprimensui[i]<-nsiddui[[i]][2] 
}
dprimensyi<-vector("numeric", length(nsiddyi))
for (i in 1:length(nsiddyi)) { 
  dprimensyi[i]<-nsiddyi[[i]][2] 
}

#this creates a dataframe with dprimes
ids<-unique(as.character(discrimdata$subject))
df<-data.frame(id=ids, longshort=dprimensiI, frontback=dprimensui, rounded=dprimensuy, frontround=dprimensyi)
colnames(df)<-c("id", "/i-\u026A/", "/u-i/", "/u-y/", "/i-y/")
test<-data.frame(subj=rep(df$id, 4), contrast=stack(df, select= c(-id)))
colnames(test)<-c("subj", "dprime", "contrast")
#Stats, dprime
library(rstatix)
#effect
yesorno<- test %>% friedman_test(dprime ~ contrast |subj)
#effect size
test %>% friedman_effsize(dprime ~ contrast |subj)
#post-hoc comparison
pwc<- test %>% wilcox_test(dprime ~ contrast, paired=TRUE, p.adjust.method = "bonferroni", ref.group="/u-i/", detailed = T) #significant to alpha .05 with outlier, to 0.01 without

#Stats, RTs
library(car)
library(lme4)

discrimdata$rt<-as.numeric(discrimdata$rt)
discnoNA<-discrimdata[!is.na(discrimdata$rt),]
discnoNA$cont<-ifelse(discnoNA$vowelcontrast=="i_short/i_long", "i/\u026A", 
                      ifelse(discnoNA$vowelcontrast=="i_long/i_long","i/i",
                             ifelse(discnoNA$vowelcontrast=="i_short/i_short", "\u026A/\u026A",
                                    ifelse(discnoNA$vowelcontrast=="u/y", "u/y",
                                           ifelse(discnoNA$vowelcontrast=="y/y", "y/y",
                                                  ifelse(discnoNA$vowelcontrast=="u/u", "u/u",
                                                         ifelse(discnoNA$vowelcontrast=="u/i_long", "u/i", "i/y")))))))

discnoNA$trialType<-ifelse(discnoNA$sameordiffstims==0, "same", "different")

discnoNA$cont<-as.factor(discnoNA$cont)
discnoNA$subject<-as.factor(discnoNA$subject)
discnoNA$stimulus<-as.factor(discnoNA$stimulus)
discnoNA$cont<-relevel(discnoNA$cont, ref="u/i")

#model 1: all together
amodel2<-lmer(rt ~ cont + (1|subject) + (1|stimulus), data=discnoNA)
library(emmeans)
m2comp<-emmeans(amodel2, list(pairwise ~ cont))

#model 2: separated, only different trials
discnoNAdiff<-subset(discnoNA, trialType=="different")
amodel3<-lmer(rt ~ cont + (1|subject) + (1|stimulus), data=discnoNAdiff)
em3<-emmeans(amodel3, list(pairwise ~ cont))

#item analysis
discnoNA$RESP<-as.factor(discnoNA$RESP)
discnoNA$sameordiffstims<-as.factor(discnoNA$sameordiffstims)
discnoNA$correct<- ifelse(discnoNA$sameordiffstims == discnoNA$RESP, "correct", "incorrect")
itemreg<-glmer(RESP ~ stimulus + (1|subject), family=binomial, data=discnoNA)


#plots
library(ggplot2)
library(ggpubr)
p<-ggboxplot(test, x="contrast", y="dprime", add="jitter", color="black")
ggpar(p, xlab = "Contrast", ylab = "Sensitivity (d')")

rtplotdisc<-ggplot(discnoNA, aes(x=cont, y=rt, color=trialType)) + geom_jitter(aes(x=cont, y=rt), colour="grey", width=0.2) + geom_boxplot(outlier.shape = NA, alpha = 0.1) + theme_classic(base_size=12)
ggpar(rtplotdisc, xlab = "Stimuli pair", ylab = "Reaction time (ms)")
