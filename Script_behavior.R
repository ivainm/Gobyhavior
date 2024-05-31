#Gobyhavior project
#Produces all figures and statistical tests for the manuscript
#The creation of the OSRdata dataset is in a companion script "Data_formatting.R"

setwd("C://Users//ivainm//Working_Document//DYNAMAR//Manuscripts//Behavior//Data")

#Load required packages
library(car)
library(ggplot2)
library(tidyr)
library(emmeans)
library(dplyr)
library(zoo)
library(lme4)
library(betareg)
library(emmeans)
library(reshape)
library(lsmeans)
library(effects)
library(ggeffects) 
library(moments)
library(forcats)
library(ggpubr)


# 0. Data import and formatting
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\
#=========================================================|
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\
#=========================================================|
#o-------------- Import census data --------------o
censdata<-read.delim("census.txt")
censdata$date2<-as.Date(censdata$date, format="%d.%m")
censdata$area<-as.factor(censdata$area)
censdata$location<-as.factor(censdata$location)
censdata$area<- factor(censdata$area, levels=c('Kristineberg', 'Arendal', 'Austevoll', 'Hitra', 'Helligvaer', 'Ringstad'))
str(censdata)
censdata$timepoint2<-as.factor(censdata$timepoint)
levels(censdata$timepoint2)<-c("Early", "Mid", "Late")
censdata$areashort<-censdata$area
levels(censdata$areashort)<-c('KBG', 'ARD', 'AUV', 'HIT', 'HEL', 'RIG')


#o-------------- Import nd format behavior and census data --------------o
OSRdata<-read.csv("OSRdata.csv")
OSRdata$timepoint2<- factor(OSRdata$timepoint2, levels=c("Early season", "Mid season", "Late season"))
OSRdata$area<- factor(OSRdata$area, levels=c('Kristineberg', 'Arendal', 'Austevoll', 'Hitra', 'Helligvaer', 'Ringstad'))
OSRdata$date<-as.Date(OSRdata$date)
str(OSRdata)
#----------------------------------
#I need to change the shape of the dataset so I can analyze both sexes together
#long data
colnames(OSRdata)
OSRMinit<-cbind(OSRdata[,c("area" , "location" ,  "timepoint" ,"timepoint2", "M_init" ,"transectlength" ,"Nm", "Om",   "OSR23" , "Mcourtprop","MCproprel","MCtotrel",
                           "MM","Magoproba", "MMprop", "Magoprobarel","Magoprobatotrel", "Density" )],sex="m")
OSRFinit<-cbind(OSRdata[,c("area" , "location" ,  "timepoint" , "timepoint2", "F_init" ,"transectlength" ,"Nf","Of23",  "OSR23", "Fcourtprop","FCproprel", "FCtotrel",
                           "FF","Fagoproba", "FFprop23" , "Fagoprobarel","Fagoprobatotrel","Density"  )],sex="f")
colnames(OSRMinit)[5]<-"Court_init" #absolute count
colnames(OSRFinit)[5]<-"Court_init"
colnames(OSRMinit)[7]<-"Sex_census" #sex specific census
colnames(OSRFinit)[7]<-"Sex_census"
colnames(OSRMinit)[8]<-"OF" #operational fraction
colnames(OSRFinit)[8]<-"OF"
colnames(OSRMinit)[10:12]<-c("Courtprop","Courtrel","Courtreltot") #Count divided by Df*Dm*transect_length / relative so max is 1 / estimate total number of encounters
colnames(OSRFinit)[10:12]<-c("Courtprop","Courtrel","Courtreltot")
colnames(OSRMinit)[13:17]<-c("Ago","Agoproba","AgoProp","Agoprobarel","Agoprobareltot")
colnames(OSRFinit)[13:17]<-c("Ago","Agoproba","AgoProp","Agoprobarel","Agoprobareltot")
str(OSRFinit)
OSRlong<-rbind(OSRMinit,OSRFinit)
OSRlong$sex<-as.factor(OSRlong$sex)
OSRlong$timepoint2<- factor(OSRlong$timepoint2, levels=c("Early season", "Mid season", "Late season"))
OSRlong$area<- factor(OSRlong$area, levels=c('Kristineberg', 'Arendal', 'Austevoll', 'Hitra', 'Helligvaer', 'Ringstad'))
OSRlong$location<-as.factor(OSRlong$location)
OSRlong<-OSRlong[!is.na(OSRlong$Courtprop),]
str(OSRlong)


# 1. Rates of sexual display behaviour over time
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\
#=========================================================|
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\
#=========================================================|


#1.a Courtship behaviours
#=========================================================|
#o-------------- Statistical Model --------------o
#Binomial model of the effect of sampling time, population and sex on courthip initiation rate

modcourttime<-glm(data=OSRlong, cbind(Court_init,Sex_census-Court_init)~timepoint+sex+area+
                    sex:timepoint + sex:area+area:timepoint+
                    sex:timepoint:area,family="binomial")

#This next line is to get the model fit into a nice data frame you can use in the plots
fittime<-as.data.frame(effect("timepoint:sex:area",modcourttime,x.var="timepoint", xlevels=20))

#Anova
Anova(modcourttime, type=3)
#Plot the model effects and perform post-hoc tests
#females do more behaviors than in Austevoll, less in Hitra, then none
plot(effect("sex:area",modcourttime), multiline=TRUE)
#This test does a pairwise comparison fo the sexes, for each area
emmeans(modcourttime, list(pairwise ~ sex|area), adjust = "tukey")
#males show a general trend of decreasing over time, females no trend
plot(effect("timepoint:sex",modcourttime,x.var="timepoint"), multiline=TRUE)
#males have a significant negative slope, females not significant
m.lst <- lstrends(modcourttime, "sex", var="timepoint") #get slopes for each sex
summary(m.lst, infer=c(TRUE,TRUE),null=0) #compare each slope to zero 

m.lst <- lstrends(modcourttime, ~sex:area, var="timepoint") #get slopes for each sex in each area
summary(m.lst, infer=c(TRUE,TRUE),null=0) #compare slopes to zero

#o-------------- Manuscript figure --------------o
#Figure 2
#Version with North populations up and South populations down,need to reorder factor levels
ggplot(data=OSRlong,aes(x=timepoint2, group=(sex)))+
  geom_boxplot(aes(y=Court_init/Sex_census, fill=sex,group=timepoint2:factor(sex)),alpha=0.3, width=0.5,position="dodge")+
  facet_wrap(~fct_rev(area), ncol=2, scales="free_y")+
  scale_fill_manual("Sex",values=c("goldenrod1","mediumpurple1"))+
  scale_shape_manual("Sex",values=c(24,22))+
  scale_color_manual("Sex",values=c("goldenrod1","mediumpurple1"))+
  #This next line does the confidence interval around the model fit
  geom_ribbon(data=fittime, aes(x = timepoint, ymin=fit-se, ymax=fit+se, fill=sex), alpha=0.2) + 
  #This next line does the model fit
  geom_line(data=fittime,aes(x=timepoint, y=fit, group=sex, color=sex), size=1.5, alpha=0.5)+
  #This plots the actual data
  geom_point(size=2,colour = "black",aes(y=Court_init/Sex_census, fill=sex, shape=sex),position=position_jitterdodge(jitter.width=0.20,jitter.height=0, dodge.width=0.5))+
  labs(x="Time period",y="Courtship iniation rate, density corrected",title="")+
  ylim(0,0.27)+
  theme(axis.text.x = element_text(angle=25, hjust = 1))


#1.b Same-sex agonistic behaviours
#=========================================================|
#o-------------- Statistical Model --------------o
modagotime<-glm(data=OSRlong, cbind(Ago,Sex_census-Ago)~timepoint+sex+area+
                  sex:timepoint + sex:area+area:timepoint+
                  sex:timepoint:area,family="binomial")
#storing model fit for plotting on figure 3
fittime2<-as.data.frame(effect("timepoint:sex:area",modagotime,x.var="timepoint", xlevels=20))
#Anova
Anova(modagotime, type=3)
#Plot relevant effects and perform post-hoc tests
#females do more behaviors than males in Auv
plot(effect("sex:area",modagotime), multiline=TRUE)
emmeans(modagotime, list(pairwise ~ sex|area), adjust = "tukey")
#males show a general trend of decreasing over time, females no trend
plot(effect("timepoint:sex",modagotime,x.var="timepoint"), multiline=TRUE)
#males have a significant negative slope, females not significant
m.lst <- lstrends(modagotime, "sex", var="timepoint") #get slopes
summary(m.lst, infer=c(TRUE,TRUE),null=0) #compare to zero
#Compare slopes?
plot(effect("timepoint:sex:area",modagotime,x.var="timepoint"), multiline=TRUE)
emtrends(modagotime, pairwise ~ sex | area, var = "timepoint",adjust = "tukey")
#KBG, ARD, m-f different slopes, Auv marginal, others non-s.
#In the case of Hell and Rig because no data in females
m.lst <- lstrends(modagotime, ~sex:area, var="timepoint") #get slopes
summary(m.lst, infer=c(TRUE,TRUE),null=0) #compare to zero
#All slopes tested against zero

#o-------------- Manuscript figure --------------o
#Figure 3
ggplot(data=OSRlong,aes(x=timepoint2, group=(sex)))+
  geom_boxplot(aes(y=Ago/Sex_census, fill=sex,group=timepoint2:factor(sex)),alpha=0.3, width=0.5,position="dodge")+
  facet_wrap(~fct_rev(area), ncol=2, scales="free_y")+
  scale_fill_manual("Sex",values=c("goldenrod1","mediumpurple1"))+
  scale_shape_manual("Sex",values=c(24,22))+
  scale_color_manual("Sex",values=c("goldenrod1","mediumpurple1"))+
  geom_ribbon(data=fittime2, aes(x = timepoint, ymin=fit-se, ymax=fit+se, fill=sex), alpha=0.2) + 
  geom_line(data=fittime2,aes(x=timepoint, y=fit, group=sex, color=sex), size=1.5, alpha=0.5)+
  geom_point(size=2,colour = "black",aes(y=Ago/Sex_census, fill=sex, shape=sex),position=position_jitterdodge(jitter.width=0.20,jitter.height=0, dodge.width=0.5))+
  labs(x="Time period",y="Agonistic behavior rate, density corrected",title="")+
  ylim(0,0.16)+
  theme(axis.text.x = element_text(angle=25, hjust = 1))




# 2. Social environment (OSR) over time
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\
#=========================================================|
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\
#=========================================================|

#o-------------- Manuscript figure --------------o
#Figure 4
ggplot(data=OSRdata, aes(x=timepoint2,y=OSR23 ))+
  stat_summary(fun.y = median, geom = 'line', aes(group = fct_rev(area), colour =fct_rev(area)),
               size=3,position=position_dodge(w=0.5),
               alpha = 0.35)+
  geom_point(shape=21, aes(y=OSR23,size=Density, fill=fct_rev(area), group=fct_rev(area)),
             position=position_dodge(w=0.5),show.legend = T, alpha=0.4)+
  geom_boxplot(aes( fill=fct_rev(area)),width=0.25, alpha=0.35, outlier.shape=NA,position=position_dodge(w=0.5))+
  geom_hline(yintercept=0.5, linetype="solid", size=1, alpha=0.2)+
  geom_hline(yintercept=1/5, linetype="dashed", size=1, alpha=0.5)+
  geom_text(aes(group=fct_rev(area),label=ifelse((Census)<31, Census, '')),position=position_dodge(w=0.5), hjust=-0.2, vjust=1.)+
  #geom_text(aes(group=area,label=ifelse(visibility=="bad", "b", '')),position=position_dodge(w=0.5), color="red",hjust=-0.3, vjust=0.)+
  scale_colour_manual("Population",values = c( "steelblue1", "blue3", "mediumpurple1", "darkorchid4","brown1","red3"))+
  scale_fill_manual("Population",values =  c( "steelblue1", "blue3", "mediumpurple1", "darkorchid4","brown1","red3"))+
  scale_size_continuous("Density (fish/m)",breaks=c(1,2,5))+
  labs(x="Time period",y="OSR",title="")


#o-------------- Suppl. figure --------------o
#Suppl. Figure S4
ggplot(data=OSRdata, aes(x=timepoint,y=OSR23 ))+
  geom_point(shape=21, aes(y=OSR23,size=Density, fill=area, group=location),
             position=position_dodge(w=0.2),show.legend = T, alpha=0.4)+
  facet_wrap(~area)+
  geom_line(aes(y=OSR23, group=location, color=area),
            size=1,position=position_dodge(w=0.2),
            alpha = 0.4)+
  #geom_boxplot(aes( fill=area),width=0.25, alpha=0.2, outlier.shape=NA)+
  geom_hline(yintercept=0.5, linetype="solid", size=1, alpha=0.2)+
  geom_hline(yintercept=1/5, linetype="dashed", size=1, alpha=0.5)+
  #geom_text(aes(label=ifelse(Density<31, Density, '')),position=position_dodge(0.5), hjust=-0.2, vjust=1.)+
  scale_colour_manual("Population",values = c("red3","brown1", "darkorchid4", "mediumpurple1", "blue3", "steelblue1"))+
  scale_fill_manual("Population",values = c("red3","brown1", "darkorchid4", "mediumpurple1", "blue3", "steelblue1"))+
  labs(x="Time period",y="OSR",title="")

#o-------------- Statistical model --------------o
#binomial regression with glmer
modOSR<-glmer(cbind(m,f2+f3)~timepoint2*areashort+(1|location),data=censdata, family="binomial")
summary(modOSR)
Anova(modOSR, type=3)

emmeans(modOSR, list(pairwise ~ areashort), adjust = "tukey")
emmeans(modOSR, list(pairwise ~ timepoint2), adjust = "tukey")
emmeans(modOSR, list(pairwise ~ timepoint2:areashort), adjust = "tukey")
#make a table to plot a nice heatmap of pariwise comparison pvalues
p.val.test<-pwpm(emmeans(modOSR, list(pairwise ~ areashort:timepoint2), adjust = "tukey"),means = FALSE, flip = T,reverse = T) # p-values presented compactly in matrix form

p.val.test<-sub("[<>]", "", p.val.test)
p.matx<-matrix(as.numeric((p.val.test)),nrow = length(p.val.test[,1]),ncol = length(p.val.test[,1])) #if your factor has 5 levels ncol and nrow=5
rownames(p.matx) <- colnames(p.matx) <-colnames(p.val.test)
p.matx[upper.tri(p.matx, diag=FALSE)] <- NA
heatmap1<-melt(p.matx)
melt(p.matx)

#o-------------- Suppl. figure --------------o
#Suppl. Figure S6
ggplot(data=heatmap1,aes(X1, forcats::fct_rev(X2), fill = value)) + geom_tile() +
  geom_text(aes(label = value), size=3)+
  scale_fill_gradientn(colours = c("limegreen","white", "darkgrey"),
                       values = c(0,0.6,0.95,1), trans="log10")+
  labs(x="",y="")+
  scale_x_discrete(position = "top")+
  theme(axis.text.x = element_text(angle = 45, vjust = -1, hjust=0))

#o-------------- Suppl. figure --------------o
#Suppl. Figure S3
ggplot(data=OSRdata, aes(x=timepoint2))+
  facet_wrap(~area, scales="fixed")+
  geom_boxplot(aes(y=(Density*(1-Om))), fill="goldenrod1",show.legend = FALSE, outlier.shape=NA,alpha=0.2)+
  geom_boxplot(aes(y=(Density*Om)), fill="mediumpurple1",show.legend = FALSE,outlier.shape=NA, alpha=0.22)+
  geom_point(shape=24, aes(y=(Density*Om)), fill="mediumpurple1",position=position_jitter(width=0.15),show.legend = FALSE, alpha=0.4)+
  geom_point(shape=23, aes(y=(Density*(1-Om))), fill="goldenrod1",position=position_jitter(width=0.15),show.legend = FALSE, alpha=0.4)+
  
  geom_smooth(aes(y=(Density*Om), group=area),color="mediumpurple1",alpha=0)+
  geom_smooth(aes(y=(Density*(1-Om)), group=area),color="goldenrod1",alpha=0)+
  scale_y_continuous(trans="log", breaks=c(0.01,0.1,0.5,1,2,5))+
  labs(x="Time period",y="Density (fish/m)",title="Male and female densities")


# 3. Sex-specific behavioural reaction norms
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\
#=========================================================|
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\
#=========================================================|

#3.a Courtship behaviours
#=========================================================|
#o-------------- Statistical model --------------o
#Model with binomial family and weights - outliers not removed
modcourtAll<-glmer(data=OSRlong, Courtrel~OSR23+sex+timepoint2+
                     sex:timepoint2+OSR23:sex+
                     OSR23:sex:timepoint2+
                     (1|location),
                   weights=Courtreltot,family="binomial",
                   control=glmerControl(optimizer="bobyqa",
                                        optCtrl=list(maxfun=2e5)))
summary(modcourtAll)
Anova(modcourtAll,type=3)
plot(effect("OSR23*sex",modcourtAll))
plot(effect("sex*timepoint2",modcourtAll), multiline=TRUE)
plot(effect("OSR23*sex*timepoint2",modcourtAll,x.var="OSR23"), multiline=TRUE)

#post hocs
#OSR by sex
emtrends(modcourtAll, pairwise ~ sex, var = "OSR23",adjust = "tukey")
#sex by timepoint
pairs(emmeans(modcourtAll, ~ sex | timepoint2,adjust = "tukey"))

OSR.lst <- lstrends(modcourtAll, ~ sex:timepoint2, var = "OSR23")
OSR.lst        # slope estimates and CIs
pairs(OSR.lst)   # comparisons
pairs(emmeans(modcourtAll, ~ sex | timepoint2,adjust = "tukey"))
#
emtrends(modcourtAll, pairwise ~ timepoint2 |sex, var = "OSR23")

#residuals
hist(residuals(modcourtAll),breaks=20)
skewness(residuals(modcourtAll))
kurtosis(residuals(modcourtAll))
#skew suggest that I remove some outliers


#Model with binomial family and weights - outliers removed
modcourtAllout<-glmer(data=OSRlong[OSRlong$Courtrel<0.6 | (OSRlong$timepoint2=="Mid season" & OSRlong$OSR23<0.75),],
                      Courtrel~OSR23+sex+timepoint2+
                        sex:timepoint2+OSR23:sex+
                        OSR23:sex:timepoint2+
                        (1|location),
                      weights=Courtreltot,family="binomial",
                      control=glmerControl(optimizer="bobyqa",
                                           optCtrl=list(maxfun=2e5)))
summary(modcourtAllout)
skewness(residuals(modcourtAllout))
kurtosis(residuals(modcourtAllout))
Anova(modcourtAllout,type=3)
plot(effect("OSR23*sex",modcourtAll))
plot(effect("sex*timepoint2",modcourtAll), multiline=TRUE)
plot(effect("OSR23*sex*timepoint2",modcourtAll,x.var="OSR23"), multiline=TRUE)

hist(residuals(modcourtAllout),breaks=20)


#o-------------- Manucript figure--------------o
#Figure 5
#Extract model fits from both version with and without outliers
fitAll<-as.data.frame(effect("OSR23*sex*timepoint2",modcourtAll,xlevels=20))
fitAllout<-as.data.frame(effect("OSR23*sex*timepoint2",modcourtAllout,xlevels=20))
top<-max(OSRdata$Density)


OSRlong$Behaviors<-OSRlong$Court_init
AFLL=ggplot(data=fitAll[fitAll$sex=="f",],aes(x=OSR23,y=fit))+
  geom_point(shape=21,data=OSRlong[OSRlong$sex=="f",],aes(x=OSR23, y=Courtrel, size=Behaviors, fill=Density), alpha=0.5)+
  scale_fill_gradientn(colours = c("white","darkslateblue","darkblue", "darkblue"),
                       values = c(0,0.75/top,5/top,1))+
  geom_line(data=fitAllout[fitAllout$sex=="f",],color="darkgrey",linetype=2, alpha=0.8, size=1)+
  geom_line(color="goldenrod1", alpha=0.7, size=2)+facet_wrap(~timepoint2, nrow=1)+
  geom_vline(xintercept=0.5, linetype="solid", size=1, alpha=0.2)+
  geom_vline(xintercept=1/5, linetype="dashed", size=1, alpha=0.4)+
  #scale_y_continuous(trans="log", limits=c(0.01,1))+
  geom_point(data=OSRlong[OSRlong$sex=="f" & (OSRlong$Courtrel>=0.6 | (OSRlong$timepoint=="Mid season" & OSRlong$OSR23>0.75)),],aes(x=OSR23, y=Courtrel), size=2, shape=4,alpha=0.8, color="red")+
  labs(x="OSR (Social environment cue)",y="Courtship propensity",title="a. female courtship ")
BFLL=ggplot(data=fitAll[fitAll$sex=="m",],aes(x=OSR23,y=fit))+
  geom_point(shape=21,data=OSRlong[OSRlong$sex=="m",],aes(x=OSR23, y=Courtrel, size=Behaviors, fill=Density), alpha=0.5)+
  scale_fill_gradientn(colours =  c("white","darkslateblue","darkblue", "darkblue"),
                       values = c(0,0.75/top,5/top,1))+
  geom_vline(xintercept=0.5, linetype="solid", size=1, alpha=0.2)+
  geom_vline(xintercept=1/5, linetype="dashed", size=1, alpha=0.4)+
  geom_line(data=fitAllout[fitAllout$sex=="m",],color="darkgrey",linetype=2, alpha=0.8, size=1)+
  geom_line(color="mediumpurple1", alpha=0.7, size=2)+facet_wrap(~timepoint2, nrow=1)+
  geom_point(data=OSRlong[OSRlong$sex=="m" & (OSRlong$Courtrel>=0.6 | (OSRlong$timepoint=="Mid season" & OSRlong$OSR23>0.75)),],aes(x=OSR23, y=Courtrel), size=2, shape=4,alpha=0.8, color="red")+
  labs(x="OSR (Social environment cue)",y="Courtship propensity",title="a. male courtship ")
ggarrange(AFLL, BFLL, nrow = 2,common.legend = TRUE, legend="right")


#3.b Same-sex agonistic behaviours
#=========================================================|
#o-------------- Statistical model --------------o
#Model with binomial family and weights -  Outliers not removed, 3-way interaction included (not final model)
modcourtAgoProp<-glmer(data=OSRlong,
                       AgoProp~OSR23+sex+timepoint2+
                         sex:timepoint2+OSR23:sex+
                         timepoint2:OSR23:sex+
                         (1|location),
                       weights=Agoprobareltot,family="binomial",
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl=list(maxfun=2e5)))
summary(modcourtAgoProp)
Anova(modcourtAgoProp,type=3)
plot(effect("OSR23*sex",modcourtAgoProp))
plot(effect("sex*timepoint2",modcourtAgoProp), multiline=TRUE)
plot(effect("OSR23*sex*timepoint2",modcourtAgoProp,x.var="OSR23"), multiline=TRUE)
#Model with binomial family and weights -  Outliers removed, 3-way interaction included (not final model)
modcourtAgoPropout<-glmer(data=OSRlong[OSRlong$AgoProp<=0.5 | (OSRlong$timepoint2=="Mid season" & OSRlong$OSR23<0.75) ,],
                          AgoProp~OSR23+sex+timepoint2+
                            sex:timepoint2+OSR23:sex+
                            timepoint2:OSR23:sex+
                            (1|location),
                          weights=Agoprobareltot,family="binomial",
                          control=glmerControl(optimizer="bobyqa",
                                               optCtrl=list(maxfun=2e5)))
Anova(modcourtAgoPropout,type=3)

#Model with binomial family and weights -  Outliers not removed, 3-way interaction removed ( not final model)
modcourtAgoPropshort<-glmer(data=OSRlong,
                            AgoProp~OSR23+sex+timepoint2+
                              sex:timepoint2+OSR23:sex+
                              (1|location),
                            weights=Agoprobareltot,family="binomial",
                            control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5)))
summary(modcourtAgoPropshort)
Anova(modcourtAgoPropshort,type=3)

#Model with binomial family and weights -  Outliers removed, 3-way interaction removed ( final model)
modcourtAgoPropoutshort<-glmer(data=OSRlong[OSRlong$AgoProp<=0.5 | (OSRlong$timepoint2=="Mid season" & OSRlong$OSR23<0.75) ,],
                               AgoProp~OSR23+sex+timepoint2+
                                 sex:timepoint2+OSR23:sex+
                                 (1|location),
                               weights=Agoprobareltot,family="binomial",
                               control=glmerControl(optimizer="bobyqa",
                                                    optCtrl=list(maxfun=2e5)))
Anova(modcourtAgoPropoutshort,type=3)
plot(effect("OSR23*sex",modcourtAgoPropoutshort))
plot(effect("sex*timepoint2",modcourtAgoPropoutshort), multiline=TRUE)
plot(effect("timepoint2",modcourtAgoProp))
#post hocs
#OSR by sex- comparing slopes! Males positive females negative
emtrends(modcourtAgoPropoutshort, pairwise ~ sex, var = "OSR23",adjust = "tukey")
#sex by timepoint - Males higher than females early and mid females higher late. All signif.
pairs(emmeans(modcourtAgoPropoutshort, ~ sex | timepoint,adjust = "tukey"))

#for model comparisons
summary(modcourtAgoProp)
skewness(residuals(modcourtAgoProp))
kurtosis(residuals(modcourtAgoProp))

summary(modcourtAgoPropout)
skewness(residuals(modcourtAgoPropout))
kurtosis(residuals(modcourtAgoPropout))

summary(modcourtAgoPropshort)
skewness(residuals(modcourtAgoPropshort))
kurtosis(residuals(modcourtAgoPropshort))

summary(modcourtAgoPropoutshort)
skewness(residuals(modcourtAgoPropoutshort))
kurtosis(residuals(modcourtAgoPropoutshort))

#o-------------- Manucript figure--------------o
#Figure 6
#Extract model fits from both version with and without outliers
fitAllAP<-as.data.frame(effect("OSR23*sex*timepoint2",modcourtAgoProp,xlevels=20))
fitAllAoutP<-as.data.frame(effect("OSR23*sex*timepoint2",modcourtAgoPropout,xlevels=20))

OSRlong$Behaviors<-OSRlong$Ago
AFLLAP=ggplot(data=fitAllAP[fitAllAP$sex=="f",],aes(x=OSR23,y=fit))+
  geom_point(shape=21,data=OSRlong[OSRlong$sex=="f",],aes(x=OSR23, y=AgoProp, size=Behaviors, fill=Density), alpha=0.5)+
  scale_fill_gradientn(colours = c("white","darkslateblue","darkblue", "darkblue"),
                       values = c(0,0.75/top,5/top,1))+
  geom_line(data=fitAllAoutP[fitAllAoutP$sex=="f",],color="darkgrey",linetype=2, alpha=0.8, size=1)+
  geom_line(color="goldenrod1", alpha=0.7, size=2)+facet_wrap(~timepoint2, nrow=1)+
  geom_vline(xintercept=0.5, linetype="solid", size=1, alpha=0.2)+
  geom_vline(xintercept=1/5, linetype="dashed", size=1, alpha=0.4)+
  guides( fill = guide_colorbar(order = 1),
          size = guide_legend(order = 2))+
  ylim(0,0.25)+
  geom_point(data=OSRlong[OSRlong$sex=="f" &  (OSRlong$AgoProp>=0.5 | (OSRlong$timepoint2=="Mid season" & OSRlong$OSR23>0.75)) ,],aes(x=OSR23, y=AgoProp), size=2, shape=4,alpha=0.8, color="red")+
  labs(x="OSR (Social environment cue)",y="Agonistic propensity",title="a. female agonistic behavior ")
BFLLAP=ggplot(data=fitAllAP[fitAllAP$sex=="m",],aes(x=OSR23,y=fit))+
  geom_point(shape=21,data=OSRlong[OSRlong$sex=="m",],aes(x=OSR23, y=AgoProp, size=Behaviors, fill=Density), alpha=0.5)+
  scale_fill_gradientn(colours = c("white","darkslateblue","darkblue", "darkblue"),
                       values = c(0,0.75/top,5/top,1))+
  geom_vline(xintercept=0.5, linetype="solid", size=1, alpha=0.2)+
  geom_vline(xintercept=1/5, linetype="dashed", size=1, alpha=0.4)+
  geom_line(data=fitAllAoutP[fitAllAoutP$sex=="m",],color="darkgrey",linetype=2, alpha=0.8, size=1)+
  geom_line(color="mediumpurple1", alpha=0.7, size=2)+facet_wrap(~timepoint2, nrow=1)+
  guides( fill = guide_colorbar(order = 1),
          size = guide_legend(order = 2))+
  ylim(0,0.25)+
  geom_point(data=OSRlong[OSRlong$sex=="m" & (OSRlong$AgoProp>=0.5 | (OSRlong$timepoint2=="Mid season" & OSRlong$OSR23>0.75)) ,],aes(x=OSR23, y=AgoProp), size=2, shape=4,alpha=0.8, color="red")+
  labs(x="OSR (Social environment cue)",y="Agonistic propensity",title="a. male agonistic behavior ")
ggarrange(AFLLAP, BFLLAP, nrow = 2,common.legend = TRUE, legend="right")

#o-------------- Suppl. figure--------------o
#Suppl. figure S7
#Same as figure 6 just above but the ylim range is (0,1) instead of (0,0.25)
AFLLAP=ggplot(data=fitAllAP[fitAllAP$sex=="f",],aes(x=OSR23,y=fit))+
  geom_point(shape=21,data=OSRlong[OSRlong$sex=="f",],aes(x=OSR23, y=AgoProp, size=Behaviors, fill=Density), alpha=0.5)+
  scale_fill_gradientn(colours = c("white","darkslateblue","darkblue", "darkblue"),
                       values = c(0,0.75/top,5/top,1))+
  geom_line(data=fitAllAoutP[fitAllAoutP$sex=="f",],color="darkgrey",linetype=2, alpha=0.8, size=1)+
  geom_line(color="goldenrod1", alpha=0.7, size=2)+facet_wrap(~timepoint2, nrow=1)+
  geom_vline(xintercept=0.5, linetype="solid", size=1, alpha=0.2)+
  geom_vline(xintercept=1/5, linetype="dashed", size=1, alpha=0.4)+
  guides( fill = guide_colorbar(order = 1),
          size = guide_legend(order = 2))+
  ylim(0,1)+
  geom_point(data=OSRlong[OSRlong$sex=="f" &  (OSRlong$AgoProp>=0.5 | (OSRlong$timepoint2=="Mid season" & OSRlong$OSR23>0.75)) ,],aes(x=OSR23, y=AgoProp), size=2, shape=4,alpha=0.8, color="red")+
  labs(x="OSR (Social environment cue)",y="Agonistic propensity",title="a. female agonistic behavior ")
BFLLAP=ggplot(data=fitAllAP[fitAllAP$sex=="m",],aes(x=OSR23,y=fit))+
  geom_point(shape=21,data=OSRlong[OSRlong$sex=="m",],aes(x=OSR23, y=AgoProp, size=Behaviors, fill=Density), alpha=0.5)+
  scale_fill_gradientn(colours = c("white","darkslateblue","darkblue", "darkblue"),
                       values = c(0,0.75/top,5/top,1))+
  geom_vline(xintercept=0.5, linetype="solid", size=1, alpha=0.2)+
  geom_vline(xintercept=1/5, linetype="dashed", size=1, alpha=0.4)+
  geom_line(data=fitAllAoutP[fitAllAoutP$sex=="m",],color="darkgrey",linetype=2, alpha=0.8, size=1)+
  geom_line(color="mediumpurple1", alpha=0.7, size=2)+facet_wrap(~timepoint2, nrow=1)+
  guides( fill = guide_colorbar(order = 1),
          size = guide_legend(order = 2))+
  ylim(0,1)+
  geom_point(data=OSRlong[OSRlong$sex=="m" & (OSRlong$AgoProp>=0.5 | (OSRlong$timepoint2=="Mid season" & OSRlong$OSR23>0.75)) ,],aes(x=OSR23, y=AgoProp), size=2, shape=4,alpha=0.8, color="red")+
  labs(x="OSR (Social environment cue)",y="Agonistic propensity",title="a. male agonistic behavior ")
ggarrange(AFLLAP, BFLLAP, nrow = 2,common.legend = TRUE, legend="right")

###
# 4. Other suppl. figures
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\
#=========================================================|
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\
#=========================================================|

#o-------------- Suppl. figure --------------o
#Suppl. Figure S5
ggplot(data=OSRdata, aes(x=timepoint2, y=Om, color=area, group=area))+
  geom_point(aes(size=Census, fill=area), shape=22, alpha=0.3, position=position_jitterdodge())+
  #scale_x_date(date_labels = "%B",breaks="month")+
  #geom_vline(xintercept=as.Date("2022-04-24"), linetype="dashed", size=1, alpha=0.4)+
  #geom_vline(xintercept=as.Date("2022-07-25"), linetype="dashed", size=1, alpha=0.4)+
  geom_hline(yintercept=0.5)+
  theme(axis.text.x = element_text(angle=0, hjust = 1), legend.position = "right",plot.margin = margin(0.1,1,0.1,0.1, "cm"))+
  #geom_smooth (aes(color=area),alpha=0.1, size=0)+
  #stat_smooth (aes(color=area),geom="line", alpha=0.2, size=3, span=1.5) +
  #stat_smooth(aes(y=(f2+f3)/(f1+f2+f3)),method = "lm", formula = y ~ x + I(x^2),  size = 0, alpha=0.1)+ 
  geom_line(stat="smooth",method = "lm", #formula = y ~ x + I(x^2),
            size = 1.5,
            alpha = 0.6)+ 
  #geom_boxplot(aes(group=area:timepoint2, fill=area), width=2, alpha=0.7, colour="black", outlier.shape = NA)+
  scale_colour_manual("Population",values = c("red3",
                                              "brown1",
                                              "darkorchid4",
                                              "mediumpurple1",
                                              "blue3",
                                              "steelblue1",
                                              "grey15"))+
  scale_fill_manual("Population",values = c("red3",
                                            "brown1",
                                            "darkorchid4",
                                            "mediumpurple1",
                                            "blue3",
                                            "steelblue1",
                                            "grey15"))+
  scale_size_continuous(limits = c(0, 1000), 
                        breaks = c(50, 100, 500, 1000))+
  labs(x="Month",y="Proportion of males",title="")
