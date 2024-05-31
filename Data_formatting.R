#Gobyhavior project
#Import raw field data of census and behavior and create OSRdata that is used in the script "Script_behavior.R"

setwd("C://Users//ivainm//Working_Document//DYNAMAR//Manuscripts//Behavior//Data")


# 1. Import and format behavior data
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\
#=========================================================|
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\
#=========================================================|

behavdata<-read.delim("behav.txt")
behavdata$date2<-as.Date(behavdata$date, format="%d.%m")
behavdata$area<-as.factor(behavdata$area)
behavdata$location<-as.factor(behavdata$location)
behavdata$area<- factor(behavdata$area, levels=c('Kristineberg', 'Arendal', 'Austevoll', 'Hitra', 'Helligvaer', 'Ringstad'))
behavdata$type<-as.factor(behavdata$type)
behavdata$visibility<-as.factor(behavdata$visibility)
#Summary variables for behavior
#male initiated courtship
behavdata$M_init<-behavdata$MF_fin+behavdata$MF_approach+behavdata$MF_lead+behavdata$MF_Lnest
behavdata$M_initR<-behavdata$MF_hook+behavdata$MF_glow+behavdata$MF_HG+behavdata$MF_foll+behavdata$MF_nest+
  behavdata$MF_FN
behavdata$M_initNR<-behavdata$MF_finNR+behavdata$MF_leadNR
#female initiated
behavdata$F_init<-behavdata$FM_hook+behavdata$FM_glow+behavdata$FM_HG
behavdata$F_initR<-behavdata$FM_hookA+behavdata$FM_hookAF+behavdata$FM_hookAFL+behavdata$FM_hookAFLN+
  behavdata$FM_glowA+behavdata$FM_glowAF+behavdata$FM_glowAFL+behavdata$FM_glowAFLN
behavdata$F_initNR<-behavdata$FM_hookNR+behavdata$FM_glowNR
#mutual courtship
behavdata$Mut_init<-behavdata$M.F_fin+behavdata$M.F_lead
behavdata$Mut_R<-behavdata$M.F_hook+behavdata$M.F_glow+behavdata$M.F_HG+
  behavdata$M.F_Lhook+behavdata$M.F_follow+behavdata$M.F_nest
behavdata$Mut_NR<-behavdata$M.F_finNR+behavdata$M.F_leadNR
#male male
behavdata$MM<-behavdata$MM_fin+behavdata$MM_chase
behavdata$MM_R<-behavdata$MM_fin.1+behavdata$MM_retreat+behavdata$MM_chase.1+behavdata$MM_Cretreat
behavdata$MM_NRtot<-behavdata$MM_NR+behavdata$MM_CNR
#female female
behavdata$FF<-behavdata$FF_hook+behavdata$FF_glow+behavdata$FF_HG
behavdata$FF_R<-behavdata$FF_hook+behavdata$FF_glow+behavdata$FF_HG
#behavdata$FF_NR<-behavdata$FF_NR

str(behavdata)


# 2. Import and format cesus data
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\
#=========================================================|
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\
#=========================================================|

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



# 3. Format OSRdata from census and behavior data
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\
#=========================================================|
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\
#=========================================================|

str(behavdata)
str(censdata)
colnames(behavdata)

# subset to remove nest transects
behavtransect<-behavdata[behavdata$type=="transect",]

table(behavtransect$area,behavtransect$timepoint)
table(censdata$area,censdata$timepoint)

table(behavtransect$location,behavtransect$timepoint)
table(censdata$location,censdata$timepoint)


behavlocs<-levels(behavtransect$location)
#checking that both data sets have the same location names
for (i in 1:length(behavlocs)) {if(behavlocs[i]==levels(censdata$location)[i]){print("ok")} else{print("FAIL")}}

#create a data set with one line for each location and timepoint
#init OSR data
OSRdata<- data.frame(matrix(ncol = 40, nrow = 0))
colnames(OSRdata)<-c("area","location","timepoint","visibility","date",
                     "M_i_prop","M_iprop_SD",
                     "F_i_prop","F_iprop_SD",
                     "M_init","M_init_SD",
                     "F_init","F_init_SD",
                     "Mut_init","Mut_init_SD",
                     "MM","MM_SD",
                     "FF","FF_SD",
                     "behav_rep","behav_n",
                     "Nm","Nf",
                     "ASR","ASR_SD","Om","Om_SD","Of23","Of23_SD","Of3","Of3_SD","OSR23","OSR23_SD","OSR3","OSR3_SD", "DME23","DME23_SD","DME3","DME3_SD",
                     "Census" )

#rm(OSRdata)
#loop and build
for (i in 1: length(behavlocs)) {
  for (j in 1:3) {
    #subset required data
    subtrans<-behavtransect[behavtransect$location==behavlocs[i] & behavtransect$timepoint==j,]
    subcens<-censdata[censdata$location==behavlocs[i] & censdata$timepoint==j,]
    #collect area location and timepoint, visibility, date info
    info<-subtrans[1,c(1:3,12,69)]
    #average, SD and sample size (replicate transects?) of behavior data
    #How to calculate? Sum behaviors of replicated transects? Calculate ratios and then average ratios and absolute values?
    subtrans$M_i_prop<-subtrans$M_init/(subtrans$M_init+subtrans$F_init) #frequency of male initiated
    subtrans$F_i_prop<-subtrans$F_init/(subtrans$M_init+subtrans$F_init) #frequency of female initiated
    subtrans$n<-(subtrans$M_init+subtrans$F_init) #sample size
    #behavior chunck to add to OSR data
    behav<-c(mean(subtrans$M_i_prop,na.rm=TRUE),sd(subtrans$M_i_prop),
             mean(subtrans$F_i_prop,na.rm=TRUE),sd(subtrans$F_i_prop),
             sum(subtrans$M_init),sd(subtrans$M_init), #sum behaviors observed on both replicate transects
             sum(subtrans$F_init),sd(subtrans$F_init),
             sum(subtrans$Mut_init),sd(subtrans$Mut_init),
             sum(subtrans$MM),sd(subtrans$MM),
             sum(subtrans$FF),sd(subtrans$FF),
             length(subtrans[,1]), sum(subtrans$n)) #check number of reps and total behav sample size
    #average, SD of census data. First calculate ratios, then average and sd
    subcens$Nf<-subcens$f1+subcens$f2+subcens$f3
    subcens$ASR<-subcens$m/(subcens$m+subcens$f1+subcens$f2+subcens$f3+subcens$fu) #ASR
    subcens$Om<-subcens$ASR  # male operational fraction is ASR for now....
    subcens$Of23<-(subcens$f2+subcens$f3)/(subcens$m+subcens$f1+subcens$f2+subcens$f3+subcens$fu) # female 23 operational fraction
    subcens$Of3<-(subcens$f3)/(subcens$m+subcens$f1+subcens$f2+subcens$f3+subcens$fu)
    subcens$OSR23<-subcens$Om/(subcens$Om+subcens$Of23)  #OSR23
    subcens$OSR3<-subcens$Om/(subcens$Om+subcens$Of3)  #OSR3  
    subcens$DME23<-subcens$Om-subcens$Of23 #DME23
    subcens$DME3<-subcens$Om-subcens$Of3 #DME3
    subcens$D<-subcens$m+subcens$f1+subcens$f2+subcens$f3+subcens$fu #sample size/Density
    #census chunk to add to OSR data
    census<-c(sum(subcens$m,na.rm=TRUE),sum(subcens$Nf,na.rm=TRUE),
              mean(subcens$ASR,na.rm=TRUE),sd(subcens$ASR),
              mean(subcens$Om,na.rm=TRUE),sd(subcens$Om),
              mean(subcens$Of23,na.rm=TRUE),sd(subcens$Of23),
              mean(subcens$Of3,na.rm=TRUE),sd(subcens$Of3),
              mean(subcens$OSR23,na.rm=TRUE),sd(subcens$OSR23),
              mean(subcens$OSR3,na.rm=TRUE),sd(subcens$OSR3),
              mean(subcens$DME23,na.rm=TRUE),sd(subcens$DME23),
              mean(subcens$DME3,na.rm=TRUE),sd(subcens$DME3),
              sum(subcens$D)) # SUm of total census, remember to divide by 2x transect length to get /m individuals
    #combine all in a line
    newline<-data.frame(c(info,behav,census))
    colnames(newline)<-colnames(OSRdata)
    #append line to OSR data
    OSRdata<-rbind(OSRdata,newline)
    #repeat
  }}
##Adding the length of each transect to calculate final density
#import transect length data
translengthdata<-read.csv("transectlength2.csv", header=T)
translengthdata$location<-as.factor(translengthdata$location)
str(translengthdata)
#check that location names match
for (i in 1:length(levels(OSRdata$location))) {if(levels(OSRdata$location)[i]==levels(translengthdata$location)[i]){print("ok")} else{print(c(levels(OSRdata$location)[i],levels(translengthdata$location)[i],"FAIL"))}}
#create new column
OSRdata$transectlength<-NA
#place the right values in that new column
for (i in 1:length(levels(OSRdata$location))) {
  OSRdata[OSRdata$location==levels(OSRdata$location)[i],]$transectlength<-translengthdata[translengthdata$location==levels(OSRdata$location)[i],]$length
}
############
#Calculate density in fish/m
OSRdata$Density<-OSRdata$Census/(2*OSRdata$transectlength)
#timepoint name change
OSRdata$timepoint2<-as.factor(OSRdata$timepoint)
levels(OSRdata$timepoint2)<-c("Early season", "Mid season", "Late season")
############
##calculate courtship propensity for each sex
OSRdata$Fcourtprop<-(OSRdata$F_init)/((OSRdata$Density^2)*OSRdata$Om*OSRdata$Of23*OSRdata$transectlength)
OSRdata$Mcourtprop<-(OSRdata$M_init)/((OSRdata$Density^2)*OSRdata$Om*OSRdata$Of23*OSRdata$transectlength)
#I also want a standardized version (per sex) that is bound between zero and 1
#this is needed to run a proper poisson or binomial model
OSRdata$FCproprel<-OSRdata$Fcourtprop/max(OSRdata$Fcourtprop, na.rm = T)
OSRdata$MCproprel<-OSRdata$Mcourtprop/max(OSRdata$Mcourtprop, na.rm = T)
#I also need to estimate my total encounter number (as an integer?) if I want to use a binomial model with weights
OSRdata$FCtotrel<-max(OSRdata$Fcourtprop, na.rm = T)*OSRdata$Density^2*OSRdata$Om*OSRdata$Of23*OSRdata$transectlength
OSRdata$MCtotrel<-max(OSRdata$Mcourtprop, na.rm = T)*OSRdata$Density^2*OSRdata$Om*OSRdata$Of23*OSRdata$transectlength
#############
#Calculating same-sex behavior propensity
#First I need to calculate the probability to observe a behavior
OSRdata$Fagoproba<-(OSRdata$FF)/(OSRdata$Density^2*OSRdata$Of23^2*OSRdata$transectlength)
OSRdata$Magoproba<-(OSRdata$MM)/(OSRdata$Density^2*OSRdata$Om^2*OSRdata$transectlength)
hist(OSRdata$Fagoproba, breaks=50) #major outlier investigate
OSRdata[OSRdata$Fagoproba>100,] #one point late season, one behavior observed. Remove
hist(OSRdata$Magoproba, breaks=50) #looks ok
OSRdata<-OSRdata[OSRdata$Fagoproba<100,]

#I will set C to max proba to standardize them so max is 1
OSRdata$Fagoprobarel<-OSRdata$Fagoproba/max(OSRdata$Fagoproba, na.rm = T)
OSRdata$Magoprobarel<-OSRdata$Magoproba/max(OSRdata$Magoproba, na.rm = T)
#Now I have a proba with proper scale I can solve for the propensity
#I may have to use the proba, not the propensity though, in order to run a binomial model properly with weights....
OSRdata$FFprop23<-1-(sqrt(1-OSRdata$Fagoprobarel))
OSRdata$MMprop<-1-(sqrt(1-OSRdata$Magoprobarel))
OSRdata<-OSRdata[!is.na(OSRdata$F_init),]
#Add the total proba as well 
OSRdata$Fagoprobatotrel<-max(OSRdata$Fagoproba, na.rm = T)*
  (OSRdata$Density^2*OSRdata$Of23^2*OSRdata$transectlength)
OSRdata$Magoprobatotrel<-max(OSRdata$Magoproba, na.rm = T)*
  (OSRdata$Density^2*OSRdata$Om^2*OSRdata$transectlength)
str(OSRdata)

#Adding hypothetical reaction norms
Rm<-2
S1<-8
OSRdata$ObsOmf<-2*OSRdata$Om*OSRdata$Of23  #proportion of observed pairs of fish that correspond to a male and female ready to mate
OSRdata$PropDME1_m<-1/(1+exp(-S1*((Rm*OSRdata$Om)-OSRdata$Of23)))   # 1st version of propensity of males to court, based on DME as perceived environment. Slope is 20, Rm is 1
OSRdata$PropDME1_f<-1/(1+exp(-S1*(-(Rm*OSRdata$Om)+OSRdata$Of23)))   # same for females
OSRdata$PropOSR1_m<-1/(1+exp(-S1*((Rm*OSRdata$Om)/(Rm*OSRdata$Om+OSRdata$Of23)-1/2)) )  # 1st version of propensity of males to court, based on OSR as perceived environment
OSRdata$PropOSR1_f<-1/(1+exp(-S1*(-(Rm*OSRdata$Om)/(Rm*OSRdata$Om+OSRdata$Of23) +1/2)))  
## same sex behavior propensities, need to check calculations again with new density calculation. In particular check conditions for non complex solution. Should be fine for most data, since the condition is that behaviors are lower than encounter rates.
DOm<-max(OSRdata$Om*OSRdata$Census) #max male density in the dataset
OSRdata$Propmm<- 1/(1+exp(-10*(OSRdata$Census*OSRdata$Om-DOm/2)/DOm))   #alternative propensity function for male competition, depending on male density

str(OSRdata)
write.csv(OSRdata,"OSRdata.csv")