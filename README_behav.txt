##############################################################
README for the Gobyhavior project
##############################################################
The project involves several datasets and scripts.

Datasets           #description
--------------------------------
behav.txt          #raw behavior field obervations data
cens.txt           #raw population census data
OSRdata.csv        #dataset produced from the first two for plotting and analysis purposes

Scripts            #description
--------------------------------
Data_formatting.R  #Imports and formats behav.txt and cens.txt and creates OSRdata.csv
Script_behavior.R  #Uses the datasets to produce all the figures and statistical analyses of the manuscript
	
	
##############################################################
Description of variables for: behav.txt
##############################################################
Each line of the dataset is a sampling transect, where fish sexual display behaviors have been recorded

Variable           #description
--------------------------------
1. Transect features variables

area               #Name of one of the six populations studied
location           #Name of the transect location, within population replicate
timepoint          #Sampling period 1=early, 2=mid, 3= late-season
date               #date of sampling
measurer           #ID of the field observer, TA=Trond Amundsen, CA=Claudia Aparicio, IM= Ivain Martinossi
waves              #estimated height of waves
sun                #weather
rain               #weather
temp               #sea surface temperature measured in the field
st.obs             #time of starting behavior observation
end.obs            #time of ending observation
visibility         #estimated visibility in the water
type               #type of observation: transect=used in the present study, nest=around artificial nests, not used in the present study

2. Raw behavior count variables

o ALL "MF" variables are either Male behavior or Female response, during Male initiated behavior
o ---- M.F-------------- Male and Female mutual behavior (unknown initiator)
o ---- M.M-------------- Male and Male behavior
o ---- FM--------------- Female behavior or Male response, during Female initiated behavior
o ---- F.F-------------- Female and Female behavior

MF_fin             #Male towards female fin display
MF_approach        #Male towards female approach
MF_hook            #Female hook in response to male behavior
MF_glow            #Female glow in response to male behavior
MF_HG              #Female hook & glow in response to male behavior
MF_finNR           #Female absence of response to male fin display
MF_lead            #Male towards female lead swim to the nest
MF_Lnest           #Male enters the nest after lead swim
MF_foll            #Female follows lead swim
MF_FN              #Female follows and enters the nest
MF_leadNR          #Female absence of response to male lead swim

M.F_fin            #Male fin display during mutual courtship
M.F_hook           #Female hook display during mutual courtship
M.F_glow           #Female glow display during mutual courtship
M.F_HG             #Female hook & glow display during mutual courtship
M.F_finNR          #Female absence of response to fin display during mutual courtship
M.F_lead           #Male lead swim during mutual courtship
M.F_Lhook          #Female responds by hook to lead swim
M.F_follow         #Female responds by following to lead swim
M.F_nest           #Female enters the nest during mutual courtship
M.F_leadNR         #Female absence of response to lead swim

MM_fin             #Male fin display to other male
MM_retreat         #Male retreats in response to fin display by other male
MM_NR              #Male absence of response to fin display by other male
MM_chase           #Male chases other male
MM_Cretreat        #Male retreats in response to chase by other male
MM_CNR             #Male absence of response to chase by other male

FM_hook            #Female towards male hook display
FM_hookA           #Male responds by approach to hook display
FM_hookAF          #Male responds by approach and fin display to hook display
FM_hookAFL         #Male responds by approach, fin display and lead swim to hook display
FM_hookAFLN        #Male responds by approach, fin display, lead swim and nest entering to hook display
FM_hookNR          #Male absence of response to hook display
FM_glow            #Female towards male glow display
FM_glowA           #Male responds by approach to glow display
FM_glowAF          #Male responds by approach and fin display to glow display
FM_glowAFL         #Male responds by approach, fin display and lead swim to glow display
FM_glowAFLN        #Male responds by approach, fin display, lead swim and nest entering to glow display
FM_glowNR          #Male absence of response to glow display
FM_HG              #Female towards male hook & glow display
FM_HGA             #Male responds by approach to HG display
FM_HGAF            #Male responds by approach and fin display to HG display
FM_HGAFL           #Male responds by approach, fin display and lead swim to HG display
FM_HGAFLN          #Male responds by approach, fin display, lead swim and nest entering to HG display
FM_HGNR            #Male absence of response to HG display

FF_hook            #Female hook display towards female
FF_glow            #Female glow display towards female
FF_HG              #Female hook & glow display towards female
FF_hook.1          #Female hook display in response to female display
FF_glow.1          #Female hook display in response to female display
FF_HG.1            #Female hook & glow display in response to female display
FF_NR              #Female absence of response to female display

date2              #date in a different format

M_init             #Combination of Male towards Female behaviors: MF_fin+MF_approach+MF_lead+MF_Lnest
M_initR            #Combination of Female responses: MF_hook+MF_glow+MF_HG+MF_foll+MF_nest+MF_FN
M_initNR	   #Absence of response to male initiated behavior

F_init             #Combination of Female towards Male behaviors: FM_hook+FM_glow+FM_HG
F_initR            #Combination of Male responses:FM_hookA+FM_hookAF+FM_hookAFL+FM_hookAFLN+FM_glowA+FM_glowAF+FM_glowAFL+FM_glowAFLN+
F_initNR           #Absence of response to female initiated behavior

Mut_init           #Combination of mutual behaviors
Mut_R              #Combination of responses
Mut_NR             #Absence of response

MM                 #Combination of Male Male behavior MM_fin+MM_chase
MM_R               #Combination of responses: MM_fin1+MM_retrat+MM_chase.1+MM_Cretreat
MM_NRtot           #MM_NR+MM_CNR

FF	           #Combination of Female Female behavior: FF_hook+FF_glow+FF_HG
FF_R		   #FF_hook.1+FF_glow.1+FF_HG.1

	
##############################################################
Description of variables for: cens.txt
##############################################################
Each line of the dataset is a sampling transect, where fish population census was performed

Variable           #description
--------------------------------
1. Transect features variables

area               #Name of one of the six populations studied
location           #Name of the transect location, within population replicate
timepoint          #Sampling period 1=early, 2=mid, 3= late-season
date               #date of sampling
measurer           #ID of the field observer, TA=Trond Amundsen, CA=Claudia Aparicio, IM= Ivain Martinossi
waves              #estimated height of waves
sun                #weather
rain               #weather
temp               #sea surface temperature measured in the field
st.obs             #time of starting behavior observation
end.obs            #time of ending observation
visibility         #estimated visibility in the water
replicate          #ID replicate, 1 or 2, since two consecutive transect were done
date2              #different date format
timepoint2         #different format for timepoint
areashort          #Short handle for population name

2. Population census variables

m                 #Count of males
f1                #Count of roundness 1 females
f2                #Count of roundness 2 females
f3                #Count of roundness 3 females
fu                #Count of females of unknown roundness
u                 #Count of adults of uncertain sex


