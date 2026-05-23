#Longitudinal Data WTC Frailty Index calculation


setwd("J:/PM/DC_KO Frailty Aim 3") #--change to your own directory containing WTC data

#Function to map each frailty index item into binary values representing presence(1)/absence(0) of deficits on item
bin_mapper <- function(dat, neg_vals, pos_vals) {
  return(ifelse(dat %in% neg_vals, 0, ifelse(dat %in% pos_vals, 1, NA)))
}

#############################################
############### physical exam ###############
#############################################
WTC_PE <- read.csv("KO210414WTCFrailty_PE.csv")
names(WTC_PE)
frailty_vars_PE <- c('EYES','EARS','EXTREMITIES','HEART','GEN_APPEARANCE')
for(v in frailty_vars_PE) {
  print(v)
  print(table(WTC_PE[v], useNA="always"))
}

#GEN_APPEARANCE
WTC_PE$frailty_GEN_APPEARANCE <- bin_mapper(WTC_PE$GEN_APPEARANCE, c('Normal','Muscular'), c('Abnormal','Ill-appearing','Obese','Wasted'))
with(WTC_PE, table(GEN_APPEARANCE, frailty_GEN_APPEARANCE, useNA='always'))


#eyes
WTC_PE$frailty_EYES <- bin_mapper(WTC_PE$EYES, c('Normal'), c('Abnormal'))
with(WTC_PE, table(EYES, frailty_EYES, useNA='always'))

#ears
WTC_PE$frailty_EARS <- bin_mapper(WTC_PE$EARS, c('Normal'), c('Abnormal'))
with(WTC_PE, table(EARS, frailty_EARS, useNA='always'))

#extremities
WTC_PE$frailty_EXTREMITIES <- bin_mapper(WTC_PE$EXTREMITIES, c('Normal'), c('Abnormal'))
with(WTC_PE, table(EXTREMITIES, frailty_EXTREMITIES, useNA='always'))

#heart
WTC_PE$frailty_HEART <- bin_mapper(WTC_PE$HEART, c('Normal'), c('Abnormal'))
with(WTC_PE, table(HEART, frailty_HEART, useNA='always'))




###########################################
################## SAMHQ ##################
###########################################
WTC_SAMHQ <- read.csv("KO210414WTCFrailty_SAMHQ.csv")
names(WTC_SAMHQ)[names(WTC_SAMHQ)=='visit_Number'] <- 'visit_number'
frailty_vars_SAMHQ <- c(paste0('SAMHQ_01_',formatC(1:12,width=2,format='d',flag=0)),'SAMHQ_01_14', 'SAMHQ_04_15')
for(v in frailty_vars_SAMHQ) {
  print(v)
  print(table(WTC_SAMHQ[v], useNA="always"))
}
#SAMHQ_01_01 : In general, would you say your health is:
WTC_SAMHQ$frailty_SAMHQ_01_01 <- bin_mapper(WTC_SAMHQ$SAMHQ_01_01, c('1 = excellent','2 = very good','3 = good'), c('4 = fair','5 = poor') )
with(WTC_SAMHQ, table(SAMHQ_01_01, frailty_SAMHQ_01_01, useNA="always"))

#SAMHQ_01_02 : "Does your health now limit you in : Moderate activities, such as moving a table, pushing a vacuum cleaner, bowling, or playing golf
WTC_SAMHQ$frailty_SAMHQ_01_02 <- bin_mapper(WTC_SAMHQ$SAMHQ_01_02, c('3 = no  not limited at all', '2 = yes  limited a little'), c('1 = yes  limited a lot') )
with(WTC_SAMHQ, table(SAMHQ_01_02, frailty_SAMHQ_01_02, useNA="always"))

#SAMHQ_01_03 : "Does your health now limit you in : Climbing several flights of stairs
WTC_SAMHQ$frailty_SAMHQ_01_03 <- bin_mapper(WTC_SAMHQ$SAMHQ_01_03, c('3 = no  not limited at all', '2 = yes  limited a little'), c('1 = yes  limited a lot') )
with(WTC_SAMHQ, table(SAMHQ_01_03, frailty_SAMHQ_01_03, useNA="always"))

#SAMHQ_01_04 : During the past 4 weeks, have you had any of the following problems with your work or regular daily activities as a result of 
#your physical health: Accomplished less than you would like
WTC_SAMHQ$frailty_SAMHQ_01_04 <- bin_mapper(WTC_SAMHQ$SAMHQ_01_04, c('2 = No'), c('1 = Yes') )
with(WTC_SAMHQ, table(SAMHQ_01_04, frailty_SAMHQ_01_04, useNA="always"))

#SAMHQ_01_05 : During the past 4 weeks, have you had any of the following problems with your work or regular daily activities as a result of
#your physical health: Were limited in the kind of work or other activities
WTC_SAMHQ$frailty_SAMHQ_01_05 <- bin_mapper(WTC_SAMHQ$SAMHQ_01_05, c('2 = No'), c('1 = Yes') )
with(WTC_SAMHQ, table(SAMHQ_01_05, frailty_SAMHQ_01_05, useNA="always"))

#SAMHQ_01_06 : During the past 4 weeks, have you had any of the following problems with your work or other regular daily activities as a result of 
#any emotional problems (such as feeling depressed or anxious)? - Accomplished less than you would like
WTC_SAMHQ$frailty_SAMHQ_01_06 <- bin_mapper(WTC_SAMHQ$SAMHQ_01_06, c('2 = No'), c('1 = Yes') )
with(WTC_SAMHQ, table(SAMHQ_01_06, frailty_SAMHQ_01_06, useNA="always"))

#SAMHQ_01_07 : During the past 4 weeks, have you had any of the following problems with your work or other regular daily activities as a result of 
#any emotional problems (such as feeling depressed or anxious)? - Did work or other activities less carefully than usual
WTC_SAMHQ$frailty_SAMHQ_01_07 <- bin_mapper(WTC_SAMHQ$SAMHQ_01_07, c('2 = No'), c('1 = Yes') )
with(WTC_SAMHQ, table(SAMHQ_01_07, frailty_SAMHQ_01_07, useNA="always"))

#SAMHQ_01_08 : During the past 4 weeks, how much did pain interfere with your normal work (including both work outside the home and housework)?
WTC_SAMHQ$frailty_SAMHQ_01_08 <- bin_mapper(WTC_SAMHQ$SAMHQ_01_08, c('1 = Not at all','2 = A little bit','3 = Moderately'), c('4 = Quite a bit','5 = Extremely') )
with(WTC_SAMHQ, table(SAMHQ_01_08, frailty_SAMHQ_01_08, useNA="always"))

#SAMHQ_01_09 : How much of the time during the past 4 weeks.... Have you felt calm and peaceful?
WTC_SAMHQ$frailty_SAMHQ_01_09 <- bin_mapper(WTC_SAMHQ$SAMHQ_01_09, c('1 = all of the time','2 = most of the time','3 = a good bit of the time','4 = some of the time'), c('5 = a little of time','6 = none of the time') )
with(WTC_SAMHQ, table(SAMHQ_01_09, frailty_SAMHQ_01_09, useNA="always"))

#SAMHQ_01_10 : How much of the time during the past 4 weeks.... Did you have a lot of energy?
WTC_SAMHQ$frailty_SAMHQ_01_10 <- bin_mapper(WTC_SAMHQ$SAMHQ_01_10, c('1 = all of the time','2 = most of the time','3 = a good bit of the time','4 = some of the time'), c('5 = a little of time','6 = none of the time') )
with(WTC_SAMHQ, table(SAMHQ_01_10, frailty_SAMHQ_01_10, useNA="always"))

#SAMHQ_01_11 : How much of the time during the past 4 weeks.... Have you felt downhearted and blue?
WTC_SAMHQ$frailty_SAMHQ_01_11 <- bin_mapper(WTC_SAMHQ$SAMHQ_01_11, c('6 = none of the time','5 = a little of time','4 = some of the time','3 = a good bit of the time'), c('1 = all of the time','2 = most of the time') )
with(WTC_SAMHQ, table(SAMHQ_01_11, frailty_SAMHQ_01_11, useNA="always"))

#SAMHQ_01_12 : During the past 4 weeks, how much of the time has your physical health or emotional problems interfered with your social activities (like visiting friends, relatives, etc.)?
WTC_SAMHQ$frailty_SAMHQ_01_12 <- bin_mapper(WTC_SAMHQ$SAMHQ_01_12, c('3 = Some of the time','4 = A little of the time','5 = None of the time'), c('1 = All of the time','2 = Most of the time') )
with(WTC_SAMHQ, table(SAMHQ_01_12, frailty_SAMHQ_01_12, useNA="always"))

#SAMHQ_01_14 : How much trouble have you had with your short-term memory (e.g. forgetting where you left your keys, items at the grocery store, etc.)
WTC_SAMHQ$frailty_SAMHQ_01_14 <- bin_mapper(WTC_SAMHQ$SAMHQ_01_14, c('1 = Not at all','2 = A little bit','3 = Moderately'), c('4 = Quite a bit','5 = Extremely') )
with(WTC_SAMHQ, table(SAMHQ_01_14, frailty_SAMHQ_01_14, useNA="always"))

#SAMHQ_04_15 : In the past month, how much have you been bothered by: frequency of difficulty concentrating
WTC_SAMHQ$frailty_SAMHQ_04_15 <- bin_mapper(WTC_SAMHQ$SAMHQ_04_15, c('1 = Not at all','2 = A little bit','3 = Moderately'), c('4 = Quite a bit','5 = Extremely') )
with(WTC_SAMHQ, table(SAMHQ_04_15, frailty_SAMHQ_04_15, useNA="always"))



#####################################################
################## IAMQ: Symptoms ###################
#####################################################
WTC_IAMQ <- read.csv("KO210414WTCFrailty_IAMQV2on.csv")
frailty_vars_IAMQ <- c('IAMQ_11A_10_02', 'IAMQ_11A_07_02','IAMQ_11A_04_02','IAMQ_07_01_01','IAMQ_07_03_01','IAMQ_05A_07_01','IAMQ_07_05_01','IAMQ_11A_03_02')
#make sure all the variables required are present in IAMQ dataframe
setdiff(frailty_vars_IAMQ, names(WTC_IAMQ))
for(v in frailty_vars_IAMQ) {
  print(v)
  print(table(WTC_IAMQ[v], useNA="always"))
}

#persistent fatigue
WTC_IAMQ$frailty_persfatg <- bin_mapper(WTC_IAMQ$IAMQ_11A_10_02, c('no'), c('yes') )
with(WTC_IAMQ, table(IAMQ_11A_10_02, frailty_persfatg, useNA="always"))

#unexplained weight loss?
WTC_IAMQ$frailty_wtloss <- bin_mapper(WTC_IAMQ$IAMQ_11A_07_02, c('no'), c('yes') )
with(WTC_IAMQ, table(IAMQ_11A_07_02, frailty_wtloss, useNA="always"))

#difficulty urinating?
WTC_IAMQ$frailty_diff_urin <- bin_mapper(WTC_IAMQ$IAMQ_11A_04_02, c('no'), c('yes') )
with(WTC_IAMQ, table(IAMQ_11A_04_02, frailty_diff_urin, useNA="always"))

#inability to taste?
WTC_IAMQ$frailty_diff_taste <- bin_mapper(WTC_IAMQ$IAMQ_07_01_01, c('no'), c('yes') )
with(WTC_IAMQ, table(IAMQ_07_01_01, frailty_diff_taste, useNA="always"))

#difficulty swallowing?
WTC_IAMQ$frailty_diff_swall <- bin_mapper(WTC_IAMQ$IAMQ_07_03_01, c('no'), c('yes') )
with(WTC_IAMQ, table(IAMQ_07_03_01, frailty_diff_swall, useNA="always"))

#difficulty hearing?
WTC_IAMQ$frailty_diff_hear <- bin_mapper(WTC_IAMQ$IAMQ_05A_07_01, c('no'), c('yes') )
with(WTC_IAMQ, table(IAMQ_05A_07_01, frailty_diff_hear, useNA="always"))

#Coughing after you lie down or eat?
WTC_IAMQ$frailty_coughlie <- bin_mapper(WTC_IAMQ$IAMQ_07_05_01, c('no'), c('yes') )
with(WTC_IAMQ, table(IAMQ_07_05_01, frailty_coughlie, useNA="always"))

#Change in bowel habits?
WTC_IAMQ$frailty_bowelchang <- bin_mapper(WTC_IAMQ$IAMQ_11A_03_02, c('no'), c('yes') )
with(WTC_IAMQ, table(IAMQ_11A_03_02, frailty_bowelchang, useNA="always"))


#############################################
###### IAMQ: self-reported conditions ######
#############################################
#pneumonia: to be done
#diabetes: to be done


#############################################
###### IAMQ: self-reported medications ######
#############################################
#polypharmacy: to be done




################################################################################################################################

########## MERGE FILES ##########
#Will merge WTC_PE, WTC_SAMHQ, WTC_IAMQ, by variables 'extract_id' (ID of each subject) and 'visit_number' (integers 1,2,3,... indicating subject's current visit number)
merge_keys <- c('extract_id', 'visit_number')


####do some cleaning before merging: remove non-numeric subject IDs and visit_numbers from WTC_IAMQ file (both 'extract_id' and 'visit_number' should be numeric)
class(WTC_IAMQ$extract_id)   #it is character, but should be numeric
class(WTC_IAMQ$visit_number) #it is character, but should be numeric
tail(subset(WTC_IAMQ, is.na(as.numeric(extract_id)), select=c('extract_id','visit_number')))
WTC_IAMQ$extract_id <- as.numeric(WTC_IAMQ$extract_id)
WTC_IAMQ$visit_number <- as.numeric(WTC_IAMQ$visit_number)
WTC_IAMQ_clean <- subset(WTC_IAMQ, !(is.na(extract_id) | is.na(visit_number)) )
nrow(WTC_IAMQ)
nrow(WTC_IAMQ_clean)


####few checks before merging
summary(WTC_IAMQ_clean$visit_number)
summary(WTC_PE$visit_number)
summary(WTC_SAMHQ$visit_number)

summary(WTC_IAMQ_clean$extract_id)
summary(WTC_PE$extract_id)
summary(WTC_SAMHQ$extract_id)


####set up dataframes to merge
dfs_to_merge <- list(WTC_PE         [, c(merge_keys, grep('frailty_',names(WTC_PE),value=T))], 
                     WTC_SAMHQ      [, c(merge_keys, grep('frailty_',names(WTC_SAMHQ),value=T))], 
                     WTC_IAMQ_clean [, c(merge_keys, grep('frailty_',names(WTC_IAMQ_clean),value=T))])

for(df in dfs_to_merge) {
  print(nrow(df))
}

WTC_PE_IAMQ_SAMHQ <- Reduce(function(df1, df2) merge(df1, df2, by = merge_keys, all = TRUE), dfs_to_merge)
print(dim(WTC_PE_IAMQ_SAMHQ))

head(WTC_PE_IAMQ_SAMHQ)

#check for duplicates in c('extract_id', 'visit_number') [should be none]
sum(duplicated(WTC_PE_IAMQ_SAMHQ[,c('extract_id','visit_number')]))


#frailty index components
frailty_index_vars <- grep('frailty_',names(WTC_PE_IAMQ_SAMHQ),value=T)
length(frailty_index_vars)
frailty_index_vars

#check proportion of this longitudinal dataset with non-missing values on the set of frailty components
nrow(na.omit(WTC_PE_IAMQ_SAMHQ[,frailty_index_vars])) / nrow(WTC_PE_IAMQ_SAMHQ)


#Compute frailty (only for records with complete [non-missing] set)
WTC_PE_IAMQ_SAMHQ$frailty.index <- rowMeans(WTC_PE_IAMQ_SAMHQ[,frailty_index_vars])
summary(WTC_PE_IAMQ_SAMHQ$frailty.index)
quantile(WTC_PE_IAMQ_SAMHQ$frailty.index, probs=c(.99), na.rm=T)
#JAH paper (full [30-item] frailty index) with up to 2017 data (cross-sectional): frailty index mean=.18, median=.13, 99th %-ile=.63


#### TODO
#Merge this dataset with the demo (KO210414WTCFrailty_Demographics.csv) data to provide full visit data for each subject, as well as baseline demographics
