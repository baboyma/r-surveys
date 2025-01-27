# PROJECT: r-survery
# PURPOSE: Sample code for PHIA Data Analysis
# AUTHOR: Baboyma Kagniniwa | USAID/GH - Office of HIV-AIDS
# LICENSE: MIT
# REF. ID: 5703aac1
# CREATED: 2025-01-23
# UPDATED: 2025-01-23
# NOTES: data source: https://phia.icap.columbia.edu/countries/zambia/


# Libraries ====

  library(tidyverse)
  library(survey)
  library(foreign)
  library(glamr)

# PARAMS ----

  ## Directories
  
  dir_temp <- "./Data/Temp"
  dir_data <- "./Data/Raw"
  dir_dataout <- "./Data/Processed"


# Load Data ====

  ############################################################
  ### Example 1. Estimate 90-90-90 indicators among adults ### 
  ############################################################

  ### Load in Adult BIO dataset 
  adultbio <- read.csv('C:/Desktop/zamphia2016adultbio.csv') 

  ### Ensure that weight variables are converted to numeric type, and drop  
  
  ### records with no blood weight 
  wtname <- 'btwt' 
  adultbio <- adultbio %>%   
    filter(bt_status == 1) %>%   
    mutate(across(contains(wtname), as.double))  
  
  ### Create survey object, JK2 
  # Recode analytic outcomes to 0/1 
  vars <- c('hivstatusfinal', 'aware', 'art', 'vls', 'tri90') 
  
  svydata <- adultbio 
  
  for(i in 1:length(vars)){   
    varname <- vars[i]   
    if(is.factor(svydata[,varname])) {     
      svydata[,varname] <- as.numeric(levels(svydata[,varname]))[svydata[,varname]]     
      # coerces factor variables to numeric before recoding   
    }   
    
    svydata[,varname][(svydata[,varname]==2)] <- 0 
  }  
  
  # Extract analytic variables, base weights and JK replicate weights 
  #   and create survey object, 
  #   specifying variance estimation method "JK" and JK 
  coefficients=1 
  
  svydata1 <- svrepdesign(   
    variables=svydata[,vars],   
    weights=svydata[,paste0(wtname,0)],   
    repweights=svydata[,grep(wtname,names(svydata))][,-1],   
    type="JKn", 
    scale=1, 
    rscales=1
  )  
  
  ### Conduct analyses using survey weights # HIV prevalence 
  res <- svyciprop(~I(hivstatusfinal==1),                  
                   design=svydata1,                  
                   method="mean",level=0.95,df=25) 
  
  (c(res[[1]],attr(res,"ci"))) # display results 
  table(svydata1$variables$hivstatusfinal) 
  
  
  # HIV awareness 
  
  svydata1_aware <- subset(svydata1, tri90 == 1 & hivstatusfinal != 99 & aware != 99) 
  
  res <- svyciprop(formula=~I(aware==1),                  
                   design= svydata1_aware,                  
                   method="mean",level=0.95,df=25) 
  
  (c(res[[1]],attr(res,"ci"))) # display results 
  table(svydata1_aware$variables$aware)  
  
  # ARV use 
  
  svydata1_art <- subset(svydata1, tri90 == 1 & aware == 1 & art != 99) 
  
  res <- svyciprop(formula=~I(art==1),                  
                   design= svydata1_art,                  
                   method="mean",level=0.95,df=25) 
  
  (c(res[[1]],attr(res,"ci"))) # display results 
  table(svydata1_art$variables$art)  # Viral suppression 
  
  svydata1_vls <- subset(svydata1, tri90 == 1 & aware == 1 & art == 1 & vls != 99) 
  
  res <- svyciprop(formula=~I(vls==1),                  
                   design=svydata1_vls,                  
                   method="mean",level=0.95,df=25) 
  
  (c(res[[1]],attr(res,"ci"))) # display results 
  
  table(svydata1_vls$variables$vls)  
  
  ### Create survey object, Taylor series linearization 
  
  # Recode analytic outcomes to 0/1 
  vars <- c('hivstatusfinal', 'aware', 'art', 'vls', 'tri90') 
  wtname <- 'btwt' 
  strataname <- 'varstrat' 
  clustername <- 'varunit' 
  svydata <- adultbio 
  
  for(i in 1:length(vars)){   
    varname <- vars[i]   
    if(is.factor(svydata[,varname])) {     
      svydata[,varname] <- as.numeric(levels(svydata[,varname]))[svydata[,varname]]     
      # coerces factor variables to numeric before recoding   
    }   
    
    svydata[,varname][(svydata[,varname]==2)] <- 0 
  }  
  
  # Extract analytic variables, base weights and variance unit/strata variables 
  #   and create survey object 
  svydata1 <- svydesign(   
    variables=svydata[,vars],   
    weights=svydata[,paste0(wtname,0)],   
    strata=svydata[,strataname],   
    ids=~svydata[,clustername],   
    nest=TRUE
  )  
  
  ### Conduct analyses using survey weights 
  # HIV prevalence 
  
  res <- svyciprop(~I(hivstatusfinal==1),                  
                   design=svydata1,                  
                   method="mean",level=0.95,df=25) 
  
  (c(res[[1]],attr(res,"ci"))) # display results 
  
  table(svydata1$variables$hivstatusfinal)  
  
  # HIV awareness 
  
  sbydata1_aware <- subset(svydata1, tri90 == 1 & hivstatusfinal != 99 & aware != 99) 
  res <- svyciprop(formula=~I(aware==1),                  
                   design=svydata1_aware,                  
                   method="mean",level=0.95,df=25) 
  
  (c(res[[1]],attr(res,"ci"))) # display results 
  table(svydata1_aware$variables$aware) 
  
  # ARV use 
  svydata1_art <- subset(svydata1, tri90 == 1 & aware == 1 & art != 99) 
  
  res <- svyciprop(formula=~I(art==1), 
                   design=svydata1_art, 
                   method="mean",
                   level=0.95,df=25) 
  
  (c(res[[1]],attr(res,"ci"))) # display results 
  table(svydata1_art$variables$art) 
  
  # Viral suppression 
  
  svydata1_vls <- subset(svydata1, tri90 == 1 & aware == 1 & art == 1 & vls != 99) 
  
  res <- svyciprop(formula=~I(vls==1), 
                   design=svydata1_vls, 
                   method="mean",
                   level=0.95,
                   df=25) 
  
  (c(res[[1]],attr(res,"ci"))) # display results 
  
  table(svydata1_vls$variables$vls) 
  
  ########################################################## 
  ### Example 2. Estimate adult HIV care by Hep B status ### 
  ###    Note: Hep B testing was not performed in all    ### 
  ###       PHIA surveys. hepb used for demonstration    ### 
  ###       purposes only                                ### 
  ##########################################################  
  
  ### Load in Adult IND dataset 
  
  adultind <- read.csv('C:/Desktop/zamphia2016adultind.csv') 
  
  ### Merge Hep B variable from Adult BIO dataset 
  
  adultbio <- read.csv('C:/Desktop/zamphia2016adultbio.csv') 
  adultbio <- adultbio[,c('personid','hepb')] 
  
  # Merge personid and hepb, all.x=TRUE option retains unmatched rows from master 
  
  adult <- merge(adultind, adultbio, by='personid', all.x = TRUE)  
  
  ### Ensure that weight variables are converted to numeric type, and drop  
  ### records with no interview weight 
  
  wtname <- 'intwt' 
  adult <- adult %>%   
    filter(indstatus == 1) %>%   mutate(across(contains(wtname), as.double))  ### Create survey object # Recode analytic variables to 0/1 vars <- c('hivcare', 'hepb') svydata <- adult for(i in 1:length(vars)){   varname <- vars[i]   if(is.factor(svydata[,varname])) {     svydata[,varname] <- as.numeric(levels(svydata[,varname]))[svydata[,varname]]     # coerces factor variables to numeric before recoding   }   else {     svydata[,varname] <- as.numeric(svydata[,varname])   }   svydata[,varname][(svydata[,varname]==2)] <- 0 }  # Extract analytic variables, base weights and JK replicate weights #   and create survey object, #   specifying variance estimation method "JK" and JK coefficients=1 svydata1 <- svrepdesign(   variables=svydata[,vars],   weights=svydata[,paste0(wtname,0)],   repweights=svydata[,grep(wtname,names(svydata))][,-1],   type="JKn", scale=1, rscales=1)  ### Conduct analyses using survey weights # HIV care by Hep B status svydata1_hepb <- subset(svydata1, hivcare %in% c(0, 1)) svyby(formula=~I(hivcare==1),by=~hepb,       design=svydata1_hepb,       FUN=svyciprop,vartype="ci",method="beta",df=25) table(svydata1_hepb$variables$hepb,       svydata1_hepb$variables$hivcare) 
  
  
  ################################################################## 
  ### Example 3. Estimate HIV prevalence among adults & children ### 
  ###               by 5-year age groups and wealth quintiles    ### 
  ##################################################################  
  
  ### Load in and merge Adult and Child BIO datasets 
  
  adultbio <- read.csv('C:/Desktop/zamphia2016adultbio.csv') 
  childbio <- read.csv('C:/Desktop/zamphia2016childbio.csv') 
  bio <- bind_rows(adultbio,childbio)  
  
  ### Merge wealth quintile variable from HH dataset 
  hh <- read.csv('C:/Desktop/zamphia2016hh.csv') 
  
  # Select columns hh2 <- select(hh, householdid, wealthquintile) bio2 <- select(bio, householdid, personid, hivstatusfinal, bt_status, starts_with("btwt")) # Merge bio3 <- merge(bio2,hh2,by='householdid')  ### Merge agegroup5population variable from Adult and Child IND datasets adultind <- read.csv('C:/Desktop/zamphia2016adultind.csv') childind <- read.csv('C:/Desktop/zamphia2016childind.csv')   # Select columns indvars <- c("personid", "agegroup5population", "country") ind <- bind_rows(select(adultind, all_of(indvars)),                   select(childind, all_of(indvars))) data <- merge(bio3, ind, by='personid', all.x=TRUE)  ### Create survey object # Recode analytic variables to 0/1 vars <- c('hivstatusfinal','wealthquintile','agegroup5population') wtname <- 'btwt' svydata <- data for(i in 1:length(vars)){   varname <- vars[i]   if(is.factor(svydata[,varname])) {     svydata[,varname] <- as.numeric(levels(svydata[,varname]))[svydata[,varname]]     # coerces factor variables to numeric before recoding   }   svydata[,varname][(svydata[,varname]==2)] <- 0 }  ### Ensure that weight variables are converted to numeric type, and drop  ### records with no blood weight svydata <- svydata %>%   filter(bt_status == 1) %>%   mutate(across(contains(wtname), as.double))  # Extract analytic variables, base weights and JK replicate weights #   and create survey object, #   specifying variance estimation method "JK" and JK coefficients=1 svydata1 <- svrepdesign(   variables=svydata[,vars],   weights=svydata[,paste0(wtname,0)],   repweights=svydata[,grep(wtname,names(svydata))][,-1],   type="JKn", scale=1, rscales=1)  ### Conduct analyses using survey weights # HIV prevalence by wealth quintile #svydata1_hivbywealth <- svydata1[!(svydata1$variables$wealthquintile %in% c(99,NA))] svydata1_hivbywealth <- subset(svydata1, !(wealthquintile %in% c(99, NA))) svyby(formula=~I(hivstatusfinal==1),by=~wealthquintile, design=svydata1_hivbywealth, FUN=svyciprop,vartype="ci",method="beta",df=25) table(svydata1_hivbywealth$variables$wealthquintile, svydata1_hivbywealth$variables$hivstatusfinal) # HIV prevalence by 5-year age groups svyby(formula=~I(hivstatusfinal==1),by=~agegroup5population, design=svydata1_hivbywealth, FUN=svyciprop,vartype="ci",method="beta",df=25) table(svydata1_hivbywealth$variables$agegroup5population, svydata1_hivbywealth$variables$hivstatusfinal) 70 PHIA Data Use Manual   PHIA Data Use Manual   71  ###################################################################### ### Example 4. Estimate child HIV prevalence by mother’s education ### ######################################################################  ### Prepare mother dataset for merging using Adult IND dataset adultind <- read.csv('C:/Desktop/zamphia2016adultind.csv') momind <- adultind[,c('personid','education')] # Rename mom’s variables names(momind)[names(momind)=='personid'] <- 'momid' names(momind)[names(momind)=='education'] <- 'momeducation' ### Load in Child BIO dataset childbio <- read.csv('C:/Desktop/zamphia2016childbio.csv')  ### Merge mother’s education variable from Adult IND dataset childbio2 <- merge(childbio,momind,by='momid')  ### Create survey object # Recode analytic variables to 0/1 vars <- c('hivstatusfinal','momid','momeducation') wtname <- 'btwt' svydata <- childbio2 for(i in 1:length(vars)){   varname <- vars[i]   if(is.factor(svydata[,varname])) {     svydata[,varname] <- as.numeric(levels(svydata[,varname]))[svydata[,varname]]     # coerces factor variables to numeric before recoding   }   svydata[,varname][(svydata[,varname]==2)] <- 0 }  ### Ensure that weight variables are converted to numeric type, and drop  ### records with no blood weight svydata <- svydata %>%   filter(bt_status == 1) %>%   mutate(across(contains(wtname), as.double))  # Extract analytic variables, base weights and JK replicate weights #   and create survey object, #   specifying variance estimation method "JK" and JK coefficients=1 svydata1 <- svrepdesign(   variables=svydata[,vars],   weights=svydata[,paste0(wtname,0)],   repweights=svydata[,grep(wtname,names(svydata))][,-1],   type="JKn", scale=1, rscales=1)  ### Conduct analyses using survey weights # Child’s HIV status by mother’s education svydata1_childhiv <- subset(svydata1, !(momeducation %in% c(99, NA))) svyby(formula=~I(hivstatusfinal==1),by=~momeducation,       design=svydata1_childhiv,       FUN=svyciprop,vartype="ci",method="beta",df=25) table(svydata1_childhiv$variables$momeducation,       svydata1_childhiv$variables$hivstatusfinal)    PHIA Data Use Manual   72  ###################################################################### ### Example 5. Estimate HIV prevalence in multi-country analysis   ### ######################################################################  ### Combine countries zambio <- read.csv('C:/Desktop/zamphia2016adultbio.csv') zimbio <- read.csv('C:/Desktop/zimphia2015adultbio.csv') malbio <- read.csv('C:/Desktop/mphia2015adultbio.csv')  zambio2 <- cbind(zambio[,c('country','hivstatusfinal')],zambio[,grep('btwt',names(zambio))])  zimbio2 <- cbind(zimbio[,c('country','hivstatusfinal')],zimbio[,grep('btwt',names(zimbio))])  malbio2 <- cbind(malbio[,c('country','hivstatusfinal')],malbio[,grep('btwt',names(malbio))]) combined.0 <- bind_rows(zambio2,zimbio2,malbio2)  ### Generate replicate weights for each country # Manually enter these parameters into matrices #   Country name, number replicate weights per country (retain same order) countries <- c('Zambia', 'Zimbabwe', 'Malawi') numwts <- c(253, 248, 250)  # Create new data.frame with new combined weights “b2wt” that are set equal to #   original JK replicate weights if replicate belongs to that country #   full sample weight if replicate belongs to a different country newwts <- data.frame(matrix(data=NA,nrow=nrow(combined.0),ncol=sum(numwts))) names(newwts) <- sprintf('b2wt%04d',1:sum(numwts)) # name newwts with leading zeroes combined <- cbind(combined.0,newwts)  w <- 0 # This is a cumulative counter for the number of total weights that have been generated # Loop over each country for (c in 1:length(countries)) {      countryname <- countries[c]      # Loop over number of weights in country   for (i in 1:numwts[c]) {          w <- w+1 # Increment cumulative counter by one          # Replace b2wt JK replicate weight with original JK replicate weight if in country     combined[combined$country== countryname,sprintf('b2wt%04d',w)] <-        combined[combined$country== countryname,sprintf('btwt%03d',i)]     # Replace b2wt JK replicate weight with full sample weight btwt0 if not in country     combined[combined$country!= countryname,sprintf('b2wt%04d',w)] <-        combined[combined$country!= countryname,'btwt0']   } }  ### Ensure that weight variables are converted to numeric type, and drop  ### records with no blood weight wtname <- 'b2wt' svydata <- combined %>%   mutate(across(contains(c(wtname, "btwt0")), as.double)) %>%   filter(!is.na(btwt0))   ### Create survey object, JK2 # Recode analytic outcomes to 0/1 vars <- c('country','hivstatusfinal') for(i in 1:length(vars)){ varname <- vars[i] if(is.factor(svydata[,varname])) { svydata[,varname] <- as.numeric(levels(svydata[,varname]))[svydata[,varname]] # coerces factor variables to numeric before recoding } svydata[,varname][(svydata[,varname]==2)] <- 0 } # Extract analytic variables, base weights and JK replicate weights #   and create survey object, #   specifying variance estimation method "JK" and JK coefficients=1 svydata1 <- svrepdesign( variables=svydata[,vars], weights=svydata[,'btwt0'], repweights=svydata[,grep(wtname,names(svydata))][,-1], type="JKn", scale=1, rscales=1) ### Conduct analyses using survey weights # HIV prevalence res <- svyciprop(~I(hivstatusfinal==1), design=svydata1, method="mean",level=0.95,df=25) (c(res[[1]],attr(res,"ci"))) # display results table(svydata1$variables$hivstatusfinal) 73 PHIA Data Use Manual   PHIA Data Use Manual   74  ########################################################################### ### Example 6. Analyses of data from optional modules                   ### ###          (HIV knowledge and violence)                               ### ###########################################################################  ### Load in Adult IND dataset adultind <- read.csv('C:/Desktop/zamphia2016adultind.csv')  ### Code new outcomes # All knowledge questions correct svydata <- select(adultind, -contains(c("intwt", "vmpswt"))) %>%   mutate(across(all_of(c("hivk_status", "onepartnr", "mosquito", "condoms", "sharefood", "healthyinf")), as.numeric)) svydata$knowledge[svydata$hivk_status==1 & svydata$onepartnr==1 & svydata$mosquito==1 & svydata$condoms==1 & svydata$sharefood==1 & svydata$healthyinf==1] <- 1 svydata$knowledge[svydata$hivk_status==1 & !(svydata$onepartnr==1 & svydata$mosquito==1 & svydata$condoms==1 & svydata$sharefood==1 & svydata$healthyinf==1)] <- 0  ### Ensure that weight variables are converted to numeric type and ### drop those with missing weights wtname <- 'hivkpswt' svydata <- svydata %>%   mutate(across(contains(wtname), as.double)) %>%   filter(!is.na(hivkpswt0))  # Extract analytic variables, base weights and JK replicate weights #   and create survey object, #   specifying variance estimation method "JK" and JK coefficients=1 vars <- c('knowledge','hivk_status') svydata1 <- svrepdesign(   variables=svydata[,vars],   weights=svydata[,paste0(wtname,0)],   repweights=svydata[,grep(wtname,names(svydata))][,-1],   type="JKn", scale=1, rscales=1)  ### Conduct analyses using survey weights # Proportion of HIV knowledge module respondents answering all 5 questions correctly svydata1_hivknowledge <- subset(svydata1, hivk_status == 1) res <- svyciprop(~I(knowledge==1),                  design=svydata1,                  method="mean",level=0.95,df=25) (c(res[[1]],attr(res,"ci"))) # display results table(svydata1_hivknowledge$variables$knowledge)  ### Code new outcomes # Sexual or physical violence by partner in last 12 months svydata <- adultind %>%   mutate(across(all_of(c("vmflag", "sexualviolencepart12mo", "physicalviolencepart12mo")), as.numeric))  svydata$violencepart12mo[svydata$vmflag==1 & (svydata$sexualviolencepart12mo==1 | svydata$physicalviolencepart12mo==1)] <- 1 svydata$violencepart12mo[svydata$vmflag==1 & !(svydata$sexualviolencepart12mo==1 | svydata$physicalviolencepart12mo==1)] <- 0  ### Ensure that weight variables are converted to numeric type and ### drop those with missing weights wtname <- 'vmpstw' svydata <- svydata %>%   mutate(across(contains(wtname), as.double)) %>%   filter(!is.na(vmpstw0)) # Extract analytic variables, base weights and JK replicate weights #   and create survey object, #   specifying variance estimation method "JK" and JK coefficients=1 vars <- c('violencepart12mo','vmflag') svydata1 <- svrepdesign( variables=svydata[,vars], weights=svydata[,paste0(wtname,0)], repweights=svydata[,grep(wtname,names(svydata))][,-1], type="JKn", scale=1, rscales=1) ### Conduct analyses using survey weights # Proportion of violence module respondents experiencing either sexual or physical violence from a partner in the past 12 months svydata1_viol <- subset(svydata1, vmflag == 1) res <- svyciprop(~I(violencepart12mo==1), design=svydata1_viol, method="mean",level=0.95,df=25) (c(res[[1]],attr(res,"ci"))) # display results table(svydata1_viol$variables$violencepart12mo)
  
  
  