# Código respaldo
#Data
chi09 <-  as.data.frame(read_sas("~/Dropbox/book_authoritarianism/Data/Original/2009/islchlc2.sas7bdat"))
chi16 <-  as.data.frame(read_sas("~/Dropbox/book_authoritarianism/Data/Original/2016/islchlc3.sas7bdat"))
mex09 <-  as.data.frame(read_sas("~/Dropbox/book_authoritarianism/Data/Original/2009/islmexc2.sas7bdat"))
mex16 <-  as.data.frame(read_sas("~/Dropbox/book_authoritarianism/Data/Original/2016/islmexc3.sas7bdat"))
col09 <- as.data.frame(read_sas("~/Dropbox/book_authoritarianism/Data/Original/2009/islcolc2.sas7bdat"))
col16 <- as.data.frame(read_sas("~/Dropbox/book_authoritarianism/Data/Original/2016/islcolc3.sas7bdat"))
dom09 <- as.data.frame(read_sas("~/Dropbox/book_authoritarianism/Data/Original/2009/isldomc2.sas7bdat"))
dom16 <- as.data.frame(read_sas("~/Dropbox/book_authoritarianism/Data/Original/2016/isldomc3.sas7bdat"))
pry <- as.data.frame(read_sas("~/Dropbox/book_authoritarianism/Data/Original/2009/islpryc2.sas7bdat"))
gtm <- as.data.frame(read_sas("~/Dropbox/book_authoritarianism/Data/Original/2009/islgtmc2.sas7bdat"))
per <- as.data.frame(read_sas("~/Dropbox/book_authoritarianism/Data/Original/2016/islperc3.sas7bdat"))

########################################################
library(purrr)

aux09_i <- c("chi09", "mex09", "col09", "dom09","gtm", "pry","")
aux16_i <- c("chi16", "mex16", "col16", "dom16", "", "","per")
pais0_i <- c("CHL","MEX","COL","DOM","GTM","PRY","PER")
pais_i  <- c("CHI","MEX","COL","DOM","GTM","PRY","PER")

df_final <- NULL 

for(i in 1:7){
  
  if(aux09_i[i]!="") {aux09 <- get(aux09_i[i])}
  if(aux16_i[i]!="") {aux16 <- get(aux16_i[i])}
  pais0 = pais0_i[i]
  pais = pais_i[i]
  
  aux09 <- aux09 %>% 
    mutate(dictaa = case_when(
      LS2P03D %in% c(1,2) ~ 1,
      LS2P03D %in% c(3,4) ~ 2,
      is.na(dictaa) ~ dictaa,
      TRUE ~ 0
    )) %>% 
    mutate(dictab = case_when(
      LS2P03E %in% c(1,2) ~ 1,
      LS2P03E %in% c(3,4) ~ 2,
      is.na(dictab) ~ dictaab,
      TRUE ~ 0
    )) 
  aux16 <- aux16 %>% 
    mutate(dictaa = case_when(
      LS3G02D %in% c(1,2) ~ 1,
      LS3G02D %in% c(3,4) ~ 2,
      is.na(dictaa) ~ dictaa,
      TRUE ~ 0
    )) %>% 
    mutate(dictab = case_when(
      LS3G02E %in% c(1,2) ~ 1,
      LS3G02E  %in% c(3,4) ~ 2,
      is.na(dictab) ~ dictab,
      TRUE ~ 0
    )) 
  
  if(aux09_i[i]!="" & aux16_i[i]!=""){ mergeiccs=full_join(aux16, aux09) }
  if(aux09_i[i]!="" & aux16_i[i]==""){ mergeiccs= aux09 }
  if(aux09_i[i]=="" & aux16_i[i]!=""){ mergeiccs= aux16 }
  
  mergesvy <-  mergeiccs %>%
    filter(country==pais0) %>%
    as_survey_design(
      strata = jkzones, 
      weights = totwgts, 
      ids = jkreps, 
      nest = TRUE)
  
  # frequency table via taylor series linearization
  mergesvy[["variables"]][["dictaa"]] <- as.character(mergesvy[["variables"]][["dictaa"]])
  
  table_freq_01 <- mergesvy %>%
    dplyr::group_by(time, dictaa) %>%
    summarize(proportion = survey_mean(,na.rm=TRUE))
  #help(srvyr)
  
  df = data.frame(table_freq_01, digits=2)
  
  df= df %>%
    mutate(perc = proportion * 100) %>%
    mutate(p=round(perc, 2)) %>%
    mutate(label=factor(dictaa, labels=c("Disagree", "Agree"))) %>% 
    mutate(country=pais)
  
  df1_aux <- df
  
  df_final <- rbind(df_final, df1_aux)
  
}

#Puntajes prueba de conocimiento 

chile <- mergeiccs %>% filter(country=="CHL" & time==2016)
a <- weighted.mean(chile$pv1civ, chile$totwgts)
b <- weighted.mean(chile$pv2civ, chile$totwgts)
c <- weighted.mean(chile$pv3civ, chile$totwgts)
d <- weighted.mean(chile$pv4civ, chile$totwgts)
e <- weighted.mean(chile$pv5civ, chile$totwgts)
weighted.mean(chile$civic_knowledge, chile$totwgts)
mean(chile$civic_knowledge)
mean(a,b,c,d,e)

chile <- mergeiccs %>% filter(country=="CHL" & time==2009)
a <- weighted.mean(chile$pv1civ, chile$totwgts)
b <- weighted.mean(chile$pv2civ, chile$totwgts)
c <- weighted.mean(chile$pv3civ, chile$totwgts)
d <- weighted.mean(chile$pv4civ, chile$totwgts)
e <- weighted.mean(chile$pv5civ, chile$totwgts)

weighted.mean(chile$civic_knowledge, chile$totwgts)
mean(chile$civic_knowledge)
mean(a,b,c,d,e)
