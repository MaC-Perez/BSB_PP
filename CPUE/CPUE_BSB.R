rm(list = ls())
library(readxl)
library(tidyverse)

# station data for Massachusetts, Buzzards Bay and Rhode Island

maStationIDBB <- read_excel('data/All stations_withAddOns.xlsx',
                            sheet = 'Buzzards Bay') %>%
                 rename(StationID = `Station ID`, StrataDepthRgM = `Depth range (m)`,
                 Strata = `Strata code`, Latitude = LATITUDE, Longitude = LONGITUDE)

maStationIDMB <- read_excel('data/All stations_withAddOns.xlsx',
                            sheet = 'Mass Bay') %>%
                 rename(StationID = `Station ID`, StrataDepthRgM = `Depth range (m)`,
                 Strata = `Strata code`, Latitude = LATITUDE, Longitude = LONGITUDE)

maStationID <- bind_rows(maStationIDBB, maStationIDMB)

maTrawlBB <- read_csv('data/Buzzards Bay 2008-2020.csv') %>%
             rename(`vent trap count` = `vented trap count`) %>%
             # For now remove the expanded depth zones
             filter(nchar(StationID) == 4)

maTrawlMB <- read_csv('data/Mass Bay 2007-2020.csv')

maTrawl <- bind_rows(maTrawlBB, maTrawlMB) %>%
           left_join(maStationID, by = c('StationID', 'Strata'))

riTrawl <- read_excel('data/VentlessData_BSB_Oct_2021.xlsx',
                      sheet = 'Trawl')

riTrap <- read_excel('data/VentlessData_BSB_Oct_2021.xlsx',
                     sheet = 'Trap') %>%
          mutate(`Sea Bass` = ifelse(Comments == 'EMPTY' & is.na(`Sea Bass`), 
                             0, `Sea Bass`)) # applies to 1 record

# Adjust nomenclature for MA data
maTrawlAdj <- maTrawl %>%
  rename(trapCount_Unvented = `ventless trap count`,
         trapCount_Vented = `vent trap count`,
         bsbCount_Unvented = `ventless count BSB`,
         bsbCount_Vented = `vent count BSB`,
         Region = Bay,
         TrawlNum = SampleAutoID,
         depthStrata = StrataDepthRgM) %>%
  mutate(depthStrata = paste(depthStrata, 'meters'),
         depthStrata = gsub(pattern = ' to ', replacement = '-', x = depthStrata),
         depthStrata = sub(pattern = '[[:blank:]]', replacement = '-', 
                           x = depthStrata),
         Date = lubridate::as_date(Date, format = '%m/%d/%Y'),
         State = 'MA') %>%
  select(Region, Date, TrawlNum, trapCount_Unvented, trapCount_Vented,
         bsbCount_Vented, bsbCount_Unvented, Latitude, Longitude, depthStrata,
         State)

# The MA ventless data are aggregated by trawl, so do the same
# for RI's data and then combine with MA
mariTrawl <- riTrap %>%
  group_by(Region, Date, TrawlNum, `Trap Type`) %>%
  summarize(trapCount = n(),
            bsbCount = sum(`Sea Bass`),
            .groups = 'drop') %>%
  pivot_wider(c(Region, Date, TrawlNum), 
              names_from = `Trap Type`, values_from = c(trapCount, bsbCount)) %>%
  # Remove 3 records that had only vented or ventless hauls
  filter(!(is.na(trapCount_Vented) | is.na(trapCount_Unvented))) %>%
  left_join(riTrawl, by = c("Region", "Date", "TrawlNum")) %>%
  mutate(`Depth Strata` = gsub(pattern = '[[:blank:]]', replacement = '-', 
                               x = `Depth Strata`),
    State = 'RI') %>%
  rename(depthStrata = `Depth Strata`) %>%
  select(Region, Date, TrawlNum, trapCount_Unvented, trapCount_Vented,
         bsbCount_Unvented, bsbCount_Vented, 
         Latitude, Longitude, depthStrata, State) %>%
  bind_rows(maTrawlAdj) %>%
  mutate(ventedCPUE = bsbCount_Vented / trapCount_Vented,
         unventedCPUE = bsbCount_Unvented / trapCount_Unvented,
         StdCatch = ventedCPUE * trapCount_Vented + 
           unventedCPUE * trapCount_Unvented,
         Year = lubridate::year(Date),
         Month = lubridate::month(Date),
         depthStrata = sub(pattern = '[[:blank:]]', replacement = '', 
                           x = depthStrata)) %>%
  filter(!is.na(depthStrata))


#write.csv(mariTrawl, file = "MA_BB_RI_TRAWL.csv", row.names = T)
#############################################################################################
rm(list = ls())
setwd("C:/Users/macristina.perez/Documents/UMassD/2021/winter/Practicum/ventless/CPUE")

#read trawl data from three bays
data <- read.csv('MA_BB_RI_TRAWL.csv', sep= ",", header=T)
#data <- read.csv('newdata.csv', sep= ",", header=T)

names(data)
head(data)
# create some tables with number of fish and cpue by year month and strata  
# MA and BUZZARDS

count_vent <- data %>% group_by(Bay, Year) %>% 
  summarise (count = sum(bsbCount_Vented, na.rm = TRUE),
             countn = n())

count_ventless <- data %>% group_by(Bay, Year) %>% 
  summarise (count = sum(bsbCount_Unvented, na.rm = TRUE),
             countn = n())

cpue_vent <- data %>% group_by(Bay, Year) %>% 
  summarise (cpue = mean(ventedCPUE, na.rm = TRUE),
             cpuen = n())

cpue_less <- data %>% group_by(Bay, Year) %>% 
  summarise (cpue = mean(unventedCPUE, na.rm = TRUE),
             cpuen = n())

zeros_vent <- data %>% group_by(Bay, Year) %>% filter (bsbCount_Vented== 0) %>%
  summarise (n = n())

zeros_less <- data %>% group_by(Bay, Year) %>% filter (bsbCount_Unvented== 0) %>%
  summarise (n = n())


summary<-data.frame(count_vent$Bay, count_vent$Year, count_vent$count, count_ventless$count, 
                    cpue_vent$cpue, cpue_less$cpue, cpue_less$cpuen, zeros_less$n, zeros_vent$n)   

#write.csv(summary, file = "summary.csv", row.names = T)

####################################################
################### CPUE STANDARDIZATION ###########
####################################################
rm(list = ls())
setwd("C:/Users/macristina.perez/Documents/UMassD/2021/winter/Practicum/ventless/CPUE")
data <- read.csv('MA_BB_RI_TRAWL.csv', sep= ",", header=T)

library(MASS)
library(dplyr)
library(lmtest)
library(ggplot2)

RI <- data %>% tibble::as_tibble() %>% 
        filter(Bay=="Rhode Island") 

BB <- data %>% tibble::as_tibble() %>% 
  filter(Bay=="BuzzardsBay") 

MA <- data %>% tibble::as_tibble() %>% 
  filter(Bay=="MassachusettsBay") 


p1 <- ggplot(data, aes(x=Bay, y=ventedCPUE)) + geom_boxplot() + 
  theme(axis.text.x=element_text(color = "black", size=8, angle=0, vjust=.8, hjust=0.5)) +
  xlab('Zona') + ylab('Rendimiento (nfish/trap.)') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8))

p1

p2 <- ggplot(data, aes(x=Bay, y=unventedCPUE)) + geom_boxplot() + 
  theme(axis.text.x=element_text(color = "black", size=8, angle=0, vjust=.8, hjust=0.5)) +
  xlab('Zona') + ylab('Rendimiento (nfish/trap.)') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8))

p2

PseudoR <- function (objeto)
{
  salida <-  1-(objeto$deviance/objeto$null.deviance)
  return(salida)
  end
}

########## MODELS ##################

ppi <- 300
#png("hist_RI.png", width=10*ppi, height=4.5*ppi, res=ppi)
par(mfcol=c(1,2))
nbb = 30
hist(RI$unventedCPUE, main='RI unvented CPUE', xlim = c(0,15))
hist(RI$ventedCPUE, main='RI vented CPUE',xlim = c(0,15))
#dev.off()

ppi <- 300
#png("hist_MA.png", width=10*ppi, height=4.5*ppi, res=ppi)
par(mfcol=c(1,2))
nbb = 30
hist(MA$unventedCPUE, main='MA unvented CPUE',xlim = c(0,5))
hist(MA$ventedCPUE, main='MA vented CPUE',xlim = c(0,5))
#dev.off()

ppi <- 300
#png("hist_BB.png", width=10*ppi, height=4.5*ppi, res=ppi)
par(mfcol=c(1,2))
nbb = 30
hist(BB$unventedCPUE, main='BB unvented CPUE',xlim = c(0,15))
hist(BB$ventedCPUE, main='BB vented CPUE',xlim = c(0,15))
#dev.off()

m1 <- glm.nb(unventedCPUE ~ as.factor(Year) + as.factor(Month)+ as.factor(depthStrata), data = RI, link = log)
m1$summary<-summary(m1)
anova(m1)
PseudoR(m1)
coeftest(m1)

m2 <- glm(unventedCPUE ~ as.factor(Year) + as.factor(Month)+ as.factor(depthStrata), family = "poisson", data = RI)
m2$summary<-summary(m2)
anova(m2)
PseudoR(m2)
coeftest(m2)

m3 <- glm(unventedCPUE ~ as.factor(Year) + as.factor(Month)+ as.factor(depthStrata), family = "quasipoisson", data = RI)
m3$summary<-summary(m3)
anova(m3)
PseudoR(m3)
coeftest(m3)

x11()
ppi <- 300
#png("termplot.png", width=10*ppi, height=4.5*ppi, res=ppi)
par(mfcol=c(2,2))
nbb = 30
termplot(m2, se=T, ylim="free")
#dev.off()

x11()
par(mfcol=c(2,2))
plot(m1)


newdat <- RI %>%mutate(presence=ifelse(unventedCPUE>0,1,0))
m_bin <- glm(presence~1, data = newdat,
             family = binomial(link=logit))
m_pos <- glm(unventedCPUE~1, data = filter(newdat,presence==1),
             family= Gamma(link=log))
plogis(coef(m_bin))
#intercept 0.60515

exp(coef(m_pos))
#intercept 1.398476

#model predictions
newdat$bin_predict <- predict(m_bin, newdata = newdat,
                              type = 'response')
newdat$pos_predict <- predict(m_pos, newdata = newdat,
                              type = 'response')
newdat <- newdat %>%
  mutate(combined_predict =
           bin_predict * pos_predict)


m1 <- glm(presence~depthStrata, data = newdat,
          family = binomial(link=logit))
m2 <- glm(unventedCPUE~depthStrata, data = filter(newdat,presence==1),
          family= Gamma(link=log))

#model predictions
newdat$bin_predict2 <- predict(m1, newdata = newdat,
                               type = 'response')
newdat$pos_predict2 <- predict(m2, newdata = newdat,
                               type = 'response')
newdat <- newdat %>%
  mutate(combined_predict2 =
           bin_predict2 * pos_predict2)

#write.csv(newdat, file = "newdata.csv", row.names = T)





# SAM
Mod <- glm(count ~ year + depth, family = 'poisson', data = trapDat)
dummyData <- tibble(year = 2006:2020, depth = mean(trapDat$depth))
idx <- predict(Mod, newdata = 'dummyData', type = 'response', se.fit = TRUE)







## logit-poisson
## "art ~ ." is the same as "art ~ . | .", i.e.
## "art ~ fem + mar + kid5 + phd + ment | fem + mar + kid5 + phd + ment"

##### just for integer values######################################
fm_hp1 <- hurdle(unventedCPUE ~ as.factor(Year)+ as.factor(Month)+ as.factor(depthStrata), data = temp,
                 dist="poisson", zero.dist="binomial", link="logit")

summary(fm_hp1)

##### just for integer values######################################
## geometric-poisson
fm_hp2 <- hurdle(unventedCPUE ~ as.factor(Year)+ as.factor(Month)+ as.factor(depthStrata), data = temp, zero = "geometric")
summary(fm_hp2)

## logit and geometric model are equivalent
coef(fm_hp1, model = "zero") - coef(fm_hp2, model = "zero")

##### just for integer values######################################
## logit-negbin
fm_hnb1 <- hurdle(unventedCPUE ~ as.factor(Year)+ as.factor(Month)+ as.factor(depthStrata), data = temp, dist = "negbin")
summary(fm_hnb1)

##### just for integer values######################################
## negbin-negbin
## (poorly conditioned zero hurdle, note the standard errors)
fm_hnb2 <- hurdle(unventedCPUE ~ as.factor(Year)+ as.factor(Month)+ as.factor(depthStrata), data = temp, dist = "negbin", zero = "negbin")
summary(fm_hnb2)


















years<-seq(2006,2020,1)
cpue_mod1 <- exp(c(m1$summary$coefficients[1,1],  
                   m1$summary$coefficients[1,1] + m1$summary$coefficients[2:length(years),1]))
plot(2006:2020,cpue_mod1, type="b")

cpue_mod2 <- exp(c(m2$summary$coefficients[1,1],  
                   m2$summary$coefficients[1,1] + m2$summary$coefficients[2:length(years),1]))
plot(2006:2020,cpue_mod2, type="b")


















# create some tables with number of fish and cpue by year month and strata  
# RHODE ISLAND.

#vented
vent_cpue <- dataRI %>% group_by(year,Date, TrawlNum) %>% filter(Trap_Type=="Vented") %>%
  summarise (count = n())

vent_fish <- dataRI %>% group_by(year,Date, TrawlNum) %>% filter(Trap_Type=="Vented") %>%
  summarise (count_fish = sum(`Sea Bass`))

cpue_vent<- vent_cpue$count/vent_fish$count_fish 

#cpue_vent<-data.frame(vent_cpue$year, vent_cpue$Date, cpue_vent)   

#ventless
less_cpue <- dataRI %>% group_by(year,Date, TrawlNum) %>% filter(Trap_Type=="Unvented") %>%
  summarise (count = n())

less_fish <- dataRI %>% group_by(year,Date, TrawlNum) %>% filter(Trap_Type=="Unvented") %>%
  summarise (count_fish = sum(`Sea Bass`))

cpue_less<- count_less$count/count_fish$count_fish 

cpue<-data.frame(less_cpue$year, less_cpue$Date, cpue_less)   

#write.csv(cpue, file = "CPUEless_RI.csv", row.names = T)

