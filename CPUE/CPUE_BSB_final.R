rm(list = ls())
library(readxl)
library(tidyverse)

#read trawl data from three bays
data <- read.csv('CPUE/MA_BB_RI_TRAWL.csv', sep= ",", header=T)

# creates a summary with number of fish and cpue by year month and strata  
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


library(MASS)
library(hurdlr)
library(pscl)
library(lmtest)
library(ggplot2)
library(tidyverse)

RI <- data %>% tibble::as_tibble() %>% 
        filter(Bay=="Rhode Island") 

BB <- data %>% tibble::as_tibble() %>% 
  filter(Bay=="BuzzardsBay") 

MA <- data %>% tibble::as_tibble() %>% 
  filter(Bay=="MassachusettsBay") 


p1 <- ggplot(data, aes(x=Bay, y=ventedCPUE)) + geom_boxplot() + 
  theme(axis.text.x=element_text(color = "black", size=8, angle=0, vjust=.8, hjust=0.5)) +
  xlab('Zone') + ylab('nfish/trap') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8))

p1

p2 <- ggplot(data, aes(x=Bay, y=unventedCPUE)) + geom_boxplot() + 
  theme(axis.text.x=element_text(color = "black", size=8, angle=0, vjust=.8, hjust=0.5)) +
  xlab('Zone') + ylab('nfish/trap') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8))

p2


########## MODELS ##################

#function to calculate explained deviance
PseudoR <- function (objeto)
{
  salida <-  1-(objeto$deviance/objeto$null.deviance)
  return(salida)
  end
}

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

##########################
###### BUZZARDS BAY ######
##########################

# First model GLM including year month and strata, poisson family 
# Unvented BB

BBunv <- glm(bsbCount_Unvented ~ as.factor(Year) + as.factor(Month)+ as.factor(depthStrata), family = "poisson", 
              data = BB, offset(trapCount_Unvented))

BBunv$summary<-summary(BBunv)
anova(BBunv)
PseudoR(BBunv)

cbind(coef(BBunv), confint(BBunv))

# Second model GLM including year month and strata, poisson family 
# Vented BB

BBv <- glm(bsbCount_Vented ~ as.factor(Year) + as.factor(Month)+ as.factor(depthStrata), family = "poisson", 
            data = BB, offset(trapCount_Vented))

BBv$summary<-summary(BBv)
anova(BBv)
PseudoR(BBv)

cbind(coef(BBv), confint(BBv))

#check overdispersion model 1 Unvented 
E2 = resid(BBunv, type = "pearson")
N  = nrow(BB)
p  = length(coef(BBunv))   
OM1<- sum(E2^2) / (N - p)

#check overdispersion model 2 Vented
E2 = resid(BBv, type = "pearson")
N  = nrow(BB)
p  = length(coef(BBv))   
OM2<- sum(E2^2) / (N - p)

AIC(BBunv,BBv)

# select model to plot
model<-BBunv #BBv

par(mfcol=c(2,2))
termplot(model, se=T, ylim="free")

drop1(model, test="F")
lrtest(BBunv, BBv)  

# Third model negative binomial GLM including year month and strata 
# Unvented BB

BBunv_nb<- glm.nb(bsbCount_Unvented ~ as.factor(Year) + as.factor(Month)+ as.factor(depthStrata), data = BB,
             link = log, offset(trapCount_Unvented))

BBunv_nb$summary<-summary(BBunv_nb)
anova(BBunv_nb)
PseudoR(BBunv_nb)

cbind(coef(BBunv_nb), confint(BBunv_nb))

# Fourth model negative binomial GLM including year month and strata 
# Vented BB

BBv_nb <- glm.nb(bsbCount_Vented ~ as.factor(Year) + as.factor(Month)+ as.factor(depthStrata), data = BB, 
               link = log, offset(trapCount_Vented))

BBv_nb$summary<-summary(BBv_nb)
anova(BBv_nb)
PseudoR(BBv_nb)

cbind(coef(BBv_nb), confint(BBv_nb))

E2 = resid(BBunv_nb, type = "pearson")
N  = nrow(BB)
p  = length(coef(BBunv_nb))   
OM3<-sum(E2^2) / (N - p)

E2 = resid(BBv_nb, type = "pearson")
N  = nrow(BB)
p  = length(coef(BBv_nb))   
OM4<-sum(E2^2) / (N - p)

AIC(BBunv_nb,BBv_nb)

##########################
###### RHODE ISLAND ######
##########################

# First model GLM including year month and strata, poisson family 
# Unvented RI

RIunv <- glm(bsbCount_Unvented ~ as.factor(Year) + as.factor(Month)+ as.factor(depthStrata), 
          family = "poisson", data = RI, offset(trapCount_Unvented))

RIunv$summary<-summary(RIunv)
anova(RIunv)
PseudoR(RIunv)
AIC(RIunv)

cbind(coef(RIunv), confint(RIunv))

#check overdispersion 
E2 = resid(RIunv, type = "pearson")
N  = nrow(RI)
p  = length(coef(RIunv))   
OM5<- sum(E2^2) / (N - p)

# Second model GLM including year month and strata, poisson family 
# Vented RI

RIv <- glm(bsbCount_Vented ~ as.factor(Year) + as.factor(Month)+ as.factor(depthStrata), 
            family = "poisson", data = RI, offset(trapCount_Vented))

RIv$summary<-summary(RIv)
anova(RIv)
PseudoR(RIv)
AIC(RIv)

AIC(RIv, RIunv)

cbind(coef(RIv), confint(RIv))

#check overdispersion 
E2 = resid(RIv, type = "pearson")
N  = nrow(RI)
p  = length(coef(RIv))   
OM6<- sum(E2^2) / (N - p)


# Third model negative binomial GLM including year month and strata 
# Unvented RI

RIunv_nb <- glm.nb(bsbCount_Unvented ~ as.factor(Year) + as.factor(Month)+ as.factor(depthStrata), 
             data = RI, link = log, offset(trapCount_Unvented))

RIunv_nb$summary<-summary(RIunv_nb)
anova(RIunv_nb)
PseudoR(RIunv_nb)

cbind(coef(RIunv_nb), confint(RIunv_nb))

E2 = resid(RIunv_nb, type = "pearson")
N  = nrow(RI)
p  = length(coef(RIunv_nb))   
OM7<-sum(E2^2) / (N - p)


# Fourth model negative binomial GLM including year month and strata 
# Vented RI

RIv_nb <- glm.nb(bsbCount_Vented ~ as.factor(Year) + as.factor(Month)+ as.factor(depthStrata), 
               data = RI, link = log, offset(trapCount_Vented))

RIv_nb$summary<-summary(RIv_nb)
anova(RIv_nb)
PseudoR(RIv_nb)

cbind(coef(RIv_nb), confint(RIv_nb))

E2 = resid(RIv_nb, type = "pearson")
N  = nrow(RI)
p  = length(coef(RIv_nb))   
OM8<-sum(E2^2) / (N - p)

AIC(RIunv_nb, RIv_nb)

# UNVENTED CPUE BUZZARDS BAY SUMMARY
 
mean_BBunv <- BB %>% group_by(Year) %>% 
  summarise (cpue = mean(bsbCount_Unvented, na.rm = TRUE),
             cpuen = n())

cpue_BBunv <- exp(c(BBunv$summary$coefficients[1,1],  
                 BBunv$summary$coefficients[1,1] + BBunv$summary$coefficients[2:12,1]))
cpue_BBunv<-as.data.frame(cpue_BBunv)

BBunv_nb <- exp(c(BBunv_nb$summary$coefficients[1,1],  
               BBunv_nb$summary$coefficients[1,1] + BBunv_nb$summary$coefficients[2:12,1]))
BBunv_nb<-as.data.frame(BBunv_nb)

BBpred_unv<- cbind(mean_BBunv[1],mean_BBunv[2], cpue_BBunv$cpue_BBunv, BBunv_nb$BBunv_nb)
colnames(BBpred_unv) <- c('year','mean CPUE','BBunv','BBunv_nb')

BBpred_plot_unv <- BBpred_unv %>%
  pivot_longer(cols=2:ncol(.), names_to = "n_fish") %>%
  # Add in missing 2013 data (will be NAs)
  complete(year = min(BBpred_unv$year):max(BBpred_unv$year),
           n_fish)

# Specify x axis breaks
xbrk <- seq(from = min(BBpred_plot_unv$year), to = max(BBpred_plot_unv$year), by =2)

# Create the plot
ppi <- 300
png("Buzzards Bay unvented.png", width=10*ppi, height=4.5*ppi, res=ppi)
plot1<- BBpred_plot_unv %>% 
  ggplot() +
  aes(x=year, y = value) +
  geom_line(aes(col = n_fish), linewidth=1) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = xbrk) +
  labs(x="Year", y="Number of Fish")+
  guides(col = guide_legend(nrow = 1))+
           ggtitle("Buzzards Bay unvented")
plot1
dev.off()


# UNVENTED CPUE RHODE ISLAND

mean_RIunv <- RI %>% group_by(Year) %>% 
  summarise (cpue = mean(bsbCount_Unvented, na.rm = TRUE),
             cpuen = n())

cpue_RIunv <- exp(c(RIunv$summary$coefficients[1,1],  
                 RIunv$summary$coefficients[1,1] + RIunv$summary$coefficients[2:15,1]))
cpue_RIunv<-as.data.frame(cpue_RIunv)

RIunv_nb <- exp(c(RIunv_nb$summary$coefficients[1,1],  
               RIunv_nb$summary$coefficients[1,1] + RIunv_nb$summary$coefficients[2:15,1]))
RIunv_nb<-as.data.frame(RIunv_nb)

RIpred_unv<- cbind(mean_RIunv[1],mean_RIunv[2], cpue_RIunv$cpue_RIunv, RIunv_nb$RIunv_nb)
colnames(RIpred_unv) <- c('year','mean CPUE','RIunv','RIunv_nb')

RIpred_plot_unv <- RIpred_unv %>% pivot_longer(cols=2:ncol(.), names_to = "n_fish")

## initial value negative 

ppi <- 300
png("Rhode Island unvented.png", width=10*ppi, height=4.5*ppi, res=ppi)
plot2<- RIpred_plot_unv %>% 
  ggplot() +
  aes(x=year, y = value) +
  geom_line(aes(col = n_fish), linewidth=1) +
  theme(legend.position = "bottom") +
  labs(x="Year", y="Number of Fish")+
  guides(col = guide_legend(nrow = 1))+
  ggtitle("Rhode Island unvented")

plot2
dev.off()


# VENTED CPUE BUZZARDS BAY SUMMARY

mean_BBv <- BB %>% group_by(Year) %>% 
  summarise (cpue = mean(bsbCount_Vented, na.rm = TRUE),
             cpuen = n())

cpue_BBv <- exp(c(BBv$summary$coefficients[1,1],  
                 BBv$summary$coefficients[1,1] + BBv$summary$coefficients[2:12,1]))
cpue_BBv<-as.data.frame(cpue_BBv)

BBv_nb <- exp(c(BBv_nb$summary$coefficients[1,1],  
                  BBv_nb$summary$coefficients[1,1] + BBv_nb$summary$coefficients[2:12,1]))
BBv_nb<-as.data.frame(BBv_nb)

BBpred_ven<- cbind(mean_BBv[1],mean_BBv[2], cpue_BBv$cpue_BBv, BBv_nb$BBv_nb)
colnames(BBpred_ven) <- c('year','mean CPUE','BBv','BBv_nb')

BBpred_plot_ven <- BBpred_ven %>%
  pivot_longer(cols=2:ncol(.), names_to = "n_fish") %>%
  # Add in missing 2013 data (will be NAs)
  complete(year = min(BBpred_ven$year):max(BBpred_ven$year),
           n_fish)

# Specify x axis breaks
xbrk <- seq(from = min(BBpred_plot_ven$year), to = max(BBpred_plot_ven$year), by =2)

# Create the plot
plot3<- BBpred_plot_ven %>% 
  ggplot() +
  aes(x=year, y = value) +
  geom_line(aes(col = n_fish), linewidth=1) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = xbrk) +
  labs(x="Year", y="Number of Fish")+
  guides(col = guide_legend(nrow = 1))+
  ggtitle("Buzzards Bay vented")


# VENTED CPUE RHODE ISLAND

mean_RIv <- RI %>% group_by(Year) %>% 
  summarise (cpue = mean(bsbCount_Vented, na.rm = TRUE),
             cpuen = n())

cpue_RIv <- exp(c(RIv$summary$coefficients[1,1],  
                 RIv$summary$coefficients[1,1] + RIv$summary$coefficients[2:15,1]))
cpue_RIv<-as.data.frame(cpue_RIv)

RIv_nb <- exp(c(RIv_nb$summary$coefficients[1,1],  
               RIv_nb$summary$coefficients[1,1] + RIv_nb$summary$coefficients[2:15,1]))
RIv_nb<-as.data.frame(RIv_nb)

RIpred_ven<- cbind(mean_RIv[1],mean_RIv[2], cpue_RIv$cpue_RIv, RIv_nb$RIv_nb)
colnames(RIpred_ven) <- c('year','mean CPUE','RIv','RIv_nb')

RIpred_plot_ven <- RIpred_ven %>% pivot_longer(cols=2:ncol(.), names_to = "n_fish")

## initial value negative  
plot4<- RIpred_plot_ven %>% 
  ggplot() +
  aes(x=year, y = value) +
  geom_line(aes(col = n_fish), linewidth=1) +
  theme(legend.position = "bottom") +
  labs(x="Year", y="Number of Fish")+
  guides(col = guide_legend(nrow = 1))+
  ggtitle("Rhode Island vented")



#### Zero inflated models 

# First model ZI including year month and strata, negative binomial 
# Unvented Buzzards Bay 

BBunv_zi <- zeroinfl(bsbCount_Unvented ~ as.factor(Year)+ as.factor(Month)+ as.factor(depthStrata), 
                data = BB, dist = "negbin")

summary(BBunv_zi)
E2 = resid(BBunv_zi, type = "pearson")
N  = nrow(BB)
p  = length(coef(BBunv_zi))   
OM9=sum(E2^2) / (N - p)


## make predictions with model BBunv_zi

BB_zi_pred <- tibble(
    # Include each year
  Year = unique(BB$Year),
    # Use the mean of the continuous variable
  depthStrata = as.factor(min(BB$depthStrata)),
    # Use the mode of the categorical variable
  Month = names(table(BB$Month))[table(BB$Month) %>% which.max()])

print(BB_zi_pred, n = nrow(BB_zi_pred))

predsBB<- predict(BBunv_zi, newdata = BB_zi_pred, type = 'response')
predsBBBoot <- list()
for(i in 1:10){
  tmpMod <- update(BBunv_zi, data = BB[sample(1:nrow(BB), replace = TRUE),])
  if(any(is.na(tmpMod$vcov))){
    next()
  }else{
    predsBBBoot[[i]] <- predict(tmpMod, newdata = BB_zi_pred, type = 'response')
  }
}

# Number of instances where the model did not produce a variance
sum(sapply(predsBBBoot, is.null))

# Get mean and SE
predsBB_ST <- predsBBBoot %>%
  bind_rows() %>%
  rename_with(~(BB_zi_pred$Year %>% as.character())) %>%
  pivot_longer(everything(), names_to = 'Year', values_to = 'NHat') %>%
  mutate(Year = Year %>% as.numeric()) %>%
  complete(Year = 2008:2020) %>%
  group_by(Year) %>%
  summarize(Mean = mean(NHat),
            SE = sd(NHat))

upCI<-qnorm(0.975, mean = predsBB_ST$Mean, sd = predsBB_ST$SE)
loCI<-qnorm(0.025, mean = predsBB_ST$Mean, sd = predsBB_ST$SE)

predsBB_ST %>%
  ggplot(aes(x = Year, y = Mean, group = 1)) +
  geom_ribbon(aes(ymin = loCI, ymax = upCI),
              fill = 'cornflowerblue', alpha = 0.5) +
  geom_line() +
  geom_point()+
  ggtitle("Buzzards Bay unvented zero inflated (negbin)") +
  scale_x_continuous(breaks = seq(2008, 2020, 2))


# Second model ZI including year month and strata, negative binomial 
# Unvented Rhode Island 

RIunv_zi <- zeroinfl(bsbCount_Unvented ~ as.factor(Year)+ as.factor(Month)+ as.factor(depthStrata), 
                     data = RI, dist = "negbin")

summary(RIunv_zi)
E2 = resid(RIunv_zi, type = "pearson")
N  = nrow(RI)
p  = length(coef(RIunv_zi))   
OM10=sum(E2^2) / (N - p)


## make predictions with model RIunv_zi

RI_zi_pred <- tibble(
  # Include each year
  Year = unique(RI$Year),
  # Use the mean of the continuous variable
  depthStrata = as.factor(min(RI$depthStrata)),
  # Use the mode of the categorical variable
  Month = names(table(RI$Month))[table(RI$Month) %>% which.max()])

print(RI_zi_pred, n = nrow(RI_zi_pred))

predsRI<- predict(RIunv_zi, newdata = RI_zi_pred, type = 'response')
predsRIBoot <- list()
for(i in 1:10){
  tmpMod <- update(RIunv_zi, data = RI[sample(1:nrow(RI), replace = TRUE),])
  if(any(is.na(tmpMod$vcov))){
    next()
  }else{
    predsRIBoot[[i]] <- predict(tmpMod, newdata = RI_zi_pred, type = 'response')
  }
}

# Number of instances where the model did not produce a variance
sum(sapply(predsRIBoot, is.null))

# Get mean and SE
predsRI_ST <- predsRIBoot %>%
  bind_rows() %>%
  rename_with(~(RI_zi_pred$Year %>% as.character)) %>%
  pivot_longer(everything(), names_to = 'Year', values_to = 'NHat') %>%
  mutate(Year = Year %>% as.numeric()) %>%
  complete(Year = 2008:2020) %>%
  group_by(Year) %>%
  summarize(Mean = mean(NHat),
            SE = sd(NHat))

upCI<-qnorm(0.975, mean = predsRI_ST$Mean, sd = predsRI_ST$SE)
loCI<-qnorm(0.025, mean = predsRI_ST$Mean, sd = predsRI_ST$SE)

predsRI_ST %>%
  ggplot(aes(x = Year, y = Mean, group = 1)) +
  geom_ribbon(aes(ymin = loCI, ymax = upCI),
              fill = 'cornflowerblue', alpha = 0.5) +
  geom_line() +
  geom_point()+
  ggtitle("Rhode Island unvented zero inflated (negbin)") +
  scale_x_continuous(breaks = seq(2006, 2020, 2))



# Third model hurdle including year month and strata Unvented Buzzards Bay 

BBunv_hurdle <- hurdle(bsbCount_Unvented ~ as.factor(Year)+ as.factor(Month)+ as.factor(depthStrata), data = BB,
             dist="poisson", zero.dist="binomial", link="logit") 

summary(BBunv_hurdle)
coeftest(BBunv_hurdle)

E2 = resid(BBunv_hurdle, type = "pearson")
N  = nrow(BB)
p  = length(coef(BBunv_hurdle))   
OM11=sum(E2^2) / (N - p)

## geometric-poisson
BBunv_geom <- hurdle(bsbCount_Unvented ~ as.factor(Year)+ as.factor(Month)+ as.factor(depthStrata), data = BB,
             zero = "geometric")
summary(BBunv_geom)
coeftest(BBunv_geom)

E2 = resid(BBunv_geom, type = "pearson")
N  = nrow(BB)
p  = length(coef(BBunv_geom))   
OM12=sum(E2^2) / (N - p)

## logit-negbin
BBunv_negbin <- hurdle(bsbCount_Unvented ~ as.factor(Year)+ as.factor(Month)+ as.factor(depthStrata), data = BB,
             dist = "negbin")
summary(BBunv_negbin)
coeftest(BBunv_negbin)

E2 = resid(BBunv_negbin, type = "pearson")
N  = nrow(BB)
p  = length(coef(BBunv_negbin))   
OM13=sum(E2^2) / (N - p)

## negbin-negbin
## (poorly conditioned zero hurdle, note the standard errors)
BBunv_nbnb <- hurdle(bsbCount_Unvented ~ as.factor(Year)+ as.factor(Month)+ as.factor(depthStrata), data = BB,
             dist = "negbin", zero = "negbin")
summary(BBunv_nbnb)
coeftest(BBunv_nbnb)

E2 = resid(BBunv_nbnb, type = "pearson")
N  = nrow(BB)
p  = length(coef(BBunv_nbnb))   
OM14=sum(E2^2) / (N - p)


# Comparacion AIC
AIC(BBunv_hurdle,BBunv_geom, BBunv_negbin, BBunv_nbnb)

# Comparacion baja/dispersion
c(OM11, OM12, OM13, OM14)

# Rhode Island Unvented

RIunv_hurdle <- hurdle(bsbCount_Unvented ~ as.factor(Year)+ as.factor(Month)+ as.factor(depthStrata), data = RI,
                       dist="poisson", zero.dist="binomial", link="logit") 

summary(RIunv_hurdle)
coeftest(RIunv_hurdle)

E2 = resid(RIunv_hurdle, type = "pearson")
N  = nrow(RI)
p  = length(coef(RIunv_hurdle))   
OM15=sum(E2^2) / (N - p)

## geometric-poisson
RIunv_geom <- hurdle(bsbCount_Unvented ~ as.factor(Year)+ as.factor(Month)+ as.factor(depthStrata), data = RI,
                     zero = "geometric")
summary(RIunv_geom)
coeftest(RIunv_geom)

E2 = resid(RIunv_geom, type = "pearson")
N  = nrow(RI)
p  = length(coef(RIunv_geom))   
OM16=sum(E2^2) / (N - p)

## logit-negbin
RIunv_negbin <- hurdle(bsbCount_Unvented ~ as.factor(Year)+ as.factor(Month)+ as.factor(depthStrata), data = RI,
                       dist = "negbin")
summary(RIunv_negbin)
coeftest(RIunv_negbin)

E2 = resid(RIunv_negbin, type = "pearson")
N  = nrow(RI)
p  = length(coef(RIunv_negbin))   
OM17=sum(E2^2) / (N - p)

## negbin-negbin
## (poorly conditioned zero hurdle, note the standard errors)
RIunv_nbnb <- hurdle(bsbCount_Unvented ~ as.factor(Year)+ as.factor(Month)+ as.factor(depthStrata), data = RI,
                     dist = "negbin", zero = "negbin")
summary(RIunv_nbnb)
coeftest(RIunv_nbnb)

E2 = resid(RIunv_nbnb, type = "pearson")
N  = nrow(RI)
p  = length(coef(RIunv_nbnb))   
OM18=sum(E2^2) / (N - p)


# Comparacion AIC
AIC(RIunv_hurdle,RIunv_geom, RIunv_negbin, RIunv_nbnb)

# Comparacion baja/dispersion
c(OM15, OM16, OM17, OM18)



par(mfrow=c(1,2))
plot(density(BB$bsbCount_Unvented))
lines(density(predict(BBunv_nb, type='response')), col='red')

plot(density(RI$bsbCount_Unvented))
lines(density(predict(RIunv_nb, type='response')), col='red')


# for extract coefficients we want the fish count or probability of find a fish poisson or negative binomial
# we want the cpue index or we want the cpue standardization  because for the index we can just extract the coeffiecients bur if we want the standardization 
# we can use the predict function, but that assumes an homogeneus fleet 
################################################################################
################################################################################

## make predictions with model hurdle negative binomial BBunv_negbin

BB_zi_pred <- tibble(
  # Include each year
  Year = unique(BB$Year),
  # Use the mean of the continuous variable
  depthStrata = as.factor(min(BB$depthStrata)),
  # Use the mode of the categorical variable
  Month = names(table(BB$Month))[table(BB$Month) %>% which.max()])

print(BB_zi_pred, n = nrow(BB_zi_pred))

predsBB<- predict(BBunv_negbin, newdata = BB_zi_pred, type = 'response')
predsBBBoot <- list()
for(i in 1:10){
  tmpMod <- update(BBunv_negbin, data = BB[sample(1:nrow(BB), replace = TRUE),])
  if(any(is.na(tmpMod$vcov))){
    next()
  }else{
    predsBBBoot[[i]] <- predict(tmpMod, newdata = BB_zi_pred, type = 'response')
  }
}

# Number of instances where the model did not produce a variance
sum(sapply(predsBBBoot, is.null))

# Get mean and SE
predsBB_ST <- predsBBBoot %>%
  bind_rows() %>%
  #rename_with(~BB_zi_pred$Year) %>%
  pivot_longer(everything(), names_to = 'Year', values_to = 'NHat') %>%
  group_by(Year) %>%
  summarize(Mean = mean(NHat),
            SE = sd(NHat))

upCI<-qnorm(0.975, mean = predsBB_ST$Mean, sd = predsBB_ST$SE)
loCI<-qnorm(0.025, mean = predsBB_ST$Mean, sd = predsBB_ST$SE)

predsBB_ST %>%
  ggplot(aes(x = Year, y = Mean, group = 1)) +
  geom_ribbon(aes(ymin = loCI, ymax = upCI),
              fill = 'cornflowerblue', alpha = 0.5) +
  geom_line() +
  geom_point()+
  ggtitle("Buzzards Bay unvented zero inflated (negbin)")


## make predictions with model hurdle negative binomial RIunv_negbin

RI_zi_pred <- tibble(
  # Include each year
  Year = unique(RI$Year),
  # Use the mean of the continuous variable
  depthStrata = as.factor(min(RI$depthStrata)),
  # Use the mode of the categorical variable
  Month = names(table(RI$Month))[table(RI$Month) %>% which.max()])

print(RI_zi_pred, n = nrow(RI_zi_pred))

predsRI<- predict(RIunv_negbin, newdata = RI_zi_pred, type = 'response')
predsRIBoot <- list()
for(i in 1:10){
  tmpMod <- update(RIunv_negbin, data = RI[sample(1:nrow(RI), replace = TRUE),])
  if(any(is.na(tmpMod$vcov))){
    next()
  }else{
    predsRIBoot[[i]] <- predict(tmpMod, newdata = RI_zi_pred, type = 'response')
  }
}

# Number of instances where the model did not produce a variance
sum(sapply(predsRIBoot, is.null))

# Get mean and SE
predsRI_ST <- predsRIBoot %>%
  bind_rows() %>%
  rename_with(~BB_zi_pred$Year) %>%
  pivot_longer(everything(), names_to = 'Year', values_to = 'NHat') %>%
  group_by(Year) %>% arrange(Year) %>%
  summarize(Mean = mean(NHat),
            SE = sd(NHat))

upCI<-qnorm(0.975, mean = predsRI_ST$Mean, sd = predsRI_ST$SE)
loCI<-qnorm(0.025, mean = predsRI_ST$Mean, sd = predsRI_ST$SE)

predsRI_ST %>%
  ggplot(aes(x = Year, y = Mean, group = 1)) +
  geom_ribbon(aes(ymin = loCI, ymax = upCI),
              fill = 'cornflowerblue', alpha = 0.5) +
  geom_line() +
  geom_point()+
  ggtitle("Rhode Island unvented zero inflated (negbin)")



