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

# Second model GLM including year month and strata, poisson family 
# Vented BB

BBv <- glm(bsbCount_Vented ~ as.factor(Year) + as.factor(Month)+ as.factor(depthStrata), family = "poisson", 
            data = BB, offset(trapCount_Vented))

BBv$summary<-summary(BBv)
anova(BBv)
PseudoR(BBv)


#check overdispersion model 1 Unvented 
E2 = resid(BBunv, type = "pearson")
N  = nrow(BB)
p  = length(coef(BBunv))   
OM1<- sum(E2^2) / (N - p)

#check overdispersion model 2 Vented
E2 = resid(BBv, type = "pearson")
N  = nrow(BB)
p  = length(coef(BBv))   
OM1.1<- sum(E2^2) / (N - p)

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

# Fourth model negative binomial GLM including year month and strata 
# Vented BB

BBv_nb <- glm.nb(bsbCount_Vented ~ as.factor(Year) + as.factor(Month)+ as.factor(depthStrata), data = BB, 
               link = log, offset(trapCount_Vented))

BBv_nb$summary<-summary(BBv_nb)
anova(BBv_nb)
PseudoR(BBv_nb)

E2 = resid(BBunv_nb, type = "pearson")
N  = nrow(BB)
p  = length(coef(BBunv_nb))   
OM3<-sum(E2^2) / (N - p)

E2 = resid(BBv_nb, type = "pearson")
N  = nrow(BB)
p  = length(coef(BBv_nb))   
OM3.3<-sum(E2^2) / (N - p)


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

#check overdispersion 
E2 = resid(RIunv, type = "pearson")
N  = nrow(RI)
p  = length(coef(RIunv))   
OM8<- sum(E2^2) / (N - p)

# Second model GLM including year month and strata, poisson family 
# Vented RI

RIv <- glm(bsbCount_Vented ~ as.factor(Year) + as.factor(Month)+ as.factor(depthStrata), 
            family = "poisson", data = RI, offset(trapCount_Vented))

RIv$summary<-summary(RIv)
anova(RIv)
PseudoR(RIv)
AIC(RIv)

#check overdispersion 
E2 = resid(RIv, type = "pearson")
N  = nrow(RI)
p  = length(coef(RIv))   
OM8.8<- sum(E2^2) / (N - p)


# Third model negative binomial GLM including year month and strata 
# Unvented RI

RIunv_nb <- glm.nb(bsbCount_Unvented ~ as.factor(Year) + as.factor(Month)+ as.factor(depthStrata), 
             data = RI, link = log, offset(trapCount_Unvented))

RIunv_nb$summary<-summary(RIunv_nb)
anova(RIunv_nb)
PseudoR(RIunv_nb)

E2 = resid(RIunv_nb, type = "pearson")
N  = nrow(RI)
p  = length(coef(RIunv_nb))   
OM9<-sum(E2^2) / (N - p)

AIC(RIv, RIunv)


# Fourth model negative binomial GLM including year month and strata 
# Vented RI

RIv_nb <- glm.nb(bsbCount_Vented ~ as.factor(Year) + as.factor(Month)+ as.factor(depthStrata), 
               data = RI, link = log, offset(trapCount_Vented))

RIv_nb$summary<-summary(RIv_nb)
anova(RIv_nb)
PseudoR(RIv_nb)

E2 = resid(RIv_nb, type = "pearson")
N  = nrow(RI)
p  = length(coef(RIv_nb))   
OM9.9<-sum(E2^2) / (N - p)

AIC(RIunv_nb, RIv_nb)

# UNVENTED CPUE BUZZARDS BAY SUMMARY
 
mean_BBunv <- BB %>% group_by(Year) %>% 
  summarise (cpue = mean(bsbCount_Unvented, na.rm = TRUE),
             cpuen = n())

cpue_BBunv <- (c(BBunv$summary$coefficients[1,1],  
                 BBunv$summary$coefficients[1,1] + BBunv$summary$coefficients[2:12,1]))
cpue_BBunv<-as.data.frame(cpue_BBunv)

BBunv_nb <- (c(BBunv_nb$summary$coefficients[1,1],  
               BBunv_nb$summary$coefficients[1,1] + BBunv_nb$summary$coefficients[2:12,1]))
BBunv_nb<-as.data.frame(BBunv_nb)

BBpred<- cbind(mean_BBunv[1],mean_BBunv[2], cpue_BBunv$cpue_BBunv, BBunv_nb$BBunv_nb)
colnames(BBpred) <- c('year','mean CPUE','BBunv','BBunv_nb')

BBpred_plot <- BBpred %>%
  pivot_longer(cols=2:ncol(.), names_to = "n_fish") %>%
  # Add in missing 2013 data (will be NAs)
  complete(year = min(BBpred$year):max(BBpred$year),
           n_fish)

# Specify x axis breaks
xbrk <- seq(from = min(BBpred_plot$year), to = max(BBpred_plot$year), by =2)

# Create the plot
plot1<- BBpred_plot %>% 
  ggplot() +
  aes(x=year, y = value) +
  geom_line(aes(col = n_fish), size=1) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = xbrk) +
  labs(x="Year", y="Number of Fish")+
  guides(col = guide_legend(nrow = 1))


# UNVENTED CPUE RHODE ISLAND

mean_RIunv <- RI %>% group_by(Year) %>% 
  summarise (cpue = mean(bsbCount_Unvented, na.rm = TRUE),
             cpuen = n())

cpue_RIunv <- (c(RIunv$summary$coefficients[1,1],  
                 RIunv$summary$coefficients[1,1] + RIunv$summary$coefficients[2:15,1]))
cpue_RIunv<-as.data.frame(cpue_RIunv)

RIunv_nb <- (c(RIunv_nb$summary$coefficients[1,1],  
               RIunv_nb$summary$coefficients[1,1] + RIunv_nb$summary$coefficients[2:15,1]))
RIunv_nb<-as.data.frame(RIunv_nb)

RIpred<- cbind(mean_RIunv[1],mean_RIunv[2], cpue_RIunv$cpue_RIunv, RIunv_nb$RIunv_nb)
colnames(RIpred) <- c('year','mean CPUE','RIunv','RIunv_nb')

RIpred_plot <- RIpred %>% pivot_longer(cols=2:ncol(.), names_to = "n_fish")

## initial value negative  
plot2<- RIpred_plot %>% 
  ggplot() +
  aes(x=year, y = value) +
  geom_line(aes(col = n_fish), linewidth=1) +
  theme(legend.position = "bottom") +
  labs(x="Year", y="Number of Fish")+
  guides(col = guide_legend(nrow = 1))


#### Zero inflated and Hurdle models 

# First model ZI including year month and strata, negative binomial 
# Unvented RI

RIunv_zi <- zeroinfl(bsbCount_Unvented ~ as.factor(Year)+ as.factor(Month)+ as.factor(depthStrata), 
                data = RI, dist = "negbin")

summary(RIunv_zi)
E2 = resid(RIunv_zi, type = "pearson")
N  = nrow(RI)
p  = length(coef(RIunv_zi))   
OM7=sum(E2^2) / (N - p)

## make predictions with model RIunv_zi

nd_pred <- tibble(
    # Include each year
  Year = unique(BB$Year),
    # Use the mean of the continuous variable
  depthStrata = as.factor(min(BB$depthStrata)),
    # Use the mode of the categorical variable
  Month = names(table(BB$Month))[table(BB$Month) %>% which.max()])

print(nd_pred, n = nrow(nd_pred))

preds_nd <- predict(RIunv_zi, newdata = nd_pred, type = 'response')
preds_m7Boot <- list()
for(i in 1:10){
  tmpMod <- update(RIunv_zi, data = BB[sample(1:nrow(BB), replace = TRUE),])
  if(any(is.na(tmpMod$vcov))){
    next()
  }else{
    preds_m7Boot[[i]] <- predict(tmpMod, newdata = nd_pred, type = 'response')
  }
}
# Number of instances where the model did not produce a variance
sum(sapply(preds_m7Boot, is.null))

# Get mean and SE
predsM7_ST <- preds_m7Boot %>%
  bind_rows() %>%
  rename_with(~nd_pred$Year) %>%
  pivot_longer(everything(), names_to = 'Year', values_to = 'NHat') %>%
  group_by(Year) %>%
  summarize(Mean = mean(NHat),
            SE = sd(NHat))

upCI<-qnorm(0.975, mean = predsM7_ST$Mean, sd = predsM7_ST$SE)
loCI<-qnorm(0.025, mean = predsM7_ST$Mean, sd = predsM7_ST$SE)

predsM7_ST %>%
  ggplot(aes(x = Year, y = Mean, group = 1)) +
  geom_ribbon(aes(ymin = loCI, ymax = upCI),
              fill = 'cornflowerblue', alpha = 0.5) +
  geom_line() +
  geom_point()

# First model hurdle including year month and strata  dist="poisson", zero.dist="binomial", link="logit"
# Unvented RI

## geometric-poisson
RIunv_hurd1 <- hurdle(bsbCount_Unvented ~ as.factor(Year)+ as.factor(Month)+ as.factor(depthStrata), data = RI,
             dist="poisson", zero.dist="binomial", link="logit")

summary(RIunv_hurd1)
coeftest(RIunv_hurd1)

E2 = resid(RIunv_hurd1, type = "pearson")
N  = nrow(RI)
p  = length(coef(RIunv_hurd1))   
OM5=sum(E2^2) / (N - p)

# Second model hurdle including year month and strata  zero = "geometric"
# Unvented RI

RIunv_hurd2 <- hurdle(bsbCount_Unvented ~ as.factor(Year)+ as.factor(Month)+ as.factor(depthStrata), data = RI,
             zero = "geometric")
summary(RIunv_hurd2)
coeftest(RIunv_hurd2)

E2 = resid(RIunv_hurd2, type = "pearson")
N  = nrow(RI)
p  = length(coef(RIunv_hurd2))   
OM6=sum(E2^2) / (N - p)

# Third model hurdle including year month and strata  distr = "negbin"
# Unvented RI

RIunv_hurd3 <- hurdle(bsbCount_Unvented ~ as.factor(Year)+ as.factor(Month)+ as.factor(depthStrata), data = RI,
             dist = "negbin")
summary(RIunv_hurd3)
coeftest(RIunv_hurd3)

E2 = resid(RIunv_hurd3, type = "pearson")
N  = nrow(RI)
p  = length(coef(RIunv_hurd3))   
OM7=sum(E2^2) / (N - p)

# Fourth model hurdle including year month and strata  distr = "negbin"  zero = "negbin"
# Unvented RI
## (poorly conditioned zero hurdle, note the standard errors)
RIunv_hurd4 <- hurdle(bsbCount_Unvented ~ as.factor(Year)+ as.factor(Month)+ as.factor(depthStrata), data = RI,
             dist = "negbin", zero = "negbin")
summary(RIunv_hurd4)
coeftest(RIunv_hurd4)

E2 = resid(RIunv_hurd4, type = "pearson")
N  = nrow(RI)
p  = length(coef(RIunv_hurd4))   
OM8=sum(E2^2) / (N - p)


# Comparacion AIC
AIC(RIunv_hurd1,RIunv_hurd2, RIunv_hurd3, RIunv_hurd4)




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

