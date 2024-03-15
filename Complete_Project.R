### Dependencies 
library('data.table')
library('dplyr')
library('tidyr')
library("tidyverse")
library("BatchGetSymbols")
library("car")
library('stargazer')
library('forecast')


################################# Task 1 ############################################
#calculate return of smb and hml

data = read.csv('C:/Users/micha/Desktop/econometrics group/stock.csv')

data = data %>% group_by(Ticker) %>% mutate(RF_Monthly = (1 + RF)^(1/2) -1) %>% ungroup()

data = data %>% group_by(Ticker) %>% mutate(return = log(price.adjusted / lag(price.adjusted)),
                                            rmkt = log(SP500Index / lag(SP500Index))) %>% ungroup() %>% drop_na()

data = data %>% mutate(ERet = return - RF_Monthly,
                       ERet_Mkt = rmkt - RF_Monthly)
data$SMB = data$SMB/100
data$HML = data$HML/100


##### Nvidia ###############
NVDA = data %>% filter(Ticker == 'NVDA')

NVDAModel = lm(ERet ~ ERet_Mkt + SMB + HML, data = NVDA)
summary(NVDAModel)

##### Apple ###############

APPl = data %>% filter(Ticker == 'APPL')
APPlModel = lm(ERet ~ ERet_Mkt + SMB + HML, data = APPl)
summary(APPlModel)

##### AMD ###############
AMD = data %>% filter(Ticker == 'AMD')
AMDModel = lm(ERet ~ ERet_Mkt + SMB + HML, data = AMD)
summary(AMDModel)

##### Microsoft ###############
MSFT = data %>% filter(Ticker == 'MSFT')
MSFTModel = lm(ERet ~ ERet_Mkt + SMB + HML, data = MSFT)
summary(MSFTModel)

##### Amazon ###############

AMZN = data %>% filter(Ticker == 'AMZN')
AMZNModel = lm(ERet ~ ERet_Mkt + SMB + HML, data = AMZN)
summary(AMZNModel)

### stargazer output 

stargazer(NVDAModel, APPlModel,AMDModel, MSFTModel, AMZNModel,
                   type = 'html', out = 'C:/Users/micha/Desktop/econometrics group/section1.html', 
          covariate.labels=c('MKT', 'SMB', 'HML'), dep.var.labels = 'Excess Return', 
          column.labels = c('Nvidia', 'Apple', 'AMD', 'Microsoft', 'Amazon'))



################################# Task 2 ############################################

stocks = c('T', 'BAC', 'AAL', 'TSLA', 'INTC' )


## Process Fama French dataset to get mkt factor and RF

ffactors <- read.csv('C:/Users/micha/Desktop/econometrics group/F-F_Research_Data_Factors_daily.csv')
ffactors = ffactors %>% select(X, Mkt.RF, RF) %>% rename(Date = X)
ffactors$Date = as.Date(ffactors$Date, format = "%Y%m%d")
ffactors$Mkt.RF = ffactors$Mkt.RF/100
ffactors$RF = ffactors$RF/100


GetData <- BatchGetSymbols(tickers = stocks,
                           first.date = "2022-01-01",
                           last.date = "2023-01-02",
                           freq.data = "daily",
                           cache.folder = file.path(tempdir(),
                                                    "BGS_Cache") )
task2data = GetData[[2]] %>% select(ref.date, ticker, price.adjusted)

task2data = task2data %>% group_by(ticker) %>% mutate(return = log(price.adjusted / lag(price.adjusted))) %>% drop_na() %>% select(-price.adjusted)


mergeddata = merge(task2data, ffactors, by.x = 'ref.date', by.y = 'Date', all.x = TRUE)

finaldata = mergeddata %>% mutate(Exret = return - RF )

## get sample split 
index = floor(nrow(finaldata)*0.7)
date = finaldata$ref.date[index]

########## T #####
tdata = finaldata %>% filter(ticker == 'T')


tinsample = tdata %>% filter(ref.date <= date) %>% select(return)
toutofsample = tdata %>% filter(ref.date >= date) %>% select(return)

acfplot <- acf(x = tinsample, plot =FALSE)
plot(acfplot, main = 'AT & T time series ACF')

pacfplot <- pacf(x = tinsample, plot =FALSE)
plot(pacfplot, main = 'AT & T time series PACF')


tauto_mdaic <- auto.arima(y = tinsample,max.p = 3, max.q = 3,
                            max.d = 3,trace = TRUE,ic = c("aic"),
                            stepwise = TRUE,max.order = 9)

tauto_mdaic <- auto.arima(y = tinsample,max.p = 10, max.q = 10,
                            max.d = 10,trace = TRUE,ic = c("bic"),
                            stepwise = TRUE,max.order = 30)



# T CAPM ###
TModel = lm(Exret ~ Mkt.RF,  data = tdata)
summary(TModel)



######### BAC ###
bacdata = finaldata %>% filter(ticker == 'BAC')

  

bacinsample = bacdata %>% filter(ref.date <= date) %>% select(return)
bacoutofsample = bacdata %>% filter(ref.date >= date) %>% select(return)


acfplot <- acf(x = bacinsample, plot =FALSE)
plot(acfplot, main = 'Bank of America time series ACF')

pacfplot <- pacf(x = bacinsample, plot =FALSE)
plot(pacfplot, main = 'Bank of America time series PACF')  



bacauto_mdaic <- auto.arima(y = bacinsample,max.p = 10, max.q = 10,
                          max.d = 10,trace = TRUE,ic = c("aic"),
                          stepwise = FALSE,max.order = 30)

bacauto_mdbic <- auto.arima(y = bacinsample,max.p = 10, max.q = 10,
                            max.d = 10,trace = TRUE,ic = c("bic"),
                            stepwise = FALSE,max.order = 30)


acf(bacauto_mdaic$residuals,lag.max = 30)

Box.test(bacauto_mdaic$residuals,lag = 30,type = "Ljung-Box")

#prediction 
dim(bacoutofsample)
predbac = predict(object = bacauto_mdaic, n.ahead = 76)

mean((predbac$pred - bacoutofsample$return)^2)

  
BACModel = lm(Exret ~ Mkt.RF,  data = bacdata) ## null hypothesis cannot be rejected no, 
summary(BACModel)





#### AAL #########
AALdata = finaldata %>% filter(ticker == 'AAL')
AALrseries = AALdata$return


acfplot <- acf(x = AALrseries, plot =FALSE)
plot(acfplot, main = 'American Airlines time series ACF')

pacfplot <- pacf(x = AALrseries, plot =FALSE)
plot(pacfplot, main = 'American Airlines time series PACF')  

AALauto_mdaic <- auto.arima(y = AALrseries,max.p = 10, max.q = 10,
                                max.d = 10,trace = TRUE,ic = c("aic"),
                                stepwise = FALSE,max.order = 30)

AALauto_mdbic <- auto.arima(y = AALrseries,max.p = 10, max.q = 10,
                            max.d = 10,trace = TRUE,ic = c("bic"),
                            stepwise = FALSE,max.order = 30)




AALModel = lm(Exret ~ Mkt.RF,  data = AALdata)
summary(AALModel)

#### TSLA ########


TSLAdata = finaldata %>% filter(ticker == 'TSLA')
TSLAinsample = TSLAdata %>% filter(ref.date <= date) %>% select(return)
TSLAoutofsample = TSLAdata %>% filter(ref.date >= date) %>% select(return)


acfplot <- acf(x = TSLAinsample, plot =FALSE)
plot(acfplot, main = 'Tesla time series ACF')

pacfplot <- pacf(x = TSLAinsample, plot =FALSE)
plot(pacfplot, main = 'Tesla time series PACF')  



TSLAauto_mdaic <- auto.arima(y = TSLAinsample,max.p = 15, max.q = 15,
                            max.d = 15,trace = TRUE,ic = c("aic"),
                            stepwise = FALSE,max.order = 45)

TSLAauto_mdbic <- auto.arima(y = TSLAinsample,max.p = 10, max.q = 10,
                            max.d = 10,trace = TRUE,ic = c("bic"),
                            stepwise = FALSE,max.order = 30)


acf(TSLAauto_mdaic$residuals,lag.max = 30)

Box.test(TSLAauto_mdaic$residuals,lag = 30,type = "Ljung-Box")

#prediction 
dim(TSLAoutofsample)
predTSLA = predict(object = TSLAauto_mdbic, n.ahead = 76)

mean((predTSLA$pred - TSLAoutofsample$return)^2)






TSLAModel = lm(Exret ~ Mkt.RF,  data = TSLAdata)
summary(TSLAModel)

#### INTC ########## 

INTCdata = finaldata %>% filter(ticker == 'INTC')
INTCrseries = INTCdata$return

INTCinsample = INTCdata %>% filter(ref.date <= date) %>% select(return)
INTCoutofsample = INTCdata %>% filter(ref.date >= date) %>% select(return)


acfplot <- acf(x = INTCinsample, plot =FALSE)
plot(acfplot, main = 'Intel time series ACF')

pacfplot <- pacf(x = INTCinsample, plot =FALSE)
plot(pacfplot, main = 'Intel time series PACF')  


INTCauto_mdaic <- auto.arima(y = INTCinsample,max.p = 17, max.q = 17,
                             max.d = 17,trace = TRUE,ic = c("aic"),
                             stepwise = FALSE,max.order = 51)

INTCauto_mdbic <- auto.arima(y = INTCinsample,max.p = 17, max.q = 17,
                             max.d = 17,trace = TRUE,ic = c("bic"),
                             stepwise = FALSE,max.order = 51)


acf(INTCauto_mdaic$residuals,lag.max = 30)

Box.test(INTCauto_mdaic$residuals,lag = 30,type = "Ljung-Box")

#prediction 
dim(TSLAoutofsample)
predINTC = predict(object = INTCauto_mdaic, n.ahead = 76)

mean((predINTC$pred - INTCoutofsample$return)^2)






INTCModel = lm(Exret ~ Mkt.RF,  data = INTCdata)
summary(INTCModel)

#### Stargazer table 

stargazer(TModel, BACModel,AALModel, TSLAModel, INTCModel,
          type = 'html', out = 'C:/Users/micha/Desktop/econometrics group/section2.html', 
          covariate.labels=c('MKT'), dep.var.labels = 'Excess Return', 
          column.labels = c('AT&T', 'Bank of America', 'American Airlines', 'Tesla', 'Intel'))

#####################################################################################

#                                                 Reuben Work for 1.2 Starts here:

##################################################Single T-test on Co-efficient#############################################################
#NVDA, Reject all 
NVDASummary = summary(NVDAModel)
NVDA_critvalue = qt(p = 0.025,df = 283,lower.tail = FALSE) #1.968382, values T values should be within range to accept null hypoth
NVDA_Intercept_test_stat = NVDASummary$coefficients["(Intercept)", "t value"]  #2.345305, Reject null hypothesis
NVDA_ERet_Mkt_test_stat = NVDASummary$coefficients["NVDA$ERet_Mkt", "t value"] #11.07329, Reject null hypothesis
NVDA_SMB_test_stat = NVDASummary$coefficients["NVDA$SMB", "t value"] #3.402362, Reject Null hypothesis 
NVDA_HML_test_stat = NVDASummary$coefficients["NVDA$HML", "t value"] #-3.634683, Reject null hypothesis 


#APPL, Reject all
APPlSummary = summary(APPlModel)
APPl_critvalue = qt(p = 0.025,df = 283,lower.tail = FALSE) #1.968382, values T values should be within range to accept null hypoth
APPl_Intercept_test_stat = APPlSummary$coefficients["(Intercept)", "t value"]  #3.460164, Reject null hypothesis 
APPl_ERet_Mkt_test_stat = APPlSummary$coefficients["APPl$ERet_Mkt", "t value"] #17.10639, Reject null hypothesis 
APPl_SMB_test_stat = APPlSummary$coefficients["APPl$SMB", "t value"] #1.903274, Reject null hypothesis 
APPl_HML_test_stat = APPlSummary$coefficients["APPl$HML", "t value"] #-5.343237, Reject null hypothesis 



#AMD
AMDSummary = summary(AMDModel) #why does it say there is 282 degrees of freedom for this model??? Still produces idential values for Critvalues
AMD_critvalue = qt(p = 0.025,df = 282,lower.tail = FALSE) #1.968382, values T values should be within range to accept null hypoth
AMD_Intercept_test_stat = AMDSummary$coefficients["(Intercept)", "t value"]  #2.237774, Reject null hypothesis 
AMD_ERet_Mkt_test_stat = AMDSummary$coefficients["AMD$ERet_Mkt", "t value"] #12.54498, Reject null hypothesis 
AMD_SMB_test_stat = AMDSummary$coefficients["AMD$SMB", "t value"] #2.317523, Reject Null hypothesis 
AMD_HML_test_stat = AMDSummary$coefficients["AMD$HML", "t value"] #-1.599124, Accept null hypothesis 



#MSFT
MSFTSummary = summary(MSFTModel)
MSFT_critvalue = qt(p = 0.025,df = 283,lower.tail = FALSE) #1.968382, values T values should be within range to accept null hypoth
MSFT_Intercept_test_stat = MSFTSummary$coefficients["(Intercept)", "t value"]  #1.802, Accept null hypothesis 
MSFT_ERet_Mkt_test_stat = MSFTSummary$coefficients["MSFT$ERet_Mkt", "t value"] #24.624, Reject null hypothesis 
MSFT_SMB_test_stat = MSFTSummary$coefficients["MSFT$SMB", "t value"] #-0.912, Accept Null hypothesis 
MSFT_HML_test_stat = MSFTSummary$coefficients["MSFT$HML", "t value"] #-5.870, Reject null hypothesis 


#AMZN
AMZNSummary = summary(AMZNModel)
AMZN_critvalue = qt(p = 0.025,df = 283,lower.tail = FALSE) #1.968382, values T values should be within range to accept null hypoth
AMZN_Intercept_test_stat = AMZNSummary$coefficients["(Intercept)", "t value"]  #3.201, Reject null hypothesis 
AMZN_ERet_Mkt_test_stat = AMZNSummary$coefficients["AMZN$ERet_Mkt", "t value"] #17.148, Reject null hypothesis
AMZN_SMB_test_stat = AMZNSummary$coefficients["AMZN$SMB", "t value"] #0.933, Accept Null hypothesis
AMZN_HML_test_stat = AMZNSummary$coefficients["AMZN$HML", "t value"] #-7.142, Reject null hypothesis 





################################################Single F-Test on co-efficient####################################################
library(car)

#NVDA
qf(p = 0.05,df1 = 1,df2 = 283,lower.tail = FALSE)#3.874527, Reject H0 as test stat is greater than crit value 
car::linearHypothesis(NVDAModel,c("NVDA$ERet_Mkt=1"))# F-test stat valued at 0.738, Accept null 
car::linearHypothesis(NVDAModel,c("NVDA$SMB=1"))# F-test stat valued at 0.1197, Accept null 
car::linearHypothesis(NVDAModel,c("NVDA$HML=1"))# F-test stat valued at 60.285, reject null 


#APPL
qf(p = 0.05,df1 = 1,df2 = 283,lower.tail = FALSE)#3.874527, Reject H0 as test stat is greater than crit value 
car::linearHypothesis(APPlModel,c("APPl$ERet_Mkt=1"))# F-test stat valued at 3.8214 , Accept null 
car::linearHypothesis(APPlModel,c("APPl$SMB=1"))# F-test stat valued at 13.373 , Reject null 
car::linearHypothesis(APPlModel,c("APPl$HML=1"))# F-test stat valued at 131.54 , Reject null 


#AMD
qf(p = 0.05,df1 = 1,df2 = 282,lower.tail = FALSE)#3.874645, Reject H0 as test stat is greater than crit value 
car::linearHypothesis(AMDModel,c("AMD$ERet_Mkt=1"))# F-test stat valued at 12.027 , reject null 
car::linearHypothesis(AMDModel,c("AMD$SMB=1"))# F-test stat valued at 0.6312 , Accept null 
car::linearHypothesis(AMDModel,c("AMD$HML=1"))# F-test stat valued at 28.139 , Reject null 


#MSFT
qf(p = 0.05,df1 = 1,df2 = 283,lower.tail = FALSE)#3.874645, Reject H0 as test stat is greater than crit value 
car::linearHypothesis(MSFTModel,c("MSFT$ERet_Mkt=1"))# F-test stat valued at 0.6056, Accept null 
car::linearHypothesis(MSFTModel,c("MSFT$SMB=1"))# F-test stat valued at 93.363, Reject null 
car::linearHypothesis(MSFTModel,c("MSFT$HML=1"))# F-test stat valued at 240.59, Reject null 


#AMZN
qf(p = 0.05,df1 = 1,df2 = 283,lower.tail = FALSE)#3.874527, Reject H0 as test stat is greater than crit value 
car::linearHypothesis(AMZNModel,c("AMZN$ERet_Mkt=1"))# F-test stat valued at 8.3332, Reject null 
car::linearHypothesis(AMZNModel,c("AMZN$SMB=1"))# F-test stat valued at 18.495, Reject null 
car::linearHypothesis(AMZNModel,c("AMZN$HML=1"))# F-test stat valued at 166.62, Reject null 






###########################Joint F-test: #######################
#Summary of results, reject Null hypothesis for all joint Co-efficient significance tests, Meaning all co-efficients are significant in joint test
library(car)


#NVDA
qf(p = 0.05,df1 = 2,df2 = 283,lower.tail = FALSE) #3.027669, crit value
car::linearHypothesis(NVDAModel,c("NVDA$SMB=0", "NVDA$HML=0")) #14.742 , Reject null  
car::linearHypothesis(NVDAModel,c("NVDA$ERet_Mkt=0", "NVDA$HML=0")) #69.818 , Reject null 
car::linearHypothesis(NVDAModel,c("NVDA$SMB=0", "NVDA$ERet_Mkt=0")) #73.019 , Reject null  


#APPl
qf(p = 0.05,df1 = 2,df2 = 283,lower.tail = FALSE) #3.027669, crit value
car::linearHypothesis(APPlModel,c("APPl$SMB=0", "APPl$HML=0")) #17.225 , Reject null  
car::linearHypothesis(APPlModel,c("APPl$ERet_Mkt=0", "APPl$HML=0")) #160.59 , Reject null 
car::linearHypothesis(APPlModel,c("APPl$SMB=0", "APPl$ERet_Mkt=0")) #152.85 , Reject null  


#AMD
qf(p = 0.05,df1 = 2,df2 = 282,lower.tail = FALSE) #3.027783, crit value
car::linearHypothesis(AMDModel,c("AMD$SMB=0", "AMD$HML=0")) #4.3693 , Reject null  
car::linearHypothesis(AMDModel,c("AMD$ERet_Mkt=0", "AMD$HML=0")) #81.611 , Reject null 
car::linearHypothesis(AMDModel,c("AMD$SMB=0", "AMD$ERet_Mkt=0")) #89.904 , Reject null  


#MSFT
qf(p = 0.05,df1 = 2,df2 = 283,lower.tail = FALSE) #3.027669, crit value
car::linearHypothesis(MSFTModel,c("MSFT$SMB=0", "MSFT$HML=0")) #17.229 , Reject null  
car::linearHypothesis(MSFTModel,c("MSFT$ERet_Mkt=0", "MSFT$HML=0")) #327.38, Reject null 
car::linearHypothesis(MSFTModel,c("MSFT$SMB=0", "MSFT$ERet_Mkt=0")) #305.62, Reject null  


#AMZN
qf(p = 0.05,df1 = 2,df2 = 283,lower.tail = FALSE) #3.027669, crit value
car::linearHypothesis(AMZNModel,c("AMZN$SMB=0", "AMZN$HML=0")) #27.709, Reject null  
car::linearHypothesis(AMZNModel,c("AMZN$ERet_Mkt=0", "AMZN$HML=0")) #178.26, Reject null 
car::linearHypothesis(AMZNModel,c("AMZN$SMB=0", "AMZN$ERet_Mkt=0")) #151.91, Reject null  



############################Section 1.3 Reuben Continued####################

lmtest::dwtest(formula = NVDAModel,alternative = "two.sided")
#DW = 1.6346, p-value = 0.0016 
#p-value = 0.0016 < 5%, hence Reject H0, evidence for first order autocorrelation of the error term. 


lmtest::dwtest(formula = APPlModel,alternative = "two.sided")
#DW = 1.9246, p-value = 0.4901 
#p-value = 0.4901 > 5%, hence we accept H0: there is no evidence for first order autocorrelation of the error term.


lmtest::dwtest(formula = AMDModel,alternative = "two.sided")
#DW = 1.7672, p-value = 0.04255
#p-value = 0.04255 < 5%,hence Reject H0, evidence for first order autocorrelation of the error term. 


lmtest::dwtest(formula = MSFTModel,alternative = "two.sided")
#DW = 2.2936, p-value = 0.01436
#p-value = 0.01436 < 5%,hence Reject H0, evidence for first order autocorrelation of the error term. 


lmtest::dwtest(formula = AMZNModel,alternative = "two.sided")
#DW = 1.914, p-value = 0.4353
#p-value = 0.4353 > 5%, hence we accept H0: there is no evidence for first order autocorrelation of the error term.




############################Section 1.4 Reuben Continued####################
#Get residuals
#Create Auxiliary with square components for CAPM model




                                              ###NVDA White test###
#Get residual squared
NVDA_hat_sq = NVDAModel$residuals^2
#Create Auxiliary
auxillary_NVDA <- lm(data = data, formula = NVDA_hat_sq
        ~ NVDA$ERet_Mkt + NVDA$SMB + NVDA$HML +
        +I(NVDA$ERet_Mkt*NVDA$SMB)+I(NVDA$ERet_Mkt*NVDA$HML)
        +I(NVDA$SMB*NVDA$HML)+I(NVDA$ERet_Mkt^2)+I(NVDA$SMB^2)+I(NVDA$HML^2))
summary(auxillary_NVDA)
###Get values from summary
#get df from summary thing says 9 = df
#Multiple R-squared = 0.093
#number of observations = 287, from data
287*0.093 
26.691 #Test statistic for NVDA


#Calculate Crit-Value, should be close to P-value from summary aux_model
qchisq(p = 0.05,df = 9,lower.tail = FALSE)
16.91898 #Crit value for NVDA
#26.691 > 16.91898, fail to reject H0, 
#Not enough evidence to conclude that the white test fails to meet the assumptions of homoscedasticity. 
#Good thing error terms are homoscedasticity.











                                                ###APPL White test###
#Get residual squared
APPl_hat_sq = APPlModel$residuals^2
#Create Auxiliary
auxillary_APPl <- lm(data = data, formula = APPl_hat_sq
                     ~ APPl$ERet_Mkt + APPl$SMB + APPl$HML +
                       +I(APPl$ERet_Mkt*APPl$SMB)+I(APPl$ERet_Mkt*APPl$HML)
                     +I(APPl$SMB*APPl$HML)+I(APPl$ERet_Mkt^2)+I(APPl$SMB^2)+I(APPl$HML^2))
summary(auxillary_APPl)
###Get values from summary
#get df from summary thing says 9 = df
#Multiple R-squared = 0.1453
#number of observations = 287, from data
287*0.1453
41.7011 #Test statistic for APPL


#Calculate Crit-Value, should be close to P-value from summary aux_model
qchisq(p = 0.05,df = 9,lower.tail = FALSE)
16.91898 #Crit value for APPL
#41.7011 > 16.91898, fail to reject H0, 
#Not enough evidence to conclude that the white test fails to meet the assumptions of homoscedasticity. 
#Good thing error terms are homoscedasticity.








                                    ###AMD White Test###
AMD_hat_sq = AMDModel$residuals^2
#Create Auxiliary
auxillary_AMD <- lm(data = data, formula = AMD_hat_sq
                     ~ AMD$ERet_Mkt + AMD$SMB + AMD$HML +
                       +I(AMD$ERet_Mkt*AMD$SMB)+I(AMD$ERet_Mkt*AMD$HML)
                     +I(AMD$SMB*AMD$HML)+I(AMD$ERet_Mkt^2)+I(AMD$SMB^2)+I(AMD$HML^2))
summary(auxillary_AMD)
###Get values from summary
#get df from summary thing says 9 = df
#Multiple R-squared = 0.1155
#number of observations = 287, from data
287*0.1155
33.1485 #Test statistic for AMD


#Calculate Crit-Value, should be close to P-value from summary aux_model
qchisq(p = 0.05,df = 9,lower.tail = FALSE)
16.91898 #Crit value for AMD
#33.1485 > 16.91898, fail to reject H0, 
#Not enough evidence to conclude that the white test fails to meet the assumptions of homoscedasticity. 
#Good thing error terms are homoscedasticity.






                             ###MSFT White test###
#Get residual squared
MSFT_hat_sq = MSFTModel$residuals^2
#Create Auxiliary
auxillary_MSFT <- lm(data = data, formula = MSFT_hat_sq
                     ~ MSFT$ERet_Mkt + MSFT$SMB + MSFT$HML +
                       +I(MSFT$ERet_Mkt*MSFT$SMB)+I(MSFT$ERet_Mkt*MSFT$HML)
                     +I(MSFT$SMB*MSFT$HML)+I(MSFT$ERet_Mkt^2)+I(MSFT$SMB^2)+I(MSFT$HML^2))
summary(auxillary_MSFT)
###Get values from summary
#get df from summary thing says 9 = df
#Multiple R-squared = 0.093
#number of observations = 287, from data
287*0.1299
37.2813 #Test statistic for MSFT


#Calculate Crit-Value, should be close to P-value from summary aux_model
qchisq(p = 0.05,df = 9,lower.tail = FALSE)
16.91898 #Crit value for MSFT
#37.2813 > 16.91898, fail to reject H0, 
#Not enough evidence to conclude that the white test fails to meet the assumptions of homoscedasticity. 
#Good thing error terms are homoscedasticity.








                          ###AMZN White test###
#Get residual squared
AMZN_hat_sq = AMZNModel$residuals^2
#Create Auxiliary
auxillary_AMZN <- lm(data = data, formula = AMZN_hat_sq
                     ~ AMZN$ERet_Mkt + AMZN$SMB + AMZN$HML +
                       +I(AMZN$ERet_Mkt*AMZN$SMB)+I(AMZN$ERet_Mkt*AMZN$HML)
                     +I(AMZN$SMB*AMZN$HML)+I(AMZN$ERet_Mkt^2)+I(AMZN$SMB^2)+I(AMZN$HML^2))
summary(auxillary_AMZN)
###Get values from summary
#get df from summary thing says 9 = df
#Multiple R-squared = 0.093
#number of observations = 287, from data
287*0.06513
18.69231 #Test statistic for AMZN


#Calculate Crit-Value, should be close to P-value from summary aux_model
qchisq(p = 0.05,df = 9,lower.tail = FALSE)
18.69231 #Crit value for AMZN
#37.2813 > 16.91898, fail to reject H0, 
#Not enough evidence to conclude that the white test fails to meet the assumptions of homoscedasticity. 
#Good thing error terms are homoscedasticity.






