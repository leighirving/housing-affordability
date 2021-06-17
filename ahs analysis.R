data_2019 <- read.csv("ahs2019n.csv")
data_2017 <- read.csv("ahs2017n.csv")
data_2015 <- read.csv("ahs2015n.csv")

data_2019$YEAR <- 2019
data_2017$YEAR <- 2017
data_2015$YEAR <- 2015


library(dplyr)


#subset dataframe
data_2019 <- data_2019 %>% 
  select(CONTROL, OMB13CBSA, TENURE, MARKETVAL, HINCP, HHAGE, HHSPAN, HHRACE, PERPOVLVL, HHGRAD, TOTHCAMT, YRBUILT,
         RENT, MORTAMT, PROTAXAMT, INSURAMT, FIRSTHOME, RENTCNTRL, LOANTYPE1, YEAR, BEDROOMS, BATHROOMS,
         UNITSIZE, RATINGHS, RATINGNH)

data_2017 <- data_2017 %>% 
  select(CONTROL, OMB13CBSA, TENURE, MARKETVAL, HINCP, HHAGE, HHSPAN, HHRACE, PERPOVLVL, HHGRAD, TOTHCAMT, YRBUILT,
         RENT, MORTAMT, PROTAXAMT, INSURAMT, FIRSTHOME, RENTCNTRL, LOANTYPE1, YEAR, BEDROOMS, BATHROOMS,
         UNITSIZE, RATINGHS, RATINGNH)

data_2015 <- data_2015 %>% 
  select(CONTROL, OMB13CBSA, TENURE, MARKETVAL, HINCP, HHAGE, HHSPAN, HHRACE, PERPOVLVL, HHGRAD, TOTHCAMT, YRBUILT,
         RENT, MORTAMT, PROTAXAMT, INSURAMT, FIRSTHOME, RENTCNTRL, LOANTYPE1, YEAR, BEDROOMS, BATHROOMS,
         UNITSIZE, RATINGHS, RATINGNH)

merge <- rbind(data_2019, data_2017)
d <- rbind(merge, data_2015)

### Data Cleaning

#convert relevant variables to int
d$LOANTYPE1 <- strtoi(gsub("'", '', d$LOANTYPE1))
d$CONTROL <- strtoi(gsub("'", '', d$CONTROL))
d$HISPANIC <- strtoi(gsub("'", '', d$HISPANIC))
d$RACE <- strtoi(gsub("'", '', d$RACE))
d$EDU_LEVEL <- strtoi(gsub("'", '', d$EDU_LEVEL))
d$TENURE <- strtoi(gsub("'", '', d$TENURE))
d$FIRSTHOME <- strtoi(gsub("'", '', d$FIRSTHOME))
d$RENTCNTRL <- strtoi(gsub("'", '', d$RENTCNTRL))
d$UNITSIZE <- strtoi(gsub("'", '', d$UNITSIZE))


#re-encode NA values
d$HHI[d$HHI== -6] <- NA 
d$HHAGE[d$HHAGE== -6] <- NA
d$PERPOVLVL[d$PERPOVLVL== -6] <- NA
d$EDU_LEVEL[d$EDU_LEVEL== -6] <- NA
d$RENT[d$RENT== -6] <- NA
d$MORTGAGE[d$MORTGAGE== -6] <- NA
d$HISPANIC[d$HISPANIC== -6] <- NA
d$RACE[d$RACE== -6] <- NA
d$REAL_ESTATE_TAX[d$REAL_ESTATE_TAX== -6] <- NA
d$INSURANCE[d$INSURANCE== -6] <- NA
d$MARKETVAL[d$MARKETVAL== -6] <- NA
d$FIRSTHOME[d$FIRSTHOME== -6] <- NA
d$MTHLY_HOUSING_COST[d$MTHLY_HOUSING_COST== -6] <- NA
d$TENURE[d$TENURE== -6] <- NA
d$RENTCNTRL[d$RENTCNTRL== -6] <- NA
d$RATINGHS[d$RATINGHS== -6] <- NA
d$RATINGNH[d$RATINGNH== -6] <- NA

d$FIRSTHOME[d$FIRSTHOME== -9] <- NA
d$RENTCNTRL[d$RENTCNTRL== -9] <- NA
d$UNITSIZE[d$UNITSIZE== -9] <- NA
d$LOANTYPE1[d$LOANTYPE1== -9] <- NA


#relabel HISPANIC
d$HISPANIC[d$HISPANIC==2] <- 0 #relabel non-hispanic

#relabel OWNER
d$FIRSTHOME[d$FIRSTHOME== 2] <- 0
d$RENTCNTRL[d$RENTCNTRL== 2] <- 0

d$TENURE[d$TENURE==2] <- 0
d$TENURE[d$TENURE==3] <- 0

#relabel MSA's
d$MSA[d$MSA== "'31080'"] <- "Los Angeles-Long Beach-Anaheim"
d$MSA[d$MSA== "'35620'"] <- "New York-Newark-Jersey City"
d$MSA[d$MSA== "'16980'"] <- "Chicago-Naperville-Elgin"
d$MSA[d$MSA== "'19100'"] <- "Dallas-Fort Worth-Arlington"
d$MSA[d$MSA== "'37980'"] <- "Philadelphia-Camden-Wilmington"
d$MSA[d$MSA== "'26420'"] <- "Houston-The Woodlands-Sugar Land"
d$MSA[d$MSA== "'47900'"] <- "Washington-Arlington-Alexandria"
d$MSA[d$MSA== "'33100'"] <- "Miami-Fort Lauderdale-West Palm Beach"
d$MSA[d$MSA== "'12060'"] <- "Atlanta-Sandy Springs-Roswell"
d$MSA[d$MSA== "'14460'"] <- "Boston-Cambridge-Newton"
d$MSA[d$MSA== "'41860'"] <- "San Francisco-Oakland-Hayward"
d$MSA[d$MSA== "'19820'"] <- "Detroit-Warren-Dearborn"
d$MSA[d$MSA== "'33100'"] <- "Miami-Fort Lauderdale-West Palm Beach"
d$MSA[d$MSA== "'40140'"] <- "Riverside-San Bernardino-Ontario"
d$MSA[d$MSA== "'38060'"] <- "Phoenix-Mesa-Scottsdale"
d$MSA[d$MSA== "'42660'"] <- "Seattle-Tacoma-Bellevue"
d$MSA[d$MSA== "'99998'"] <- "Non-MSA"
d$MSA[d$MSA== "'99999'"] <- "Non-MSA"

#relabel EDU_LEVEL
d$EDU_LEVEL[d$EDU_LEVEL==31] <- "Less than 9th grade"
d$EDU_LEVEL[d$EDU_LEVEL==32] <- "Less than 9th grade"
d$EDU_LEVEL[d$EDU_LEVEL==33] <- "Less than 9th grade"
d$EDU_LEVEL[d$EDU_LEVEL==34] <- "Less than 9th grade"
d$EDU_LEVEL[d$EDU_LEVEL==35] <- "9th to 12th grade, no diploma"
d$EDU_LEVEL[d$EDU_LEVEL==36] <- "9th to 12th grade, no diploma"
d$EDU_LEVEL[d$EDU_LEVEL==37] <- "9th to 12th grade, no diploma"
d$EDU_LEVEL[d$EDU_LEVEL==38] <- "9th to 12th grade, no diploma"
d$EDU_LEVEL[d$EDU_LEVEL==39] <- "High school graduate or equivalent"
d$EDU_LEVEL[d$EDU_LEVEL==40] <- "High school graduate or equivalent"
d$EDU_LEVEL[d$EDU_LEVEL==41] <- "High school graduate or equivalent"
d$EDU_LEVEL[d$EDU_LEVEL==42] <- "Associates degree"
d$EDU_LEVEL[d$EDU_LEVEL==43] <- "Associates degree"
d$EDU_LEVEL[d$EDU_LEVEL==44] <- "Bachelors degree"
d$EDU_LEVEL[d$EDU_LEVEL==45] <- "Graduate or professional degree"
d$EDU_LEVEL[d$EDU_LEVEL==46] <- "Graduate or professional degree"
d$EDU_LEVEL[d$EDU_LEVEL==47] <- "Graduate or professional degree"

#relabel RACE
d$RACE[d$RACE==1] <- "White alone"
d$RACE[d$RACE==2] <- "Black alone"
d$RACE[d$RACE==3] <- "American Indian or Alaska Native alone"
d$RACE[d$RACE==4] <- "Asian alone"
d$RACE[d$RACE==5] <- "Pacific Islander alone"
d$RACE[d$RACE %in% c(6,7,8,9,10,11,12,13,14,15,16,18,19,20,21)] <- "Two or more races"


#correct data types
d$YEAR <- as.factor(d$YEAR)
d$HISPANIC <- as.factor(d$HISPANIC)
d$RACE <- as.factor(d$RACE)
d$FIRSTHOME <- as.factor(d$FIRSTHOME)
d$UNITSIZE <- as.factor(d$UNITSIZE)
d$RENTCNTRL <- as.factor(d$RENTCNTRL)
d$TENURE <- as.factor(d$TENURE)
d$YRBUILT <- as.factor(d$YRBUILT)
d$MSA <- as.factor(d$MSA)
d$EDU_LEVEL <- as.factor(d$EDU_LEVEL)


#relevel relevant factors
d$RACE <-relevel(d$RACE, "White alone")
d$EDU_LEVEL <-relevel(d$EDU_LEVEL, "Less than 9th grade")

d <- d[!is.na(d$HHI), ] #drop rows where HHI is na

d <- d[d$HHI > 15000, ] #drop rows below cut-off point


#Subset dataframe

d <- d[c("HHI","HHAGE", "HISPANIC", "RACE", "PERPOVLVL", "EDU_LEVEL", "TENURE")]

colSums(is.na(d))
d <- d[complete.cases(d), ]                            # Drop incomplete rows

### Model

set.seed(42)
trainIndex <- sample(1:nrow(d), size=round(0.75*nrow(d)), replace=FALSE)
train <- d[trainIndex,]
test  <- d[-trainIndex,]

logit  <- glm(TENURE ~ HHI + HHAGE + HISPANIC + RACE + PERPOVLVL + EDU_LEVEL, family=binomial (link="logit"), data=d)
summary(logit)

test_x <- test[ , c(1:6)]
predlogit <-predict(logit, newdata=test_x, type="response")
predlogit <- ifelse(predlogit>0.5, 1, 0)

table(test$TENURE, predlogit)                         # Confusion matrix
ClassificationError <- mean(predlogit != test$TENURE) # Classification error
print(paste("Accuracy = ", 1-ClassificationError))        # Accuraty rate

library(ROCR)
pr <- prediction(predlogit, test$TENURE)
prf <- performance(pr, measure="tpr", x.measure="fpr")
plot(prf)                                                 # ROC plot: TPR vs FPR

auc <- performance(pr, measure="auc")
auc <- auc@y.values[[1]]
auc                                                       # Area Under the Curve