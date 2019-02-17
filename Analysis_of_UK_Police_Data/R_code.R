# **************************************************************#
# Trend of crime
# **************************************************************#

# 1.Gathering data
setwd("~/Downloads/Crime/dataset")
fileNames <- list.files(pattern = "*.csv")  # get the files names
readFiles <- lapply(fileNames,  # apply read.csv
                    function(x) 
                      read.csv(x, stringsAsFactors = F, header = T))
fileBind <- do.call(rbind, readFiles)  # rbind them
write.csv(fileBind, "crimedata_2015-12_2018-11.csv")

crime.data <- read.csv("crimedata_2015-12_2018-11.csv", 
                       header = TRUE, 
                       stringsAsFactors = FALSE)
crime.data <- crime.data[, -1]  # delete sequence number
View(crime.data)

# 2.Structuring data

library(dplyr)
crime.data.monthly <- crime.data %>%  # count by month
  group_by(Month) %>% 
  summarise(Num = n())
View(crime.data.monthly)

# 3.Visualizing data
library(ggplot2)
ggplot(crime.data.monthly, aes(Month, Num, group = 1)) + 
  geom_line() +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Month", y = "Crime Count", title = "The number of crime over month") +
  theme(axis.text.x = element_text(  # change text format
    size = 10, face = "bold", vjust = 0.5, hjust = 0.5, angle = 45)) +
  theme(plot.title = element_text(hjust = 0.5))  # center the title


# **************************************************************#
# Analysis of crime types
# **************************************************************#

# 1.Structuring data
crime.data$Month <- gsub("2016-07", "Jul", crime.data$Month)
crime.data$Month <- gsub("2017-07", "Jul", crime.data$Month)
crime.data$Month <- gsub("2018-07", "Jul", crime.data$Month)
crime.data$Month <- gsub("2016-10", "Oct", crime.data$Month)
crime.data$Month <- gsub("2017-10", "Oct", crime.data$Month)
crime.data$Month <- gsub("2018-10", "Oct", crime.data$Month)
crime.data.peak <- filter(crime.data, (Month %in% c("Jul", "Oct")))
View(crime.data.peak)

# 2.Visualising data
ggplot(data=crime.data.peak) + 
  geom_bar(mapping=aes(x = Month, fill=Crime.type), position="fill") +
  coord_flip() +
  labs(fill = "Crime type") +
  ylab("Precentage of crime count") +
  ggtitle("The number of crime type in peak months") +
  theme(plot.title = element_text(hjust = 0.5))  # center the title


# **************************************************************#
# Visualisation of “anti-social behaviour” on map
# **************************************************************#

# 1.Gathering data
library(rgdal)
london.shape <- readOGR(dsn = "~/Downloads/Crime/BoundaryData", 
                        layer = "england_lsoa_2011")

crime.data.antisocial <- filter(crime.data.peak, 
                                Crime.type == "Anti-social behaviour")
View(crime.data.antisocial)

# 2.Structuring data
library(dplyr)
crime.antisocial.by.LSOA <- crime.data.antisocial %>% 
  group_by(LSOA.code) %>%  # count by LSOA code
  summarise(Num = n())
View(crime.antisocial.by.LSOA)

london.shape@data <- left_join(london.shape@data, 
                               crime.antisocial.by.LSOA,
                               by = c('code' = 'LSOA.code'))
london.shape[is.na(london.shape@data$Num)] <- 0  # remove missing data
View(london.shape@data)

# 3.Visualising data
library(tmap)
tmap_mode("plot")
tm_shape(london.shape) +
  tm_fill("Num", alpha = 0.5, style = "kmeans", border.col = "black") +
  tm_borders(alpha = 0.5) +
  tm_compass(position=c("right", "top")) +
  tm_scale_bar()  # add a scale bar


# ***********************************************************************#
# Prediction of crime
# ***********************************************************************#


# 1.Gathering data
frequent.antisocial.LSOA.code <- filter(crime.antisocial.by.LSOA, Num >= 95)
View(frequent.antisocial.LSOA.code)

setwd("~/Downloads/Crime")
deprivation2015 <- read.csv("Deprivation2015.csv")
View(deprivation2015)

# 2.Structuring data
deprivation2015.variables <- deprivation2015 %>% 
  select(LSOA.code..2011., LSOA.name..2011., 
         Index.of.Multiple.Deprivation..IMD..Score,
         Income.Score..rate., Employment.Score..rate., 
         Education..Skills.and.Training.Score,
         Health.Deprivation.and.Disability.Score, 
         Barriers.to.Housing.and.Services.Score,
         Living.Environment.Score, 
         Total.population..mid.2012..excluding.prisoners.)

names(deprivation2015.variables)[names(deprivation2015.variables)=="LSOA.code..2011."]<-"LSOA.code"
names(deprivation2015.variables)[names(deprivation2015.variables)=="LSOA.name..2011"]<-"LSOA.name"
names(deprivation2015.variables)[names(deprivation2015.variables)=="Index.of.Multiple.Deprivation..IMD..Score"]<-"LSOA.deprivation"
names(deprivation2015.variables)[names(deprivation2015.variables)=="Income.Score..rate."]<-"LSOA.income"
names(deprivation2015.variables)[names(deprivation2015.variables)=="Employment.Score..rate."]<-"LSOA.employment"
names(deprivation2015.variables)[names(deprivation2015.variables)=="Education..Skills.and.Training.Score"]<-"LSOA.edu"
names(deprivation2015.variables)[names(deprivation2015.variables)=="Health.Deprivation.and.Disability.Score"]<-"LSOA.health"
names(deprivation2015.variables)[names(deprivation2015.variables)=="Barriers.to.Housing.and.Services.Score"]<-"LSOA.barries"
names(deprivation2015.variables)[names(deprivation2015.variables)=="Living.Environment.Score"]<-"LSOA.enviroment"
names(deprivation2015.variables)[names(deprivation2015.variables)=="Total.population..mid.2012..excluding.prisoners."]<-"LSOA.population"
View(deprivation2015.variables)

antisocial.with.variables <- left_join(deprivation2015.variables, 
                                  frequent.antisocial.LSOA.code,
                                  by = "LSOA.code")
anyNA(antisocial.with.variables)  # if data contain NA values
is.na(antisocial.with.variables)  # find out NA values
antisocial.with.variables <- na.omit(antisocial.with.variables)  # remove NA values
View(antisocial.with.variables)


antisocial.LSOA.outlier <- frequent.antisocial.LSOA.code %>% 
  mutate(Crime.type = "Antisocial")
View(antisocial.LSOA.outlier)
ggplot(antisocial.LSOA.outlier, aes(Crime.type, Num)) +  # find outliers by boxplot
  geom_boxplot() +
  labs(x = "Crime type", y = "Crime Count", title = "The boxplot of crime count every region") +
  theme(plot.title = element_text(hjust = 0.5))  # center the title
  

antisocial.with.variables <- antisocial.with.variables %>%  # remove outlier
  filter(antisocial.with.variables$Num <= 300)
View(antisocial.with.variables)

# 3.Exploring data

antisocial.model <- lm(
  formula = Num ~ LSOA.population + LSOA.income + LSOA.deprivation + LSOA.employment + LSOA.edu + LSOA.health + LSOA.barries + LSOA.enviroment,
  data = antisocial.with.variables)
summary(antisocial.model)

par(mfrow=c(2,2))
plot(antisocial.model)

antisocial.model.reduced <- step(  # stepwise regression
  antisocial.model, direction = "backward")
summary(antisocial.model.reduced)

antisocial.model.final <- lm(
  formula = Num ~ LSOA.population + LSOA.income,
  data = antisocial.with.variables)
summary(antisocial.model.final)

anova(antisocial.model.final,antisocial.model.reduced)


# **************************************************************#
# Classification of hotspot change
# **************************************************************#

# 1.Gathering data

library(dplyr)
crime.Jun.16.by.LSOA <- crime.data %>%
  filter(crime.data$Month == "2016-06") %>% 
  group_by(LSOA.code) %>% 
  summarise(num.Jun = n())
View(crime.Jun.16.by.LSOA)

crime.Jul.16.by.LSOA <- crime.data %>%
  filter(crime.data$Month == "2016-07") %>% 
  group_by(LSOA.code) %>% 
  summarise(num.Jul = n())
View(crime.Jul.16.by.LSOA)

crime.change.16.by.LSOA <- crime.Jun.16.by.LSOA %>% 
  left_join(crime.Jul.16.by.LSOA, by = "LSOA.code")
crime.change.16.by.LSOA.100 <- filter(crime.change.16.by.LSOA, 
                                      crime.change.16.by.LSOA$num.Jul >= 100)
View(crime.change.16.by.LSOA.100)

anyNA(crime.change.16.by.LSOA.100)  # if data contain NA values
crime.change.16.by.LSOA.100 <- crime.change.16.by.LSOA.100[-1, ]  # remove NULL value
crime.change.16.by.LSOA.100 <- mutate(
  crime.change.16.by.LSOA.100, 
  crime.increased = num.Jul > num.Jun)
View(crime.change.16.by.LSOA.100)

# 2.Structuring data
# test for one area
crime.Jun.16.type.n.F <- crime.data %>%  
  filter(crime.data$Month == "2016-06" & LSOA.code == c("E01004734")) %>% 
  group_by(Crime.type) %>% 
  summarise(n.type.Jun.16 = n())
View(crime.Jun.16.type.n.F)

LSOA.code <- c("E01004734")  # interchange columns and rows
data.frame(crime.Jun.16.type.n.F, row.names=1) 
t1 <- t(data.frame(crime.Jun.16.type.n.F,row.names=1))
t2 <- as.data.frame(t1,row.names=F)
t3.F <- as.data.frame(cbind(LSOA.code,t2))
View(t3.F)
t3.F <- select(t3.F, LSOA.code, `Anti-social behaviour`, `Violence and sexual offences`, `Other theft`, `Theft from the person`)

crime.change.16.by.LSOA.test.F <- crime.change.16.by.LSOA.100 %>% 
  left_join(t3.F, by = "LSOA.code")
View(crime.change.16.by.LSOA.test.F)

# apply to every LSOA areas
LSOA.code.list <- crime.change.16.by.LSOA.100$LSOA.code  
View(LSOA.code.list)

for (LSOA.code.each in LSOA.code.list) {
  crime.Jun.16.type.n <- crime.data %>%
    filter(crime.data$Month == "2016-06" & LSOA.code == LSOA.code.each) %>% 
    group_by(Crime.type) %>% 
    summarise(n.type.Jun.16 = n())
  LSOA.code <- LSOA.code.each
  data.frame(crime.Jun.16.type.n, row.names=1)
  t1 <- t(data.frame(crime.Jun.16.type.n,row.names=1))
  t2 <- as.data.frame(t1,row.names=F)
  t3 <- as.data.frame(cbind(LSOA.code,t2))
  t3 <- select(t3, LSOA.code, `Anti-social behaviour`, `Violence and sexual offences`, `Other theft`, `Theft from the person`)
  t3.F <- rbind(t3, t3.F)
  View(t3.F)
}

crime.change.16.by.LSOA.final <- crime.change.16.by.LSOA.100 %>% 
  left_join(t3.F, by = "LSOA.code")
crime.change.16.by.LSOA.final <- crime.change.16.by.LSOA.final[, c(-1, -2, -3)]

names(crime.change.16.by.LSOA.final)[names(crime.change.16.by.LSOA.final)=="Anti-social behaviour"]<-"anti.social"
names(crime.change.16.by.LSOA.final)[names(crime.change.16.by.LSOA.final)=="Violence and sexual offences"]<-"violence.sexual.offences"
names(crime.change.16.by.LSOA.final)[names(crime.change.16.by.LSOA.final)=="Other theft"]<-"other.theft"
names(crime.change.16.by.LSOA.final)[names(crime.change.16.by.LSOA.final)=="Theft from the person"]<-"theft.from.person"

View(crime.change.16.by.LSOA.final)

# scaling the data
library(tidyverse)  
crime.change.16.by.LSOA.final$crime.increased <- as.factor(crime.change.16.by.LSOA.final$crime.increased)
x <- crime.change.16.by.LSOA.final %>% select(crime.increased)  # save binary crime condition
crime.change.16.by.LSOA.final <- 
  scale(crime.change.16.by.LSOA.final[, -1])  # only apply to numeric
crime.change.16.by.LSOA.final <- cbind(crime.change.16.by.LSOA.final, x)  # add condition back

# Split data into training and test sets
library(caret)  
anyNA(crime.change.16.by.LSOA.final)  # check NA
set.seed(123)
crime.data.split <- crime.change.16.by.LSOA.final$crime.increased %>%
  createDataPartition(p = 0.8, list = FALSE)
crime.train.data <- crime.change.16.by.LSOA.final[crime.data.split, ]
crime.test.data <- crime.change.16.by.LSOA.final[-crime.data.split, ]

# 3.Exploring data

# perform classification
library(caret)
control <- trainControl(method = "cv", number = 10)  # using 10-fold cross validation
metric <- "Accuracy"  # measure by accuracy

nb.model <- train(crime.increased~., data = crime.train.data, method = "nb", metric = metric, trControl = control)
lda.model <- train(crime.increased~., data = crime.train.data, method = "lda", metric = metric, trControl = control)
knn.model <- train(crime.increased~., data=crime.train.data, method="knn", metric=metric, trControl=control)
svm.model <- train(crime.increased~., data=crime.train.data, method="svmRadial", metric=metric, trControl=control)
rf.model <- train(crime.increased~., data=crime.train.data, method="rf", metric=metric, trControl=control)

results <- resamples(list(nb = nb.model,
                          lda = lda.model,
                          knn = knn.model,
                          svm = svm.model,
                          rf = rf.model))  # compare each results
summary(results)  # k nearest neighbour (knn) has maxmium mean accuracy score

knn.predictions <- predict(knn.model, crime.test.data)  # using test data
confusionMatrix(knn.predictions, 
                crime.test.data$crime.increased, 
                positive = "TRUE")  # compare results

# decision tree
library(rpart)
set.seed(123)
rpart.model <- rpart(crime.increased~.,
                     data = crime.train.data, 
                     method = "class")  # simply build decision tree

par(xpd = NA)  # prevent the text in plot overlapped
plot(rpart.model, main = "Classification Tree for crime (trainging data)")
text(rpart.model, digits = 2, cex = 0.6)   #  add text
