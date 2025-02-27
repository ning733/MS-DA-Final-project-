# Package
library(ROSE)
library(gridExtra)
library(plyr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(caret)
library(C50)
library(rpart)
library(RColorBrewer)
library(rpart.plot)
library(data.table)
library(e1071)
library(class)

####################################################################################
# Function used
# Count Number of NAs and 0s
NumNA <- function(df){
  temp <- data.frame(matrix(nrow = length(df), ncol = 2))
  for (a in 1:length(df)){
    temp[a,1] <- sum(is.na(df[,a]))
    temp[a,2] <- sum(df[,a] == 0, na.rm = TRUE)
    row.names(temp) <- colnames(df)
    colnames(temp) <- c("Num of NAs", "Num of 0")
  }
  return(temp)
}

# Finding distance with latitude and longitude
SphD <- function(L1,L2,Lg1,Lg2){
  rad = pi/180
  A1 = L1*rad; A2 = Lg1*rad
  B1 = L2*rad; B2 = Lg2*rad
  dlg = B2 - A2
  dla = B1 - A1
  a = (sin(dla/2)^2 + cos(A1) + cos(B1)*(sin(dlg/2)^2))
  c = 2*atan2(sqrt(a), sqrt(1-a))
  R = 6378.145
  d = R*c
  return(d)
}

# Find percentage of types in variables
Percentage_var <- function(variable){
  distribution <- table(variable)
  total <- sum(table(variable))
  distribution/total * 100
}

# For Normalize data
normalize <- function(x) {
  return((x - min(x,na.rm = TRUE)) / (max(x,na.rm = TRUE) - min(x,na.rm = TRUE)))
}

# finding cutoff point
cut <- function(num,data,space,test_data){
  temp <- data.frame(matrix(nrow = num, ncol = 3))
  skip <- space
  for(a in 1:num){
    pred_glm <- ifelse(data > skip, 1, 0)
    c <- confusionMatrix(as.factor(pred_glm), as.factor(test_data))
    temp[a,] <- c(c$overall[1],c$byClass[1],c$byClass[2])
    skip <- skip + space
  }
  colnames(temp) <- c("Accuracy","Sensitivity","Specificity")
  return(temp)
}

# For getting the first and last column of the id (only for Mortgage data set)
GOSI_Mortgage <- function(df,num,choice){
  temp <- data.frame(matrix(nrow = num, ncol = length(df)))
  for (a in 1:num){
    time <- choice(df[df$id == a,]$time)
    temp[a,] <- df[df$id == a & df$time == time,]
  }
  return(temp)
}

########################################################################################################
############################### Case 1 ####################################
# Attributes interpretation
Taxi.df <- read.csv(".\\data\\Taxi-cancellation-case.csv")
glimpse(Taxi.df) # Figure 1

ggplot(Taxi.df, aes(x = factor(Car_Cancellation)), y = Car_Cancellation) +
  geom_bar(fill=c("seagreen4","violetred")) + xlab("Car_cancellation") + coord_flip() # Figure 2

# Dimensional reduction

Taxi.df$Distance <- SphD(Taxi.df$from_lat,Taxi.df$to_lat,Taxi.df$from_long,Taxi.df$to_long)
Taxi.df$Distance <- round_any(Taxi.df$Distance,1)
Taxi.df <- Taxi.df[,!(names(Taxi.df) %in% c("from_lat","from_long","to_lat", "to_long"))]
Taxi.df <- Taxi.df[,!(names(Taxi.df) %in% c("row.","user_id"))]
NumNA(Taxi.df)[c(4,5,6,7),] # Figure 3

Taxi.df <- Taxi.df[,!(names(Taxi.df) %in% c("from_area_id","to_area_id","from_city_id","to_city_id"))]
table(Taxi.df$travel_type_id)
NumNA(Taxi.df[Taxi.df$travel_type_id == 1,])[2,]
NumNA(Taxi.df[Taxi.df$travel_type_id == 2,])[2,]
NumNA(Taxi.df[Taxi.df$travel_type_id == 3,])[2,] # Figure 4

Taxi.df <- Taxi.df[,!(names(Taxi.df) %in% "package_id")]
glimpse(Taxi.df[,c(3,4,7)])

Taxi.df$from_date <- as.Date(Taxi.df$from_date, format = "%m/%d/%Y", "h:m")
Taxi.df$to_date <- as.Date(Taxi.df$to_date, format = "%m/%d/%Y", "h:m")
Taxi.df$booking_created <- as.Date(Taxi.df$booking_created, format = "%m/%d/%Y", "h:m")
NumNA(Taxi.df)[c(3,4,7),] # Figure 5

Taxi.df <- Taxi.df[,!(names(Taxi.df) %in% "to_date")]

glimpse(Taxi.df[,c(3,6)])
Taxi.df$booking_created <- difftime(Taxi.df$from_date,Taxi.df$booking_created, units = "days")
Taxi.df$booking_created = as.numeric(Taxi.df$booking_created)
Taxi.df$from_date <- weekdays(Taxi.df$from_date)

glimpse(Taxi.df[,c(3,6)])

NumNA(Taxi.df)
NumNA(Taxi.df[Taxi.df$travel_type_id == 1,])[8,]
NumNA(Taxi.df[Taxi.df$travel_type_id == 2,])[8,]
NumNA(Taxi.df[Taxi.df$travel_type_id == 3,])[8,]
Taxi.df$Distance <- ifelse(is.na(Taxi.df$Distance), 0,Taxi.df$Distance)

temp <- Taxi.df[Taxi.df$Distance != 0, ]
ggplot(temp,aes(x = Distance)) +
  geom_density( fill="dodgerblue", alpha=0.5) +
  scale_x_log10() +
  geom_vline(xintercept=17975, size=1.5, color="darkblue") +
  geom_vline(xintercept=18000, size=1.5, color="darkblue") +
  ylab("density") # Figure 7

Taxi.df$Distance <- ifelse(Taxi.df$Distance <= 17975, "Short",ifelse(Taxi.df$Distance > 18000, "Long", "Medium"))
Taxi.df$from_date <- ifelse(Taxi.df$from_date %in% c("Friday","Saturday","Sunday"), "Weekend", "Buisness_days")
glimpse(Taxi.df) # Figure 8
NumNA(Taxi.df)

# Data exploration
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(20)
ggplot(Taxi.df, aes(x = factor(vehicle_model_id)), y = Car_Cancellation) +
  geom_bar(fill = mycolors) + xlab("vehicle_model_id") + ylab("number of rides") # Figure 9

mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(38)
ggplot(Taxi.df, aes(x = factor(booking_created)), y = Car_Cancellation) +
  geom_bar(fill = mycolors) + xlab("booking_created") + ylab("number of rides") # Figure 10

mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(3)
p1 <- ggplot(Taxi.df, aes(x = factor(travel_type_id)), y = Car_Cancellation) +
  geom_bar(fill = mycolors) + xlab("travel_type_id") + ylab("number of rides")
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(2)
p2 <- ggplot(Taxi.df, aes(x = factor(from_date)), y = Car_Cancellation) +
  geom_bar(fill = mycolors) + xlab("from_date") + ylab("number of rides") 
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(2)
p3 <- ggplot(Taxi.df, aes(x = factor(online_booking)), y = Car_Cancellation) +
  geom_bar(fill = mycolors) + xlab("online_booking") + ylab("number of rides") 
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(2)
p4 <- ggplot(Taxi.df, aes(x = factor(mobile_site_booking)), y = Car_Cancellation) +
  geom_bar(fill = mycolors) + xlab("mobile_site_booking") + ylab("number of rides") 
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(3)
p5 <- ggplot(Taxi.df, aes(x = Distance), y = Car_Cancellation) +
  geom_bar(fill = mycolors) + xlab("Distance") + ylab("number of rides") 
grid.arrange(p1,p2,p3,p4,p5, ncol = 2) # Figure 11

ggplot(Taxi.df, aes(fill = factor(travel_type_id), x = factor(vehicle_model_id))) +
  geom_bar(position='dodge', stat='count') + xlab("vehicle_model_id") + ylab("number of rides") # Figure 12

ggplot(Taxi.df, aes(fill = factor(travel_type_id), x = factor(Distance))) +
  geom_bar(position='dodge', stat='count') + xlab("Distance") + ylab("number of rides") +
  coord_flip() # Figure 13

# balance the data set
bal.df <- ovun.sample(Car_Cancellation ~ ., data = Taxi.df,
                      method = "over")$data
table(bal.df$Car_Cancellation)  # Figure 14

# logistic regression model check
fit1 <- glm(Car_Cancellation~., data=bal.df, family=binomial)
summary(fit1) # Figure 15

# Data partition
set.seed(12345)
part <- createDataPartition(bal.df$Car_Cancellation, p=0.6, list=FALSE)
train <- bal.df[part,]
valid <- bal.df[-part,]


# C50 classification tree
set.seed(1234)
train$Car_Cancellation <- as.factor(train$Car_Cancellation)
train_tree<-C5.0(train[,-7], train[,7], trails = 5)
Ctree_result <- predict(train_tree, valid[ ,-7])
confusionMatrix(table(Ctree_result, as.factor(valid[ ,7]))) 

# rpart classification tree
set.seed(12345)
tree_model = rpart(Car_Cancellation~., data = train, method="class", minsplit = 10, minbucket=3)
rpart.plot(tree_model)
tree_model.p<-predict(tree_model, valid[ ,-7])
table(tree_model.p[,2])
x <- ifelse(tree_model.p[,2] > 0.6,1,0)
confusionMatrix(table(as.factor(x), valid[ ,7]))

# logistic regression
glm_model <- glm(Car_Cancellation~., data=train, family=binomial)
summary(glm_model)
glm_model.p <-predict(glm_model, valid[ ,-7])
glm_model.p <- normalize(glm_model.p)
c <- cut(10*3,glm_model.p,0.1/3,valid$Car_Cancellation)
ggplot(c) +
  geom_line(aes(x = Sensitivity, y = Specificity),color = c("cadetblue1")) +
  geom_point(aes(x = Sensitivity, y = Specificity),color = c("cornflowerblue"))
x <- ifelse(glm_model.p > 0.1/3 * 17, 1, 0)
confusionMatrix(table(as.factor(x), valid[ ,7]))
0.1/3*17




# Unbalanced dataset
train <- Taxi.df[part,]

# C50 classification tree
set.seed(1234)
train$Car_Cancellation <- as.factor(train$Car_Cancellation)
train_tree<-C5.0(train[,-7], train[,7], trails = 5)
Ctree_result <- predict(train_tree, valid[ ,-7])
confusionMatrix(table(Ctree_result, as.factor(valid[ ,7])))

# rpart classification tree
set.seed(12345)
tree_model = rpart(Car_Cancellation~., data = train, method="class", minsplit = 10, minbucket=3)
rpart.plot(tree_model)
tree_model.p<-predict(tree_model, valid[ ,-7])
table(tree_model.p[,2])
x <- ifelse(tree_model.p[,2] > 0.7,1,0)
x <- factor(x, level = c(0,1))
confusionMatrix(table(x, valid[ ,7]))

# logistic regression
glm_model <- glm(Car_Cancellation~., data=train, family=binomial)
summary(glm_model)
glm_model.p <-predict(glm_model, valid[ ,-7])
glm_model.p <- normalize(glm_model.p)
c <- cut(10*3,glm_model.p,0.1/3,valid$Car_Cancellation)
ggplot(c) +
  geom_line(aes(x = Sensitivity, y = Specificity),color = c("cadetblue1")) +
  geom_point(aes(x = Sensitivity, y = Specificity),color = c("cornflowerblue"))
x <- ifelse(glm_model.p > 0.1/3 * 17, 1, 0)
confusionMatrix(table(as.factor(x), valid[ ,7]))
0.1/3*17

#########################################################################################
#################################Case Study 2###############################################
# Attributes interpretation
Mortgage.df <- read.csv(".\\data\\Mortgage.csv")
glimpse(Mortgage.df) # Figure 16

NumNA(Mortgage.df)

ggplot(Mortgage.df, aes(x = factor(status_time), y = ..count..)) +
  geom_bar(color = c("darkblue"),
           fill=c("seagreen3","grey","dodgerblue")) + 
  xlab("Mortgage status") +
  scale_y_continuous(labels = scales::comma) +
  geom_text(stat='count',
            aes(label= paste(round_any((..count../sum(..count..)*100),0.1), "%", sep = " "))
            , hjust=-0.1, colour = "black") +
  coord_flip() # Figure 18

bower23195 <- Mortgage.df[Mortgage.df$id == 23195,]
ggplot(bower23195, aes(x = time, y = balance_time)) +
  geom_line(color = c("cadetblue1")) + geom_point(color = c("cornflowerblue"))+
  ylab("outstanding balance") # Figure 19

# Dimensional Reduction
Mortgage.df$mat_time <- Mortgage.df$mat_time - Mortgage.df$orig_time
Mortgage.df <- Mortgage.df[,!(names(Mortgage.df) %in% c("orig_time","first_time"))]
Mortgage.df <- Mortgage.df[,!(names(Mortgage.df) %in% c("default_time","payoff_time"))]

last_r.df <- setDT(Mortgage.df)[, .SD[which.max(time)], by=id]
glimpse(last_r.df) # Figure 21

first_r.df <- last_r.df[,-c(4,5,6,7)]
last_r.df <- last_r.df[,-c(14,16,17,18)]
glimpse(last_r.df)
glimpse(first_r.df) # Figure 22

last_r.df <- last_r.df[,-c(1,2)]
first_r.df <- first_r.df[,-c(1,2)]
count(is.na(last_r.df$LTV_time))
count(is.na(first_r.df$LTV_orig_time)) # Figure 23

last_r.df <- last_r.df[-c(39722:39738,49658),]
first_r.df <- first_r.df[-c(39722:39738,49658),]

# Data exploration
ggplot(last_r.df, aes(x = factor(round_any(mat_time,10)),fill = factor(status_time))) +
  geom_bar() + xlab("mat_time") + ylab("number of borrowers") # Figure 24

ggplot(last_r.df, aes(x = round_any(balance_time,500),fill = factor(status_time))) +
  geom_bar() + xlab("balance") + ylab("number of borrowers") + scale_x_continuous(limits = c(1, 868000)) # Figure 25

ggplot(last_r.df, aes(x = round_any(interest_rate_time,1),fill = factor(status_time))) +
  geom_bar() + xlab("interest rate") + ylab("number of borrowers") + scale_x_continuous(limits = c(1, 38)) # Figure 26

p1 <- ggplot(last_r.df, aes(x = factor(REtype_CO_orig_time),fill = factor(status_time))) +
  geom_bar() + xlab("condominium") + ylab("number of borrowers") 
p2 <- ggplot(last_r.df, aes(x = factor(REtype_PU_orig_time),fill = factor(status_time))) +
  geom_bar() + xlab("planned urban development") + ylab("number of borrowers")
p3 <- ggplot(last_r.df, aes(x = factor(REtype_SF_orig_time),fill = factor(status_time))) +
  geom_bar() + xlab("Single family") + ylab("number of borrowers")
grid.arrange(p1,p2,p3, ncol = 3)  # Figure 27

temp <- last_r.df
temp$baldiff <- temp$balance_time - first_r.df$balance_orig_time
temp$LTVdiff <- temp$LTV_time - first_r.df$LTV_orig_time
temp$irdiff <- temp$interest_rate_time - first_r.df$Interest_Rate_orig_time
temp$hpidiff <- temp$hpi_time - first_r.df$hpi_orig_time

ggplot(temp, aes(x = round_any(baldiff,500),fill = factor(status_time))) +
  geom_bar() + xlab("difference in balance") + ylab("number of borrowers") + scale_x_continuous(limits = c(-32500, 50000)) # Figure 28

ggplot(temp, aes(x = round_any(LTVdiff,10),fill = factor(status_time))) + 
  geom_bar() + xlab("difference in LTV") + ylab("number of borrowers") + scale_x_continuous(limits = c(-150, 200)) # Figure 29

ggplot(temp, aes(x = round_any(irdiff,1),fill = factor(status_time))) +
  geom_bar() + xlab("difference in interest rate") + ylab("number of borrowers") + scale_x_continuous(limits = c(-10, 20)) # Figure 30

ggplot(temp, aes(x = round_any(hpidiff,1),fill = factor(status_time))) +
  geom_bar() + xlab("difference in hpi") + ylab("number of borrowers") + scale_x_continuous(limits = c(-80, 150)) # Figure 31


# Creating Training and validation
last_r.df <- last_r.df[last_r.df$status_time != 0]
last_r.df$status_time <- ifelse(last_r.df$status_time == 1, 0, 1)
last_r.df$status_time <- as.factor(last_r.df$status_time)
check <- glm(status_time~., data=last_r.df, family = "binomial")
summary(check) # Figure 32

last_r.df <- last_r.df[,-c(8,9,10)]
glimpse(last_r.df)

set.seed(12345)
part <- createDataPartition(last_r.df$status_time, p=0.6, list=FALSE)
train <- last_r.df[part,]
valid <- last_r.df[-part,]

# Creating origination time data set
glimpse(first_r.df)
glimpse(last_r.df)
names(first_r.df)[names(first_r.df) == "balance_orig_time"] <- "balance_time"
names(first_r.df)[names(first_r.df) == "LTV_orig_time"] <- "LTV_time"
names(first_r.df)[names(first_r.df) == "Interest_Rate_orig_time"] <- "interest_rate_time"
names(first_r.df)[names(first_r.df) == "hpi_orig_time"] <- "hpi_time"
first_r.df <- first_r.df[,-c(4,5,6,13)]

# Model k-nn
set.seed(12345)
knntuning1 = tune.knn(x = train[,-10], y = as.factor(train$status_time), k = 1:30)
summary(knntuning1)
g <- knntuning1$performances
ggplot(g) +
  geom_line(aes(x = k, y = error),color = c("cadetblue1")) +
  geom_point(aes(x = k, y = error),color = c("cornflowerblue"))  # Figure 33

Mor_knn_1<-knn(train=train[,-10], test=valid[,-10], cl=train$status_time, k=30)
confusionMatrix(table(Mor_knn_1, valid$status_time))  # Figure 34

# C.50 classification tree
set.seed(12345)
Mor_tree<-C5.0(train[,-10], as.factor(train$status_time))
Mor_tree.p<-predict(Mor_tree, valid[,-10])
confusionMatrix(table(Mor_tree.p, valid$status_time))  # Figure 35

# random forest (takes a lot of time to run)
set.seed(12345)
Mor_rf <- train(status_time~.,
                data = train,
                method = "rf",
                trControl = trainControl(method = 'cv',
                                         number = 5))

Mor_rf.p<-predict(Mor_rf, valid[ ,-10])
confusionMatrix(table(Mor_rf.p, valid$status_time))   # Figure 36

# results
Mor_rf.p<-predict(Mor_rf, first_r.df)
Mor_rf.p <- data.frame(Mor_rf.p)
p1 <- ggplot(Mor_rf.p, aes(x = factor(Mor_rf.p), y = ..count..)) +
  geom_bar(fill = c("red","blue")) + xlab("Pay-off status") + ylab("number of borrowers")
p2 <- ggplot(last_r.df, aes(x = factor(status_time), y = ..count..)) +
  geom_bar(fill = c("red","blue")) + xlab("Pay-off status") + ylab("number of borrowers")
grid.arrange(p1,p2, ncol = 2)   # Figure 37

table(Mor_rf.p$Mor_rf.p)/length(Mor_rf.p$Mor_rf.p)
table(last_r.df$status_time)/length(last_r.df$status_time)   # Figure 38

#########################################################################################
#################################Case Study 3###############################################
# Attributes interpretation
HR.df <- read.csv(".\\data\\HR.csv")
glimpse(HR.df) # Figure 39

head(HR.df,5)
tail(HR.df,5) # Figure 40

# Data exploration
HR.df[,c(9,10)]
colnames(HR.df)[9] <- "Department"
HR.df$salary <- ifelse(HR.df$salary == "low", 0,
                       ifelse(HR.df$salary == "medium",1,2))
HR.df[,c(9,10)]  # Figure 41

ggplot(HR.df, aes(x = factor(left), y = ..count..)) +
  geom_bar(color = c("darkblue"),
           fill = c("grey", "dodgerblue")) + xlab("Left the company") +
  geom_text(stat='count',
            aes(label= paste(round_any((..count../sum(..count..)*100),0.1), "%", sep = " "))
            , hjust=0.3, vjust = -7.6, colour = "black") +
  coord_flip()  # Figure 42

ggplot(HR.df, aes(x = factor(Department), y = ..count..)) +
  geom_bar(color = c("darkblue"),
           fill = c(1:10)) + xlab("Department") +
  geom_text(stat='count',
            aes(label= ..count..), hjust=0.35, vjust = -0.1, colour = "black") # Figure 43

unique(HR.df$salary)

ggplot(HR.df, aes(x = factor(salary), y = ..count..)) +
  geom_bar(color = c("darkblue"),
           fill = c("dodgerblue",3,7)) + xlab("Salaries") +
  geom_text(stat='count',
            aes(label= ..count..), hjust=0.35, vjust = -5, colour = "black")+
  coord_flip() # Figure 44


ggplot(HR.df, aes(fill = factor(salary), x = factor(Department))) +
  geom_bar(position='dodge', stat='count') + xlab("Department") +
  labs(fill = "Salary") # Figure 45

par(mfrow=c(1,3))
boxplot(satisfaction_level~left,data=HR.df, main="left vs satisfaction",
        xlab="employee left", ylab="Satisfaction")
boxplot(last_evaluation~left,data=HR.df, main="left vs last_evaluation",
        xlab="employee left", ylab="last evaluation")
boxplot(number_project~left,data=HR.df, main="left vs number_project",
        xlab="employee left", ylab="number of project did") # Figure 46

par(mfrow = c(1,3))
par(mar=c(3,3,3,3))
boxplot(average_montly_hours~left,data=HR.df, main="left vs average_montly_hours",
        xlab="employee left", ylab="average montly hours")
plot.new()
par(mar = c(3,3,3,3))
boxplot(time_spend_company~left,data=HR.df, main="left vs time_spend_company",
        xlab="employee left", ylab="time spend at company") # Figure 47

p1 <- ggplot(HR.df, aes(fill = factor(Work_accident), x = factor(left))) +
  geom_bar(position='dodge', stat='count') + xlab("left") +
  labs(fill = "Work_accident") + scale_x_discrete(labels=c("not left", "left")) +
  scale_fill_discrete(labels = c("No", "Yes"))
p2 <- ggplot(HR.df, aes(fill = factor(promotion_last_5years), x = factor(left))) +
  geom_bar(position='dodge', stat='count') + xlab("left") +
  labs(fill = "promotion_last_5years") + scale_x_discrete(labels=c("not left", "left")) +
  scale_fill_discrete(labels = c("No", "Yes"))
p3 <- ggplot(HR.df, aes(fill = factor(Department), x = factor(left))) +
  geom_bar(position='dodge', stat='count') + xlab("left") +
  labs(fill = "Department") + scale_x_discrete(labels=c("not left", "left"))
p4 <- ggplot(HR.df, aes(fill = factor(salary), x = factor(left))) +
  geom_bar(position='dodge', stat='count') + xlab("left") +
  labs(fill = "salary") + scale_x_discrete(labels=c("not left", "left")) +
  scale_fill_discrete(labels = c("low", "medium", "high"))
grid.arrange(p1,p2,p3,p4, ncol = 2) # Figure 48

# Hypothesis 1
text <- round_any(as.numeric(table(HR.df[HR.df$left == 1,]$salary)/table(HR.df$salary) * 100),0.1)
ggplot(HR.df, aes(fill = factor(left), x = factor(salary))) +
  geom_bar(position='dodge', stat='count') + xlab("Salary") +
  labs(fill = "left") +
  scale_x_discrete(labels=c('low', 'medium', 'high')) +
  geom_text(x = 1.35, y = 2300,
            label= paste(text[1], "%", sep = " ")) +
  geom_text(x = 2.35, y = 1450,
            label= paste(text[2], "%", sep = " ")) +
  geom_text(x = 3.35, y = 200,
            label= paste(text[3], "%", sep = " ")) +
  scale_fill_discrete(labels = c("not left", "left")) # Figure 49

# Hypothesis 2
text <- round_any(as.numeric(table(HR.df[HR.df$left == 1,]$Work_accident)/table(HR.df$Work_accident) * 100),0.1)
ggplot(HR.df, aes(fill = factor(left), x = factor(Work_accident))) +
  geom_bar(position='dodge', stat='count') + xlab("Work_accident") +
  labs(fill = "left") +
  scale_x_discrete(labels=c('no', 'yes')) +
  geom_text(x = 1.25, y = 3700,
            label= paste(text[1], "%", sep = " ")) +
  geom_text(x = 2.25, y = 400,
            label= paste(text[2], "%", sep = " ")) +
  scale_fill_discrete(labels = c("not left", "left")) # Figure 50

#Hypothesis 3
temp <- HR.df
temp$A_N <- temp$average_montly_hours/temp$number_project
ggplot(temp,aes(x = satisfaction_level))+
  geom_density( fill="dodgerblue", alpha=0.5)+
  scale_x_log10() +
  geom_vline(xintercept=mean(HR.df$satisfaction_level), size=1, color="dark blue") +
  geom_text(aes(x = mean(HR.df$satisfaction_level) - 0.1, y = 2),
            label= paste0("Mean\n",round_any(mean(HR.df$satisfaction_level),0.001))) +
  ylab("density") +
  geom_hline(yintercept=2.85, size=1, color="black")+
  geom_text(aes(x = 0.84, y = 3.1),label = paste0("peak\n", "~0.84")) # Figure 51

ggplot(temp,aes(x = A_N))+
  geom_density( fill="dodgerblue", alpha=0.5)+
  scale_x_log10() +
  geom_vline(xintercept=mean(temp$A_N), size=1, color="dark blue") +
  geom_text(aes(x = mean(A_N) - 10, y = 2),
            label= paste0("Mean\n",round_any(mean(temp$A_N),0.001))) +
  ylab("density")  # Figure 52

ggplot(temp,aes(x = factor(Work_accident), y = ..count..))+
  geom_bar(fill = c("deepskyblue2","darkolivegreen3")) + xlab("Work_accident")  +
  scale_x_discrete(labels=c('no', 'yes')) +
  geom_text(stat='count',
            aes(label= paste(round_any((..count../sum(..count..)*100),0.1), "%", sep = " "))) +
  coord_flip()  # Figure 53

ggplot(temp,aes(x = factor(promotion_last_5years), y = ..count..))+
  geom_bar(fill = c("deepskyblue2","darkolivegreen3")) + xlab("promotion_last_5years")  +
  scale_x_discrete(labels=c('no', 'yes')) +
  geom_text(stat='count',
            aes(label= paste(round_any((..count../sum(..count..)*100),0.1), "%", sep = " "))) +
  coord_flip()  # Figure 54

ggplot(temp,aes(x = last_evaluation))+
  geom_density( fill="dodgerblue", alpha=0.5)+
  scale_x_log10() +
  geom_vline(xintercept=mean(temp$last_evaluation), size=1.5, color="red") +
  geom_text(aes(x = mean(last_evaluation) - 0.1, y = 2),
            label= paste0("Mean\n",round_any(mean(temp$last_evaluation),0.001))) +
  ylab("density") +
  geom_hline(yintercept=3.91, size=1, color="black") +
  geom_text(aes(x = 0.905, y = 3.71),label = paste0("peak\n", "~0.905"))+
  geom_hline(yintercept=2.77, size=1, color="black") +
  geom_text(aes(x = 0.547, y = 2.97),label = paste0("peak\n", "~0.547"))  # Figure 55

# Data partitioning
HR.df_Glm <- glm(left~., data=HR.df, family = "binomial")
summary(HR.df_Glm) # Figure 56

HR.df$left <- as.factor(HR.df$left)
set.seed(12345)
part<- createDataPartition(HR.df$left, p=0.6, list=FALSE)
train <- HR.df[part,]
valid <- HR.df[-part,]

# Models result
set.seed(12345)
HR_tree<-C5.0(train[,-7], train[,7])
HR_tree.p<-predict(HR_tree, valid[,-7])
confusionMatrix(table(HR_tree.p, valid[ ,7]))

set.seed(12345)
tree_model = rpart(left~., data = train, method="class", minsplit = 10, minbucket=3)
printcp(tree_model)
hr_model_pruned <- prune(tree_model, cp = 0.01)
HR_tree2.p<-predict(hr_model_pruned, valid[,-7])
x <- ifelse(HR_tree2.p[,2]- HR_tree2.p[,1] > 0,1,0)
confusionMatrix(table(x, valid[ ,7]))
