
#set working directory
setwd("C:/Dev/kaggle/titanic")

#load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

data.combined <- rbind(train, test.survived)

str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

#gross survival rates
table(data.combined$Survived)

#dist among classes
table(data.combined$Pclass)

library(ggplot2)

#H0 - Upper class passengers survived at a higher rate
train$Pclass <- as.factor(train$Pclass)
str(train)

ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  stat_count(width = 0.5) + 
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")


#how many unique names across combined data sets
length(unique(as.character(data.combined$Name)))

#2 dup names
#first, get dup names and store as vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

#second, take a look at the dups in the combined data set
data.combined[which(data.combined$Name %in% dup.names),]

library(stringr)

#noticing differentiation between Miss and Mrs.  Let's see if there's some pattern there
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

#H1 - age correlates with name
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

#H2 - gender correlates with survivability
males <- data.combined[which(data.combined$Sex =="male"),]
males[1:5,]



#extract title into its own variable
extractTitle <- function(name){
  name <- as.character(name)
  if(length(grep("Miss.", name)) > 0){
    return ("Miss.")
  }
  else if(length(grep("Mrs.", name)) > 0){
    return ("Mrs.")
  }  
  else if(length(grep("Mr.", name)) > 0){
    return ("Mr.")
  }
  else if(length(grep("Master.", name)) > 0){
    return ("Master.")
  }  
  else{
    return ("Other")
  }
}

titles <- NULL
for(i in 1:nrow(data.combined)){
  titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}
data.combined$title <- as.factor(titles)

#plot first 891 records, since train records are the only ones to have survived values
ggplot(data.combined[1:891,], aes(x = title, fill = Survived)) +
  stat_count(width = 0.5) + 
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") + 
  ylab("Total Count") + 
  labs(fill = "Survived")


#by sex
#plot first 891 records, since train records are the only ones to have survived values
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  stat_count(width = 0.5) + 
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Sex") + 
  ylab("Total Count") + 
  labs(fill = "Survived")


summary(data.combined$Age)


#plot first 891 records, since train records are the only ones to have survived values
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  geom_bar(width = 10) + 
  facet_wrap(~Sex + Pclass) +
  ggtitle("Pclass") +
  xlab("Age") + 
  ylab("Total Count")


boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$Age)
summary(boys$Sex)
#Master clearly denotes young males

misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$Age)
summary(misses$Sex)

ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) + 
  facet_wrap(~Pclass) +
  geom_bar(width = 1) +
  ggtitle("Age for 'Miss.' by Pclass") +
  xlab("Age") + 
  ylab("Total Count")


#explore misses survival rates
misses.alone <- misses[which(misses$Parch == 0 & misses$SibSp == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))


#moving on to sibsp
summary(data.combined$SibSp)

#can we treat as a factor?  
#number of unique values
length(unique(data.combined$SibSp))

#convert to factor
data.combined$SibSp <- as.factor(data.combined$SibSp)

ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  stat_count(width = 0.5) + 
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("SibSp") + 
  ylab("Total Count") + 
  ylim(0, 100)
  labs(fill = "Survived")
  
  
  
#moving on to ParCh
summary(data.combined$Parch)
  
#can we treat as a factor?  
#number of unique values
length(unique(data.combined$Parch))
  
#convert to factor
data.combined$Parch <- as.factor(data.combined$Parch)

ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
  stat_count(width = 0.5) + 
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Parch") + 
  ylab("Total Count") + 
  ylim(0, 100)
labs(fill = "Survived")  


#let's try some feature engineering.  Family Size feature
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)

#Visualize family size to see if it is predictive
ggplot(data.combined[1:891,], aes(x = family.size, fill = Survived)) +
  stat_count(width = 0.5) + 
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("family.size") + 
  ylab("Total Count") + 
  ylim(0, 300)
labs(fill = "Survived")  


#ticket prices
str(data.combined$Ticket)

#redefine ticket as character since the large number of values doesn't really warrant a factor type
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1, 1))
unique(ticket.first.char)

length(unique(ticket.first.char))

#16 distinct first characters for ticket classification so we can make them factors
data.combined$Ticket.first.char <- as.factor(ticket.first.char)

#Visualize high-level survivability by ticket first char to see if it is predictive
ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) +
  stat_count() + 
  ggtitle("Survivability by ticket first char") +
  xlab("Ticket.first.char") + 
  ylab("Total Count") + 
  ylim(0, 350)
labs(fill = "Survived") 

#looks predictive, so lets plot by char and pclass
ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) +
  stat_count() + 
  facet_wrap(~Pclass) +
  ggtitle("Survivability by ticket first char and passenger class") +
  xlab("Ticket.first.char") + 
  ylab("Total Count") + 
  ylim(0, 350)
labs(fill = "Survived") 

#looks predictive, so lets plot by char and pclass and title
ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) +
  stat_count() + 
  facet_wrap(~Pclass + title) +
  ggtitle("Survivability by ticket first char, title and passenger class") +
  xlab("Ticket.first.char") + 
  ylab("Total Count") + 
  ylim(0, 350)
labs(fill = "Survived") 


summary(data.combined$Fare)
length(unique(data.combined$Fare))
#282 unique values so dont make it a factor

ggplot(data.combined, aes(x = Fare)) +
  geom_histogram(binwidth = 5) + 
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") + 
  ylab("Total Count") + 
  ylim(0, 200) 
labs(fill = "Survived") 

#looks predictive, so lets plot by fare and pclass and title
ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth = 5) + 
  facet_wrap(~Pclass + title) +
  ggtitle("Survivability by Fare, title and passenger class") +
  xlab("Fare") + 
  ylab("Total Count") + 
  ylim(0, 100)
labs(fill = "Survived") 


str(data.combined$Cabin)

data.combined$Cabin <- as.character(data.combined$Cabin)

unique(data.combined$Cabin)

data.combined[which(is.na(data.combined$Cabin)), "Cabin"] <- "U"
data.combined$Cabin[1:100]

cabin.first.char <- substr(data.combined$Cabin, 1, 1)

cabin.first.char <- as.factor(cabin.first.char)

str(cabin.first.char)

unique(cabin.first.char)
levels(cabin.first.char)


data.combined$cabin.first.char <- as.factor(cabin.first.char)

ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  stat_count() + 
  ggtitle("Survivability by cabin first char") +
  xlab("cabin.first.char") + 
  ylab("Total Count") + 
  ylim(0, 1000)
labs(fill = "Survived") 


#looks predictive, so lets plot by fare and pclass and title
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  stat_count() + 
  facet_wrap(~Pclass + title) +
  ggtitle("Survivability by Cabin First Char, title and passenger class") +
  xlab("cabin first char") + 
  ylab("Total Count") + 
  ylim(0, 500)
labs(fill = "Survived") 


data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))
data.combined[which(data.combined$cabin.multiple == "Y"),]
str(data.combined$cabin.multiple)

ggplot(data.combined[1:891,], aes(x = cabin.multiple, fill = Survived)) +
  stat_count() + 
  facet_wrap(~Pclass + title) +
  ggtitle("PClass, Title") +
  xlab("cabin.multiple") + 
  ylab("Total Count") + 
  ylim(0, 500)
labs(fill = "Survived") 

str(data.combined$Embarked)

ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
  stat_count() + 
  facet_wrap(~Pclass + title) +
  ggtitle("PClass, Title") +
  xlab("Embarked") + 
  ylab("Total Count") + 
  ylim(0, 500)
labs(fill = "Survived") 




#Exploratory Modeling


library(randomForest)

rf.train.1 <- data.combined[1:891, c("Pclass", "title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)


rf.train.2 <- data.combined[1:891, c("Pclass", "title", "SibSp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)


rf.train.3 <- data.combined[1:891, c("Pclass", "title", "SibSp", "Parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)


rf.train.4 <- data.combined[1:891, c("Pclass", "title", "family.size")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)




