library(readr)
library(dplyr)
library(psych)

# Load data and check the structure 
data <- read.csv("Spotify_Tracks_March2021.csv",header = T)
data1 <- data
dim(data1)
str(data1)

# Convert "album_release_date" variable to the date type
data1$album_release_date <- as.Date(data1$album_release_date,'%m/%d/%Y')
str(data1$album_release_date)
summary(data1)


# Handling "spotify_id" duplicate observations
dupes <- duplicated(data1$spotify_id)
table(dupes)
a <- which(dupes=="TRUE")
data2 <- data1[-a,]


# Check missing value 
na_count <- sapply(data2, function(y)
  sum(length(which(is.na(y)))))
na_df <- data.frame(na_count)
print(na_df)
data2 <- na.omit(data2)


# Create an indicator response variable "highly_popular"
data2$highly_popular <- ifelse(data2$song_popularity>=80,1,0)
data2$highly_popular <- as.factor(data2$highly_popular)


# Exploring categorical variables
data2_c <- data2[,sapply(data2,class)=="character"]
t2 <- data2_c %>% dplyr::summarise_all(dplyr::funs(dplyr::n_distinct(.)))  
print(t2)

# Check the normality of dependent variable
summary(data2$song_popularity)
hist(data2$song_popularity)


# Create train and test subset
set.seed(123)
splitSample <- sample(1:3, size=nrow(data2), prob=c(0.4,0.15,0.45), replace = TRUE)
train <- data2[splitSample==1,]
test <- data2[splitSample==2,]
intersect(train$spotify_id,test$spotify_id)


######### GLM Model #################################
# Remove original data
rm(list = c("data","data1","data2","data2_c"))


# Exploring relationships among features
train.glm <- train[,-which(sapply(train,is.numeric)=="FALSE")]
test.glm <- test[,-which(sapply(test,is.numeric)=="FALSE")]
d <- cor(train.glm)
library(corrplot)
corrplot::corrplot(d,method = "circle",type="upper")



# Build model using train data
model1 <- glm(song_popularity~ acousticness+energy+instrumentalness+loudness+speechiness+time_signature,
              family=gaussian(link = "identity"), data = train.glm)
summary(model1)
model2 <- step(glm(song_popularity~.,family=gaussian(link = "identity"),data = train.glm),trace = F)
summary(model2)
par(mfrow=c(2,2))
plot(model2)


# Calculate the predict value using test subset
fit1 <- predict(model1,test.glm)
fit2 <- predict(model2,test.glm)
summary(fit1)
summary(fit2)

# Evaluate the model
fit1.value <- caret::postResample(pred = fit1,obs = test.glm$song_popularity)
fit2.value <- caret::postResample(pred = fit2,obs = test.glm$song_popularity)
fit1.value
fit2.value


############# Logistic Model##############

# Exploring relationships among features
train.glm$highly_popular <- ifelse(train.glm$song_popularity>=80,1,0)
test.glm$highly_popular <- ifelse(test.glm$song_popularity>=80,1,0)
table(train.glm$highly_popular)
table(tes.glmt$highly_popular)
train.glm <- train.glm[,-2]
test.glm <- test.glm[,-2]
train.glm$highly_popular <- as.factor(train.glm$highly_popular)
test.glm$highly_popular <- as.factor(test.glm$highly_popular)



### Use the glm() function to fit a logistic regression model to the training set using at least two predictors
model3 <- glm(highly_popular ~index + album_release_year + 
                album_release_month + acousticness + danceability + energy + 
                instrumentalness + key + liveness + loudness + mode + speechiness + 
                time_signature + total_available_markets + valence,data = train.glm,family = binomial(link = "logit"))
summary(model3)
model4 <- glm(highly_popular ~ acousticness+danceability+energy+instrumentalness+loudness+speechiness+total_available_markets,data = train.glm,family = binomial(link = "logit"))
summary(model3) 
coef(model3)
exp(coef(model3))


### Create a confusion matrix and report the results of your model for the test set.
library(caret)
predict.test <- ifelse(predict(model3,newdata = test.glm,type = "response")>=0.5,"1","0")
predict.test <- as.factor(predict.test)
confusionMatrix(predict.test,test.glm$highly_popular)

glm_control <- caret::trainControl(method = "cv",number = 5,returnResamp = "final")
x <- train.glm[,-18]
y <- as.factor(train.glm$highly_popular)
set.seed(1988)
glm_fit <- caret:: train(x,y,method="glm",trControl=glm_control,trace=F)
glm_train_pred <- predict(glm_fit,train.glm,type = "prob")
colnames(glm_train_pred)<- c("zero","one")
glm_cutoff <- InformationValue::optimalCutoff(train.glm$highly_popular,
                                              glm_train_pred$one,
                                              optimiseFor = "Both",
                                              returnDiagnostics = T)
glm_cutoff
InformationValue::confusionMatrix(train.glm$highly_popular,glm_train_pred$one,threshold = 0.79)




