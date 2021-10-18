# Random Forest

library(ggplot2)
library(cowplot)
library(randomForest)

# Load data

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

data <- read.csv(url, header=FALSE)

head(data)

colnames(data) <- c(
  "age",
  "sex",
  "cp",
  "trestbps",
  "chol",
  "fbs",
  "restecg",
  "thalach",
  "exang",
  "oldpeak",
  "slope",
  "ca",
  "thal",
  "hd"
)

str(data)

data[data == "?"] <- NA

data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex)

data$cp <- as.factor(data$cp)

data$fbs <- as.factor(data$fbs)

data$restecg <- as.factor(data$restecg)

data$exang <- as.factor(data$exang)

data$slope <- as.factor(data$slope)

data$ca <- as.integer(data$ca)
data$ca <- as.factor(data$ca)

data$thal <- as.integer(data$thal)
data$thal <- as.factor(data$thal)

data$hd <- ifelse(test=data$hd == 0, yes="Healthy", no="Unhealthy")
data$hd <- as.factor(data$hd)

str(data)


# Impute values for the NAs in the data set

set.seed(42)

data.imputed <- rfImpute(hd ~ ., data=data, iter=6)

# Build the random forest

model <- randomForest(hd ~ ., data=data.imputed, proximity =TRUE)

model # Type of random forest : Classification
# Number of trees : 500
# Number of variables considered at each internal node : 3
# OOB error estimate : 16.83%. 83.17% of the samples were correctly
# classified
# Confusion matrix
# Y = 0 predicted as Y = 0  :  86.5%
# Y = 1 predicted as Y = 1  :  79.1%


# Verifying if 500 trees is enough for optimal classification
# Plot the error rates

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"Healthy"], 
          model$err.rate[,"Unhealthy"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

# In general, we see the error rates decrease when the n° of trees increases.
# If we added more trees, would the error rate go down further?

model2 <- randomForest(hd ~ ., data=data.imputed, ntree=1000, proximity=TRUE)

model2   # number of trees : 1000
# OBB error estimate : 17.16%. 82.84% of the samples were correctly
# classified
# Y = 0 predicted as Y = 0 :  85.6%
# Y = 1 predicted as Y = 1 :  79.1%
# The confusion matrix shows that we did not do a better job 
# classifying patients


# Plot the error rates

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model2$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model2$err.rate[,"OOB"], 
          model2$err.rate[,"Healthy"], 
          model2$err.rate[,"Unhealthy"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

# The error rates stabilize right after 500 trees

# Verifying that we are considering the optimal number of variables
# at each internal node


oob.values <- vector(length=10)

for(i in 1:10) {
  temp.model <- randomForest(hd ~ ., data=data.imputed, mtry=i, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}

oob.values

# The 5th value, corresponding to mtry=5, has the lowest OBB error rate

# create a model for proximities using the best value for mtry

model3 <- randomForest(hd ~ ., 
                       data=data.imputed,
                       ntree=1000, 
                       proximity=TRUE, 
                       mtry=which(oob.values == min(oob.values)))


model3


# Use the random forest to draw an MDS plot with samples

distance.matrix <- as.dist(1-model$proximity)

mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

mds.values <- mds.stuff$points

mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=data.imputed$hd)

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) + 
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")












