#Reading data
getwd()
setwd("C:\\Users\\ceyhu\\Downloads")
newdata <- read.csv("manipulated4.csv",sep=",")
head(newdata)
dim(newdata)



#install.packages("dplyr")
library(dplyr)
filter_data <- newdata %>% select(-"HospitalAdmissions", -"HealthImpactClass",-"RecordID")
# Randomly sample 2000 rows without replacement
set.seed(123)  # For reproducibility
filtered_data <- filter_data[sample(nrow(filter_data), 2000), ]
dim(filtered_data)


par(mfrow = c(3, 4), mar = c(4, 4, 2, 1)) 
for (var in colnames(filtered_data)) {
  qqnorm(filtered_data[[var]], main = paste("Q-Q Plot:", var))
  qqline(filtered_data[[var]], col = "red", lwd = 2)
}




#######EXPLATORY DATA ANALYSIS
##Data description
dim(filtered_data)
str(filtered_data)
summary(filtered_data) # Ranges of the data are very different from each other, so we should scaling.

##multivariate normality check 
#install.packages("MVN")
library(MVN)
# H0:The data follows normal distribution
result <- mvn(filtered_data, mvnTest = "royston")
result$multivariateNormality # p=1.652174e-42<0.05 so reject H0
result$univariateNormality
result_plot <- mvn(filtered_data, mvnTest = "royston", univariatePlot = "qqplot") #univariate plots to find the reason for deviation from MVN

##make it data normal by using bestnormalize function
#install.packages("bestNormalize")
library(bestNormalize)
normalization <- lapply(filtered_data, bestNormalize) #ordernorm transformation is the best way
normal_data <- filtered_data
for (col in colnames(normal_data)) {
  if (is.numeric(normal_data[[col]])) {
    best_norm <- bestNormalize(normal_data[[col]])
    normal_data[[col]] <- predict(best_norm, normal_data[[col]])
  }
}


par(mfrow = c(3, 4))  # To display multiple plots at once
for (col in colnames(normal_data)) {
  if (is.numeric(normal_data[[col]])) {
    qqnorm(normal_data[[col]], main = paste("Q-Q Plot of", col))
    qqline(normal_data[[col]], col = "red")
  }
}   
mvn(normal_data,mvnTest = "royston")$multivariateNormality ## data is normal now.


##outlier detection 
outlier <- mvn(normal_data, mvnTest = "royston", multivariateOutlierMethod = "quan") #204 outlier
# Adjusted Mahalanobis distance
outlier2 <- mvn(normal_data, mvnTest = "royston", multivariateOutlierMethod = "adj") #155 outlier



######## Inferences About Mean Vector
##Are the mean levels of air pollutants (PM10, NO2, SO2, O3) significantly different from established safe limits or benchmarks?
# H0:mu=mu0   ! we extract PM2.5!
# We are constructing hypothesis testing so we need to check normality assumption.
data2 <- normal_data[,c(2,4,5,6)] # data2 <- PM10, NO2, SO2, O3
mu <- matrix(0,4,1)
benchmarks <- c(150,100,75,120)
mu0 <- matrix(benchmarks,4,1)

x_bar <- matrix(colMeans(data2))
S <- cov(data2)
n <- nrow(data2)
p <- ncol(data2)
T2 <- n*t(x_bar - mu0) %% solve(S) %% (x_bar - mu0)
f <- T2* (n-p)/((n-1)*p)
pf(f,p,n-p,lower.tail=FALSE)

#install.packages("rrvoc")
library(rrcov)
T2.test(data2,mu=mu0)  #to check our result


install.packages("ICSNP")
library("ICSNP")


data3 <- normal_data[,c(4,5)]
mu3 <- matrix(0,2,1)
benchmarks3 <- c(100,75)
mu03 <- matrix(benchmarks3,2,1)

x_bar3 <- matrix(colMeans(data3))
S3 <- cov(data3)
n3 <- nrow(data3)
p3 <- ncol(data3)
T2_3 <- n3*t(x_bar3 - mu03) %% solve(S3) %% (x_bar3 - mu03)
f3 <- T2_3* (n3-p3)/((n3-1)*p3)
pf(f3,p3,n3-p3,lower.tail=FALSE)

install.packages("rrvoc")
library(rrcov)
T2.test(data3,mu=mu03)  #to check our result

install.packages("psych")
library(psych)
error.bars (data3, ylab="Group Means", xlab=" Dependent Variables")

#one-at-a-time confidence interval for each variable 
SO2 <- lm(SO2 ~ 1, data = data3)
confint(SO2)

NO2 <- lm(NO2 ~ 1, data = data3)
confint(NO2)


#Simultaneous Confidence Intervals and Confidence Region
install.packages("mvdalab")
library(mvdalab)
MVcis(data3)  




##### COMPARISONS OF SEVERAL MULTIVARIATE MEANS (MANOVA)
#Are there significant differences between the means of temperature group 'from -9.99 to 15' and 'from 15 to 25' and 'from 25 to 40'
#H0: trt1 = trt2 = trt3 (no treatment effect)
summary(normal_data$Temperature)

breaks <- c(-3.5, -0.6, 0.6, 3.5)  # Adjust the breakpoints as needed

# Categorize Temperature into new groups based on normalized values
normal_data$TemperatureGroup <- cut(normal_data$Temperature,
                                    breaks = breaks,
                                    labels = c('LOW', 'MEDIUM', 'HIGH'))

library(car)
manova_data <- normal_data %>%
  filter(!is.na(TemperatureGroup)) %>%
  select(SO2, NO2, O3, PM10,PM2_5, TemperatureGroup)

head(manova_data)

library(heplots)
boxM(Y = cbind(manova_data$SO2,manova_data$NO2,manova_data$O3,manova_data$PM10,manova_data$PM2_5), group = factor(manova_data$TemperatureGroup))

# List of pollutants to plot
pollutants <- c("PM10", "PM2_5", "NO2", "SO2", "O3")

# Set up plotting layout
par(mfrow = c(3, 2))  # Adjust layout: 3 rows, 2 columns (one empty space)

# Loop through each pollutant and create a boxplot
for (pollutant in pollutants) {
  boxplot(normal_data[[pollutant]] ~ normal_data$TemperatureGroup,
          main = paste("Boxplot of", pollutant, "by Temperature Group"),
          xlab = "Temperature Group",
          ylab = pollutant,
          col = "lightblue",
          border = "darkblue")
}

# Reset layout
par(mfrow = c(1, 1))
# Perform MANOVA on the normalized data
manova_model <- manova(cbind(PM10, PM2_5, NO2, SO2, O3) ~ TemperatureGroup,
                       data = manova_data)

summary(manova_model)

# Test for individual pollutant effects (optional)
summary.aov(manova_model)

#DISCR??M??NATION
summary(normal_data$AQI)

AQI_index<- function(x) {
  if (x <= 0) { 
    return("Acceptable")
  } else { 
    return("Unhealthy")
  }
}

df <- normal_data %>%
  mutate(AQI_INDEX = sapply(AQI, AQI_index))

df$AQI_INDEX <- as.factor(df$AQI_INDEX)
head(df)
df<-df[,-c(1)]

library(vcvComp)
# Calculate group covariance matrices
cov_matrices_vcv <- cov.group(as.matrix(df[, 2:12]), groups = df$AQI_INDEX)

# Print the covariance matrices
print(cov_matrices_vcv)


set.seed(1)
library(caret)
training.samples <- df$AQI_INDEX %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- df[training.samples, ]
test.data <- df[-training.samples, ]

# Estimate preprocessing parameters
preproc.param <- train.data %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)



library(MASS)
model <- lda(AQI_INDEX~., data = train.data)
model
plot(model)
# Make predictions
predictions <- model %>% predict(test.data)
# Model accuracy
mean(predictions$class==test.data$AQI_INDEX) #0.96 overall


#take a look at confusion matrix
library(caret)

# Get the predicted class labels
predicted_class <- predictions$class

# Create the confusion matrix
confusion_matrix <- table(test.data$AQI_INDEX, predicted_class)

# Plot the confusion matrix using the caret package
confusionMatrix(confusion_matrix, main = "LDA Model Confusion Matrix")


#test performance
test_predict<- predict(model,test.data)$class
table_test<- table(Predicted =test_predict, Actual = test.data$AQI_INDEX)
table_test

sum(diag(table_test))/sum(table_test)

#train performance
train_predict<- predict(model,train.data)$class
table_train <- table(Predicted =train_predict, Actual = train.data$AQI_INDEX)
table_train

sum(diag(table_train))/sum(table_train)

#modelling
# Extract the coefficients from the LDA model
coef <- model$scaling

# Calculate the intercept from the LDA model's means
means <- model$means
intercept <- -0.5 * sum(coef * (means["Unhealthy", ] + means["Acceptable", ]))

# Generate the decision boundary line
boundary_line <- function(x) {
  -(coef[1] / coef[2]) * x - (intercept / coef[2])
}

# Create the plot with the decision boundary
# Assuming "Feature1" and "Feature2" are the actual column names
ggplot(data = test.data, aes(x = Feature1, y = Feature2, color = AQI_INDEX)) +
  geom_point(size = 2) +
  stat_function(fun = boundary_line, color = "black", size = 1, linetype = "dashed") +
  labs(title = "LDA Classification with Decision Boundary", 
       x = "Feature 1", 
       y = "Feature 2", 
       color = "AQI Index") +
  scale_color_manual(values = c("Acceptable" = "blue", "Unhealthy" = "red")) +
  theme_minimal()


#canonical correlations
library(CCA)  
colnames(filtered_data)
x1<-filtered_data[,c('NO2','SO2','O3')] #we take the subset of variable x1 and x2. 
x2<-filtered_data[,c('Temperature','Humidity','WindSpeed')]

cca_result <- cancor(x1, x2)

# Display canonical correlations
cca_result$cor

cca_result$xcoef
cca_result$ycoef



U <- as.matrix(x1) %*% cca_result$xcoef  # Canonical variates for X
V <- as.matrix(x2) %*% cca_result$ycoef  # Canonical variates for Y

library(CCA)
cca_significance <- matcor(x1, x2)
print(cca_significance)



# Scatterplot of the first pair of canonical variates
plot(U[,1], V[,1], 
     xlab = "First Canonical Variable (X)", 
     ylab = "First Canonical Variable (Y)", 
     main = "Canonical Correlation Analysis")


##################PCA
#install.packages("car")
library(car)
corr <- cor(normal_data)
library(corrplot)
corrplot(corr) 

scaled_data <- scale(normal_data)
all.equal(cor(scaled_data), cov(scaled_data)) 

mydata <- scaled_data[,-12] #put response variable put a side
pca1 <- prcomp(mydata)
summary(pca1)  
names(pca1)

pca1$rotation # V Matrix-Eigenvectors
pca1$x # Z matrix- Each column is a principal component
pca1$sdev

#decide how many components should we include in our analysis.
#install.packages("factoextra")
library(factoextra)
fviz_eig(pca1,addlabels=TRUE) 
pca <- pca1$x[,1:3]  
head(pca)

# check whether they are linearly independent or not.
res1 <- cor(pca, method="pearson")
corrplot::corrplot(res1, method= "color", order = "hclust") 
cor(mydata,pca)  

fviz_pca_var(pca1,axes = c(1, 2)) 
fviz_pca_var(pca1,axes = c(7, 8)) 
fviz_pca_var(pca1, select.var = list(contrib = 3)) 


ols.data <- data.frame(response=scaled_data[,12],pca)
lmodel <- lm(response ~ ., data = ols.data)
summary(lmodel)
mean((ols.data$response - predict(lmodel))^2) #mse 0.01 geldi


############Factor Analysis 
#install.packages("psych")
library(psych)
KMO(r=corr)  
cortest.bartlett(corr,nrow(normal_data))

## decide number of factors
library(psych)
parallel <- fa.parallel(normal_data, fm = "minres", fa = "fa") 
#H0: 3 factors are sufficient
factanal(normal_data, factors =3, method ="mle")$PVAL

##factor analysis
f<-factanal(normal_data, factors = 3)
f
f_eigenvalues <- f$values 
f_eigenvalues 

# Plot the scree plot for visual inspection
scree(f) 
#install.packages("fa.parallel")
library(fa.parallel)
fa.parallel(normal_data, fm = "minres", fa = "fa")  


load <- f$loadings[,1:2]
plot(load,type="n") 
text(load,labels=names(normal_data),cex=.7)


names(f$loadings[,1])[abs(f$loadings[,1])>0.0]
f1<-normal_data[,names(f$loadings[,1])[abs(f$loadings[,1])>0.0]]  
f2 <-normal_data[,names(f$loadings[,2])[abs(f$loadings[,2])>0.0]]
summary(alpha(f1, check.keys=TRUE)) 
summary(alpha(f2, check.keys=TRUE)) 



scores<-factanal(normal_data, factors = 2,scores="regression")$scores 
head(scores)
cm1 <- cor(scores, method="pearson")
corrplot::corrplot(cm1, method= "number", order = "hclust") 



######CLUSTERING
##Agglomerative Hierarchical clustering
dm <- dist(scaled_data) 
set.seed(123)
indices <- sample(1:nrow(scaled_data),25) 
sampled_data <- scaled_data[indices,]
dm2 <- dist(sampled_data)

par(mfrow=c(2,2),mar=c(1,2,1,2))
plot(cs <- hclust(dm2, method = "single"),main = "Single Linkage", ylab="Height")
plot(cc <- hclust(dm2, method = "complete"),main = "Complete Linkage",ylab="Height")
plot(ca <- hclust(dm2, method = "average"),main = "Average Linkage",ylab="Height")
plot(cw <- hclust(dm2, method = "ward.D2"),main = "Ward Method",ylab="Height") 

##Divisive Hierarchical Clutering (DIANA)
library(cluster)
library(factoextra)
res.diana <- diana(sampled_data, stand = TRUE)
fviz_dend(res.diana, cex = 0.5,
          k = 4, # Cut in four groups
          palette = "jco" # Color palette
) 


###K-means Clustering:
nrow(scaled_data)
wss <- sapply(1:10, function(k) {
  kmeans(scaled_data, centers = k, nstart = 25)$tot.withinss
})


#install.packages("tibble")
library(tibble)
wss_df <- tibble(clusters = 1:10, wss = wss)
library(ggplot2)
scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')
scree_plot


kmeans <- kmeans(scaled_data, centers = 4, nstart = 25) 
kmeans(scaled_data, centers = 4)$cluster 
table(kmeans$cluster)