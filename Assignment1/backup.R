#ASSIGNMENT 1 

#qUESTION 1 HISTOGRam

#Affect
boxplot(cvbase[1:11])

affect_range = 1:11
my_histogram(12,4, affect_range)


n_rows <- ceiling(12 / 4)
par(mfrow = c(n_rows, 4))

for(i in 1:11){
  name = names(cvbase)[i]
  hist(cvbase[,i], main = paste0("Histogram of ",name), xlab = "Response")
}

#Likehood
boxplot(cvbase[12:13])

n_rows <- ceiling(2 / 1)
par(mfrow = c(n_rows, 1))

for(i in 12:13){
  name = names(cvbase)[i]
  hist(cvbase[,i], main = paste0("Histogram of ",name), xlab = "Response")
}

#social discontent
boxplot(cvbase[14:16])

n_rows <- ceiling(3 / 2)
par(mfrow = c(n_rows, 2))

for(i in 14:16){
  name = names(cvbase)[i]
  hist(cvbase[,i], main = paste0("Histogram of ",name), xlab = "Response")
}

#Job Insecurity
boxplot(cvbase[17:20])

n_rows <- ceiling(4 / 2)
par(mfrow = c(n_rows, 2))

for(i in 17:20){
  name = names(cvbase)[i]
  hist(cvbase[,i], main = paste0("Histogram of ",name), xlab = "Response")
}

#employment status #double check this
boxplot(cvbase[21:30], las = 2)
#might required its own table to store the sum of employee status for each category and hence we can do distribition


#perceived financial strain
boxplot(cvbase[31:33])

n_rows <- ceiling(3 / 2)
par(mfrow = c(n_rows, 2))

for(i in 31:33){
  name = names(cvbase)[i]
  hist(cvbase[,i], main = paste0("Histogram of ",name), xlab = "Response")
}

#disempowerment
boxplot(cvbase[34:36])

n_rows <- ceiling(3 / 2)
par(mfrow = c(n_rows, 2))

for(i in 34:36){
  name = names(cvbase)[i]
  hist(cvbase[,i], main = paste0("Histogram of ",name), xlab = "Response")
}


#live satisfaction
boxplot(cvbase[37:39])

n_rows <- ceiling(3 / 2)
par(mfrow = c(n_rows, 2))

for(i in 34:36){
  name = names(cvbase)[i]
  hist(cvbase[,i], main = paste0("Histogram of ",name), xlab = "Response")
}

#Corona community injunctive norm
boxplot(cvbase[40:44])

n_rows <- ceiling(5 / 2)
par(mfrow = c(n_rows, 2))

for(i in 40:44){
  name = names(cvbase)[i]
  hist(cvbase[,i], main = paste0("Histogram of ",name), xlab = "Response")
}


#trust in government
boxplot(cvbase[45:46])

n_rows <- ceiling(2 / 1)
par(mfrow = c(n_rows, 1))

for(i in 45:46){
  name = names(cvbase)[i]
  hist(cvbase[,i], main = paste0("Histogram of ",name), xlab = "Response")
}


#gender age, education #fix x axis of this histogram
boxplot(cvbase[47:50])

n_rows <- ceiling(4 / 2)
par(mfrow = c(n_rows, 2))

for(i in 47:50){
  name = names(cvbase)[i]
  hist(cvbase[,i], main = paste0("Histogram of ",name), xlab = "Response")
}


#corona pro social behavior
boxplot(cvbase[51:54])

n_rows <- ceiling(4 / 2)
par(mfrow = c(n_rows, 2))

for(i in 51:54){
  name = names(cvbase)[i]
  hist(cvbase[,i], main = paste0("Histogram of ",name), xlab = "Response")
}


data_long <- tidyr::pivot_longer(data, cols = everything(), names_to = "variable", values_to = "response")
ggplot(data_long, aes(x = response, fill = variable)) + 
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity') + 
  ggtitle("Histogram of Affect") + 
  theme(plot.title = element_text(hjust = 0.5)) #center the title

#predict 

japan_fit1 <- lm(japan_data$c19ProSo01 ~ . , data = japan_data[, 1:20]) #column 1-13
japan_fit1
summary(japan_fit1)


#predict individual concept
#c19oriSo01: Willing to help others------

predict_c19proSo <- function(data , column_name ){
  fit <- lm(column_name ~ . , data = data)
  summary(fit)
  
}

predict_c19proSo(japan_data[, affect_column], japan_data$c19ProSo01)
predict_c19proSo(japan_data[, likehood_column], japan_data$c19ProSo01)
predict_c19proSo(japan_data[, social_column], japan_data$c19ProSo01)
predict_c19proSo(japan_data[, job_column], japan_data$c19ProSo01)
predict_c19proSo(japan_data[, finance_column], japan_data$c19ProSo01)
predict_c19proSo(japan_data[, disempowerment_column], japan_data$c19ProSo01)
predict_c19proSo(japan_data[, disempowerment_column], japan_data$c19ProSo01) #live satisfaction needs to be fix
predict_c19proSo(japan_data[, injunctive_column], japan_data$c19ProSo01) 
predict_c19proSo(japan_data[, trustgov_column], japan_data$c19ProSo01) 

#other consideration

japan_genderfit1 <- lm(japan_data$c19ProSo01 ~ japan_data$gender)
japan_agefit1 <- lm(japan_data$c19ProSo01 ~ japan_data$age)
japan_edufit1 <- lm(japan_data$c19ProSo01 ~ japan_data$edu)

japan_agefit1
summary(japan_agefit1)

japan_genderfit1
summary(japan_genderfit1)

japan_edufit1
summary(japan_edufit1)

predict_c19proSo(japan_data[, affect_column], japan_data$c19ProSo02)
predict_c19proSo(japan_data[, likehood_column], japan_data$c19ProSo02)
predict_c19proSo(japan_data[, social_column], japan_data$c19ProSo02)
predict_c19proSo(japan_data[, job_column], japan_data$c19ProSo02)
predict_c19proSo(japan_data[, finance_column], japan_data$c19ProSo02)
predict_c19proSo(japan_data[, disempowerment_column], japan_data$c19ProSo02)
predict_c19proSo(japan_data[, disempowerment_column], japan_data$c19ProSo02) #live satisfaction needs to be fix
predict_c19proSo(japan_data[, injunctive_column], japan_data$c19ProSo02) 
predict_c19proSo(japan_data[, trustgov_column], japan_data$c19ProSo02) 


japan_genderfit2 <- lm(japan_data$c19ProSo02 ~ japan_data$gender)
japan_agefit2 <- lm(japan_data$c19ProSo02 ~ japan_data$age)
japan_edufit2 <- lm(japan_data$c19ProSo02 ~ japan_data$edu)

japan_agefit2
summary(japan_agefit2)

japan_genderfit1
summary(japan_genderfit2)

japan_edufit2
summary(japan_edufit2)


library(ggplot2)
install.packages(gridExtra)
library(gridExtra)

# Create a data frame with age and gender columns
data <- data.frame(age = c(25, 32, 40, 27, 36, 44, 29, 31),
                   gender = c("male", "female", "male", "female", "male", "male", "female", "male"))

# Create a bar chart showing the distribution of age
age_plot <- ggplot(data, aes(x = age)) + 
  geom_bar(stat = "count") + 
  labs(x = "Age", y = "Count") + 
  theme_classic()

# Create a bar chart showing the distribution of gender
gender_plot <- ggplot(data, aes(x = gender, fill = gender)) + 
  geom_bar(stat = "count") + 
  labs(x = "Gender", y = "Count", fill = "Gender") + 
  theme_classic()

# Plot the two bar charts side-by-side
grid.arrange(age_plot, gender_plot, ncol = 2)


# Create a data frame with age and gender columns
data <- data.frame(age = c(25, 32, 40, 27, 36, 44, 29, 31),
                   gender = c("male", "female", "male", "female", "male", "male", "female", "male"))

# Create the bar chart
ggplot(data, aes(x = age, fill = gender)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "Age", y = "Count", fill = "Gender") +
  theme_classic()




# Create a data frame with age, gender, and education columns
data <- data.frame(age = c(25, 32, 40, 27, 36, 44, 29, 31),
                   gender = c("male", "female", "male", "female", "male", "male", "female", "male"),
                   education = c("high school", "bachelor's degree", "bachelor's degree", "master's degree", "high school", "master's degree", "high school", "bachelor's degree"))

# Create a bar chart showing the distribution of age and gender, age and education, and education and gender
ggplot(data, aes(x = gender, fill = age)) + 
  geom_bar(position = "dodge", stat = "count") + 
  facet_wrap(~education) +
  labs(x = "Gender", y = "Count", fill = "Age") + 
  theme_classic()

ggplot(data, aes(x = education, fill = age)) + 
  geom_bar(position = "dodge", stat = "count") + 
  facet_wrap(~age) +
  labs(x = "Education", y = "Count", fill = "Age") + 
  theme_classic()

ggplot(data, aes(x = gender, fill = education)) + 
  geom_bar(position = "dodge", stat = "count") + 
  facet_wrap(~age) +
  labs(x = "Gender", y = "Count", fill = "Education") + 
  theme_classic()



#plot residual test

# Load the dataset
data(mtcars)

dev.off()


# Fit a linear regression model
model <- lm(mpg ~ . , data = mtcars)
par(mfrow=c(2,2))
plot(model)

# Get the predicted (fitted) values
fitted_values <- fitted(model)
View(fitted_values)

# Plot the predicted values against the predictor variable
plot(mtcars$wt, fitted_values, xlab = "Weight", ylab = "Predicted MPG")
abline(lm(mpg ~ ., data = mtcars), col = "red")


#japan plot original
Japan_plot <- function(){
  
  par(mfrow=c(2,2))
  
  # First plot
  p1 <- ggplot(japan_data, aes(x = japan_data$affEnerg, y = japan$data_c19ProSo01)) +
    geom_point() +
    geom_smooth(method = "lm", color = "blue", size = 1, se = FALSE)
  
  # Second plot
  p2 <- ggplot(japan_data, aes(x = employstatus_9, y = c19ProSo02)) +
    geom_point() +
    geom_smooth(method = "lm", color = "blue", size = 1, se = FALSE)
  
  p3 <- ggplot(japan_data, aes(x = employstatus_9, y = c19ProSo03)) +
    geom_point() +
    geom_smooth(method = "lm", color = "blue", size = 1, se = FALSE)
  
  p4 <- ggplot(japan_data, aes(x = affEnerg, y = c19ProSo04)) +
    geom_point() +
    geom_smooth(method = "lm", color = "blue", size = 1, se = FALSE)
  
  grid.arrange(p1, p2, p3, p4, ncol = 2)
  
  par(mfrow=c(1,1))
  
}


#clustering backup
dist_matrix <- dist(unemp)

hc <- hclust(dist_matrix, method = "ave")
plot(hc, hang = -1)

# Determine the optimal number of clusters using the elbow method
i_silhouette_score <- function(k){ 
  km <- kmeans(unemp, k, nstart = 50) 
  ss <- silhouette(km$cluster, dist_matrix)
  mean(ss[, 3]) 
} 

k <- 10:20
avg_sil <- sapply(k, i_silhouette_score)
max_index <- which(avg_sil == max(avg_sil[order(avg_sil)]))
max_clusters <- k[max_index]
max_clusters

plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores') 

clusters <- cutree(hc, k = max_clusters)
table(Country = unemployment$LOCATION, Cluster = clusters)
#write.csv(output, file = "cluster_table.csv", row.names = FALSE)







japan_histogram <- function(nrow_numerator, nrow_denominator, range) {
  
  n_rows <- ceiling(nrow_numerator / nrow_denominator)
  par(mfrow = c(n_rows, nrow_denominator))
  
  for(i in range){
    name = names(cvbase)[i]
    hist(japan_data[,i], main = paste0("Histogram of ",name), xlab = "Response")
  }
  
}


#japan_histogram(12,4, affect_column)
japan_histogram(2,1, likehood_column)
japan_histogram(3,2, social_column)
japan_histogram(4,2, job_column)
japan_histogram(3,2, finance_column)
japan_histogram(3,2, disempowerment_column)
japan_histogram(3,2, live_column)
japan_histogram(5,2, injunctive_column)
japan_histogram(2,1, trustgov_column)
#japan_histogram(12,4, injunctive_column) #gender, age FIX THIS
japan_histogram(4,2, covidbehavior_column)


#--------------------------------------------------------------

#Looking at residuals on the most significant predictor -------------------------------------------------

#JAPAN
predictors <- list(japan_data$affEnerg, japan_data$employstatus_9, japan_data$employstatus_9, japan_data$c19NormShould)
predictor_names <- c("affEnerg", "employstatus_9", "employstatus_9", "c19NormShould")
responses <- list(japan_data$c19ProSo01, japan_data$c19ProSo02, japan_data$c19ProSo03, japan_data$c19ProSo04)

par(mfrow=c(4,2))
for (i in seq_along(predictors)) {
  model <- lm(responses[[i]] ~ predictors[[i]], data = japan_data)
  print(summary(model))
  
  plot(predictors[[i]], model$residuals, 
       main = paste0("Japan's c19ProSo0", i, "Residuals vs ", predictor_names[i]), 
       xlab = predictor_names[i], 
       ylab = "Residuals")
  qqnorm(resid(model), main = paste("Japan's", predictor_names[i], "qq plot", sep = " "))
  qqline(resid(model), col = "red")
}
ggsave("japan_residuels.png")

#WORLD

responses <- list(world_data$c19ProSo01, world_data$c19ProSo02, world_data$c19ProSo03, world_data$c19ProSo04)

par(mfrow=c(4,2))
for (i in seq_along(responses)) {
  model <- lm(responses[[i]] ~ world_data$c19NormShould, data = world_data)
  print(summary(model))
  plot(world_data$c19NormShould, model$residuals, 
       main = paste0("Other Countries c19ProSo0", i , " Residuals vs c19NormShould"), 
       xlab = "C19NormShould", 
       ylab = "Residuals")
  qqnorm(resid(model), main = "Other Countries c19NormShould qq plot")
  qqline(resid(model), col = "red")
  
}

ggsave("japan_residuels.png") #uncomment this for submission

dev.off()





par(mfrow=c(1,2))
plot(japan_model)
summary(japan_model)


Japan_plot <- function(){
  
  # create a list of plot titles and variables to plot
  plot_list <- list(
    list(title = "c19ProSo01", x_var = "affEnerg", y_var = "c19ProSo01"),
    list(title = "c19ProSo02", x_var = "employstatus_9", y_var = "c19ProSo02"),
    list(title = "c19ProSo03", x_var = "employstatus_9", y_var = "c19ProSo03"),
    list(title = "c19ProSo04", x_var = "affEnerg", y_var = "c19ProSo04")
  )
  
  # create a list of ggplots using lapply
  plot_output <- lapply(plot_list, function(plot_info) {
    ggplot(japan_data, aes_string(x = plot_info$x_var, y = plot_info$y_var)) +
      geom_point() +
      geom_smooth(method = "lm", color = "blue", size = 1, se = FALSE) +
      labs(title = plot_info$title)
  })
  
  # combine the plots using grid.arrange
  grid.arrange(grobs = plot_output, ncol = 2)
  
}
Japan_plot()

qplot(japan_data$affEnerg, japan_data$c19ProSo01, data = japan_data) +
  geom_smooth(method = "lm", color = "blue", size = 1, se = FALSE)

japan_fitted <- fitted(japan_model)
plot(japan_data$c19ProSo01, japan_fitted , xlab = "Weight", ylab = "Predicted MPG") 



#Basic Histogram function

my_histogram <- function(nrow_numerator, nrow_denominator, range) {
  
  n_rows <- ceiling(nrow_numerator / nrow_denominator)
  par(mfrow = c(n_rows, nrow_denominator))
  
  for(i in range){
    name = names(cvbase)[i]
    hist(cvbase[,i], main = paste0("Histogram of ",name), xlab = "Response")
  }
  
}

#male and female data age distribution

par(mfrow = c(1, 2))

# Plot the histogram for males
male_data <- subset(cvbase, gender == 2)
hist(male_data$age, main = "Histogram for Male Ages", xlab = "Age", col = "blue")

# Plot the histogram for females
male_female <- subset(cvbase, gender == 1)
hist(male_female$age, main = "Histogram for Female Ages", xlab = "Age", col = "pink")

# Reset the plotting area to its default settings
par(mfrow = c(1, 1))






#Affect
boxplot(cvbase[1:11])
my_histogram(12,4, affect_column)

#Likehood
boxplot(cvbase[12:13])
my_histogram(2,1, likehood_column )

#social discontent
boxplot(cvbase[14:16])
my_histogram(3,2, social_column )

#Job Insecurity
boxplot(cvbase[17:20])
my_histogram(4,2, job_column )

#employment status #double check this
boxplot(cvbase[21:30], las = 2)
#might required its own table to store the sum of employee status for each category and hence we can do distribition


#perceived financial strain
boxplot(cvbase[31:33])
my_histogram(3,2, finance_column )

#disempowerment
boxplot(cvbase[34:36])
my_histogram(3,2, disempowerment_column)

#live satisfaction
boxplot(cvbase[37:39])
my_histogram(3,2, live_column)

#Corona community injunctive norm
boxplot(cvbase[40:44])
my_histogram(5,2, injunctive_column )


#trust in government
boxplot(cvbase[45:46])
my_histogram(2,1, trustgov_column )


#gender age, education #fix x axis of this histogram
boxplot(cvbase[47:50])

n_rows <- ceiling(4 / 2)
par(mfrow = c(n_rows, 2))

for(i in 47:50){
  name = names(cvbase)[i]
  hist(cvbase[,i], main = paste0("Histogram of ",name), xlab = "Response")
}


#corona pro social behavior
boxplot(cvbase[51:54])
my_histogram(4,2, covidbehavior_column )


