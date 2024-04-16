#Load this library
# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("gridExtra")
# install.packages("cluster")
# install.packages("knitr")



library(ggplot2)
library(tidyverse)
library("dplyr")
library(gridExtra)
library(cluster) 
library(knitr)

#Create a function for all histogram and test it out

rm(list = ls()) #remove all the variable in the session
set.seed(31538312) # XXXXXXXX = your student ID
cvbase = read.csv("PsyCoronaBaselineExtract.csv")
cvbase <- cvbase[sample(nrow(cvbase), 40000), ] # 40000 rows
attach(cvbase)

#1************************************************************************

#Description and summary of data-------------------------------------
dim(cvbase) #40,000 rows and 54 columns
str(cvbase) #data type
summary(cvbase)
#View(cvbase)


#Percentage of missing (N/A) for each column------------------------------
as.data.frame(colMeans(is.na(cvbase))* 100) #percentage missing value of the data

#1a
#Demographic Distribution----------------------------------------

#bar chart for employee status
emp_barchart <- function(data, bar_title){
  
  sum_employee <- colSums(data[, 21:30], na.rm = TRUE)
  names(sum_employee) <- c("Employed 1-24hr/week", "Employed 24-39 hr/week",
                           "Employed 40+ hr/week", "Not employed-looking for work",
                           "Not employed-not looking for work", "Homemaker",
                           "Retired", "Disabled-cannot work", "Student", "Volunteering")
  
  # Create a data frame for the bar chart
  emp_df <- data.frame(Status = names(sum_employee), Count = sum_employee)
  
  emp_df$Status <- reorder(emp_df$Status, +emp_df$Count)
  
  bar_chart <- ggplot(emp_df, aes(x = Status, y = Count)) +
    geom_bar(stat = "identity", fill = "blue") +
    ggtitle(bar_title)
  
  bar_chart + coord_flip() +
    geom_text(aes(label = paste0(signif(Count/sum(Count)*100, 3), "%")), 
              position = position_stack(vjust = 0.5), size = 4, color = "white")
  
}
emp_barchart(cvbase, "Employee Status Bar Chart")


#Distribution of gender by age and edu
demographic <- cvbase[,47:49]
#View(demographic)

genderChart <- demographic$gender
genderChart <- factor(genderChart, levels = c(1, 2, 3), labels = c("Female", "Male", "Other"))

age_label <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65-75", "75-85", "85+")
demographic$age <- factor(demographic$age, levels = 1:8, labels = age_label)

edu_label <- c("Primary", "Secondary", "Vocational", "Higher", "Bachelor", "Masters", "PHD")
demographic$edu <- factor(demographic$edu, levels = 1:7, labels = edu_label)


gender_bar <- function(data, x_value, x_label, x_title, title, fill_value){
  
  ggplot(data, aes(x = x_value, fill = fill_value)) +
    geom_bar(position = "dodge", stat = "count") +
    labs(x = x_title, y = "Count", fill = "Gender") +
    scale_x_discrete(labels = x_label) +
    ggtitle(title) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
}

#run the gender bar function above first and its variable
dev.off()
gender_bar(demographic, demographic$age, age_label, "Age", "Distribution of Gender by Age Group", genderChart)
gender_bar(demographic, demographic$edu, edu_label, "Education", "Distribution of Gender by Education Group", genderChart)

#correlation heat map
demographic$gender <- ifelse(demographic$gender == 1, "Female", "Male")

ggplot(demographic, aes(x = edu, y = age, fill = gender)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("red", "blue")) +
  labs(title = "Education vs. Age by Gender") +
  theme(plot.title = element_text(hjust = 0.5))

#Distribution of attributes-----------------------------------------------------------

#Column index for each of the concept, run this variable before produce histogram
affect_column = 1:11
likehood_column = 12:13
social_column = 14:16
job_column = 17:20
finance_column = 31:33
disempowerment_column = 34:36
injunctive_column = 40:44
trustgov_column = 45:46


condense_histogram <- function(concept, data) {
  data_long <- tidyr::pivot_longer(data, cols = everything(), names_to = "variable", values_to = "response")
  ggplot(data_long, aes(x = response, fill = variable)) + 
    geom_histogram(binwidth = 1, color='#8da0cb', alpha=0.6, position='identity') + 
    ggtitle(paste0("Histogram of ", concept)) + 
              theme(plot.title = element_text(hjust = 0.5)) +  scale_fill_manual(values = rainbow(11))
  
}

#(run each of them to get the histogram)
condense_histogram("Affect", cvbase[affect_column])
condense_histogram("Likelihood", data.frame(cvbase[likehood_column]))
condense_histogram("Societal Discontent", data.frame(cvbase[social_column]))
condense_histogram("Job Insecurity", data.frame(cvbase[job_column]))
condense_histogram("Perceived Financial Strain", data.frame(cvbase[finance_column]))
condense_histogram("Disempowerment", data.frame(cvbase[disempowerment_column]))


#live satsisfaction concept, uses different scale of response, so it must be seperate
condense_histogram("Hapiness", data.frame(cvbase[37]))
condense_histogram("Life satisfaction", data.frame(cvbase[38]))
condense_histogram("My life has a clear sense of purpose", data.frame(cvbase[39]))


condense_histogram("Corona Community Injuctive norms", data.frame(cvbase[injunctive_column]))
condense_histogram("Trust in Government", data.frame(cvbase[trustgov_column]))


#1b (NOTE THAT ALL THE CODE IN THIS SECTION MUST BE RUN BEFORE PROCEED TO sECTION 2)
#Pre processing--------------------------------------------------------------------------

#dealing with N/A value
#affect/likelihood concept column N/A value are replace with 0
#happy/lifesat column 
#c19IsStrict,punish, org, trustgovctry/state, gender, age, edu

cvbase[, c(1:13,21:30,37:38, 42:49)][is.na(cvbase[,c(1:13,21:30,37:38, 42:49)])] <- 0 #replace N/A value for employee_status columns with 0
cvbase[,]

#Job insecurities columns N/A value will be replace with 3 since the response in codebook defines it that way
cvbase[, 17:20][is.na(cvbase[,17:20])] <- 3 #replace N/A value for employee_status columns with 0

as.data.frame(colMeans(is.na(cvbase))* 100) #check the percentage of N/A value of the data

cvbase <- na.omit(cvbase) #remove all of the N/A value that are in negative scale


#2*********************************************************************************

#2a
#FOCUS COUNTRY: JAPAN
japan_data = cvbase[cvbase$coded_country == "Japan",]
japan_data <- subset(japan_data, select = -coded_country) #remove country code column
#View(japan_data)

#Other COUNTRY AS A GROUP
world_data = cvbase[cvbase$coded_country != "Japan",]
World_data <- subset(world_data, select = -coded_country) #remove country code column

#Japan----------------------------------------------------------------------------------------------
#Japan Demographic
dev.off()


emp_barchart(japan_data, "Japan Employee Status Bar chart") 


#Japan Gender and edu
japan_demographic <- japan_data[,47:49]
japan_chart <- japan_demographic$gender
japan_chart<- factor(japan_chart, levels = c(1,2,3), labels = c("Female", "Male", "Others"))
japan_demographic$age <- factor(japan_demographic$age, levels = 1:8, labels = age_label)
japan_demographic$edu <- factor(japan_demographic$edu, levels = 1:7, labels = edu_label)

#run 2 function here to get the bar chart for gender and edu
#make sure 5 variable above are executed and run each function below

gender_bar(japan_demographic, japan_demographic$age, age_label, 
           "Age", "Distribution of Gender by Age Group", japan_chart)

gender_bar(japan_demographic, japan_demographic$edu, edu_label, 
           "Education", "Distribution of Gender by Education Group", japan_chart)


#Japan distribution of attribute (run each of them to get the histogram)
condense_histogram("Japan Affect", data.frame(japan_data[affect_column]))
condense_histogram("Japan Likelihood", data.frame(japan_data[likehood_column]))
condense_histogram("Japan Societal Discontent", data.frame(japan_data[social_column]))
condense_histogram("Japan Job Insecurity", data.frame(japan_data[job_column]))
condense_histogram("Japan Perceived Financial Strain", data.frame(japan_data[finance_column]))
condense_histogram("Japan Disempowerment", data.frame(japan_data[disempowerment_column]))

#Japan live satisfaction
condense_histogram("Japan Happiness", data.frame(japan_data[37]))
condense_histogram("Japan Satisfaction", data.frame(japan_data[38]))
condense_histogram("Japan MLQ", data.frame(japan_data[39]))

condense_histogram("Japan Corona Community Injuctive norms", data.frame(japan_data[injunctive_column]))
condense_histogram("Japan Job Insecurity", data.frame(japan_data[job_column]))
condense_histogram("Japan Trust in Government", data.frame(japan_data[trustgov_column]))

#Other Countries-------------------------------------------------------------------------------------
dev.off()
#Other countries employee status
emp_barchart(world_data, "Rest of the World Employee Status Bar chart")

#World distribution of attribute (run each of them to get the histogram)
condense_histogram("Other countries Affect", data.frame(world_data[affect_column]))
condense_histogram("Other countries World Likelihood", data.frame(world_data[likehood_column]))
condense_histogram("Other countries Societal Discontent", data.frame(world_data[social_column]))
condense_histogram("Other countries Job Insecurity", data.frame(world_data[job_column]))
condense_histogram("Other countries Perceived Financial Strain", data.frame(world_data[finance_column]))
condense_histogram("Other countries Disempowerment", data.frame(world_data[disempowerment_column]))

condense_histogram("Other countries  Happiness", data.frame(world_data[37]))
condense_histogram("Other countries Satisfaction", data.frame(world_data[38]))
condense_histogram("Other countries  MLQ", data.frame(world_data[39]))

condense_histogram("Other countries Corona Community Injuctive norms", data.frame(world_data[injunctive_column]))
condense_histogram("Other countries  Trust in Government", data.frame(world_data[trustgov_column]))


#2b
#predict pro social behavior for Japan------------------------------------------------------------
dev.off()

#Predict function for japan
predictJapan_c19proSo <- function(c19ProSoNo, title){
  
  fit <- lm(c19ProSoNo ~ . , data = japan_data[1:49])
  print(summary(fit))
  
  summary_output <- summary(fit)
  
  par(mfrow=c(2,2))
  plot(fit)
  
  plot(fit$fitted.values, resid(fit), xlab = "Fitted Values", ylab = "Residuals", main = paste(title, "Residuals vs. Fitted"))
  abline(h = 0, col = "red")
  abline(lm(resid(fit)~fit$fitted.values), col="blue")
  
  # Create the normal QQ plot and add a line of best fit
  qqnorm(resid(fit), main = paste(title, "Normal Q-Q Plot"))
  qqline(resid(fit), col = "red")
  
}

#make sure the function above is run
#c19oriSo01: Willing to help others
predictJapan_c19proSo(japan_data$c19ProSo01, "Japan Pro Social 1") 

#c19oriSo02: Willing to make donations
predictJapan_c19proSo(japan_data$c19ProSo02, "Japan Pro Social 2")

#c19oriSo03: Protect the vulnerale group at my own
predictJapan_c19proSo(japan_data$c19ProSo03, "Japan Pro Social 3")

#c19oriSo04: make personal sacrifice to prevent spread
predictJapan_c19proSo(japan_data$c19ProSo04, "Japan Pro Social 4")


#predict pro social behavior for Other country as a group------------------------------
dev.off()

#function for predict other countries
predictOther_c19proSo <- function(c19ProSoNo){
  fit <- lm(c19ProSoNo ~ . , data = world_data[1:49])
  print(summary(fit))
  
  summary_output <- summary(fit)
  
  par(mfrow=c(2,2))
  
  plot(fit$fitted.values, resid(fit), xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted")
  abline(h = 0, col = "red")
  abline(lm(resid(fit)~fit$fitted.values), col="blue")
  
  # Create the normal QQ plot and add a line of best fit
  qqnorm(resid(fit), main = "Normal Q-Q Plot")
  qqline(resid(fit), col = "red")
  
  # Get the p-values from the summary for predictor variables
  p_values <- summary_output$coefficients[-1, 4]
  
  # Sort the p-values from lowest to highest
  p_values_sorted <- p_values[order(p_values)]
  
  # Create a data frame with predictor variable names, coefficients, and p-values
  var_names <- rownames(summary_output$coefficients[-1,])
  coefficients <- summary_output$coefficients[-1,2]
  df <- data.frame(var_names, coefficients, p_values, row.names=NULL)
  
  # Sort the data frame by p-values and add the intercept row at the top
  intercept_row <- data.frame(var_names = "(Intercept)", 
                              coefficients = summary_output$coefficients[1, 1], 
                              p_values = summary_output$coefficients[1, 4])
  df <- rbind(intercept_row, df[order(df$p_values), ])
  
  # Print the sorted data frame
  print(df)
}

#c19oriSo01: Willing to help others
predictOther_c19proSo(world_data$c19ProSo01) 

#c19oriSo02: Willing to make donations
predictOther_c19proSo(world_data$c19ProSo02)


#c19oriSo03: Protect the vulnerale group at my own
predictOther_c19proSo(world_data$c19ProSo03)

#c19oriSo04: make personal sacrifice to prevent spread
predictOther_c19proSo(world_data$c19ProSo04)



#3******************************************************************************************************

#Reading external data for clustering
indicator <- read.csv("indicator.csv")
#View(indicator)

dev.off()
#clustering function 
clustering <- function(data, column, country_column, title){
  
  set.seed(31538312)
  
  scaled_data <- scale(column)
  dist_matrix <- dist(scaled_data)
  
  hc <- hclust(dist_matrix, method = "ave")
  
  i_silhouette_score <- function(k){ 
    km <- kmeans(scaled_data, k, nstart = 50) 
    ss <- silhouette(km$cluster, dist_matrix)
    mean(ss[, 3]) 
  }
  
  k <- 7:20
  avg_sil <- sapply(k, i_silhouette_score)
  max_index <- which(avg_sil == max(avg_sil[order(avg_sil)]))
  max_clusters <- k[max_index]
  
  #plot sihouette score graph vs number of clusters
  plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores',
       main = paste(title, " Sihouette score vs number of clusters")) 
  
  #outputing the table of cluster based on the k value ideal score
  clusters <- cutree(hc, k = max_clusters)
  result <- table(Country = country_column, Cluster = clusters)
  result_df <- as.data.frame.matrix(result)
  kable(result_df)
  
}

clustering(indicator, indicator[,2:6], indicator$LOCATION, "Indicators")


#3b-c Preidct cluster of countries predictors----------------------------------------------------
#Australia, France, Israel, Italy and Korea are countries that share the same cluster as japan

cluster_countries <- c("Australia", "France", "Israel", "Italy", "South Korea")
cluster_data = cvbase[cvbase$coded_country %in% cluster_countries,]
cluster_data <- subset(cluster_data, select = -coded_country) #remove country code for japan data
#View(cluster_data)

dev.off()
#function that predict cluster countries
predictCluster_c19proSo <- function(c19ProSoNo){
  
  fit <- lm(c19ProSoNo ~ . , data = cluster_data[1:49])
  print(summary(fit))
  
  summary_output <- summary(fit)
  
  par(mfrow=c(2,2))
  plot(fit)
  
  plot(fit$fitted.values, resid(fit), xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted")
  abline(h = 0, col = "red")
  abline(lm(resid(fit)~fit$fitted.values), col="blue")
  
  # Create the normal QQ plot and add a line of best fit
  qqnorm(resid(fit), main = "Normal Q-Q Plot")
  qqline(resid(fit), col = "red")
  
}  

predictCluster_c19proSo(cluster_data$c19ProSo01)
predictCluster_c19proSo(cluster_data$c19ProSo02)
predictCluster_c19proSo(cluster_data$c19ProSo03)
predictCluster_c19proSo(cluster_data$c19ProSo04)


#plot r squared result for all groups------------------------------------------
x <- c(1:4)

#R squared result from each group and each predictor
y1 <- c(0.1445, 0.2344, 0.1696, 0.1746) #Japan 
y2 <- c(0.09419, 0.1335, 0.09941, 0.1414) #other
y3 <- c(0.1127, 0.1636, 0.1186, 0.2153) #cluster

df <- data.frame(x, y1, y2, y3)

ggplot(df, aes(x = x)) +
  geom_point(aes(y = y1, color = "Japan"), shape = 16, size = 7) +
  geom_point(aes(y = y2, color = "Other"), shape = 17, size = 7) +
  geom_point(aes(y = y3, color = "Cluster"), shape = 18, size = 7) +
  geom_line(aes(y = y1, color = "Japan"), size = 2) +
  geom_line(aes(y = y2, color = "Other"), size = 2) +
  geom_line(aes(y = y3, color = "Cluster"), size = 2) +
  scale_color_manual(name = "Legend", 
                     values = c("Japan" = "red", "Other" = "blue", "Cluster" = "orange")) +
  labs(title = "R Squared Value by country group",
       x = "C19ProSoNo",
       y = "R Squared Value") +
  theme(plot.title = element_text(hjust = 0.5))













