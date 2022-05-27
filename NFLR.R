install.packages("Rtools")
install.packages("ggplot2")
library(ggplot2)
data <- read.csv('NFL.csv', header = TRUE)

#Variation USED BELOW 2:
ggplot(data = data) + geom_freqpoly(mapping = aes(x = AdjSalary), color = "blue", binwidth = 1, size = 2) +
  xlim(0, 40) + labs(title = "Salary Cap usage count by Quarterbacks adjusted for inflation", x = "Adjusted Salary Cap usage", y = "Count")

ggplot(data = data) + geom_density(mapping = aes(x = AdjSalary), color = "blue", size = 2, kernel = "gaussian") +
  xlim(0, 40) + labs(title = "Salary Cap usage count by Quarterbacks adjusted for inflation - DENSITY", x = "Adjusted Salary Cap usage", y = "Density")

ggplot(data = data) + geom_histogram(mapping = aes(x = QBR), color = "red", binwidth = 1, size = 1, fill = "black") +
  xlim(10, 90) + labs(title = "Quarterback Rating (QBR) count by NFL Quarterbacks from 2009-2018", x = "Quarterback Rating (QBR)", y = "Count")

ggplot(data = data) + geom_histogram(mapping = aes(x = Rate), color = "blue", binwidth = 1, size = 1, fill = "black") +
  xlim(48, 125) + labs(title = "Passer Rating count by NFL Quarterbacks from 2009-2018", x = "Passer Rating", y = "Count")

ggplot(data = data) + geom_bar(mapping = aes(x = Twins),  color = "green", binwidth = 1, size = 1, fill = "black") +
  labs(title = "Total Wins count by NFL Teams year-by-year from 2009-2018", x = "Team Wins", y = "Count")

ggplot(data = data) + geom_bar(mapping = aes(x = Tlosses),  color = "orange", binwidth = 1, size = 1, fill = "black") +
  labs(title = "Total Losses count by NFL Teams year-by-year from 2009-2018", x = "Team Losses", y = "Count")

#####
#Covariation
df = data 

df$contract <- as.factor(ifelse(df$AdjSalary < 4, 'Rookie',
                            ifelse(df$AdjSalary < 11, 'Standard', 
                                   ifelse(df$AdjSalary < 23, 'Star', 
                                          ifelse(df$AdjSalary < 40, 'Superstar')))))

#Rough Work below didn't use it
ggplot(data = df) + geom_histogram(mapping = aes(x = TWinPer, color = contract), binwidth = 1, size = 1, fill = "black") +
  labs(title = "Quarterback Rating (QBR) count by NFL Quarterbacks", x = "Quarterback Rating (QBR)", y = "Count")

#USED BELOW 3 for Covariation on Winning Metrics:
ggplot(data = df) + geom_jitter(mapping = aes(x = AdjSalary, y = TWinPer, color = contract), binwidth = 1, size = 3, fill = "black") +
  labs(title = "Adjusted Quarterback Salaries with Team Winning Percentage", x = "Quarterback Salary Adjusted for Salary Cap Inflation", y = "Team Winning Percentage %")

ggplot(data = df) + geom_histogram(mapping = aes(x = AdjSalary, fill = Playoffs), color = "black", binwidth = 0.5) +
  labs(title = "Adjusted Quarterback Salaries with Team Playoff Participation", x = "Adjusted Salary Cap usage", y = "Count")

ggplot(data = df) + geom_histogram(mapping = aes(x = AdjSalary, fill = SuperBowl), color = "black", binwidth = 0.5) +
  labs(title = "Adjusted Quarterback Salaries with Team SuperBowl Wins", x = "Adjusted Salary Cap usage", y = "Count")

#USED BELOW for Quarterback Dependent Metrics:
ggplot(data = df) + geom_bin2d(mapping = aes(x = AdjSalary, y = QBR, color = contract), size = 1, binwidth = 2) +
  labs(title = "Quarterback Rating (QBR) count with Adjusted Quarterback Salaries", x = "Quarterback Salary Adjusted for Salary Cap Inflation", y = "QBR")

ggplot(data = df) + geom_bin2d(mapping = aes(x = AdjSalary, y = Rate, color = contract), size = 1, binwidth = 2) +
  labs(title = "Passer Rating count with Adjusted Quarterback Salaries", x = "Quarterback Salary Adjusted for Salary Cap Inflation", y = "Passer Rating")

ggplot(data = df) + geom_smooth(mapping = aes(x = Twins, y = QBR), size = 2, fill = "red") +
  labs(title = "Quarterback Rating (QBR) smooth plot with Team Wins", x = "Team Wins", y = "QBR")

ggplot(data = df) + geom_smooth(mapping = aes(x = Twins, y = Rate), size = 2, fill = "black") +
  labs(title = "Passer Rating smooth plot with Team Wins", x = "Team Wins", y = "Passer Rating")

#Install needed for correlation coefficients
install.packages("ggpubr")

#Calculating the Correlation Coefficients for the above smoothing graphs.
res <- cor.test(df$Twins, df$Rate, method = "pearson")
res

res <- cor.test(df$Twins, df$QBR, method = "pearson")
res

#Used for analysing team total points below
ggplot(data = df) + geom_violin(mapping = aes(x = contract, y = TeamPoints, color = contract), size = 2, fill = "black") +
  labs(title = "Total Team Points via alternative Quarterback contracts", x = "Quarterback Contract", y = "Total Team Points")


ggplot(data = df) + geom_histogram(mapping = aes(x = TeamPoints, fill = Playoffs), color = "black", binwidth = 5) +
  labs(title = "Total Team Points with Team Playoff Participation", x = "Total Team Points", y = "Count")

