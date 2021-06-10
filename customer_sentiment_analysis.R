#The main objective of this file is to look into the insights of customer segmentation
#also known as Market Basket Analysis. The data is taken from a particular mall which
#recorded the different features of their customers
#Link to original dataset: https://www.kaggle.com/vjchoudhary7/customer-segmentation-tutorial-in-python

#The dataset was downloaded from the the Mall Customer Segmentation Dataset from
#Kaggle website

#Import dataset into the current directory
customers <- read.csv('...Mall Customer Segmentation/Mall_Customers2.csv') #add dataset path here

#First look at the dataset
head(customers)
tail(customers)
dim(customers)

#CustomerID = is the unique identification number of each customer
#Gender = each customer (Male and Female)
#Age = of each customer
#Annual Income = in USD
#Spending Score = is the score assigned by shopping center according to the customer's
#ranges from 1-100
#purchasing behavior

#Fix column names
colnames(customers)
names(customers)[names(customers) == 'CustomerID'] <- 'Customer_Id'
names(customers)[names(customers) == 'Annual.Income'] <- 'Annual_Income'
names(customers)[names(customers) == 'Spending.Score..1.100.'] <- 'Spending_Score'

#Loading automatic EDA library
library(SmartEDA)

#Brief summary of the dataset
ExpData(customers, type=1)

#For a more clear summary
ExpData(customers, type=2)

#Summary statistics of all numerical variables
numerical_summary <- ExpNumStat(customers)
numerical_summary #Too many unnecessary columns of information
numerical_summary[, 11:20] #to use only specific columns
#Thi function automatically identifies the continuous features

#This function can be used further to summarize grouped data
gp_income_summary <- ExpNumStat(customers[,-1], by='G', gp='Gender')
#customers[,-1] is to remove Customer ID column
gp_income_summary
#nPos = number of positive values and nNeg is vise versa, nZero is number of zeros
#NegInf = Negative infinite counts and PosInf is vice versa
#TN is the sum of total samples (including NA values)
#SD = Standard Deviation, CV = Coefficient of Variations
#IQR = Inter quartile range

#To display fewer and important information columns
cbind(gp_income_summary[, 1:2], gp_income_summary[, 12:20])
#there might be two median values in the dataset but the function only
#display's the first median it gets

#From the above result it is evident that mean annual income of women = 58250$
#which is lower than the mean annual income of men = 62227$
#Another important information is that mean spending score of women = 51.527
#is lower than mean spending score of men = 48.511

#Statistical summary of all categorical features with respect to target variable
ExpCatStat(customers, Target = 'Spending_Score', plot = TRUE)

#If information value is <0.03 then predictive power = 'Not Predictive'
#If information value is between 0.03 to 0.1 then predictive power = 'Somewhat Predictive'
#If information value is between 0.1 to 0.3 then predictive power = 'Medium Predictive'
#If information value is >0.3 then predictive power = 'Highly Predictive'

#This single line of code has power to generate Chi-Squared test and P-Value between 
#each feature and target feature. It also provides the predictive power which can 
#easily identify feature value with respect to the target feature

#To plot numerical graphs, passing argument [4:5] tells the function to plot
#graph between annual income and age then between spending score and otherwise it would
#also plot age in the same graph which is not recommended for 2D graphs
ExpNumViz(customers, scatter = TRUE)[4:5]

#Box plot for comparison of Annual Income and Spending Score with Gender
ExpNumViz(data = customers, target = 'Gender')[3:4]

#Studying the box plot between Annual Income and Gender, it is seen that maximum
#annual income of both genders are approximately the same. However, the minimum 
#annual income of the men is higher than that of women

#From the box plot of Spending score and Gender, it is evident that there is a higher
#range of spending score of men as compared to the women. The minimum spending score
#of women is approximately 10 points higher than that of male

#For visualizing categorical features
ExpCatViz(customers)

#Since there is only one categorical feature (Gender), only one plot is generated
#From the Gender bar plot above, it is evident that Percentage of women is 12% high

#To figure out the outliers in the dataset
ExpOutliers(customers, varlist = c('Age', 'Annual_Income', 'Spending_Score'),
            method = 'BoxPlot', capping = c(0.1, 0.9), outflag = TRUE)

#Boxplot in the code above will not plot the box plot graph instead it is only
#used as a method to detect outlier

#To plot univariate Quantile Quantile normalized plots 
ExpOutQQ(customers)

#To create tables of categorical features to generate frequency or cross tables
ExpCTable(customers)


#Loading automatic EDA library
library(explore)

#To describe the overall dataset
describe_tbl(customers)
#The first line shows that there are 200 records/observations and 5 features/columns
#Number of missing features are 0

#There are many different ways to describe data in explore package
#This will describe categorical and continuous both variables
describe_all(customers)

#To describe specific numerical variable
describe_num(customers, 'Annual_Income')
describe_num(customers, 'Age')
describe_num(customers, 'Spending_Score')
#From the q25|q75, it can be proved that most of the annual incomes are between
#41500 and 78000

#Similarly, to describe categorical variable
describe_cat(customers, 'Gender')

#Converting Gender into binary values for use of some functions
#Female = 0 and Male = 1
customers$Gender <- ifelse(customers$Gender == 'Female', 0, 1)
head(customers)

#To explain a binary target using a logistic regression
#To generate tibble output
explain_logreg(customers, target = Gender, out = 'tibble')
#To generate model as an output
explain_logreg(customers, target = Gender, out = 'model')

#Explain a target variable using a decision tree
explain_tree(customers, Gender, out = 'model')
#The decision tree gives a structured way to reach to the conclusion

#The most powerful feature of the explore package is the line below
explore(customers)
#This will open a interactive interface called Shiny. In this interface, one can
#easily select the target and variable for comparison using drop down menu.
#Report all button generates the html page showing summary, distribution scatter plot
#among other useful information. This functionality is useful when there is a need to
#explore dataset by changing the targets and variables and generating their results

#To explore all other features with respect to the specific feature
explore_all(customers, target = Gender)
explore_all(customers, target = Age)
#Ignoring the first graph in both cases it only shows the customer id.

#explore_bar is used to plot bar plot between target (discrete) and var (continuous feature)
explore_bar(data = customers, var = Annual_Income, target = Age, max_cat = 100)
explore_bar(data = customers, var = Annual_Income, target = Gender, max_cat = 100)
explore_bar(data = customers, var = Spending_Score, target = Age, max_cat = 100)
explore_bar(data = customers, var = Spending_Score, target = Gender, max_cat = 100)

#In the first bar plot it is seen that customers with the age of 30 and 31 have the
#highest annual income. Highest number of people with same annual income are aged 49 and 30

#In the second bar plot it is seen that highest annual income is of a women

#In the third bar plot highest spending score is of people aged 35 (approx. 95)
#Highest frequency of score is of people aged 30 (Score approx. 70).

#The fourth and the last bar plot shows that first two highest spending score is for 
#females and the third highest score is held men. Whereas lowest spending score is 
#also held by men. Highest frequency of score is of females (score: approx.40)

#From the above conclusions we can check the correlations
explore_cor(customers, Annual_Income, Age, target = Gender)
#The above correlation scatter plot shows that highest three incomes are of women
#as seen in the previous explore_bar function. Lowest four scores are also held by women

explore_cor(customers, Spending_Score, Age, target = Gender)
#It is clearly evident from the above scatter plot that score >61 is only received by
#people aged <40. Therefore, it be concluded that younger people spend the most
