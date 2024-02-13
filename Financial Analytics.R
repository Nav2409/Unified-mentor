# The following report provides an analysis of market competition among various companies of India through
# their financial data

# DATASET OVERVIEW
#The given data set had information on the market capitalization of the top 500 companies in India with columns
#Serial Number,Name-Name of Company,Mar Cap – Crore-Market Capitalization in Crores
#Sales Qtr – Crore-Quarterly Sale in crores.
# Key metrics like annual revenue, Price-sales ratio, return on equity, etc. have been derived from the given
# variables and taken into consideration to analyse the financial condition of the companies.
# They have been further ranked according to these metrics to provide a clear picture of the competitive position 
# of the companies

#Loading the dataset
library(readxl)
getwd()
# Load the data from the Excel file
excel_file <-"C:/Users/TANNU/Documents/Financial Analytics data.xlsx"
data <- read_excel(excel_file)
data

# Remove rows with all NAs
data <- na.omit(data)
data
is.data.frame(data)
data$`Sales Qtr - Crore` <- as.numeric(data$`Sales Qtr - Crore`)
summary(data)
nrow(data)

range(data$`Mar Cap - Crore`)
sd(data$`Mar Cap - Crore`)
#A larger standard deviation indicates a wider range of market capitalization values among the companies. 
#This implies that the dataset comprises companies with diverse sizes in terms of market capitalization,
#ranging from small-cap to large-cap companies.Higher variability in market capitalization may imply
#higher risk exposure for investors, but also potential for higher returns

library(dplyr)
# Arrange data by market capitalization in descending order
data1 <- arrange(data, desc(`Mar Cap - Crore`))
data1
head(data1)
tail(data1)
# top company on the basis of market capitalization
data1$Name[1]
# bottom company on the basis of market capitalization
data1$Name[459]

#large-cap companies:companies with their market capitalisation over Rs. 20,000 crores
large_cap <- data %>%
  filter(`Mar Cap - Crore` > 20000) %>%
  arrange(desc(`Mar Cap - Crore`)) %>%
  select(Name,`Mar Cap - Crore`)
large_cap
#mid-cap companies:companies with moderate market capitalisation between Rs.5000 crores & Rs.20,000 crores
mid_cap <- data %>%
  filter(`Mar Cap - Crore` < 20000,`Mar Cap - Crore` > 5000) %>%
  arrange(desc(`Mar Cap - Crore`)) %>%
  select(Name,`Mar Cap - Crore`)
mid_cap
#large-cap companies:companies with their market capitalisation less than Rs.5,000 crores
small_cap <- data %>%
  filter(`Mar Cap - Crore` < 5000) %>%
  arrange(desc(`Mar Cap - Crore`)) %>%
  select(Name,`Mar Cap - Crore`)
small_cap

my_data<-data%>%
  mutate(yearly_sales=`Sales Qtr - Crore`*4 )
my_data
summary(my_data)
range(my_data$yearly_sales)
my_data

range(my_data$yearly_sales) 
#companies with zero sales are also present in the data set
sd(my_data$yearly_sales)
# large standard deviation indicate that Some companies might have substantially larger or smaller sales figures
#compared to the average, contributing to the overall variability.

top_sales <-my_data %>%
  filter(yearly_sales > 350000) %>%
  arrange(desc(yearly_sales)) %>%
  select(Name,yearly_sales)
top_sales     #I O C L and Reliance Inds.are the top companies in terms of annual sales
#this shows the effectiveness of sales strategies 

bottom_sales <- my_data %>%
  filter(yearly_sales < 100) %>%
  arrange(desc(yearly_sales)) %>%
  select(Name,yearly_sales)
bottom_sales                
# SPARC has annual sales less than 100 crores 
#Ujjivan Fin.Ser.has 0 sales
#these companies need to identify areas for improvement in revenue generation.
company_bysales<-my_data%>%
  arrange(desc(yearly_sales))
company_bysales

my_data<-my_data%>%
  mutate(P_S_ratio=`Mar Cap - Crore`/yearly_sales)%>%
  arrange(desc(P_S_ratio))
my_data
summary(my_data)
#Investors often compare the P/S ratio of a company with its peers. 

undervalued<-my_data%>%
  filter(P_S_ratio<1)%>%
  arrange(desc(P_S_ratio))
undervalued
tail(undervalued)
#A P/S ratio less than 1 might suggest that the company is trading at a lower valuation compared 
#to its competitors, which could be due to factors such as market sentiment, growth prospects, or financial health.

# P/S ratio less than 1 suggests that the stock is trading at a relatively low valuation compared to its
#sales revenue, potentially presenting investment opportunities.Investors may perceive this as an opportunity
#to purchase the stock at a discount relative to the company's sales performance.

medium_value<-my_data%>%
  filter(P_S_ratio>1,P_S_ratio<2)%>%
  arrange(desc(P_S_ratio))
medium_value
tail(medium_value)
#A P/S ratio between 1 and 2 suggests that investors are willing to pay between 1 to 2 times the company's 
#annual sales revenue for each unit of its stock. 

high_value<-my_data%>%
  filter(P_S_ratio>2,P_S_ratio<10)%>%
  arrange(desc(P_S_ratio))
print(high_value,n=100)

overvalued<-my_data%>%
  filter(P_S_ratio>10)%>%
  arrange(desc(P_S_ratio))
overvalued
head(overvalued)
#it suggests that the stock is overvalued and the investors are overly optimistic.

#removing infinity values 
my_data1 <- my_data %>% filter(!is.infinite(P_S_ratio))
summary(my_data1)

#ranking the companies on the basis of market cap and p-s ratios
company_Rank = my_data1%>%
  mutate(
    mkt_cap_rank=rank(-`Mar Cap - Crore`),
P_S_Ratio_Rank = rank(P_S_ratio))%>%
  arrange(desc(`Mar Cap - Crore`))%>%
  select(Name, `Mar Cap - Crore`, yearly_sales,P_S_ratio,mkt_cap_rank,P_S_Ratio_Rank)
company_Rank
#Market Capitalization Rank: This rank indicates the position of each company based on its market capitalization
#A higher rank suggests a larger market capitalization relative to other companies in the dataset.

#P/S Ratio Rank: This rank evaluates the price-to-sales ratio for each company.
#A lower rank suggests a lower price-to-sales ratio, which could indicate a potentially undervalued stock compared to others.

library(corrplot)
my_data1
summary(my_data1)
my_data2<-my_data1[,-c(1,2)]
cor_mat<-cor(my_data2)
cor_mat
corrplot(cor_mat,method="circle",type="lower",order="original",tl.col = "black",tl.srt = 45)
#The correlation coefficients between Market Capitalization and Sales are approximately 0.62, indicating
#a moderate positive correlation.companies with higher sales tend to have higher market capitalizations, which is a typical relationship.
#There is a negative correlation between Sales and the Price-to-Sales Ratio.(approximately -0.10) indicating a weak negative correlation.
#This suggests that as sales increase, the price-to-sales ratio tends to decrease slightly, meaning 
#that investors may be willing to pay a lower premium for each unit of sales revenue.

"the correlation matrix suggests some expected relationships between market capitalization, sales figures,
and the price-to-sales ratio. However, these correlations are not extremely strong, indicating that other factors
beyond these variables may also influence the market dynamics and investor sentiment".

library(ggplot2)
ggplot(my_data1, aes(x = `Mar Cap - Crore` , y = P_S_ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter plot with Regression Line",
       x = "market capital (crores)",
       y = "Price_sales ratio")

#regression of annual sales on market cap(in crores)
lm_model <- lm(yearly_sales ~`Mar Cap - Crore` , data = my_data1)
lm_model

# Summary of linear regression model
summary(lm_model)
#Intercept (3349.3915): This is the value of "yearly_sales" when the independent variable "Mar Cap - Crore" is zero."
"In this context, it might not make sense since Market Capitalization typically wouldn't be zero.
It's the baseline value of yearly sales when all other factors are zero."
#Mar Cap - Crore (0.4119): for every one crore increase in Market Capitalization, yearly sales increase by
#approximately 0.4119 units.

#plotting the regression line
plot(my_data1$`Mar Cap - Crore`, my_data1$yearly_sales, main = "Yearly Sales vs. Market Cap",
     xlab = "Market Cap (Crore)", ylab = "Yearly Sales", col = "blue", pch = 19)
abline(lm_model, col = "red")

#regression of p-s ratio on market capitalisation
regn <- lm(P_S_ratio ~`Mar Cap - Crore` , data = my_data1)
regn
summary(regn)
#Intercept (3.815e+00): value of "P_S_ratio" when the independent variable "Mar Cap - Crore" is zero.
#Mar Cap - Crore (6.465e-07): This coefficient represents the change in "P_S_ratio" for a one-unit
#increase in "Mar Cap - Crore" while holding all other variables constant. In this case, for every 
#one crore increase in Market Capitalization, the Price to Sales ratio increases by approximately 6.465e-07 units.
"the p-value is very high and the adjusted r-squared is very low which shows that the coefficient
is statistically insignificant"
#plotting the regression line
plot(my_data1$`Mar Cap - Crore`, my_data1$P_S_ratio, main = "p-s ratio vs. Market Cap",
     xlab = "Market Cap (Crore)", ylab = "P-S Ratio", col = "blue", pch = 19)
abline(lm_model, col = "red")

regn2 <- lm(P_S_ratio ~yearly_sales , data = my_data1)
regn2
#Intercept (4.117e+00): value of "P_S_ratio" when the independent variable "yearly_sales" is zero.
#It represents the baseline value of the Price to Sales ratio when all other factors are zero.
# for every one-crore increase in yearly sales, the Price to Sales ratio decreases by approximately 1.854e-05 units.
summary(regn2)
#p-value is low , the coefficient is statistically significant
"A low adjusted R-squared value indicates that the independent variables in a regression model explain only
a small proportion of the variability observed in the dependent variable."

plot(my_data1$yearly_sales, my_data1$P_S_ratio, main = "p-s ratio vs. yearly_sales",
     xlab = "yearly_sales (Crore)", ylab = "P-S Ratio", col = "blue", pch = 19)
abline(lm_model, col = "red")

#calculation of ROE
my_data1 <- mutate(my_data1,
               ROE = yearly_sales / `Mar Cap - Crore` * 100
)
my_data1
summary(my_data1)

corr_mat <- cor(my_data1[, c("Mar Cap - Crore",  "yearly_sales", "P_S_ratio", "ROE")])
print(corr_mat)
corrplot(corr_mat,method="circle",type="lower",order="original",tl.col = "blue",tl.srt = 45)
"There is a moderate negative correlation (-0.245) between the Price to Sales ratio and Return on Equity. 
This suggests that companies with lower Price to Sales ratios tend to have higher Return on Equity, and vice versa."
"There is a weak negative correlation (-0.072) between Market Capitalization and Return on Equity."
"It could be due to the company's strategy of reinvesting profits back into the business for future growth and capital building  "

my_data1 <- company_Rank %>%
  mutate(
    Competitiveness_Score = (mkt_cap_rank + P_S_Ratio_Rank) / 2
  )
my_data1
# Order data by Competitiveness Score
my_data1 <-my_data1 %>%
  arrange(Competitiveness_Score)
my_data1
#The competitiveness score is calculated as the average of the two ranks. 
#By combining these ranks into a single score, the competitiveness score provides a holistic view of each company's
#competitive position, considering market capitalization, sales growth, and valuation metrics.

#Interpreting the competitiveness score:
#A lower competitiveness score indicates a stronger competitive position, as it suggests higher ranks 
#in market capitalization, sales growth, and/or lower P/S ratio.
#Conversely, a higher competitiveness score suggests a weaker competitive position relative to other 
#companies in the dataset,as it reflects lower ranks in market capitalization, sales growth, and/or higher P/S ratio.

#Therefore, the competitiveness score helps to infer how well-positioned each company is within
#the competitive landscape based on these key financial metrics.
