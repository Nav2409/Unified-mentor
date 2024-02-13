# The following report presents Sector-wise investment analysis and Year-wise investment analysis of 
# Foreign Direct Investment in India by using the past FDI data of 17 years.
#DATASET OVERVIEW
#To understand the Foreign direct investment in India for the last 17 years from 2000-01 to 2016-17. 
#This dataset contains sector and financial year-wise data of FDI in India

#Loading the dataset
library(readxl)
getwd()
setwd("C:/Users/TANNU/Downloads")
# Load the data from the Excel file
excel_file <-"C:/Users/TANNU/Downloads/FDI data.xlsx"
data1 <- read_excel(excel_file)
data1
str(data1)
summary(data1)
library(dplyr)
install.packages("tidyr")
library(tidyr)

#top 5 sectors in each year
top_sectors_by_year <- function(year) {
  data1 %>%
    select(Sector, {{year}}) %>%
    arrange(desc({{year}})) %>%
    slice_head(n = 5)
}
# List to store the results for each year
top_sectors_list <- list()

# Loop through each year column and store the top 5 sectors
year_columns <- colnames(data1)[2:ncol(data1)]  # Assuming year columns start from the second column
for (year_col in year_columns) {
  top_sectors_list[[year_col]] <- top_sectors_by_year(!!sym(year_col))
}
print(top_sectors_list)

#sectors with the highest fdi in each year
top_sectors_by_year <- data1 %>%
  gather(key = "Year", value = "fdi", -Sector) %>%
  group_by(Year) %>%
  slice(which.max(fdi)) %>%
  arrange(desc(fdi))
print(top_sectors_by_year)


#Calculation of the total fdi for each sector across all years
total_fdi_sectorwise <-data1 %>%
  select(Sector, `2000-01`:`2016-17`) %>%
  gather(key = "Year", value = "fdi", -Sector) %>%
  group_by(Sector) %>%
  summarise(total_fdi = sum(fdi, na.rm = TRUE))
total_fdi_sectorwise


# Calculation of the average fdi for each sector across all years
average_fdi_by_sector <- data1 %>%
  select(Sector, `2000-01`:`2016-17`) %>%
  gather(key = "Year", value = "fdi", -Sector) %>%
  group_by(Sector) %>%
  summarise(average_fdi = mean(fdi, na.rm = TRUE))
average_fdi_by_sector

#sum of fdi in each year (Select only numeric columns and calculate the sum of each column)
annual_total_fdi<- data1 %>%
  select_if(is.numeric) %>%
  colSums(na.rm = TRUE)
annual_total_fdi<-as.data.frame(annual_total_fdi)
annual_total_fdi
summary(annual_total_fdi)

library(ggplot2)

#bar graph showing the top fdi receiving sectors in each year
ggplot(top_sectors_by_year, aes(x = Year, y = fdi,colour=Sector)) +
  geom_bar(stat = "identity") +
  labs(title = "Top FDI in each year",
       x = "Years",
       y = "Total FDI Amount") +
  theme(axis.title.x = element_text(angle = 45, hjust = 1))

#"FDI Over the Years by Sector"
fdi_data_long <- gather(data1, key = "Year", value = "FDI_amount", -Sector)
fdi_data_long
print(fdi_data_long,n=1000)
ggplot(fdi_data_long, aes(x = Year, y = FDI_amount)) +
  geom_line() +
  labs(title = "FDI Over the Years by Sector",
       x = "Year",
       y = "FDI Amount") +
  theme_minimal()

#Fdi distribution in each sector
ggplot(fdi_data_long, aes(x = Sector, y = FDI_amount)) +
  geom_boxplot() +
  labs(title = "FDI Distribution in Each Sector",
       x = "Sector",
       y = "FDI Amount") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
data1

#FDI comparison across sectors
barplot(data1$`2016-17`, names.arg = data1$Sector, 
        xlab = "Sector", ylab = "FDI", main = "FDI Comparison Across Sectors in 2016-17")

barplot(data1$`2015-16`, names.arg = data1$Sector, 
        xlab = "Sector", ylab = "FDI", main = "FDI Comparison Across Sectors in 2015-16")

barplot(data1$`2014-15`, names.arg = data1$Sector, 
        xlab = "Sector", ylab = "FDI", main = "FDI Comparison Across Sectors in 2014-15")

barplot(data1$`2013-14`, names.arg = data1$Sector, 
        xlab = "Sector", ylab = "FDI", main = "FDI Comparison Across Sectors in 2013-14")

barplot(data1$`2012-13`, names.arg = data1$Sector, 
        xlab = "Sector", ylab = "FDI", main = "FDI Comparison Across Sectors in 2012-13")

barplot(data1$`2011-12`, names.arg = data1$Sector, 
        xlab = "Sector", ylab = "FDI", main = "FDI Comparison Across Sectors in 2011-12")

barplot(data1$`2010-11`, names.arg = data1$Sector, 
        xlab = "Sector", ylab = "FDI", main = "FDI Comparison Across Sectors in 2010-11")

barplot(data1$`2009-10`, names.arg = data1$Sector, 
        xlab = "Sector", ylab = "FDI", main = "FDI Comparison Across Sectors in 2009-10")

barplot(data1$`2008-09`, names.arg = data1$Sector, 
        xlab = "Sector", ylab = "FDI", main = "FDI Comparison Across Sectors in 2008-09")

barplot(data1$`2007-08`, names.arg = data1$Sector, 
        xlab = "Sector", ylab = "FDI", main = "FDI Comparison Across Sectors in 2007-08")

barplot(data1$`2006-07`, names.arg = data1$Sector, 
        xlab = "Sector", ylab = "FDI", main = "FDI Comparison Across Sectors in 2006-07")

barplot(data1$`2005-06`, names.arg = data1$Sector, 
        xlab = "Sector", ylab = "FDI", main = "FDI Comparison Across Sectors in 2005-06")

barplot(data1$`2004-05`, names.arg = data1$Sector, 
        xlab = "Sector", ylab = "FDI", main = "FDI Comparison Across Sectors in 2004-05")

barplot(data1$`2003-04`, names.arg = data1$Sector, 
        xlab = "Sector", ylab = "FDI", main = "FDI Comparison Across Sectors in 2003-04")

barplot(data1$`2002-03`, names.arg = data1$Sector, 
        xlab = "Sector", ylab = "FDI", main = "FDI Comparison Across Sectors in 2002-03")

barplot(data1$`2001-02`, names.arg = data1$Sector, 
        xlab = "Sector", ylab = "FDI", main = "FDI Comparison Across Sectors in 2001-02")

barplot(data1$`2000-01`, names.arg = data1$Sector, 
        xlab = "Sector", ylab = "FDI", main = "FDI Comparison Across Sectors in 2000-01")

# Assuming data contains only numeric columns
library(corrplot)
cor_matrix <- cor(data1[, -1])  # Exclude the first column (Sector)
cor_matrix
corrplot(cor_matrix,method="circle",type="lower",order="original",tl.col = "blue",tl.srt = 45)
# this shows each sectors' fdi is positively correlated with the other sectors

data_cleaned <- data1 %>%
  gather(Year, FDI, -Sector) %>%
  mutate(Year = gsub("X", "", Year), # Remove "X" prefix in year column
         Year = as.numeric(gsub("-", "", Year))) # Convert year to numeric
data_cleaned

summary_stats <- data_cleaned %>%
  group_by(Sector) %>%
  summarize(Total_FDI = sum(FDI), Mean_FDI = mean(FDI), Max_FDI = max(FDI))
summary_stats      #this shows the total, mean and maximum fdi of each sector

"Total Foreign Direct Investment (FDI) by Sector"
ggplot(data_cleaned, aes(x = reorder(Sector, FDI), y = FDI)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Foreign Direct Investment (FDI) by Sector",
       x = "Sector",
       y = "Total FDI (in million USD)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  coord_flip()

"Mean Foreign Direct Investment (FDI) by Sector"
ggplot(data_cleaned, aes(x = reorder(Sector, FDI), y = FDI)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Foreign Direct Investment (FDI) by Sector",
       x = "Sector",
       y = "Mean FDI (in million USD)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  coord_flip()

