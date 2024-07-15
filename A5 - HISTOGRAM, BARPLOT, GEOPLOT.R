options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Set the working directory and verify it
setwd('E:\\ASSIGNMENT\\Data')
getwd()

# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA")
lapply(libraries, install_and_load)

# Reading the file into R
data <- read.csv("NSSO68.csv")

# Filtering for Pun
df <- data %>%
  filter(state_1 == "Pun")

# Display dataset info
cat("Dataset Information:\n")
print(names(df))
print(head(df))
print(dim(df))

# Finding missing values
missing_info <- colSums(is.na(df))
cat("Missing Values Information:\n")
print(missing_info)

# Subsetting the data
punnew <- df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
punnew$Meals_At_Home <- impute_with_mean(punnew$Meals_At_Home)

# Finding outliers and removing them
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}

outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  punnew <- remove_outliers(punnew, col)
}

# Summarize consumption
punnew$total_consumption <- rowSums(punnew[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

# Summarize and display top consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- punnew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)

# Rename districts and sectors
district_mpunping <- c("1" = "Gurdaspur","2" = "Amritsar","3" = "Kapurthala","4" = "Jalandhar","5" = "Hoshiarpur","6" = "Nawanshahr","7" = "Rupnagar","8" = "Fatehgarh Sahib","9" = "Ludhiana",
                       "10"= "Moga","11" = "Firozpur","12"="Muktsar","13" = "Faridkot",
                       "14" = "Bathinda", "15" = "Mansa","16" = "Sangrur", "17" = "Patiala",
                       "18" = "SJAS Nagar (Mohali)","19" = "Barnala","20" = "Tarn Taran")
sector_mpunping <- c("2" = "URBAN", "1" = "RURAL")

punnew$District <- as.character(punnew$District)
punnew$Sector <- as.character(punnew$Sector)
punnew$District <- ifelse(punnew$District %in% names(district_mpunping), district_mpunping[punnew$District], punnew$District)
punnew$Sector <- ifelse(punnew$Sector %in% names(sector_mpunping), sector_mpunping[punnew$Sector], punnew$Sector)

View(punnew)

hist(punnew$total_consumption, breaks = 10, col = 'blue', border = 'black', 
     xlab = "Consumption", ylab = "Frequency", main = "Consumption Distribution in PUNJAB State")

pun_consumption <- aggregate(total_consumption ~ District, data = punnew, sum) 
View(pun_consumption)
??barplot
barplot(pun_consumption$total_consumption, 
        names.arg = pun_consumption$District, 
        las = 2, # Makes the district names vertical
        col = 'blue', 
        border = 'black', 
        xlab = "District", 
        ylab = "Total Consumption", 
        main = "Total Consumption per District",
        cex.names = 0.7) # Adjust the size of district names if needed


# b) Plot {'any variable of your choice'} on the PUNJAB state mpun using NSSO68.csv data

library(ggplot2) 
library(sf) # mpunping
library(dplyr) 
Sys.setenv("SHpunE_RESTORE_SHX" = "YES") 

data_mpun <- st_read("E:\\ASSIGNMENT\\Data\\PUNJAB_DISTRICTS.geojson") 
View(data_mpun)

data_mpun <- data_mpun %>% 
  rename(District = dtname) 
colnames(data_mpun) 
data_mpun_data <- merge(pun_consumption,data_mpun,by = "District") 
View(data_mpun_data)
ggplot(data_mpun_data) + 
  geom_sf(aes(fill =total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption_by_District") 

ggplot(data_mpun_data) + 
  geom_sf(aes(fill = total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption by District") +
  geom_sf_text(aes(label = District, geometry = geometry), size = 3, color = "black")
