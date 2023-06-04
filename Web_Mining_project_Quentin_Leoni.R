################################################################################
#### HSG - Introduction to Web-Mining
### Spring 2023 - Term paper
## Quentin Leoni

################################################################################
## Preamble

# Load libraries
library(httr)
library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)
library(jtools)

# Define path
setwd("C:/Users/Quentin/OneDrive/Studies/HSG/2nd semester/Intro to web mining/Term paper")

# Define headers to prevent Error 403
header <- c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.182 Safari/537.36",
            "Referer" = "https://www.seloger.com/",
            "Accept" =	"text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8",
            "Accept-Language" = "en-US,en;q=0.6")

################################################################################
## I) Start with France on seloger.com

# Define and parse URL
SEARCH_URL <- parse_url("https://www.seloger.com/list.htm?projects=2%2C5&types=2%2C1&natures=1%2C2%2C4&places=[{%22inseeCodes%22%3A[10160]}]&enterprise=0&qsVersion=1.0&LISTING-LISTpg=1")

# Define variables
# Create an empty list
data_FR <- list()

# ZIP code
# Gex: 10173
# Miribel: 10249

for (i in c(10173, 10249)){
  
  p <- 26
  PAGE <- 0
  
  while(((p)>0)){
    
    PAGE <- PAGE+1
    
    # Replace the value in the URL
    SEARCH_URL$query$places <- paste('[{"inseeCodes":[',i,']}]')
    SEARCH_URL$query$`LISTING-LISTpg` <- PAGE
    
    # Fetch the website via a HTTP GET request
    URL <- build_url(SEARCH_URL)
    search_result <- GET(URL, add_headers(header))
    
    # Parse the content of the response (the html code)
    search_result_html <- read_html(search_result)
    
    # Extract the data of interest
    # a) Price
    prices_nodes <- html_nodes(search_result_html, xpath="//div[contains(@data-test, 'sl.price-label')]")
    prices <- html_text(prices_nodes)
    
    # b) Type of estate
    type_nodes <- html_nodes(search_result_html, xpath="//div[contains(@data-test, 'sl.title')]")
    type <- html_text(type_nodes)
    
    # c) Specifications
    specs_nodes <- html_nodes(search_result_html, xpath="//ul[contains(@data-test, 'sl.tagsLine')]")
    specs <- html_text(specs_nodes)
    
    # D) Number of page
    pages_nodes <- html_nodes(search_result_html, xpath="//span[contains(@class, 'MapSwitchButton__CountSpan')]")
    pages <- html_text(pages_nodes)
    
    # Format and save data for further processing
    concat <- data.frame(prices, type, specs)
    concat$Town <- i
    data_FR <- rbind(data_FR, concat)
    
    # Test if any page left
    res <- str_extract(pages[1], "\\d+")
    res <- as.numeric(res)
    p <- res-(PAGE*25)
  }
}
# Define columns names
colnames(data_FR) <- c("Price","Type","Specs","Town")

################################################################################
## Clean the data

# Create a copy to work
data_f <- data_FR

# Price
data_f$Price <- gsub("\\€", "", data_FR$Price)
data_f$Price <- str_replace_all(data_f$Price, "\\s+", "")
data_f$Price <- as.numeric(data_f$Price)

# Type
data_f$Type <- ifelse(grepl("Appartement", data_f$Type), 1, data_f$Type)
data_f$Type <- ifelse(grepl("Maison|Villa", data_f$Type), 2, data_f$Type)
data_f <- subset(data_f, Type == 1 | Type == 2)

# Specs
# Remove the exceeding part
data_f$Specs <- str_remove(data_f$Specs, "\\m²[^m²]*$")
# Repeat
data_f$Specs <- str_remove(data_f$Specs, "\\m²[^m²]*$")
# Extract the relevant numbers
data_f$Size_1 <- str_extract(data_f$Specs, "\\d{3}+")
data_f$Specs <- str_remove(data_f$Specs, "\\d{3}+")
data_f$Size_2 <- str_extract(data_f$Specs, "\\d{2}+")
data_f$Specs <- str_remove(data_f$Specs, "\\d{2}+")
# Keep number of rooms if no bedroom number
data_f$chambres <- str_extract(str_extract(data_f$Specs, "\\d+ ch"), "\\d+")
data_f$pieces <- str_extract(str_extract(data_f$Specs, "\\d+ p"), "\\d+")

# Merge the columns
data_f$Footage <- coalesce(data_f$Size_1, data_f$Size_2)
data_f$Footage <- as.numeric(data_f$Footage)
data_f$Room <- ifelse(is.na(data_f$chambres), data_f$pieces, data_f$chambres)
data_f$Room <- as.numeric(data_f$Room)

# Remove irrelevant columns
data_f <- subset(data_f, select = -c(Specs, Size_1, Size_2, chambres, pieces))

# Treatment
data_f$Town[data_f$Town==10249] <- 0
data_f$Town[data_f$Town==10173] <- 1
colnames(data_f)[colnames(data_f)=="Town"] <- "Treatment"
data_f$Treatment <- as.character(data_f$Treatment)

# Drop NA
data_f <- na.omit(data_f)

################################################################################
## Visualize Data

# Reorder columns
data_f <- data_f[c("Price","Type","Room","Footage","Treatment")]

# Create subsets
data_f1 <- subset(data_f, data_f$Treatment==1)
comp_f1 <-count(data_f1, Type)

data_f0 <- subset(data_f, data_f$Treatment==0)
comp_f0 <-count(data_f0, Type)

sumup_f <- select(data.frame(c(comp_f0, comp_f1)),-1,-3)
colnames(sumup_f) <- c("Miribel","Gex")
rownames(sumup_f) <- c("Flat","House")

summary(data_f$Price)
summary(data_f$Room)
summary(data_f$Footage)
print(sumup_f)

################################################################################
# Test ATE
treated_f <- mean(data_f1$Price)
non_treated_f <- mean(data_f0$Price)
ATE_FR <- treated_f - non_treated_f
print(paste0("The naive ATE for France is ", trunc(ATE_FR)))

# Test ATE on apartment only
treated_f_t1 <- mean(data_f1$Price[data_f1$Type==1])
non_treated_f_t1 <- mean(data_f0$Price[data_f0$Type==1])
ATE_FR_t1 <- treated_f_t1 - non_treated_f_t1
print(paste0("The ATE for France apartment only is ", trunc(ATE_FR_t1)))

# Test ATE on house only
treated_f_t2 <- mean(data_f1$Price[data_f1$Type==2])
non_treated_f_t2 <- mean(data_f0$Price[data_f0$Type==2])
ATE_FR_t2 <- treated_f_t2 - non_treated_f_t2
print(paste0("The ATE for France house only is ", trunc(ATE_FR_t2)))

################################################################################
# Regression with interaction effect
model_f <- lm(data_f$Price~data_f$Type+data_f$Room+data_f$Footage+data_f$Treatment
              +data_f$Type*data_f$Treatment
              +data_f$Room*data_f$Treatment
              +data_f$Footage*data_f$Treatment)
summ(model_f)

################################################################################
# Plot
ggplot(data_f,aes(x=Footage, y=Price, col=Treatment))+geom_point()+geom_smooth(se=FALSE, method="lm")+ggtitle("France")+theme(plot.title = element_text(hjust = 0.5))

################################################################################
## II) Repeat for Germany on wohnungsboerse.net

# define path
SEARCH_URL <- parse_url("https://www.wohnungsboerse.net/searches/index?isSearch=1&country=DE&estate_marketing_types=kauf%2C1&marketing_type=kauf&estate_types%5B0%5D=1&zipcode_city=Leonberg&term=Leonberg&pricetext=beliebig&sizetext=beliebig&roomstext=beliebig&umkreistext=keiner&page=1")

# Define variables
# No need of ZIP code here
Towns <- c("Leinfelden-Echterdingen", "Loerrach")
# Apartment = 1, House = 3
Type <- list(c(1,"kauf%2C1"),c(3,"kauf%2C3"))
# Create any empty dataframe
data_DE <- data.frame()

for(i in Towns){
  
  for(j in Type){
    
    PAGE <- 0
    p <- 16
    
    while(((p)>0)){
      
      PAGE <- PAGE+1
      
      # Replace the value in the URL
      SEARCH_URL$query$zipcode_city <- i
      SEARCH_URL$query$`estate_types[0]` <- j[1]
      SEARCH_URL$query$estate_marketing_types <- j[2]
      SEARCH_URL$query$page <- PAGE
      
      # Fetch the website via a HTTP GET request
      URL <- build_url(SEARCH_URL)
      search_result <- GET(URL, add_headers(header))
      
      # Parse the content of the response (the html code)
      search_result_html <- read_html(search_result)
      
      # Extract the data of interest
      # a) Price
      prices_nodes <- html_nodes(search_result_html, xpath="//dd[@class='text-base font-bold']")
      prices <- html_text(prices_nodes, trim=TRUE)
      
      temp_group <- c()
      temp_data <- data.frame()
      for(x in seq_along(prices)){
        temp_group <- c(temp_group, prices[x])
        if (x%% 3 == 0) {
          temp_data <- rbind(temp_data, as.list(temp_group))
          temp_group <- c()
        }
      }
      # Add estate type
      temp_data$Type <- j[1]
      # Add town name
      temp_data$Town <- i
      # Merge data
      colnames(temp_data) <- colnames(data_DE)
      data_DE <- rbind(data_DE, temp_data)
      
      # b) Number of page
      pages_nodes <- html_nodes(search_result_html, xpath="//span[@class='whitespace-nowrap']")
      pages <- html_text(pages_nodes)
      
      # Test if any page left
      res <- str_extract(pages[1], "\\d+")
      res <- as.numeric(res)
      p <- res-(PAGE*15)
    } 
  }
}
# Define columns names
colnames(data_DE) <- c("Price","Room","Footage","Type","Town")

################################################################################
## Clean the data

# Create a copy to wok
data_d <- data_DE

# Price column
data_d$Price <- gsub("\\€", "", data_d$Price)
data_d$Price <- gsub("\\.","", data_d$Price)
data_d$Price <- str_replace_all(data_d$Price, "\\s+", "")
data_d$Price <- as.numeric(data_d$Price)

# Room size
data_d$Room <- gsub("\\Zi.", "", data_d$Room)
data_d$Room <- str_replace_all(data_d$Room, "\\s+", "")
data_d$Room <- as.numeric(data_d$Room)

# Footage
data_d$Footage <- gsub("\\m²", "", data_d$Footage)
data_d$Footage <- str_replace_all(data_d$Footage, "\\s+", "")
data_d$Footage <- as.numeric(data_d$Footage)

# Type
data_d$Type[data_d$Type==3] <- 2
data_d$Type <- as.character(data_d$Type)

# Treatment
data_d$Town[data_d$Town=="Leinfelden-Echterdingen"] <- 0
data_d$Town[data_d$Town=="Loerrach"] <- 1
colnames(data_d)[colnames(data_d)=="Town"] <- "Treatment"
data_d$Treatment <- as.character(data_d$Treatment)

# Drop NA
data_d <- na.omit(data_d)

################################################################################
## Visualize Data

# Reorder columns
data_d <- data_d[c("Price","Type","Room","Footage","Treatment")]

# Create subdsets
data_d1 <- subset(data_d, data_d$Treatment==1)
comp_d1 <- count(data_d1, Type)

data_d0 <- subset(data_d, data_d$Treatment==0)
comp_d0 <- count(data_d0, Type)

sumup_d <- select(data.frame(c(comp_d0, comp_d1)),-1,-3)
colnames(sumup_d) <- c("Leinfelden","Lörrach")
rownames(sumup_d) <- c("Flat","House")

summary(data_d$Price)
summary(data_d$Room)
summary(data_d$Footage)
print(sumup_d)

################################################################################
# Test ATE
treated_d <- mean(data_d1$Price)
non_treated_d <- mean(data_d0$Price)
ATE_DE <- treated_d - non_treated_d
print(paste0("The naive ATE for Germany is ", trunc(ATE_DE)))

# Test ATE on apartment only
treated_d_t1 <- mean(data_d1$Price[data_d1$Type==1])
non_treated_d_t1 <- mean(data_d0$Price[data_d0$Type==1])
ATE_DE_t1 <- treated_d_t1 - non_treated_d_t1
print(paste0("The ATE for Germany apartment only is ", trunc(ATE_DE_t1)))

# Test ATE on house only
treated_d_t2 <- mean(data_d1$Price[data_d1$Type==2])
non_treated_d_t2 <- mean(data_d0$Price[data_d0$Type==2])
ATE_DE_t2 <- treated_d_t2 - non_treated_d_t2
print(paste0("The ATE for Germany house only is ", trunc(ATE_DE_t2)))

################################################################################
# Regression with interaction term
model_d <- lm(data_d$Price~data_d$Type+data_d$Room+data_d$Footage+data_d$Treatment
              +data_d$Type*data_d$Treatment
              +data_d$Room*data_d$Treatment
              +data_d$Footage*data_d$Treatment)
summ(model_d)

################################################################################
# Plot
ggplot(data_d,aes(x=Footage, y=Price, col=Treatment))+geom_point()+geom_smooth(se=FALSE, method='lm')+ggtitle("Germany")+theme(plot.title = element_text(hjust = 0.5))
