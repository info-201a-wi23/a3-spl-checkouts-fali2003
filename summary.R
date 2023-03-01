Bio_auto_data <- read.csv("C:\\Users\\iyoab\\Desktop\\INFO201\\datasets for a3\\Biography and Autobiography data.csv")
library("tidyverse")
library("stringr")
library("dplyr")

#Summary Statistics
# What Year had the highest number of checkouts?
most_year <- Bio_auto_data %>% 
  group_by(CheckoutYear) %>% 
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  filter(total_checkouts == max(total_checkouts, na.rm = TRUE)) %>% 
  pull(CheckoutYear)
#2022 had the highest number of checkouts for biographies and autobiographies.

# Which author had the most amount of total checkouts?
most_amt_checkouts_author <- Bio_auto_data %>% 
  group_by(Creator) %>% 
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))
most_amt_checkouts_author <- na.omit(most_amt_checkouts_author)
most_amt_checkouts_author <- slice_max(most_amt_checkouts_author, total_checkouts, n = 50)
most_amt_checkouts_author$Creator[str_detect(most_amt_checkouts_author$Creator, "Noah")] <- "Trevor Noah"
most_amt_checkouts_author$Creator[str_detect(most_amt_checkouts_author$Creator, "Westover")] <- "Tara Westover"
most_amt_checkouts_author$Creator[str_detect(most_amt_checkouts_author$Creator, "1964-")] <- "Michelle Obama"
max_author <- most_amt_checkouts_author %>% 
  group_by(Creator) %>% 
  summarize(total_checkouts = sum(total_checkouts, na.rm = TRUE))

most_amt_author <- max_author %>% 
  filter(total_checkouts == max(total_checkouts, na.rm = TRUE)) %>% 
  pull(Creator)
# Michelle Obama has the most amount of checkouts.

# Which author had the highest number of checkouts in a month?
highest_num_checkouts_author <- Bio_auto_data %>% 
  filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% 
  pull(Creator)
#Highest number of checkouts in a month was Ibram X. Kendi.

#Which author had the greatest amount of checkouts in 2022?
authors_2022 <- Bio_auto_data %>% 
  filter(CheckoutYear == 2022) %>% 
  group_by(Creator) %>% 
  summarize(Checkouts = sum(Checkouts, na.rm = TRUE)) 

max_authors_2022 <- authors_2022 %>% 
  filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% 
  pull(Creator)

#The author with the most amount of checkouts in 2022 was Michelle Zauner.

# What is the proportion of audiobooks, ebooks, and physical books?
total_amt <- nrow(Bio_auto_data)
MaterialVar <- Bio_auto_data$MaterialType
num_of_audiobooks <- length(MaterialVar[MaterialVar == 'AUDIOBOOK'])
num_of_ebook <- length(MaterialVar[MaterialVar == 'EBOOK'])
num_of_physical <- length(MaterialVar[MaterialVar == 'BOOK'])
prop_of_audiobooks <- num_of_audiobooks/total_amt
prop_of_ebooks <- num_of_ebook/total_amt
prop_of_physical <- num_of_physical/total_amt

summary_info <- list()
summary_info$most_year <- most_year
summary_info$most_amt_author <- most_amt_author
summary_info$highest_num_checkouts_author <- highest_num_checkouts_author
summary_info$max_authors_2022 <- max_authors_2022
summary_info$prop_of_audiobooks <- prop_of_audiobooks
summary_info$prop_of_ebooks <- prop_of_ebooks
summary_info$prop_of_physical <- prop_of_physical