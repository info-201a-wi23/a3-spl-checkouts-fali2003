library("dplyr")
library("stringr")
library("ggplot2")
library("plotly")
library("styler")
Bio_auto_data <- read.csv("C:\\Users\\iyoab\\Desktop\\INFO201\\datasets for a3\\Biography and Autobiography data.csv")
# Trend 1:
Bio_auto_data <- Bio_auto_data %>%
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-", "01"))
Bio_auto_data$date <- as.Date(Bio_auto_data$date, format = "%Y-%m-%d")

top_authors_over_time <- Bio_auto_data %>%
  group_by(Creator, date) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))
top_authors_over_time$Creator[str_detect(top_authors_over_time$Creator, "Michelle")] <- "Michelle Obama"
top_authors_over_time$Creator[str_detect(top_authors_over_time$Creator, "Tara")] <- "Tara Westover"
top_authors_over_time$Creator[str_detect(top_authors_over_time$Creator, "Barack")] <- "Barack Obama"
top_authors_over_time <- na.omit(top_authors_over_time)

top_Creators_over_time <- top_authors_over_time %>%
  filter(Creator %in% c("Michelle Obama", "Tara Westover", "Ta-Nehisi Coates", "Ron Chernow", "Cheryl Strayed", "Ibram X. Kendi", "Paul Kalanithi", "Barack Obama"))

top_eight <- top_Creators_over_time %>%
  group_by(Creator, date) %>%
  summarize(total_checkouts = sum(total_checkouts, na.rm = TRUE))

top_eight_plot <- ggplot(top_eight) +
  geom_line(aes(
    x = date,
    y = total_checkouts,
    color = Creator,
    text = paste("Author:", Creator)
  )) +
  labs(
    title = "Top 8 authors of Biographies and Autobiographies over time",
    x = "Year",
    y = "Checkouts",
    color = "Author"
  )

ggplotly(top_eight_plot, tooltip = c("text"))
