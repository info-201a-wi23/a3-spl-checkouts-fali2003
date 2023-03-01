# Fal's Favorite Memoirs
library("styler")

favorite_memoirs <- Bio_auto_data %>%
  filter(Creator %in% c("Jacqueline Woodson", "Haben Girma", "bell hooks", "Trevor Noah", "Maya Angelou", "Imani Perry"))

fav_memoirs <- favorite_memoirs %>%
  group_by(date, Creator) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))

fav_plot <- ggplot(fav_memoirs) +
  geom_line(aes(
    x = date,
    y = total_checkouts,
    color = Creator,
    text = paste("Creator:", Creator)
  )) +
  labs(
    title = "SPL Trends of Fal's Personal Biographies and Autobiographies Picks",
    x = "Year",
    y = "Checkouts",
    color = "Author"
  )


ggplotly(fav_plot, tooltip = c("text"))
