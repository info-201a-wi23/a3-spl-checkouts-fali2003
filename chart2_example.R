library("styler")
# Trend 2 for Biographies/Autobiographies - Do people prefer to read or listen to autobiographies and biographies? How has this interest changed over time?
material_bio <- Bio_auto_data %>%
  filter(MaterialType %in% c("EBOOK", "AUDIOBOOK", "BOOK"))

total_checkouts_material_time <- material_bio %>%
  group_by(date, MaterialType) %>%
  summarize(total_amt = sum(Checkouts, na.rm = TRUE))

# Ebooks has the most amount of checkouts out of the three material types.
material_plot <- ggplot(total_checkouts_material_time) +
  geom_line(aes(
    x = date,
    y = total_amt,
    color = MaterialType,
    text = paste("MaterialType:", MaterialType)
  )) +
  labs(
    title = "More People Now Prefer to Listen to Biographies and Autobiographies",
    x = "Year",
    y = "Total Amount of Checkouts",
    color = "Material Type"
  )

ggplotly(material_plot, tooltip = c("text"))

# More people listen to audiobooks currently. As technology progresses, the capabilities of audiobooks does as well. Books have returned gradually to their pre-pandemic levels.
