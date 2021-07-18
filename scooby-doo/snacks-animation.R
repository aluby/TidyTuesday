library(tidyverse)

# Racing barchart inspired by: 
# https://evamaerey.github.io/little_flipbooks_library/racing_bars/racing_barcharts.html

scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

images = tibble(
  name = c("velma", "scooby", "shaggy", "fred", "daphnie"),
  url = c("https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQNEoWAkGNRQ-E1_DNBo8PfNSSp8MMLsWzrEw&usqp=CAU",
          "https://static.wikia.nocookie.net/charactercommunity/images/c/c9/Png-scooby-doo-scooby-doo-png-250.png/revision/latest?cb=20200318022450",
          "https://static.wikia.nocookie.net/hanna-barbera/images/3/3e/Shaggy.jpg/revision/latest/scale-to-width-down/300?cb=20090511190708",
          "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQ75Q5qtjgz5idkkgmboyHp84o7hRNo557tUVgOuf7nqcY17ZwAOMCLGzhBwu6oxUmlN_0&usqp=CAU",
          "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQWcTJGrZAY7OcXz7ARADy_88eKJ3Pm1519LA&usqp=CAU")
)

scooby_colors = c("#E8AA57", "#856842", "#98BF6D", "#4166B7", "#6C4287")

scoobydoo %>%
  select(contains("snack_"), "date_aired") %>%
  separate("date_aired", into = c("year", "month", "day"), remove = FALSE) %>%
  pivot_longer(contains("snack_")) %>%
  separate(name, into = c("snack", "character")) %>%
  group_by(character, year) %>%
  summarize(
    snacks = sum(value == TRUE)
  ) %>% 
  ungroup() %>%
  group_by(character) %>%
  mutate(
    cumulative_snacks = cumsum(snacks)
  ) %>%
  ungroup() %>%
  group_by(year) %>%
  arrange(year, -cumulative_snacks) %>% 
  mutate(rank = 1:n()) -> ranked_by_date


ranked_by_date %>%
  left_join(images, by = c("character" = "name")) %>%
  mutate(
    character = factor(character, levels = c("velma", "scooby", "shaggy", "fred", "daphnie"))
  ) %>%
  ggplot(aes(y = rank, fill = character)) + 
  aes(xmin = 0 ,  
      xmax = cumulative_snacks) +  
  aes(ymin = rank - .45,  
      ymax = rank + .45) +  
  facet_wrap(~ year) +  
  geom_rect(alpha = .7) +  
  scale_fill_manual(values = scooby_colors) +  
  scale_x_continuous(  
    limits = c(-3, 55)
    ) +  
  # geom_text(col = "gray13",
  #           hjust = "right",
  #           aes(label = character),
  #           x = -1, 
  #           family = "Spicy Rice") + 
  ggimage::geom_image(hjust = "right",
            aes(image = url),
            x = -1,
            size = .15, 
            by = "height") + 
  scale_size_identity() +
  scale_y_reverse() +  
  labs(x = '# of Episodes',
       y = "",
       fill = NULL,
       title = "Who snacked in the most episodes?",
       caption = "Source: #TidyTuesday/Kaggle \nGraphic: Amanda Luby") -> snacks_plot

snacks_animation = snacks_plot +  
  facet_null() +  
  geom_text(aes(x = 40, y = 5, label = year),  
            size = 10, col = "black", family = "Spicy Rice") +  
  aes(group = character) +  
  theme_minimal(base_family = "Spicy Rice", base_size = 10) + 
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        plot.caption = element_text(size = 5)
  ) +
  gganimate::transition_time(as.integer(year))


gganimate::animate(snacks_animation, nframes = 100, height = 675, width = 1200, units = "px", res = 300)
gganimate::anim_save("snacks-animation.gif")
