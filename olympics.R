library(dplyr)
library(ggplot2)
library(gganimate)

tuesdata <- tidytuesdayR::tt_load('2021-07-27')
olympics <- tuesdata$olympics
# solo verano
olympics.s <- olympics %>% filter(season == "Summer")

members <- olympics.s %>% 
  group_by(noc, year) %>% 
  summarise(members = n_distinct(id)) 

medals <- olympics.s %>% 
  group_by(noc, year, event) %>% 
  filter(medal == "Gold") %>% 
  slice(1) %>%  
  group_by(noc, year) %>%
  summarise(gold = sum(medal == "Gold", na.rm = T)) %>%
  left_join(members, by = c("noc", "year")) %>%
  arrange(desc(year), desc(gold))


country.5 <- medals$noc[1:5]
medals.5 <- medals %>% filter(noc %in% country.5)


p4 <- ggplot(medals.5, aes(x = year, y =gold, col = noc)) + 
  geom_point(aes(size = members), alpha = 1/2) +
  scale_color_brewer(palette = "Set1") +
  scale_size(range = c(.1, 20), guide = "none") +
  geom_text(data = medals.5 %>% group_by(noc) %>% slice(1), 
            aes(label = noc, x=1950, y = 70), size = 10) + 
  theme_classic() +
  theme(legend.position = "none") +
  labs(title = "Number of gold medals",
        subtitle = "Size of burbles denote olympic delegation members") +
  transition_states(noc)
animate(p4)
anim_save("olympic.gif")
