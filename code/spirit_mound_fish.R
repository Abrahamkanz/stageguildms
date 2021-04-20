library(tidyverse)

# fish_data <- read_csv("https://raw.githubusercontent.com/Abrahamkanz/stageguildms/master/diet_2019.csv") %>% 
#   select(-X1)

# write.csv(fish_data, file = "data/fish_data.csv")
# fixed names in excel file

fish_data <- read_csv("data/fish_data.csv")


unique(fish_data$fish)


#plot sprit mound

spirit_fish <- fish_data %>% filter(site == "spirit mound") %>% 
  group_by(fish) %>% 
  tally() %>% 
  arrange(-n)



(plot_spiritfish <- spirit_fish %>% 
  ggplot(aes(x = reorder(fish, n), y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=n), hjust= -0.2, vjust = 0.15) +
  coord_flip() +
  theme_classic() +
  labs(y = "Number sampled",
       x = "",
       title = "Fish sampled at Spirit Mound - Summer 2019") + 
  NULL)

ggsave(plot_spiritfish, file = "plot_spiritfish.jpg")

