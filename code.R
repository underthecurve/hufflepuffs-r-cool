## Download the 'harrypotter' package
## code inspired by: https://uc-r.github.io/sentiment_analysis

# if (packageVersion("devtools") < 1.6) {
#   install.packages("devtools")
# }
# 
# devtools::install_github("bradleyboehmke/harrypotter")

library('tidyverse')
library('stringr')
library('tidytext')
library('harrypotter')
library('ggthemes')
        
philosophers_stone[1] # for instance, this is the first chapter of the first book

titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")

books <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
              goblet_of_fire, order_of_the_phoenix, half_blood_prince,
              deathly_hallows)

series <- tibble()

for(i in seq_along(titles)) {
  
  clean <- tibble(chapter = seq_along(books[[i]]),
                  text = books[[i]]) %>%
    unnest_tokens(word, text) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, clean)
}


## calculate number and percent of the time each of the four houses were mentioned

series <- series %>% group_by(book) %>% mutate(total.words = n(), 
                                               house = case_when(
                                                 grepl('gryffindor', word) ~ 'Gryffindor',
                                                 grepl('hufflepuff', word) ~ 'Hufflepuff',
                                                 grepl('ravenclaw', word) ~ 'Ravenclaw',
                                                 grepl('slytherin', word) ~ 'Slytherin'),
                                               rank = row_number()) 


# set factor to keep books in order of publication
series$book <- factor(series$book, levels = (titles))

house.summary <- series %>% group_by(book, house, total.words) %>% 
  summarise(house.n = n()) %>%
  mutate(house.prop.index = house.n/(total.words/10000))

house.summary <- house.summary %>% arrange(desc(book)) 
house.summary


## plot it: basic bar chart

ggplot(house.summary %>% filter(is.na(house) == F), 
              aes(x = factor(book, levels = rev(levels(book))), 
                  y = house.prop.index)) + 
  geom_bar(stat = 'identity', aes(fill = house)) + 
  facet_wrap( ~ house, nrow = 1) + 
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c('#7F0909', '#ecb939', '#222f5b', '#1a472a')) +
  geom_hline(yintercept = 0, color = 'grey', size = 1) +
  labs(x = '', 
       y = '\nwords per 10,000 words', 
       title = '         \n') +
  theme(plot.background = element_rect(fill = "white"), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 10), 
        axis.title = element_text(size = 10, color = 'gray26'),
        plot.title = element_text(size = 18, face = 'bold'),
        strip.text = element_text(size = 13, face = 'bold'),
        legend.position = 'none', 
        panel.background = element_blank(),
        plot.caption = element_text(hjust = -.01, color = 'gray47', size = 9))

ggsave('barplot.png', width = 8, height = 3)

## plot it: barcode/segment plot (https://twitter.com/christinezhang/status/995441946599534592)
## code inspired by: https://lsru.github.io/tv_course/TD_ggplot2_solution.html
## to be continued

ggplot(series %>% filter(is.na(house) ==  F), 
               aes(x = rank/total.words, xend = rank/total.words, 
                   y = 2, yend = 1, colour = house)) + 
  geom_segment(alpha=.75) +
  facet_grid(house~book, switch = 'y') +
  scale_colour_manual(values = c('#7F0909', '#ecb939', '#222f5b', '#1a472a')) +
  theme(plot.background = element_rect(fill = "white"), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 10), 
        axis.title = element_text(size = 10, color = 'gray26'),
        plot.title = element_text(size = 18, face = 'bold'),
        strip.text = element_text(size = 13, face = 'bold'),
        legend.position = 'none', 
        panel.background = element_blank(),
        plot.caption = element_text(hjust = -.01, color = 'gray47', size = 9))
