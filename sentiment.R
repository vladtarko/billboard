library(tidyverse)
library(sentimentr)
theme_set(theme_light(base_size = 20))

# load Billboard 100 music lyrics data
music <- rio::import("https://raw.githubusercontent.com/walkerkq/musiclyrics/master/billboard_lyrics_1964-2015.csv")

lyrics <- music %>% pull(Lyrics)

sentiment <- sentiment(lyrics)
emotion   <- emotion(lyrics)
profanity <- profanity(lyrics)

music_analyzed <- cbind(music, 
                         sentiment %>% select(word_count, sentiment), 
                         emotion %>% select(emotion_type, emotion_count, emotion),
                         profanity %>% select(profanity_count, profanity))

# summaries -------------------------------------------------------------------

music_summary <- music_analyzed %>% 
  group_by(Year) %>% 
  summarise(sentiment = mean(sentiment, na.rm = TRUE),
            emotion   = mean(emotion, na.rm = TRUE),
            profanity = mean(profanity, na.rm = TRUE))

emotions_summary <- music_analyzed %>% 
  group_by(Year, emotion_type) %>% 
  summarise(emotion   = mean(emotion, na.rm = TRUE)) %>% 
  filter(!str_detect(emotion_type, "_negated"))

emotions_summary_agg <- music_analyzed %>% 
  group_by(emotion_type) %>% 
  summarise(emotion   = mean(emotion, na.rm = TRUE)) %>% 
  filter(!str_detect(emotion_type, "_negated"))


# plots ------------------------------------------------------------------------------

music_summary %>% 
  ggplot(aes(x = Year, y = sentiment)) +
  geom_smooth() +
  geom_path() +
  labs(x = "",
       y = "Mean Positivity",
       title = "Positivity in Billboard 100 Music Lyrics")

ggsave("positivity.png", width = 15, height = 10)


music_summary %>% 
  ggplot(aes(x = Year, y = profanity)) +
  geom_smooth() +
  geom_path() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "",
       y = "Mean profanity\n(% of words used that are profane)",
       title = "Profanity in Billboard 100 Music Lyrics")

ggsave("profanity.png", width = 15, height = 10)


emotions_summary %>% 
  ggplot(aes(x = Year, y = emotion, color = emotion_type)) +
  geom_smooth() +
  geom_path() +
  facet_wrap(~emotion_type, scales = "free", ncol = 4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "",
       y = "Mean emotion\n(% of words used that are emotional)",
       title = "Emotions in the Billboard 100 music lyrics") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45))

ggsave("emotions.png", width = 15, height = 10)


emotions_summary_agg %>% 
  ggplot(aes(x = emotion_type, y = emotion, fill = emotion_type)) +
  geom_col() +
  scale_color_metro_d(reverse = TRUE) +
  labs(x = "",
       y = "Freqeuncy",
       title = "Emotions in the Billboard 100 music lyrics") +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(legend.position = "none")

ggsave("emotions_summary.png", width = 15, height = 10)


