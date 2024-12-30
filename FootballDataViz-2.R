
library(worldfootballR)
library(dplyr)


ligue1_shot_location <- understat_league_season_shots(league = "Ligue 1", season_start_year = 2024)
epl_shot_location <- understat_league_season_shots(league = "EPL", season_start_year = 2024)
laliga_shot_location <- understat_league_season_shots(league = "La liga", season_start_year = 2024)
bundesliga_shot_location <- understat_league_season_shots(league ="Bundesliga", season_start_year = 2024)
seriea_shot_location <- understat_league_season_shots(league = "Serie A", season_start_year = 2024)


data <- data_shot_location

data <- data %>%
  filter(result != "OwnGoal")


# Gol olup olmadığını kontrol eden bir sütun ekleme
data$goal <- data$result == "Goal"


data <- data %>%
  mutate(minute = as.numeric(minute))


# Situation ve Result'a göre gruplandırma
grouped_situation_result <- data %>%
  filter(situation != "Penalty") %>%
  group_by(situation, result) %>%
  summarise(
    mean_xG = round(mean(xG, na.rm = TRUE), 2) # xG değerini yuvarlama
  ) %>%
  ungroup()

# Bubble chart oluşturma
ggplot(grouped_situation_result, aes(x = situation, y = result, size = mean_xG, fill = result)) +
  geom_point(shape = 21, color = "black", alpha = 0.7) +
  geom_text(aes(label = mean_xG), vjust = -0, size = 3) + # xG değerlerini yazdırma
  scale_size(range = c(5, 20), name = "Mean xG") +
  scale_fill_brewer(palette = "Set2", name = "Result") +
  labs(
    title = "Mean xG by Situation and Result",
    x = "Situation",
    y = "Result"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right"
  )













