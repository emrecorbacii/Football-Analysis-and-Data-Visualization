library(tidyverse)
library(ggplot2)
library(dplyr)
library(car) # Uniform dağılım testi için
library(worldFootballR) # Veri seti
library(corrplot)

teams <- c(
  # Premier League
  "Arsenal", "Aston Villa", "Bournemouth", "Brentford", "Brighton", 
  "Burnley", "Chelsea", "Crystal Palace", "Everton", "Fulham", 
  "Liverpool", "Luton Town", "Manchester City", "Manchester United", 
  "Newcastle United", "Nottingham Forest", "Sheffield United", 
  "Tottenham Hotspur", "West Ham United", "Wolverhampton Wanderers",
  
  # La Liga
  "Alavés", "Athletic Bilbao", "Atlético Madrid", "Barcelona", 
  "Cádiz", "Celta Vigo", "Getafe", "Girona", "Granada", 
  "Las Palmas", "Mallorca", "Osasuna", "Rayo Vallecano", 
  "Real Betis", "Real Madrid", "Real Sociedad", "Sevilla", 
  "Valencia", "Villarreal",
  
  # Bundesliga
  "Augsburg", "Bayer Leverkusen", "Bayern Munich", "Bochum", 
  "Borussia Dortmund", "Borussia Mönchengladbach", "Darmstadt", 
  "Eintracht Frankfurt", "Freiburg", "Heidenheim", "Hoffenheim", 
  "Köln", "Leipzig", "Mainz", "Stuttgart", "Union Berlin", 
  "Werder Bremen", "Wolfsburg",
  
  # Serie A
  "Atalanta", "Bologna", "Cagliari", "Empoli", "Fiorentina", 
  "Frosinone", "Genoa", "Inter", "Juventus", "Lazio", 
  "Lecce", "Milan", "Monza", "Napoli", "Roma", 
  "Salernitana", "Sassuolo", "Torino", "Udinese", "Verona",
  
  # Ligue 1
  "Clermont", "Le Havre", "Lens", "Lille", "Lorient", 
  "Lyon", "Marseille", "Metz", "Monaco", "Montpellier", 
  "Nantes", "Nice", "Paris Saint-Germain", "Reims", 
  "Rennes", "Strasbourg", "Toulouse"
)




# Bir ligden tüm takımları çekmek
league_url <- "https://understat.com/league/EPL/2023"
teams <- understat_team_meta(league_url)

# Yalnızca 2023 sezonuna ait verileri filtreleme
teams_2023 <- teams %>%
  filter(year == 2023) %>%
  pull(team_url)  # Sadece url sütununu alır


player_data <- player_data %>%
  mutate(
    xG_per_minute = ifelse(time > 0, xG / time, NA),
    xA_per_minute = ifelse(time > 0, xA / time, NA),
    xGChain_per_minute = ifelse(time > 0, xGChain / time, NA),
    xGBuildup_per_minute = ifelse(time > 0, xGBuildup / time, NA)
  )




  long_data <- player_data %>%
  select(player_name, xG_per_minute, xGChain_per_minute) %>%
  pivot_longer(cols = c(xG_per_minute, xGChain_per_minute),
               names_to = "Metric", values_to = "Value")

# Boxplot ile görselleştirme
ggplot(long_data, aes(x = Metric, y = Value, fill = Metric)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  coord_cartesian(ylim = c(0, quantile(long_data$Value, 0.95, na.rm = TRUE))) + # Aykırı değerleri gizlemek için
  labs(
    title = "Comparison of xG and xGChain per Minute",
    x = "Metric",
    y = "Value per Minute"
  ) +
  theme_minimal()



# player_id sütununu çıkararak ve sadece sayısal sütunları seçerek
numeric_data <- player_data %>%
  select(-player_id) %>%
  select_if(is.numeric)  # Sayısal sütunları seçer

# Korelasyon matrisi hesaplama
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Korelasyon matrisini görselleştirme
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.cex = 0.8, title = "Correlation Matrix of Player Data")



# 270 dakikadan fazla oynayan oyuncuları filtreleme
filtered_data <- player_data %>%
  filter(time >= 270)

# xGChain için en yüksek 10 oyuncuyu sıralamak (oynama süresine göre)
top_10_xGChain <- filtered_data %>%
  arrange(desc(time)) %>%
  slice_head(n = 10)  # İlk 10 oyuncuyu seç

# Barplot: xGChain ile oynama süresini göstermek
ggplot(top_10_xGChain, aes(x = reorder(player_name, time), y = xGChain_per_minute)) +
  geom_col(aes(fill = time), show.legend = TRUE) +
  labs(title = "Top 10 Players by xGChain per Minute (270+ Minutes Played)",
       subtitle = "Sorted by Total Minutes Played",
       x = "Player",
       y = "xGChain per Minute",
       fill = "Minutes Played") +
  theme_minimal() +
  coord_flip()  # Yatay barplot



# Pozisyona göre xGChain ve xGBuildup ortalamalarını hesaplama
position_averages <- player_data %>%
  group_by(position) %>%
  summarise(
    avg_xGChain = mean(xGChain_per_minute, na.rm = TRUE),
    avg_xGBuildup = mean(xGBuildup_per_minute, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(avg_xGChain, avg_xGBuildup), 
               names_to = "metric", 
               values_to = "value")  # Uzun formata dönüştürme

# Ortalamaya göre azalan sırada sıralama
position_averages <- position_averages %>%
  mutate(position = reorder(position, value, FUN = mean))  # Pozisyonları ortalamaya göre sıralama

# Çift barplot
ggplot(position_averages, aes(x = position, y = value, fill = metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +  # Barlar arasındaki mesafe
  labs(title = "Average xGChain and xGBuildup per Minute by Position",
       x = "Position",
       y = "Average Value",
       fill = "Metric") +
  theme_minimal() +
  coord_flip()  # Yatay barplot


# Ortalama değerleri hesaplama
league_summary <- data.frame(
  League = c("EPL", "La Liga", "Serie A", "Bundesliga", "Ligue 1"),
  
  # Normal ortalamalar
  Avg_xGChain = c(mean(epl_players$xGChain, na.rm = TRUE),
                  mean(laliga_players$xGChain, na.rm = TRUE),
                  mean(seriea_players$xGChain, na.rm = TRUE),
                  mean(bundesliga_players$xGChain, na.rm = TRUE),
                  mean(ligue1_players$xGChain, na.rm = TRUE)),
  
  Avg_xGBuildup = c(mean(epl_players$xGBuildup, na.rm = TRUE),
                    mean(laliga_players$xGBuildup, na.rm = TRUE),
                    mean(seriea_players$xGBuildup, na.rm = TRUE),
                    mean(bundesliga_players$xGBuildup, na.rm = TRUE),
                    mean(ligue1_players$xGBuildup, na.rm = TRUE)),
  
  # Per minute ortalamalar
  Avg_xGChain_per_minute = c(mean(epl_players$xGChain_per_minute, na.rm = TRUE),
                             mean(laliga_players$xGChain_per_minute, na.rm = TRUE),
                             mean(seriea_players$xGChain_per_minute, na.rm = TRUE),
                             mean(bundesliga_players$xGChain_per_minute, na.rm = TRUE),
                             mean(ligue1_players$xGChain_per_minute, na.rm = TRUE)),
  
  Avg_xGBuildup_per_minute = c(mean(epl_players$xGBuildup_per_minute, na.rm = TRUE),
                               mean(laliga_players$xGBuildup_per_minute, na.rm = TRUE),
                               mean(seriea_players$xGBuildup_per_minute, na.rm = TRUE),
                               mean(bundesliga_players$xGBuildup_per_minute, na.rm = TRUE),
                               mean(ligue1_players$xGBuildup_per_minute, na.rm = TRUE))
)

# Ligleri birleştirme
epl_players$League <- "EPL"
laliga_players$League <- "La Liga"
seriea_players$League <- "Serie A"
bundesliga_players$League <- "Bundesliga"
ligue1_players$League <- "Ligue 1"

# Tek bir veri setine birleştirme
all_leagues <- rbind(epl_players, laliga_players, seriea_players, bundesliga_players, ligue1_players)



ggplot(all_leagues, aes(x = League, y = xGChain, fill = League)) +
  geom_boxplot() +
  labs(
    title = "xGChain Distribution by League",
    x = "League",
    y = "xGChain"
  ) +
  theme_minimal()

ggplot(all_leagues, aes(x = League, y = xGBuildup, fill = League)) +
  geom_boxplot() +
  labs(
    title = "xGBuildup Distribution by League",
    x = "League",
    y = "xGBuildup"
  ) +
  theme_minimal()



# Azalan sıralama ile barplot
ggplot(league_summary_long, aes(x = League, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  facet_wrap(~ Metric_Type, scales = "free_y") +
  labs(
    title = "Comparison of xGChain and xGBuildup Metrics Across Leagues (Sorted by Value)",
    subtitle = "Includes Total and Per Minute Averages, Sorted in Descending Order",
    x = "League",
    y = "Value",
    fill = "Metric"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )





  # Puan dağılımını görselleştirme
ggplot(leagues, aes(x = Puan, fill = Lig)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  labs(title = "Liglere Göre Puan Dağılımı",
       x = "Puan",
       y = "Frekans") +
  theme_minimal() +
  facet_wrap(~Lig) # Her lig için ayrı grafik









# Uniform dağılım testi için bir fonksiyon tanımlama
uniform_test <- function(data) {
  return(ks.test(data$Puan, "punif", min(data$Puan), max(data$Puan)))
}

# Her lig için testi uygulama
test_results <- leagues %>%
  group_by(Lig) %>%
  summarise(p_value = uniform_test(cur_data())$p.value)

print(test_results)




ggplot(leagues, aes(x = Puan, fill = Lig)) +
  geom_histogram(binwidth = 5, position = "dodge", alpha = 0.7) +
  labs(title = "Liglere Göre Puan Dağılımı",
       x = "Puan",
       y = "Frekans") +
  theme_minimal() +
  facet_wrap(~Lig) # Her lig için ayrı grafik



# Test sonuçlarını görselleştirme
ggplot(test_results, aes(x = Lig, y = p_value, fill = Lig)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
  labs(title = "Liglere Göre Uniform Dağılım Testi P-değerleri",
       x = "Lig",
       y = "P-değeri") +
  theme_minimal() +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") + # 0.05 eşik çizgisi
  annotate("text", x = 1:length(test_results$Lig), y = 0.06, 
           label = "Eşik: 0.05", color = "red", size = 4, hjust = 0.5)

