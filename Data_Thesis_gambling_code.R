#installing packages
install.packages("readr")
install.packages("dplyr")
install.packages("stargazer")
install.packages("progress")
install.packages("lfe")
install.packages("lme4")
library(progress)
library(readr)
library(dplyr)
library(stargazer)
library(lfe)
library(lme4)

#download dataset
Insurance_poker <- read_csv("~/Insurance_poker.csv")

#filter out playernames that have no more than 1 observation
player_counts <- Insurance_poker %>%
  group_by(playername) %>%
  summarise(n = n())

players_more_than_once <- player_counts %>%
  filter(n > 1)

summary_n <- summary(players_more_than_once$n)

#Caculate number of observations for players >1 game
sum(players_more_than_once$n)

#new dataset with players >1 observation
filtered_data <- Insurance_poker %>%
  filter(playername %in% players_more_than_once$playername)


#Remove irrelevant variables
variabels_to_keep <- c("hand_id", "Date", "playername", "stake", "Stack", "win_prob_insurance", 
                       "expected_profit_at_showdown", "pot", "insurance_payout","My C Won.x",
                       "realized_profit", "Payout Fee", "num_hands_played", 
                       "percentage_hands_played", "avg_win_prob_realized", 
                       "num_sessions_played", "profit_per_hundred_hands_noins", 
                       "total_insurance_payout", "expected_total_profit", 
                       "insurance")
filtered_data <- filtered_data[ , variabels_to_keep]

#Create four group according to decision on insurance
filtered_data <- filtered_data %>%
  mutate(decision_category = case_when(
    # Group 1: Good decision with insurance
    insurance == 1 & realized_profit > `My C Won.x` ~ 1,
    
    # Group 2: Good decision without insurance
    insurance == 0 & realized_profit > 0 ~ 2,
    
    # Group 3: Bad decision with insurance
    insurance == 1 & realized_profit < `My C Won.x` ~ 3,
    
    # Group 4: Bad decision without insurance
    insurance == 0 & realized_profit < 0 ~ 4
  ))

#Sumarry of decision_category
group_counts <- filtered_data %>%
  count(decision_category)

print(group_counts)

#Sort dataset by player and game
filtered_data <- filtered_data %>%
  arrange(playername, Date)

#Create variable previous_decision according to decision_category
filtered_data <- filtered_data %>%
  group_by(playername) %>%
  mutate(previous_decision = lag(decision_category, order_by = Date)) %>%
  ungroup()


filtered_data <- filtered_data %>%
  mutate(group1_indicator = ifelse(decision_category == 1, 1, 0))

#Create factors for the different groups 
filtered_data <- filtered_data %>%
  mutate(
    previous_experience_group1 = ifelse(previous_decision == 1, 1, 0),
    previous_experience_group2 = ifelse(previous_decision == 2, 1, 0),
    previous_experience_group3 = ifelse(previous_decision == 3, 1, 0),
    previous_experience_group4 = ifelse(previous_decision == 4, 1, 0)
  )

#logistic model with group 1 as compare group
fixed_effect_model <- felm(insurance ~ previous_experience_group2 + previous_experience_group3 + previous_experience_group4 | playername | 0 | playername, data = filtered_data)
summary(fixed_effect_model)


# pooled groups without and with insurance
filtered_data <- filtered_data %>%
  mutate(
    pooled_group = case_when(
      previous_decision %in% c(1, 3) ~ "Insurance",    # Verzekering in vorige ronde
      previous_decision %in% c(2, 4) ~ "No-insurance", # Geen verzekering in vorige ronde
      TRUE ~ NA_character_                             # Fallback voor NA of ongeldige waarden
    )
  )
model <- glm(
  insurance ~ pooled_group,
  data = filtered_data,
  family = binomial
)

summary(model)


#create time_diff variable
filtered_data$Date <- as.POSIXct(filtered_data$Date, format="%Y/%m/%d %I:%M %p")
# Installeer en laad het progress pakket


# unique players
unique_players <- unique(filtered_data$playername)

# create progressbar
pb <- progress_bar$new(
  format = "  Verwerken [:bar] :percent in :elapsed",
  total = length(unique_players), clear = FALSE, width = 60
)

# column time_diff
filtered_data$time_diff <- NA

# Loop time diff by players
for (player in unique_players) {
  # Update progressbar
  pb$tick()
  
  # filter player and sort by date
  player_data <- filtered_data %>% 
    filter(playername == player) %>%
    arrange(Date) 
  
  # calculate time_diff
  filtered_data$time_diff[filtered_data$playername == player] <- difftime(player_data$Date, lag(player_data$Date), units = "mins")
}

summary(filtered_data$time_diff)
write.csv(filtered_data, file = "filtered_data.csv", row.names = FALSE)



######moderator model ##################
# Create interaction terms for moderator
filtered_data$time_diff <- as.numeric(filtered_data$time_diff)
filtered_data <- filtered_data %>%
  mutate(
    interaction_group1 = previous_experience_group1 * time_diff,
    interaction_group2 = previous_experience_group2 * time_diff,
    interaction_group3 = previous_experience_group3 * time_diff,
    interaction_group4 = previous_experience_group4 * time_diff
  )

filtered_data$interaction_group1[filtered_data$interaction_group1 == 0] <- NA
filtered_data$interaction_group2[filtered_data$interaction_group1 == 0] <- NA
filtered_data$interaction_group3[filtered_data$interaction_group1 == 0] <- NA
filtered_data$interaction_group4[filtered_data$interaction_group1 == 0] <- NA

#Moderator effect on relationship iv-dv
moderator_effect_model <- felm(
  formula = insurance ~ previous_experience_group2 + previous_experience_group3 + previous_experience_group4 +
    interaction_group2 + interaction_group3 + interaction_group4 |
    playername | 0 | playername,
  data = filtered_data
)


summary(moderator_effect_model)

#####create groups for moderator ####
filtered_data$time_category <- cut(
  filtered_data$time_diff,
  breaks = c(-Inf, 60, 1440, Inf),
  labels = c("same_hour", "same_day", "later"),
  right = TRUE
)

table(filtered_data$time_category)

filtered_data$time_category <- as.factor(filtered_data$time_category)

moderator_model <- felm(
  formula = insurance ~ previous_experience_group2 + previous_experience_group3 + previous_experience_group4 +
    time_category + 
    previous_experience_group2:time_category +
    previous_experience_group3:time_category +
    previous_experience_group4:time_category |
    playername | 0 | playername,
  data = filtered_data
)

summary(moderator_model)

#create better looking table
stargazer(
  moderator_model, 
  type = "text",  # Kies "html" of "latex" voor andere formaten
  title = "Moderation Effect of Time Category",
  dep.var.labels = "Probability of Choosing Insurance",
  covariate.labels = c(
    "Previous Experience: Group 2", 
    "Previous Experience: Group 3", 
    "Previous Experience: Group 4",
    "Time: Same Day", 
    "Time: Later",
    "Group 2 x Same Day", 
    "Group 2 x Later",
    "Group 3 x Same Day", 
    "Group 3 x Later",
    "Group 4 x Same Day", 
    "Group 4 x Later"
  ),
  out = "moderation_table.txt"  # Of sla op als "moderation_table.html" voor HTML-output
)
#####check why no effect ##

filtered_data <- filtered_data %>%
  mutate(
    interaction_group2 = previous_experience_group2 * time_diff,
    interaction_group3 = previous_experience_group3 * time_diff,
    interaction_group4 = previous_experience_group4 * time_diff
  )
moderator_effect_model <- felm(
  formula = insurance ~ previous_experience_group2 + previous_experience_group3 + previous_experience_group4 +
    time_diff + interaction_group2 + interaction_group3 + interaction_group4 |
    playername | 0 | playername,
  data = filtered_data
)

# Bekijk de resultaten
summary(moderator_effect_model)

#create histogram of time between games
install.packages("ggplot2")
library(ggplot2)

ggplot(filtered_data, aes(x = time_diff)) +
  geom_histogram(bins = 50, color = "black", fill = "lightblue") +
  labs(
    title = "Distribution of Time Between Games (Limited to 500 Minutes)",
    x = "Time Between Games (minutes)",
    y = "Frequency"
  ) +
  xlim(0, 500) +  # Adjust the range as needed
  theme_minimal()

#Logistic regression model 
filtered_data$previous_experience <- as.factor(filtered_data$previous_experience)

library(lfe)


#Create table
install.packages("stargazer")
library(stargazer)

stargazer(fixed_effect_model, type = "text", title = "Fixed Effects Model Results", 
          dep.var.labels = "Insurance Choice", 
          covariate.labels = c("Previous Experience Group 2", 
                               "Previous Experience Group 3", 
                               "Previous Experience Group 4", 
                               "Interaction Group 2", 
                               "Interaction Group 3", 
                               "Interaction Group 4"),
          omit.stat = c("f", "ser"), 
          digits = 3)

#logistic model with group 2 as compare group
fixed_effect_model2 <- felm(insurance ~ previous_experience_group1 + previous_experience_group3 + previous_experience_group4 | playername | 0 | playername, data = filtered_data)
summary(fixed_effect_model2)


# Ensure time_diff is numeric
filtered_data$time_diff <- as.numeric(filtered_data$time_diff)

# Create interaction terms for moderator
filtered_data$time_diff <- as.numeric(filtered_data$time_diff)
filtered_data <- filtered_data %>%
  mutate(
    interaction_group1 = previous_experience_group1 * time_diff,
    interaction_group2 = previous_experience_group2 * time_diff,
    interaction_group3 = previous_experience_group3 * time_diff,
    interaction_group4 = previous_experience_group4 * time_diff
  )

filtered_data$interaction_group1[filtered_data$interaction_group1 == 0] <- NA
filtered_data$interaction_group2[filtered_data$interaction_group1 == 0] <- NA
filtered_data$interaction_group3[filtered_data$interaction_group1 == 0] <- NA
filtered_data$interaction_group4[filtered_data$interaction_group1 == 0] <- NA

#Moderator effect on relationship iv-dv
moderator_effect_model <- felm(
  formula = insurance ~ previous_experience_group2 + previous_experience_group3 + previous_experience_group4 +
    interaction_group2 + interaction_group3 + interaction_group4 |
    playername | 0 | playername,
  data = filtered_data
)

# Summarize the model
summary(moderator_effect_model)

# table
stargazer(moderator_effect_model, type = "text", title = "Moderator Effect Model Results", 
          dep.var.labels = "Insurance Choice", 
          covariate.labels = c("Previous Experience Group 2", 
                               "Previous Experience Group 3", 
                               "Previous Experience Group 4", 
                               "Interaction Group 2 (Time Diff x Group 2)", 
                               "Interaction Group 3 (Time Diff x Group 3)", 
                               "Interaction Group 4 (Time Diff x Group 4)"),
          omit.stat = c("f", "ser"), 
          digits = 3)



