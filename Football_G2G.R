library(tidyverse)

# Get the play-by-play files
pbp_files <- list.files("./")

# Load and combine files
pbp_raw <- sapply(pbp_files, read_csv, simplify = F)

# Convert to date
pbp_raw$`pbp-2016.csv`$GameDate <- as.Date(pbp_raw$`pbp-2016.csv`$GameDate, format = "%m/%d/%Y")

# Bind the rows
pbp_raw <- bind_rows(pbp_raw)

# Remove blank columns
pbp_red <- pbp_raw[,!str_detect(colnames(pbp_raw), "X[:digit:]")]
pbp_red <- pbp_red[!is.na(pbp_red$PlayType),]

## Limit to 4th and goal and two point conversion situations
# 4th and Goal
pbp_4ng <- pbp_red %>%
  filter(Down == 4 & ToGo == 100 - YardLine) %>%
  filter(!PlayType %in% c("FIELD GOAL", "NO PLAY", "PENALTY", "QB KNEEL"), IsNoPlay == 0) 

# 2 pt conversion
pbp_2pt <- pbp_red %>%
  filter(IsTwoPointConversion == 1) %>%
  mutate(Description = str_replace_all(Description, "PASS FORMATION", ""),
         Pass = str_detect(Description, "PASS|SACK"), Rush = str_detect(Description, "RUSH")) %>%
  mutate(IsPass = ifelse(Pass == T, 1, 0), IsRush = ifelse(Rush == T, 1, 0)) %>%
  mutate(ToGo = YardLineFixed, IsTouchdown = IsTwoPointConversionSuccessful) %>%
  select(-Rush, -Pass)

# Combine 4th and goals with 2 pt conversions
pbp_all_tg <- bind_rows(pbp_2pt, pbp_4ng) 

### Summarise 4th and Goals
all_sum <- pbp_all_tg %>%
  group_by(ToGo) %>%
  summarise(Success = mean(IsTouchdown), Attempts = n())

# Get percent rate in readable form
all_sum$`Success Rate` <- paste0(100*round(all_sum$Success, 2), "%")

# Create table graphic
all_sum %>%
  filter(ToGo <= 15) %>%
  select(`Yard Line` = ToGo, `Success Rate`, Attempts) %>%
  gridExtra::grid.table(rows = NULL)+
  gridExtra::ttheme_minimal()

# Create scatter plot for success rates
all_sum %>%
  filter(ToGo <= 15) %>%
  ggplot(aes(x = ToGo, y = Success, weight = Attempts))+
  geom_smooth(se = F)+  
  geom_point(aes(size = Attempts))+
  labs(x = "Yard Line", y = "Success Rate", title = "Success Rate from Each Yard Line")+
  scale_y_continuous(breaks = c(.1, .3,.5),
                     labels = c("10%", "30%", "50%"))+
  scale_x_continuous(breaks = c(2, 5, 10, 15))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

# Get percentage of success rates for attempts between 4 and 7 yards
pbp_all_tg %>%
  filter(ToGo < 8, ToGo >= 4) %>%
  summarise(Success = mean(IsTouchdown), Attempts = n())

# Get the play-by-play files
dvoa_2013 <- read_csv(".././DVOA/DVOA 2013.csv")[,c("TEAM", "OFFENSE", "DEFENSE", "Year")]
dvoa_2014 <- read_csv(".././DVOA/DVOA 2014.csv")[,c("TEAM", "OFFENSE", "DEFENSE", "Year")]
dvoa_2015 <- read_csv(".././DVOA/DVOA 2015.csv")[,c("TEAM", "OFFENSE", "DEFENSE", "Year")]
dvoa_2016 <- read_csv(".././DVOA/DVOA 2016.csv")[,c("TEAM", "OFFENSE", "DEFENSE", "Year")]
dvoa_2017 <- read_csv(".././DVOA/DVOA 2017.csv")[,c("TEAM", "OFFENSE", "DEFENSE", "Year")]
dvoa_2018 <- read_csv(".././DVOA/DVOA 2018.csv")[,c("TEAM", "OFFENSE", "DEFENSE", "Year")]
dvoa_2019 <- read_csv(".././DVOA/DVOA 2019.csv")[,c("TEAM", "OFFENSE", "DEFENSE", "Year")]

# Combine DVOA Rating
dvoa <- bind_rows(dvoa_2013, dvoa_2014) %>%
  bind_rows(dvoa_2015) %>%
  bind_rows(dvoa_2016) %>%
  bind_rows(dvoa_2017) %>%
  bind_rows(dvoa_2018) %>%
  bind_rows(dvoa_2019)
  
### Change DVOA/pbp codings to get teams to match up
# Code Jaguars
dvoa$TEAM[dvoa$TEAM == "JAC"] <- "JAX"

# Code Chargers
dvoa$TEAM[dvoa$TEAM %in% c("LACH", "SD")] <- "LAC"
pbp_all_tg$OffenseTeam[pbp_all_tg$OffenseTeam == "SD"] <- "LAC"
pbp_all_tg$DefenseTeam[pbp_all_tg$DefenseTeam == "SD"] <- "LAC"

# Code Rams
dvoa$TEAM[dvoa$TEAM %in% c("LAR", "LARM", "STL")] <- "LA"

# Get offense and defense rating
pbp_all_tg <- left_join(pbp_all_tg, dvoa[,c("TEAM", "OFFENSE", "Year")], 
                        by = c("SeasonYear" = "Year", "OffenseTeam" = "TEAM"))

pbp_all_tg <- left_join(pbp_all_tg, dvoa[,c("TEAM", "DEFENSE", "Year")], 
                        by = c("SeasonYear" = "Year", "DefenseTeam" = "TEAM"))

# Code as numeric
pbp_all_tg$OFFENSE <- as.numeric(str_replace(pbp_all_tg$OFFENSE, "%", ""))
pbp_all_tg$DEFENSE <- as.numeric(str_replace(pbp_all_tg$DEFENSE, "%", ""))

# Get the difference between offense and defense rating
pbp_all_tg$DVOA_Diff <- pbp_all_tg$OFFENSE + pbp_all_tg$DEFENSE

### Summarise 4th and Goals
all_sum_dvoa <- pbp_all_tg %>%
  group_by(ToGo) %>%
  summarise(Success = mean(IsTouchdown), Attempts = n(),
            `Mean Offense DVOA` = mean(OFFENSE),
            `Mean Defense DVOA` = mean(DEFENSE), 
            `Mean Difference in DVOA` = mean(DVOA_Diff))

# Get percent rate in readable form
all_sum_dvoa$`Success Rate` <- paste0(100*round(all_sum_dvoa$Success, 2), "%")
all_sum_dvoa$`Mean Difference in DVOA` <- paste0(round(all_sum_dvoa$`Mean Difference in DVOA`, 2), "%")

# Get grid plot
all_sum_dvoa %>%
  filter(ToGo <= 15) %>%
  select(`Yard Line` = ToGo, `Success Rate`, `Mean Difference in DVOA`, Attempts) %>%
  gridExtra::grid.table(rows = NULL)+
  gridExtra::ttheme_minimal()
