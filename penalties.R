#load necessary packages
library(lattice)
library(BSDA)
library(cli)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggimage)
library(ggalt)
library(nflfastR)
library(nflreadr)
library(rstudioapi)
library(tidyverse)
options(scipen = 9999)

setwd(dirname(getActiveDocumentContext()$path))
data <- nflfastR::load_pbp(2011:2022)
data <- data %>%
  mutate(
    penalty_team = case_when(
      penalty_team == 'LV' ~ 'OAK',
      penalty_team == 'SD' ~ 'LAC',
      penalty_team == 'STL' ~ 'LA',
      TRUE ~ penalty_team
    ),
    posteam = case_when(
      posteam == 'LV' ~ 'OAK',
      posteam == 'SD' ~ 'LAC',
      posteam == 'STL' ~ 'LA',
      TRUE ~ posteam
    ),
    team_szn = paste0(penalty_team, season)
  )

#get postseason data by team and game
post <- data %>%
  filter(season_type != "REG", penalty == 1) %>%
  group_by(penalty_team, season, week) %>%
  summarize(penalty_yards = sum(penalty_yards),
            penalties = n()) %>%
  ungroup()

#get all postseason teams and weeks (because sometimes refs call nada)
post_teams<- data %>%
  filter(season_type != "REG") %>%
  distinct(posteam, season, week) %>%
  rename(penalty_team = posteam) %>%
  filter(!is.na(penalty_team))

#join post penalty info to post_teams
post <- post_teams %>%
  left_join(post, by = c("penalty_team" = "penalty_team", "season" = "season",
                         "week" = "week")) %>%
  replace_na(list(penalty_yards = 0, penalties = 0))

#get regular season data by team and season
reg <- data %>%
  filter(season_type == "REG", penalty == 1) %>%
  group_by(penalty_team, season) %>%
  summarize(penalty_yards = sum(penalty_yards),
            penalties = n(),
            weeks = n_distinct(week)) %>%
  ungroup()

#get regular season total averages
reg <- reg %>%
  mutate(penalty_yards = round((penalty_yards / weeks), 2),
         penalties = round((penalties / weeks), 2))

#get the average # of penalties and yards for reg and post
cat("Avg # of penalties per team postseason", mean(post$penalties))
cat("Avg # of penalty yards per team postseason", mean(post$penalty_yards))
cat("Avg # of penalties per team regular season", mean(reg$penalties))
cat("Avg # of penalty yards per team regular season", mean(reg$penalty_yards))

#join regular and postseason data
penalty_stitch <- post %>% left_join(reg, by = c(
  "penalty_team" = "penalty_team", "season" = "season"
  )) %>%
  select(-c(weeks)) %>%
  rename(team = penalty_team, penalty_yards = penalty_yards.x,
         penalties = penalties.x, penalty_yards_reg_avg = penalty_yards.y,
         penalties_reg_avg = penalties.y)

#create differences values
penalty_stitch <- within(penalty_stitch,
                         {yards_diff <- penalty_yards - penalty_yards_reg_avg;
                         penalty_diff <- penalties - penalties_reg_avg})

#get averages for data grouped by season
season_avgs <- penalty_stitch %>%
  group_by(season) %>%
  summarize(penalty_yards = mean(penalty_yards),
            penalties = mean(penalties),
            penalty_yards_reg_avg = mean(penalty_yards_reg_avg),
            penalties_reg_avg = mean(penalties_reg_avg),
            penalty_diff = mean(penalty_diff),
            yards_diff = mean(yards_diff)
            )
season_avgs$season <- as.factor(season_avgs$season)

#So the column and lines can have different colors
season_avgs <- within(season_avgs,
                      {penalty_colour <- ifelse(penalty_diff > 0,
                                                "darkorange", "turquoise4");
                      yards_colour <- ifelse(yards_diff > 0,
                                             "darkorange", "turquoise4") })

#view the means for the playoff team data
cat("Avg # of penalties per team postseason",
    mean(penalty_stitch$penalties))
cat("Avg # of penalty yards per team postseason",
    mean(penalty_stitch$penalty_yards))
cat("Avg # of penalties per team regular season",
    mean(penalty_stitch$penalties_reg_avg))
cat("Avg # of penalty yards per team regular season",
    mean(penalty_stitch$penalty_yards_reg_avg))

#perform Z-tests for penalties and penalty yards
#first we need the SD for the variables being tested
sd_reg_pen = sd(penalty_stitch$penalties_reg_avg)
sd_post_pen = sd(penalty_stitch$penalties)
sd_reg_yds = sd(penalty_stitch$penalty_yards_reg_avg)
sd_post_yds = sd(penalty_stitch$penalty_yards)
#now the Z-tests
z.test(x = penalty_stitch$penalties_reg_avg, y = penalty_stitch$penalties,
       sigma.x = sd_reg_pen, sigma.y = sd_post_pen)

z.test(x = penalty_stitch$penalty_yards_reg_avg, sigma.x = sd_reg_yds,
       y = penalty_stitch$penalty_yards, sigma.y = sd_post_yds)

#####make the plot for # of penalties
season_avgs %>%
  ggplot() +
  geom_segment(data=season_avgs, aes(y = season, yend = season, x=4, xend=7),
               color="gray85", size=0.15) +
  ggalt::geom_dumbbell(aes(y = season, x = penalties, xend = penalties_reg_avg),
                       size = 2, size_x = 5, size_xend = 5,
                       color = season_avgs$penalty_colour,
                       colour_x = "darkorange", colour_xend = "turquoise4") +
  geom_text(data=filter(season_avgs, season==2022),
            aes(x=penalties, y=season, label="Post"),
            color="darkorange2", size=3.5, vjust=-1.5, fontface="bold") +
  geom_text(data=filter(season_avgs, season==2022),
            aes(x=penalties_reg_avg, y=season, label="Regular"),
            color="turquoise4", size=3.5, vjust=-1.5, fontface="bold") +
  geom_text(data = season_avgs, color = "darkorange2", size=3.5, vjust=2,
            aes(x = penalties, y = season,
                label = sprintf("%0.2f", round(penalties, digits = 2)))) +
  geom_text(data = season_avgs, color = "turquoise4", size=3.5, vjust=2,
            aes(x = penalties_reg_avg, y = season,
                label = sprintf("%0.2f", round(penalties_reg_avg,
                                               digits = 2)))) +
  #Differences section
  geom_rect(data = season_avgs,
            aes(xmin = 7, xmax = 7.5, ymin = -Inf, ymax = Inf), fill="grey") +
  geom_text(data = season_avgs,  fontface="bold", size=4,
            color = season_avgs$penalty_colour,
            aes(label=sprintf("%0.2f", abs(round(penalty_diff, digits = 2))),
                y = season, x=7.25)) +
  geom_text(data=filter(season_avgs, season==2022), 
            aes(x = 7.25, y = season, label = "Difference"),
            color="black", size=5, vjust=-1.5, fontface="bold") +
  labs(x = 'Penalties',
       y = 'Seasons',
       title = 'Penalties Called in Regular Season VS Postseason',
       subtitle = paste('Differences measured by subtracting postseason',
                        'penalties called by the team\'s regular season',
                        'average.', collapse = ","),
       caption = "Data: @nflfastR"
  ) +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 30, hjust = .5, face = 'bold'),
    plot.subtitle = element_text(size = 20, hjust = .5),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.caption = element_text(size = 12, vjust = 7, hjust = 0.95),
    axis.title.y = element_text(size = 24, vjust = 10),
    axis.title.x = element_text(size = 24, vjust = 2)
  ) +
  scale_x_continuous()
ggsave(path = "plots", filename = "penalties.png", width = 16, height = 9,
       dpi = 80)

#####make the plot for penalty yards
season_avgs %>%
  ggplot() +
  geom_segment(data=season_avgs, aes(y = season, yend = season, x=33, xend=62),
               color="gray85", size=0.15) +
  ggalt::geom_dumbbell(aes(y = season, x = penalty_yards,
                           xend = penalty_yards_reg_avg), size = 2,
                       size_x = 5, size_xend = 5,
                       color = season_avgs$yards_colour,
                       colour_x = "darkorange", colour_xend = "turquoise4") +
  geom_text(data=filter(season_avgs, season==2022),
            aes(x=penalty_yards, y=season, label="Post"),
            color="darkorange2", size=3.5, vjust=-1.5, fontface="bold") +
  geom_text(data=filter(season_avgs, season==2022),
            aes(x=penalty_yards_reg_avg, y=season, label="Regular"),
            color="turquoise4", size=3.5, vjust=-1.5, fontface="bold") +
  geom_text(data = season_avgs,
            aes(x = penalty_yards, y = season,
                label = sprintf("%0.2f", round(penalty_yards, digits = 2))),
            color = "darkorange2", size=3.5, vjust=2) +
  geom_text(data = season_avgs, color = "turquoise4", size=3.5, vjust=2,
            aes(x = penalty_yards_reg_avg, y = season,
                label = sprintf("%0.2f", round(penalty_yards_reg_avg, digits = 2
                                               )))) +
  #create Differences section of viz
  geom_rect(data = season_avgs, aes(xmin = 60, xmax = 64, ymin = -Inf,
                                    ymax = Inf), fill="grey") +
  geom_text(data = season_avgs,  fontface="bold", size=4,
            color = season_avgs$yards_colour,
            aes(label=sprintf("%0.2f", abs(round(yards_diff, digits = 2))),
                y = season, x=62)) +
  geom_text(data=filter(season_avgs, season==2022), 
            aes(x = 62, y = season, label = "Difference"),
            color="black", size=5, vjust=-1.5, fontface="bold") +
  labs(x = 'Penalty Yards',
       y = 'Seasons',
       title = 'Penalty Yards in Regular Season VS Postseason',
       subtitle = paste('Differences measured by subtracting postseason',
                        'penalties yards by the team\'s regular season',
                        'average.', collapse = ","),
       caption = "Data: @nflfastR"
  ) +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 30, hjust = .5, face = 'bold'),
    plot.subtitle = element_text(size = 20, hjust = .5),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.caption = element_text(size = 12, vjust = 7, hjust = 0.95),
    axis.title.y = element_text(size = 24, vjust = 10),
    axis.title.x = element_text(size = 24, vjust = 2)
  ) +
  scale_x_continuous()
ggsave(path = "plots", filename = "penalty_yds.png", width = 16, height = 9,
       dpi = 80)


##### performing time series and statistical analysis
#postseason teams
team_seasons <- post %>% left_join(reg, by = c("penalty_team" = "penalty_team",
                                               "season" = "season"))
team_seasons <- unique(team_seasons[c("penalty_team", "season")])
team_seasons <- paste0(team_seasons$penalty_team, team_seasons$season)
select_data <- data %>% filter(team_szn %in% team_seasons)

#group the data
time_series_data <- select_data %>%
  group_by(penalty_team, season, week, season_type) %>%
  summarize(pen_yards = sum(penalty_yards),
            penalties = n()) %>%
  ungroup()

ts_week_mean <- time_series_data %>%
  group_by(week, season_type) %>%
  summarise(pen_yards = mean(pen_yards),
            penalties = mean(penalties),
            sample_size = n())

#fix 2021 bonus week
#do we want this?
#time_series_data <- select_data %>%
#  mutate(
#    week = case_when(
#      season >= 2021 & week > 17 ~ as.integer(week - 1),
#      week = case_when(
#        season >= 2021 & week > 17 ~ as.integer(week - 1),
#        TRUE ~ week
#      )
#    )

#testing for collinearity before fitting linear model
time_series_data <- time_series_data %>%
  mutate(
    season_typePOST = ifelse(season_type=='POST', 1, 0)
  )

cor.test(time_series_data$week, time_series_data$season_typePOST)

ts_week_mean <- ts_week_mean %>%
  mutate(
    season_typePOST = ifelse(season_type=='POST', 1, 0)
  )

#Collinearity is found between week and season_type (expected!)
#What does this mean for the upcoming regression and how it can be interpreted?
#uncertainty of coefficient estimates
#increases standard error, which causes a decline in t-stat
#This increases the likelihood to fail to reject the null hypothesis, and cannot
#as reliably lean on coefficients for scope of impact

#linear model on penalty yards
lm.pen.yds <- lm(formula = pen_yards ~ week + season_typePOST,
                 data = time_series_data)
summary(lm.pen.yds)

pen_yds_predict <- cbind(ts_week_mean,
                         predict(lm.pen.yds, interval = 'confidence',
                                 newdata = ts_week_mean))

#####penalty yards time series plot
ggplot(pen_yds_predict, aes(x=week, y=pen_yards, color = season_type)) +
  geom_point(aes(size = sample_size)) +
  geom_line() +
  scale_color_manual(values = c("REG" = "turquoise4", "POST" = "darkorange2")) +
  #geom_line(aes(week, fit, color = "black")) +
  #geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 30, hjust = .5, face = 'bold'),
    plot.caption = element_text(size = 12, vjust = 9),
    axis.title.y = element_text(size = 24),
    axis.title.x = element_text(size = 24),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
  ) +
  labs(x = 'Week',
       y = 'Penalty Yards',
       title = 'Average Penalty Yards of Playoff Teams By Week',
       caption = "Data: @nflfastR")
ggsave(path = "plots", filename = "penalty_yds_ts.png", width = 16, height = 9,
       dpi = 80)

#linear model on penalties
lm.pens <- lm(formula = penalties ~ week + season_typePOST,
              data = time_series_data)
summary(lm.pens)

ts_week_mean <- cbind(ts_week_mean,
                      predict(lm.pens, interval = 'confidence',
                              newdata = ts_week_mean))

#####count penalties time series plot
ggplot(pen_yds_predict, aes(x=week, y=penalties, color = season_type)) +
  geom_point(aes(size = sample_size)) +
  geom_line() +
  scale_color_manual(values = c("REG" = "turquoise4", "POST" = "darkorange2")) +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 30, hjust = .5, face = 'bold'),
    plot.caption = element_text(size = 12, vjust = 9),
    axis.title.y = element_text(size = 24),
    axis.title.x = element_text(size = 24),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
  ) +
  labs(x = 'Week',
       y = 'Penalties',
       title = 'Average Penalties of Playoff Teams By Week',
       caption = "Data: @nflfastR")
ggsave(path = "plots", filename = "penalties_ts.png", width = 16, height = 9,
       dpi = 80)