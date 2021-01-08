library(tidyverse)  
library(httr) 
library(jsonlite)
library(lubridate)
library(paletteer)
library(extrafont)
library(sf)
library(cowplot)
library(prismatic)
library(here)
library(magick)
library(ggtext)
library(readr)
library(janitor)

#custom theme
theme_owen <- function () { 
  theme_minimal(base_size=12, base_family="Gill Sans MT") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite"),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )
}


#read in pouplation data. add abbreviations 
pop <- read_csv("popdata.csv")
colnames(pop) <- c("StateName", "population")
pop$state <- c(state.abb, "DC", "PR")

#Get current COVID data from Johns Hopkins
jhu_confirmed_cases_wide <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>%
  clean_names() %>%
  dplyr::filter(country_region == "US")

jhu_confirmed_cases_county_level <- jhu_confirmed_cases_wide %>%
  dplyr::filter(str_detect(province_state,",|Princess")&str_detect(province_state,"County|Princess|D.C.")) %>% # need to add dplyr::filter statement to select only county-level and cruise ship data
  pivot_longer(matches("x"),
               names_to="dates",
               values_to = "case_count")%>%
  group_by(province_state) %>%
  mutate(date_count = row_number()) %>%
  ungroup() %>%
  mutate(date_str=str_replace_all(dates,"x","")) %>%
  mutate(date_str=str_replace_all(date_str,"_","/")) %>%
  mutate(date_fmt = as.Date(date_str,format="%m/%d/%y")) %>%
  dplyr::filter(date_fmt<=as.Date("2020-03-09")) %>% # only collect data before 3/10/20 in this dataframe - will be using state-level summary data after this date
  mutate(size=case_when(case_count < 10 ~ 1,
                        case_count < 50 ~ 2,
                        case_count < 100 ~ 3,
                        case_count < 200 ~ 4,
                        T  ~ 5))

# Animated map
jhu_confirmed_cases_state_level <- jhu_confirmed_cases_wide %>%
  dplyr::filter(!str_detect(province_state,",")) %>% # need to add dplyr::filter statement to select only state-level and cruise ship data - avaiable on and after 3/10/2020
  pivot_longer(matches("x"),
               names_to="dates",
               values_to = "case_count")%>%
  group_by(province_state) %>%
  mutate(date_count = row_number()) %>%
  ungroup() %>%
  mutate(date_str=str_replace_all(dates,"x","")) %>%
  mutate(date_str=str_replace_all(date_str,"_","/")) %>%
  mutate(date_fmt = as.Date(date_str,format="%m/%d/%y")) %>%
  dplyr::filter(date_fmt>=as.Date("2020-03-10")) %>% # only collect data from 3/10/20 and onward in this dataframe - will be using state-level summary data 
  mutate(size=case_when(case_count < 10 ~ 1,
                        case_count < 50 ~ 2,
                        case_count < 100 ~ 3,
                        case_count < 200 ~ 4,
                        T  ~ 5))


jhu_confirmed_cases <- jhu_confirmed_cases_county_level %>%
  bind_rows(jhu_confirmed_cases_state_level) %>%
  mutate(scaled_case_count = (case_count/(max(case_count))))


max_case_count = max(jhu_confirmed_cases$case_count)
max_size_point =   (max_case_count/10000) * 100

# Define new dataframe which says how long each date should be
date_augment_table <- jhu_confirmed_cases %>% 
  distinct(date_fmt) %>%
  arrange(date_fmt) %>%
  mutate(timesteps_amp = case_when(date_fmt < as.Date("2020-03-04") ~ 1,
                                   date_fmt < as.Date("2020-03-12") ~ 8,
                                   T ~ 16),
         timestep_cumsum = cumsum(timesteps_amp)) %>%
  mutate(timestep_amp_lag = lag(timesteps_amp,1),
         timestep_amp_lag = ifelse(is.na(timestep_amp_lag),0,timestep_amp_lag),
         timestep_cumsum_lag = cumsum(timestep_amp_lag))

transition_points <- date_augment_table %>%
  filter(timesteps_amp!=timestep_amp_lag) %>%
  left_join(date_augment_table %>% 
              count(timesteps_amp,name="number_of_days"),
            by="timesteps_amp")

first_transition_frame = transition_points$timestep_cumsum_lag[2]
first_transition_days_passed = transition_points$number_of_days[1]

second_transition_frame = transition_points$timestep_cumsum_lag[3]
second_transition_days_passed = sum(transition_points$number_of_days[1:2])

# Join new dataframe which defines the length of time step for each date to JHU dataframe

jhu_confirmed_cases_time_augmentation <- jhu_confirmed_cases %>%
  left_join(date_augment_table,
            by="date_fmt") %>%
  mutate(timesteps_amp_2 = timesteps_amp) %>%
  uncount(timesteps_amp_2) %>%
  group_by(date_fmt,province_state) %>%
  mutate(timestep = row_number()) %>%
  ungroup() %>%
  mutate(timestep_aug = timestep_cumsum_lag+timestep-1) %>%
  arrange(province_state,timestep_aug) %>%
  mutate(calc_date = as.Date(case_when(timestep_aug <= first_transition_frame  ~ as.Date("2020-01-22") + trunc(timestep_aug) - 1,
                                       timestep_aug <= second_transition_frame ~ as.Date("2020-01-22") + first_transition_days_passed + (trunc((timestep_aug-first_transition_frame)/8)),
                                       T ~ as.Date("2020-01-22") + second_transition_days_passed + (trunc((timestep_aug-second_transition_frame)/16))),origin="1970-01-01"))

states <- st_as_sf(maps::map(database = "state",plot=F,fill=T)) 

map_plot_aug <- ggplot() +
  geom_sf(data = states,
          fill = "white") +
  geom_point(data = jhu_confirmed_cases_time_augmentation %>%
               dplyr::filter(long > -140) %>%
               dplyr::filter(!str_detect(province_state, "Unassigned")) %>%
               dplyr::filter(case_count > 0),
             mapping = aes(x = long,
                           y = lat,
                           group = province_state,
                           size = scaled_case_count),
             fill = "red",
             alpha = 0.35,
             shape = 21) +
  scale_size_continuous(breaks = function(x) c(10,
                                               100,
                                               200,
                                               550)/max_case_count,
                        labels = function(x) round_half_up(x*max_case_count),
                        range = c(3,3+2*max_size_point))+
  scale_x_continuous(expand = expand_scale(mult=c(0.05,0.75))) +
  labs(title = "Distribution of U.S. Confirmed COVID-19 Cases",
       subtitle = "All Cases from All Dates Shown at Same Time",
       x = "",
       y = "",
       size="# of Confirmed\nCases") +
  coord_sf(xlim = c(-125,-63)) +
  theme_bw() +
  theme(legend.position = c(0.90,0.3),
        title=element_text(size = 20),
        legend.title = element_text(size = 14),
        legend.background = element_rect(color="black",fill="NA"),
        legend.text = element_text(size=12))
# Initial Plot
map_plot_aug

# Plot animation
map_plot_w_animate_aug <- map_plot_aug +
  labs(title = "Distribution of U.S. Confirmed COVID-19 Cases",
       subtitle = "Date: {as.Date(case_when(frame_time<=first_transition_frame ~ as.Date('2020-01-22') + trunc(frame_time) - 1,
  frame_time<=second_transition_frame ~ as.Date('2020-01-22') + first_transition_days_passed + (trunc((frame_time-first_transition_frame)/8)),
  T ~ as.Date('2020-01-22') + second_transition_days_passed + (trunc((frame_time-second_transition_frame)/16))),
  origin='1970-01-01')}") +
  transition_time(timestep_aug) +
  ease_aes()
# Create gganimation
map_overtime_plot <- animate(map_plot_w_animate_aug,
                             duration = 10,
                             fps = 10,
                             width = 650,
                             height = 400, 
                             renderer = gifski_renderer())

map_overtime_plot

# Line graph over time

sum_ov_time_df <- jhu_confirmed_cases_time_augmentation %>%
  group_by(date_fmt,timestep_aug,calc_date) %>%
  summarise(case_total = sum(case_count,na.rm=T)) %>%
  ungroup()

# Covid static line over time

plot_line_ov_time <-sum_ov_time_df %>%
  ggplot(aes(x=date_fmt,
             y=case_total,
             color=timestep_aug))+
  geom_line(size=3)+
  geom_label(aes(label=case_total),
             size=12,
             fontface="bold")+
  scale_y_continuous(breaks = seq(0,10000,200),
                     expand = expand_scale(mult=c(0.07,0.11)))+
  scale_x_date(date_breaks = "1 week",
               labels= scales::date_format("%m/%d"),
               expand = expand_scale(mult=c(0.05,0.105)),
               limits=c(min(sum_ov_time_df$date_fmt),max(sum_ov_time_df$date_fmt)))+
  scale_color_gradient(low="black",high="red")+
  labs(title="# of U.S. Confirmed COVID-19 Cases Overall",
       x="",
       y="# of Confirmed Cases") +
  guides(color=F)+
  theme_bw()+
  theme(legend.position = c(0.92,0.3),
        axis.text.x =  element_text(size=20,
                                    face="bold"),
        axis.text.y =  element_text(size=20,
                                    face="bold"),
        title=element_text(size=20))
plot_line_ov_time

# gganimating covid line over time

plot_line_ov_time_w_animate <- plot_line_ov_time + 
  labs(subtitle = "Date: {as.Date(case_when(frame_along<=first_transition_frame ~ as.Date('2020-01-22') + trunc(frame_along) - 1,
  frame_along<=second_transition_frame ~ as.Date('2020-01-22') + first_transition_days_passed + (trunc((frame_along-first_transition_frame)/8)),
  T ~ as.Date('2020-01-22') + second_transition_days_passed + (trunc((frame_along-second_transition_frame)/16))),
  origin='1970-01-01')}")+
  transition_reveal(timestep_aug)+
  shadow_mark()

# Create gganimation

line_overtime_gif <- animate(plot_line_ov_time_w_animate,
                             duration = 10,
                             fps = 10,
                             width = 600,
                             height = 400, 
                             renderer = gifski_renderer())
line_overtime_gif

# Making a combined GIF of two plots

a_mgif <- image_read(map_overtime_plot)
b_mgif <- image_read(line_overtime_gif)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 2:100){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif <- c(new_gif, combined)
}

new_gif
