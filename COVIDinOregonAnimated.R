setwd("~/Projects/COVID-19")
source("COVIDinOregon.R")

animation_df <- df %>%
  dplyr::filter(date >= as.Date("2020-02-19")) %>% # First covid case in Oregon
  mutate(textColor = ifelse(cases_per_thousand >= quantile(cases_per_thousand, .75, na.rm = TRUE), 
                            "black", 
                            "white")) %>% 
  left_join(select(covidtracking_df, date, positive), by = c("date"))

#### ANIMATION BELOW

subtitle_text <- tibble(animation_df$positive, animation_df$date)

a <- animation_df %>%
  ggplot() +
  geom_sf(data = oregon_hex_outline, color = "#fd5e53", fill = "transparent", size = 1, inherit.aes = F) +
  geom_sf(data = animation_df, aes(fill = cases_per_thousand), size = 1, color = 'white') +
  geom_text(data = animation_df, aes(x, y, label = abbr, color = textColor), 
            family = "Gill Sans MT", size = 2.25, fontface = 'bold') +
  geom_text(data = animation_df, aes(x, y, label = round(cases_per_thousand, 1), color = textColor), 
            family = "Gill Sans MT", size = 2, fontface = 'bold', vjust = 2.5) + 
  rcartocolor::scale_fill_carto_c(
    name = "Confirmed Cases Per Thousand Residents",
    palette = "Sunset",
    breaks = scales::breaks_pretty(n = 10)
  ) +
  theme_owen() +
  scale_color_identity() +
  theme(text=element_text(size=14,  family="Gill Sans MT"), 
        plot.title = element_text(hjust = 0.5, face = "bold",  vjust = 0, size = 15),
        plot.subtitle = element_text(hjust = 0.5, size = 9, vjust = 0), 
        plot.caption = element_text(face = "italic", size = 8, hjust = .5, vjust = 8), 
        legend.spacing.x = unit(0, 'cm'), 
        legend.title=element_text(size=11), 
        legend.text = element_text(size = rel(0.6)), 
        legend.margin=margin(10,0,-1,0),
        legend.position = 'bottom',
        plot.margin = margin(0, -.5, 0, -.5, "cm"), 
        legend.box.margin=margin(-30,0,15,0),
        plot.background = element_rect(fill = 'white', color = "white")) +
  labs(title = "Confirmed Cases Due To COVID-19 Per Thousand Residents", 
       caption  = paste0("Total Confirmed Cases Due To COVID-19 In Oregon: ", 
                         "{animation_df$positive[animation_df$date == as.Date(current_frame)][1]}"), 
       subtitle = "Date: {current_frame}") +
  guides(
    fill = guide_legend(
      keywidth=.5,
      keyheight=.15,
      default.unit="inch", 
      label.position = 'bottom', 
      title.position = 'top',
      title.hjust = .5,
      title.vjust = 0,
      label.vjust = 3,
      nrow = 1)
  )
  
anim_final <- a +
  transition_manual(frames = date) +
  enter_fade() +
  exit_fade()

# anim_final <- cowplot::ggdraw(anim_final) +
#   theme(plot.background = element_rect(fill="floralwhite", color = NA))
# Figure out how to make cowplot work with ggplot?

magick::image_write(
  animate(anim_final,
          width = 800,
          height = 800,
          nframes = length(unique(animation_df$date)),
          fps = 10,
          res = 150,
          end_pause = 20),
  "OregonCovid.Gif")

# Also make a leaflet
# https://code.markedmondson.me/googleCloudRunner/articles/cloudscheduler.html
# https://github.com/thomasp85/gganimate/issues/252
# https://stackoverflow.com/questions/41728575/how-to-add-specific-text-for-each-frame-of-an-animation-in-r
# https://www.google.com/search?q=gganimate+include+variables+with+current_frame&rlz=1C5CHFA_enUS861US861&oq=gganimate+include+variables+with+current_frame&aqs=chrome..69i57j33i160l4.5733j0j7&sourceid=chrome&ie=UTF-8