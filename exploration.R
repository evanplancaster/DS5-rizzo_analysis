### Title: Exploration.R
### Author: Evan Lancaster
### Purpose: Examine every pitch ever thrown to Chicago Cub first baseman Anthony Rizzo (through 2017) to find trends

# Read in packages
library(tidyverse)
library(readr)
library(plotly)
library(plyr)
library(sphereplot)

#https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
# circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
#   r = diameter / 2
#   tt <- seq(0,2*pi,length.out = npoints)
#   xx <- center[1] + r * cos(tt)
#   yy <- center[2] + r * sin(tt)
#   return(data.frame(x = xx, y = yy))
# }
# 
# weak_contact <- circleFun(center = c(0,0), diameter = 140)
# decent_contact <- circleFun(center = c(0,0), diameter = 200)
# strong_contact <- circleFun(center = c(0,0), diameter = 260)
# 
# dat <- rbind(weak_contact, decent_contact, strong_contact)
# dat$fill <- factor(rep(c("green", "yellow", "red"), each = 100),
#                    levels = rev(c("green", "yellow", "red")))

# Read in pitcher ID file (source: http://mikefast.googlepages.com/elias_to_lahman.csv)
pitcher_ids_df <- read_csv('data/pitcher_ids.csv') %>% 
  select(pitcher = mlb_id, pitcher_name = mlb_name)

# Read in pitches file (source: Baseball Savant)
rizzo_df <- read_csv('data/rizzo_pitches.csv', na = c('null', ''))

#functions for barrels
launch_vel_comp <- function(x){1.84*pnorm(x+1, 98, 9)}
launch_angle_comp <- function(x){dnorm(x = x, mean = 30, sd = 11)/dnorm(x = 30, mean = 30, sd = 11)}
barrel <- function(angle, vel){launch_angle_comp(angle) * launch_vel_comp(vel)}


# Create Crosswalk to decode and classify pitches
pitch_type_crosswalk <- data.frame(pitch_type = unique(rizzo_df$pitch_type), 
                                   pitch_name = c('Cut Fastball', '4-Seam Fastball', 'Slider', 'Curveball', '2-Seam Fastball', 'Changeup', 'Knuckle-Curve', 'Sinker', NULL, 'Split-Finger Fastball', 'Knuckleball', 'Forkball', 'Intentional Ball', NA, 'Eephus', '4-Seam Fastball', 'Screwball', 'Pitchout'),
                                   pitch_category = c('Fastball', 'Fastball', 'Off-Speed', 'Off-Speed', 'Fastball', 'Off-Speed', 'Off-Speed', 'Off-Speed', NULL, 'Fastball', 'Off-Speed', 'Off-Speed', NA, NA, 'Off-Speed', 'Fastball', 'Off-Speed', NA))

# Create Crosswalk to decode game types (regular season, NLDS, NLCS, World Series, etc.)
game_type_crosswalk <- data.frame(game_type = unique(rizzo_df$game_type), 
                                  game_desc = c('NLCS', 'NLDS', 'Regular Season', 'World Series', 'Wild Card'))

#Fill in NAs with 0 or 'No Contact'
rizzo_df$launch_speed_angle[is.na(rizzo_df$launch_speed_angle)] <- 0
rizzo_df$hit_location[is.na(rizzo_df$hit_location)] <- 0

# Merge event dataframe with crosswalk
rizzo_df <- rizzo_df %>% 
  filter(game_year >= 2015) %>% 
  mutate(hc_x = hc_x - 125.42, hc_y = 198.27 - hc_y) %>% 
  mutate(spray_angle = round(atan((hc_x)/(hc_y))*180/pi,1)) %>% 
  mutate(launch_z = launch_speed * sin(launch_angle*pi/180)) %>% 
  merge(pitch_type_crosswalk) %>% 
  merge(game_type_crosswalk) %>% 
  left_join(pitcher_ids_df) 
  #mutate(pitch_speed_cat = cut(release_speed, 
  #                             breaks = summary(rizzo_df$release_speed)[c(1:2, 5:6)],
  #                             labels = c('Bottom 25%', 'Middle 50%', 'Upper25%'))) %>% 
  #select(game_desc, pitch_name, everything(), -spin_dir, -spin_rate_deprecated, -batter, 
  #       -break_angle_deprecated, -break_length_deprecated, -stand)


# Use polar coordinates to plot outcomes based on launch angle and speed
# NOTE: polar coordinates don't seem to play well with ggplotly -- scrapping this...
# rizzo_df %>% 
#   #filter(pitch_type == 'CU', launch_speed == 113.6, launch_angle == 17.5950) %>% 
#   ggplot(aes(x = launch_angle, y = launch_speed)) +
#   geom_point(aes(color = pitch_name)) +
#   xlim(-360, 360) +
#   ylim(0,130) +
#   coord_polar(theta = 'x', direction = 1, start = 1 * pi / 2)

# Create coordinate system based on converting polar coordinates to cartesian ones.
# (Yaaaaay trig!) (x, y) = (r*cos(theta), r*sin(theta))
rizzo_df <- rizzo_df %>% 
  #filter(pitch_type == 'CU', launch_speed == 113.6, launch_angle == 17.5950) %>% 
  mutate(hit_x_coord = -1 * launch_speed * cos(launch_angle * pi / 180),
         hit_y_coord = launch_speed * sin(launch_angle * pi / 180),
         homeaway = mapvalues(inning_topbot, c('Top', 'Bot'), c('Away', 'Home')),
         month = format(game_date, '%B'),
         year = format(game_date, '%Y'),
         contact_type = mapvalues(launch_speed_angle, c(0,1,2,3,4,5,6), c('No Contact', 'Weak', 'Topped', 'Under', 'Flare/Burner', 'Solid', 'Barrel'))) %>% 
  mutate(season = mapvalues(month, c("March", "April", "May", "June", "July", "August", "September", "October", "November"),
                            c("Spring", "Spring", "Spring", "Summer", "Summer", "Summer", "Fall", "Fall", "Fall")),
          contact_info = mapvalues(description, c('called_strike', 'ball', 'blocked_ball', 'hit_by_pitch', 'intent_ball', 'pitchout', 'automatic_ball', 'hit_into_play', 'hit_into_play_score', 'swinging_strike', 'foul', 'foul_tip', 'hit_into_play_no_out', 'swinging_strike_blocked', 'missed_bunt', 'foul_bunt'), 
                                   c('take', 'take', 'take', 'take', 'take', 'take', 'take', 'contact', 'contact', 'missed swing', 'foul', 'foul', 'contact', 'missed swing', 'missed swing', 'foul')),
          launch_angle_comp = round(launch_angle_comp(launch_angle), 2),
          launch_vel_comp = round(launch_vel_comp(launch_speed), 2),
          barreled_ball = pmin(launch_angle_comp * launch_vel_comp, 1),
          #infield_outfield = as.character(cut(rizzo_df$hit_distance_sc, c(-999,110,999), c('Infield', 'Outfield'))),
          spray_angle_cat = as.character(cut(rizzo_df$spray_angle, c(-Inf,-15, 15, Inf), c('Opposite Field', 'Straight-Away', 'Pulled')))) %>% 
  replace_na(list(spray_angle_cat = 'No Contact')) 
  


          # take_or_swing = mapvalues(description, c('hit_into_play', 'hit_into_play_score', 'swinging_strike', 'foul', 'foul_tip', 'hit_into_play_no_out', 'swinging_strike_blocked', 'missed_bunt', 'foul_bunt'), 
          #                   c('swing', 'swing', 'swing', 'swing', 'swing', 'swing', 'swing', 'swing', 'swing'))) 

# p <- rizzo_df %>%
#   #filter(pitch_type == 'CU', launch_speed == 113.6, launch_angle == 17.5950) %>%
#   ggplot(aes(x = hit_x_coord, y = hit_y_coord, color = pitch_category, 
#              text = sprintf('<b>Launch Angle:</b> %.1f\n<b>Exit Velocity:</b> %.1f\n<b>Outcome: %s</b>', 
#                             round(launch_angle, 1), round(launch_speed, 1), description)))  +
#   #geom_circle(aes(x0 = 0, y0 = 0, r = 50, fill = 'blue', alpha = 0.1), show.legend = FALSE) +
#   geom_point(aes(alpha = 0.2)) +
#   #  legend(legend = pitch_category) +
#   #geom_segment(aes(x = hit_x_coord, y = hit_y_coord, xend = 0, yend = 0, alpha = 0.1)) +
#   xlim(min(rizzo_df$hit_x_coord, na.rm = TRUE), z1) +
#   ylim(min(rizzo_df$hit_y_coord, na.rm = TRUE), max(rizzo_df$hit_y_coord, na.rm = TRUE))
# # 
# # p <- rizzo_df %>% 
# #   plot_ly(
# #     r = ~launch_speed, t = ~launch_angle, color = ~pitch_category, type = 'scatter'
# #   ) %>% 
# #   layout(
# #     orientation = 180
# #     #angularaxis = list(range = c(-90, 90))
# #   )
# #   
# # p
# 
# p <- ggplot() + 
#   geom_polygon(aes(fill = fill, x = x, y = y), data = dat) + 
#   scale_fill_manual(values = rev(c("green", "yellow", "red"))) + 
#   geom_point(aes(x = hit_x_coord, y = hit_y_coord, color = pitch_category, alpha = 0.2,
#                  text = sprintf('<b>Launch Angle:</b> %.1f\n<b>Exit Velocity:</b> %.1f\n<b>Outcome: %s</b>', 
#                                 round(launch_angle, 1), round(launch_speed, 1), description)), data = rizzo_df) + 
#   coord_equal() + 
#   coord_cartesian(xlim = c(-130, 1), ylim = c(-130, 130))
# 
# ggplotly(p, tooltip = 'text')
# 
# # Fun with 3D!!!
# # p <- rizzo_df %>% 
# #   plot_ly(x = ~hc_x,  y = ~hc_y, z = ~launch_z) %>% 
# #   add_markers() %>% 
# #   layout(
# #     hovermode = 'x'
# #   )
# # p
# 
# #rizzo_df %>% 
# #  sph2car(long = rizzo_df$launch_angle, lat = rizzo_df$spray_angle, radius = rizzo_df$launch_speed, deg = TRUE)
# #rgl.sphgrid(radius = max(rizzo_df$launch_speed))
# 
# 
# #p