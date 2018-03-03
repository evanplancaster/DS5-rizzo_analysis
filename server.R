#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readr)
library(plotly)
library(plyr)
library(sphereplot)

source('exploration.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  datasetInput <- reactive({
    rizzo_df %>% 
      filter(between(release_speed, input$release_speed[1], input$release_speed[2]),
             between(release_spin_rate, input$release_spin_rate[1], input$release_spin_rate[2]),
            strikes %in% input$strikes, 
            balls %in% input$balls,
            outs_when_up %in% input$outs,
            inning %in% input$inning,
            #inning_topbot %in% input$homeaway,
            p_throws %in% input$pitcher_hand,
            pitch_name %in% input$pitch_name,
            month %in% input$month,
            season %in% input$season
            #onbase %in% input$
      )
  })
  

  output$launchPlot <- renderPlotly({
    
    p <- datasetInput() %>% 
      ggplot(aes(x = hit_x_coord, y = hit_y_coord, color = pitch_category, 
                    text = sprintf('<b>Launch Angle:</b> %.1f\n<b>Exit Velocity:</b> %.1f\n<b>Outcome: %s</b>',
                                   round(launch_angle, 1), round(launch_speed, 1), description)))  +
         #geom_circle(aes(x0 = 0, y0 = 0, r = 50, fill = 'blue', alpha = 0.1), show.legend = FALSE) +
         geom_point(aes(alpha = 0.2), show.legend = FALSE) +
         #  legend(legend = pitch_category) +
         #geom_segment(aes(x = hit_x_coord, y = hit_y_coord, xend = 0, yend = 0, alpha = 0.1)) +
         xlim(min(rizzo_df$hit_x_coord, na.rm = TRUE), 1) +
         ylim(min(rizzo_df$hit_y_coord, na.rm = TRUE), max(rizzo_df$hit_y_coord, na.rm = TRUE))
    
    ggplotly(p, tooltip = 'text')
  })
  
  output$strikezonePlot <- renderPlotly({
    
    p <- datasetInput() %>% 
      ggplot(aes(x = plate_x, y = plate_z, color = pitch_category, 
                 text = sprintf('<b>Launch Angle:</b> %.1f\n<b>Exit Velocity:</b> %.1f\n<b>Outcome: %s</b>',
                                round(launch_angle, 1), round(launch_speed, 1), description)))  +
      #geom_circle(aes(x0 = 0, y0 = 0, r = 50, fill = 'blue', alpha = 0.1), show.legend = FALSE) +
      geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5), color = 'red') +
      geom_point(aes(alpha = 0.2), show.legend = FALSE) +
      #  legend(legend = pitch_category) +
      #geom_segment(aes(x = hit_x_coord, y = hit_y_coord, xend = 0, yend = 0, alpha = 0.1)) +
      xlim(min(rizzo_df$plate_x, na.rm = TRUE), max(rizzo_df$plate_x, na.rm = TRUE)) +
      ylim(min(rizzo_df$plate_z, na.rm = TRUE), max(rizzo_df$plate_z, na.rm = TRUE))
    
    ggplotly(p, tooltip = 'text') %>% 
      layout(dragmode = 'select')
  })  
  
  output$spraychartPlot <- renderPlotly({
    s <- event_data("plotly_selected")
    if (length(s$x) > 0) {
      p <- datasetInput() %>% 
        ggplot(aes(x = hc_x, y = hc_y, color = bb_type, 
                   text = sprintf('<b>Launch Angle:</b> %.1f\n<b>Exit Velocity:</b> %.1f\n<b>Outcome: %s</b>',
                                  round(launch_angle, 1), round(launch_speed, 1), description)))  +
        #geom_circle(aes(x0 = 0, y0 = 0, r = 50, fill = 'blue', alpha = 0.1), show.legend = FALSE) +
        geom_point(aes(alpha = 0.2), show.legend = FALSE) +
        #  legend(legend = pitch_category) +
        #geom_segment(aes(x = hit_x_coord, y = hit_y_coord, xend = 0, yend = 0, alpha = 0.1)) +
        xlim(min(rizzo_df$hc_x, na.rm = TRUE), max(rizzo_df$hc_x, na.rm = TRUE)) +
        ylim(min(rizzo_df$hc_y, na.rm = TRUE), max(rizzo_df$hc_y, na.rm = TRUE))
      
      ggplotly(p, tooltip = 'text')
    }
  })  
    
  #   p <- rizzo_df %>% 
  #     ggplot(aes(x = hit_x_coord, y = hit_y_coord, color = pitch_category, 
  #              text = sprintf('<b>Launch Angle:</b> %.1f\n<b>Exit Velocity:</b> %.1f\n<b>Outcome: %s</b>', 
  #                             round(input$launch_angle, 1), round(launch_speed, 1), description)))  +
  #   #geom_circle(aes(x0 = 0, y0 = 0, r = 50, fill = 'blue', alpha = 0.1), show.legend = FALSE) +
  #   geom_point(aes(alpha = 0.2)) +
  #   #  legend(legend = pitch_category) +
  #   #geom_segment(aes(x = hit_x_coord, y = hit_y_coord, xend = 0, yend = 0, alpha = 0.1)) +
  #   xlim(min(rizzo_df$hit_x_coord, na.rm = TRUE), 1) +
  #   ylim(min(rizzo_df$hit_y_coord, na.rm = TRUE), max(rizzo_df$hit_y_coord, na.rm = TRUE))
  #   
  # })
  
})
