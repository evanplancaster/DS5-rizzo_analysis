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
library(png)
library(grid)

source('exploration.R')

#img <- readPNG('img/Anthony-Rizzo.png')
#g <- rasterGrob(img, interpolate=TRUE)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    tmp <- rizzo_df
    sel <- NULL
    
    selected <- reactive({
      event_data('plotly_selected')
    })
    
    datasetInput <- reactive({
      tmp <- tmp %>% 
        filter(between(release_speed, input$release_speed[1], input$release_speed[2]),
              between(release_spin_rate, input$release_spin_rate[1], input$release_spin_rate[2]),
              strikes %in% input$strikes, 
              balls %in% input$balls,
              outs_when_up %in% input$outs,
              inning %in% input$inning,
              inning_topbot %in% input$homeaway,
              p_throws %in% input$pitcher_hand,
              pitch_name %in% input$pitch_name,
              month %in% input$month,
              year %in% input$year)
              
              #onbase %in% input$
      sel <- tryCatch(tmp[(selected()$pointNumber+1),,drop=FALSE], error = function(e){NULL})
      
      list(data = tmp, sel = sel)
    })
    
    
    
  
    output$launchPlot <- renderPlotly({
      req(datasetInput())
      
      #s <- event_data("plotly_selected")
      
      # data <- datasetInput()
      # print(s)
      # 
      # if (length(s$x) > 0) {
      #   data <- data %>% 
      #     filter(row.names(.) %in% s$pointNumber)
      #   print(data)
      # }  
      
      p <- ggplot(data = datasetInput()$data, aes(x = hit_x_coord, y = hit_y_coord, 
                                                  text = paste('<br>Angle: ', launch_angle, 
                                                               '<br>Speed: ', launch_speed, 
                                                               '<br>Barrel %: ', round(100 * barreled_ball, 3)), 
                                                  color = barreled_ball)) + 
        geom_point() +
        xlim(-120, 5) +
        ylim(-90, 90) +
        coord_fixed() +
        theme_bw()
      
      obj <- datasetInput()$sel
      obj
      
      if(nrow(obj)!=0) {
        p <- p + geom_point(data = obj, size = 4)
      }
    
      ggplotly(p, tooltip = "text") %>% 
        layout(dragmode = 'lasso') 
    })
    
    output$strikezonePlot <- renderPlotly({
      req(datasetInput())
    #  s <- event_data("plotly_selected")
      
      # data <- datasetInput()
      # 
      # if (length(s$x) > 0) {
      #   data <- data %>% 
      #     filter(plate_x %in% s$x,
      #            plate_z %in% s$y)
      # }  
      # 
      #strikezone box
      sz_box <- data.frame(vert_x = seq(-1, 1, 2/3), 
                           vert_y = c(1.5, 1.5, 1.5, 1.5), 
                           vert_xend = seq(-1, 1, 2/3), 
                           vert_yend = c(3.5, 3.5, 3.5, 3.5), 
                           hor_x = c(-1, -1, -1, -1), 
                           hor_y = seq(1.5, 3.5, 2/3), 
                           hor_xend = c(1, 1, 1, 1), 
                           hor_yend = seq(1.5, 3.5, 2/3))
      
      p <- ggplot(data = datasetInput()$data, aes(x = plate_x, y = plate_z, color = take_or_swing, alpha = 0.2)) +#, 
                                                  #text = sprintf('<b>Launch Angle:</b> %.1f\n<b>Exit Velocity:</b> %.1f\n<b>Outcome: %s</b>',
                                                                 #round(launch_angle, 1), round(launch_speed, 1), description)))  +
        #geom_circle(aes(x0 = 0, y0 = 0, r = 50, fill = 'blue', alpha = 0.1), show.legend = FALSE) +
        geom_point() +
        #  legend(legend = pitch_category) +
        geom_segment(data = sz_box, aes(x = vert_x, y = vert_y, xend = vert_xend, yend = vert_yend, color = I('white')), showlegend = FALSE) +
        geom_segment(data = sz_box, aes(x = hor_x, y = hor_y, xend = hor_xend, yend = hor_yend, color = I('white')), showlegend = FALSE) +
        #scale_color_manual(limits = )
        xlim(-3, 3) +
        ylim(-2, 6) +
        coord_fixed() +
        theme_bw()
      
        obj <- datasetInput()$sel
        obj
        
        if(nrow(obj)!=0) {
          p <- p + geom_point(data = obj, size = 4)
        }
        
        ggplotly(p, tooltip = 'text') %>%
          layout(dragmode = 'lasso')
  
      
      
      
      # p <- datasetInput() %>% 
      #   ggplot(aes(x = plate_x, y = plate_z, color = take_or_swing, 
      #              text = sprintf('<b>Launch Angle:</b> %.1f\n<b>Exit Velocity:</b> %.1f\n<b>Outcome: %s</b>',
      #                             round(launch_angle, 1), round(launch_speed, 1), description)))  +
      #   #geom_circle(aes(x0 = 0, y0 = 0, r = 50, fill = 'blue', alpha = 0.1), show.legend = FALSE) +
      #   geom_point(aes(alpha = 0.2), show.legend = FALSE) +
      #   #  legend(legend = pitch_category) +
      #   #geom_segment(aes(x = hit_x_coord, y = hit_y_coord, xend = 0, yend = 0, alpha = 0.1)) +
      #   add_polygons()
      # geom_rect(aes(xmin = c(-1,0,-1,0), xmax = c(0,1,0,1), ymin = c(1.5,1.5,2.5,2.5), ymax = c(2.5,2.5,3.5,3.5)), color = 'white', fill = NA) +
      #   xlim(-3, 3) +
      #   ylim(-2, 6) +
      #   coord_fixed()
      # 
      # ggplotly(p, tooltip = 'text') %>% 
      #   layout(dragmode = 'select')
      
      
      
      
      
      
    })  
    
  
    
    
    
    output$spraychartPlot <- renderPlotly({
     # s <- event_data("plotly_selected")
      if (length(s$x) == 0) {
        data <- datasetInput()
      } else {
        data <- datasetInput() %>% 
          filter(plate_x %in% s$x,
                 plate_z %in% s$y)
        
        #cat(str(data))
      }  
      
      
      p <- data %>% 
        #ggplot(aes(x = hc_x, y = hc_y, color = bb_type, 
        ggplot(aes(x = hc_x, y = hc_y, color = bb_type, 
                   text = sprintf('<b>Launch Angle:</b> %.1f\n<b>Exit Velocity:</b> %.1f\n<b>Outcome: %s</b>',
                                  round(launch_angle, 1), round(launch_speed, 1), description)))  +
        #geom_circle(aes(x0 = 0, y0 = 0, r = 50, fill = 'blue', alpha = 0.1), show.legend = FALSE) +
        geom_point(aes(alpha = 0.2), show.legend = FALSE) +
        #  legend(legend = pitch_category) +
        #geom_segment(aes(x = hit_x_coord, y = hit_y_coord, xend = 0, yend = 0, alpha = 0.1)) +
        xlim(min(rizzo_df$hc_x, na.rm = TRUE), max(rizzo_df$hc_x, na.rm = TRUE)) +
        ylim(min(rizzo_df$hc_y, na.rm = TRUE), max(rizzo_df$hc_y, na.rm = TRUE)) +
        coord_equal()
      
      ggplotly(p, tooltip = 'text') %>% 
        layout(dragmode = 'lasso')    
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
    
    session$onSessionEnded(stopApp)
})

