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
library(DT)

source('exploration.R')

#img <- readPNG('img/Anthony-Rizzo.png')
#g <- rasterGrob(img, interpolate=TRUE)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    # tmp <- rizzo_df
    # sel <- NULL
    # 
    # selected <- reactive({
    #   event_data('plotly_selected')
    # })
    # 
    datasetInput <- reactive({
      #tmp <- tmp %>% 
      tmp <- rizzo_df %>% 
        filter(between(release_speed, input$release_speed[1], input$release_speed[2]),
               #between(spray_angle, input$spray_angle[1], input$spray_angle[2]),
              between(release_spin_rate, input$release_spin_rate[1], input$release_spin_rate[2]),
              #between(hit_distance_sc, input$hit_distance_sc[1], input$hit_distance_sc[2]),
              strikes %in% input$strikes, 
              balls %in% input$balls,
              outs_when_up %in% input$outs,
              inning %in% input$inning,
              inning_topbot %in% input$homeaway,
              p_throws %in% input$pitcher_hand,
              pitch_name %in% input$pitch_name,
              month %in% input$month,
              year %in% input$year,
              contact_info %in% input$contact_info,
              launch_speed_angle %in% input$ball_flight,
              #hit_location %in% input$field_zone,
              zone %in% input$strikezone,
              #infield_outfield %in% input$infield_outfield,
              spray_angle_cat %in% input$spray_angle_cat
              )
              
              #onbase %in% input$
      #sel <- tryCatch(tmp[(selected()$pointNumber+1),,drop=FALSE], error = function(e){NULL})
      
      list(data = tmp
           #, sel = sel
           )
    })
    
    
    
  
    output$launchPlot <- renderPlotly({
      req(nrow(datasetInput()$data) > 0)
      
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
        theme_void() +
        ggtitle("Launch Chart (Exit Velocity/Launch Angle)") + 
        theme(plot.title = element_text(size = 10, face = 'bold'))
      
      #obj <- datasetInput()$sel
      #obj
      
      # if(nrow(obj)!=0) {
      #   p <- p + geom_point(data = obj, size = 4)
      # }
    
      ggplotly(p, tooltip = "text") %>% 
        config(displayModeBar = F)
    })
    
    output$strikezonePlot <- renderPlotly({
      req(nrow(datasetInput()$data) > 0)
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
      sz_box <- data.frame(vert_x = c(-0.839, -0.839 + 1.678/3, -.839+2*1.678/3, 0.839), 
                           vert_y = c(1.52, 1.52, 1.52, 1.52), 
                           vert_xend = c(-0.839, -0.839 + 1.678/3, -.839+2*1.678/3, 0.839),
                           vert_yend = c(3.52, 3.52, 3.52, 3.52), 
                           hor_x = c(-0.839, -0.839, -0.839, -0.839), 
                           hor_y = seq(1.52, 3.52, 2/3), 
                           hor_xend = c(0.839, 0.839, 0.839, 0.839), 
                           hor_yend = seq(1.52, 3.52, 2/3))
      
      p <- ggplot(data = datasetInput()$data, aes(x = plate_x, y = plate_z, color = contact_info, alpha = 0.2))  +
        #geom_circle(aes(x0 = 0, y0 = 0, r = 50, fill = 'blue', alpha = 0.1), show.legend = FALSE) +
        geom_point(aes(text = sprintf('<b>Launch Angle:</b> %.1f\n<b>Exit Velocity:</b> %.1f\n<b>Outcome: %s</b>',
                                      round(launch_angle, 1), round(launch_speed, 1), description))) +
        #  legend(legend = pitch_category) +
        geom_segment(data = sz_box, aes(x = vert_x, y = vert_y, xend = vert_xend, yend = vert_yend, color = I('white')), show.legend = FALSE) +
        guides(color = 'none') +
        geom_segment(data = sz_box, aes(x = hor_x, y = hor_y, xend = hor_xend, yend = hor_yend, color = I('white')), show.legend = FALSE) +
        guides(color = 'none') +
        #scale_color_manual(limits = )
        xlim(-3, 3) +
        ylim(-2, 6) +
        coord_fixed() +
        theme_void() +
        ggtitle("Strike Zone Chart (Catcher's Perspective)") + 
        theme(plot.title = element_text(size = 10, face = 'bold'))
      
        # obj <- datasetInput()$sel
        # obj
        # 
        # if(nrow(obj)!=0) {
        #   p <- p + geom_point(data = obj, size = 4)
        # }
        
        ggplotly(p, tooltip = 'text') %>%
          layout( 
            legend = list(x = -1, y = 0.95)
          ) %>% 
          config(displayModeBar = F)
          
          #layout(dragmode = 'lasso')
        
      
      
      
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
    
    # output$swing_contact_rates <- renderPlotly({
    #   req(datasetInput())
    #   
    #   data <- data.frame(x = swing_count()$swing_count/pitch_count()$pitch_count*100, 
    #                      y = contact_count()$contact_count/swing_count()$swing_count*100)
    #   
    #   
    #   p <- ggplot(data = data, aes(x = x, y = y))  +
    #     #geom_circle(aes(x0 = 0, y0 = 0, r = 50, fill = 'blue', alpha = 0.1), show.legend = FALSE) +
    #     geom_point(
    #       aes(size = swing_count()$swing_count, text = sprintf('<b>Swing Rate:</b> %.1f%%\n<b>Contact Rate:</b> %.1f%%',
    #                                   x, y))
    #       ) +
    #   
    #     xlim(-1, 101) +
    #     ylim(-1, 101) +
    #     coord_fixed() +
    #     theme_bw()
    #   
    #   ggplotly(p, tooltip = 'text')
    # })
    
    
    
    output$spraychartPlot <- renderPlotly({
     # s <- event_data("plotly_selected")
      # if (length(s$x) == 0) {
      #   data <- datasetInput()
      # } else {
      #   data <- datasetInput() %>% 
      #     filter(plate_x %in% s$x,
      #            plate_z %in% s$y)
      #   
      #   #cat(str(data))
      # }  
      req(nrow(datasetInput()$data) > 0)
      
      p <- ggplot(data = datasetInput()$data, aes(x = hc_x, y = hc_y, color = bb_type, 
                   text = sprintf('<b>Change this!</b>')))  +
        #geom_circle(aes(x0 = 0, y0 = 0, r = 50, fill = 'blue', alpha = 0.1), show.legend = FALSE) +
        geom_point(aes(alpha = 0.2), show.legend = FALSE) +
        #  legend(legend = pitch_category) +
        #geom_segment(aes(x = hit_x_coord, y = hit_y_coord, xend = 0, yend = 0, alpha = 0.1)) +
        xlim(min(rizzo_df$hc_x, na.rm = TRUE), max(rizzo_df$hc_x, na.rm = TRUE)) +
        ylim(min(rizzo_df$hc_y, na.rm = TRUE), max(rizzo_df$hc_y, na.rm = TRUE)) +
        coord_equal() +
        theme_void() +
        ggtitle("Spray Chart (Aerial View)") + 
        theme(plot.title = element_text(size = 10, face = 'bold'))
      
      ggplotly(p, tooltip = 'text') %>% 
        config(displayModeBar = F) #%>% 
        #layout(dragmode = 'lasso')    
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
    
    #Calculate swing_rate
    swing_count <- reactive({
      datasetInput()$data %>% 
        filter(contact_info != 'take') %>% 
        summarise(swing_count = n())
    })
    
    pitch_count <- reactive({datasetInput()$data %>% 
        summarise(pitch_count = n())
    })
    
    contact_count <- reactive({
      datasetInput()$data %>% 
        filter(contact_info == 'contact') %>% 
        summarise(contact_count = n())
    })
    
    output$swing_rate <- renderText({
      paste("Swing Rate: ", round(swing_count()/pitch_count()*100, 2), '%')
    })
    
    output$swing_count <- renderText({
      paste("# Swings:\n\n", swing_count())
    })
    
    output$pitch_count <- renderText({
      paste("# Pitches:\n\n", pitch_count())
    })
    
    output$contact_count <- renderText({
      paste("# Contacts:\n\n", contact_count())
    })
    
    output$contact_rate <- renderText({
      paste("Contact Rate: ", round(contact_count()/swing_count()*100, 2), '%')
    })
    
    avg_barrel_pct <- reactive({
      datasetInput()$data %>% 
        filter(contact_info == 'contact') %>% 
        summarise(avg_barrel_pct = round(mean(barreled_ball)*100, 2))
    })
    
    output$avg_barrel_pct <- reactive({
      paste("Average Barrel Percent: ", avg_barrel_pct(), '%')
    })
    
    tango_barrel_count <- reactive({
      datasetInput()$data %>% 
        filter(launch_speed_angle == 6) %>% 
        summarise(tango_barrel_count = n())
    })
    
    output$tango_barrel_rate <- reactive({
      paste("Tango Barrel Rate: ", round(tango_barrel_count()/contact_count()*100, 2), ' / 100 balls in play')
    })
    
    output$datatable <- DT::renderDataTable({
      DT::datatable(datasetInput()$data[, c('game_date', 
                                            'inning_topbot', 
                                            'inning', 
                                            'balls', 
                                            'strikes', 
                                            'outs_when_up', 
                                            'pitcher_name', 
                                            'events',
                                            'des'), 
                                        drop = FALSE],
                    colnames = c('Game Date','Top/Bottom of Inning', 'Inning', 'Balls', 'Strikes', 'Outs',
                                 'Pitcher', 'Event', 'Description'))
    })
    
    output$barrel_bars <- renderPlotly({
      p <- datasetInput()$data %>% 
        filter(launch_speed_angle > 0) %>% 
        group_by(contact_type) %>% 
        ggplot(aes(x = contact_type)) +
        geom_bar() +
        theme_bw() +
        theme(axis.text.x = element_text(size=7, angle=15)) + 
        theme(axis.title.x = element_blank()) +
        theme(axis.title.y = element_blank()) +
        ggtitle("Frequencies for Contact Types") + 
        theme(plot.title = element_text(size = 10, face = 'bold'))
      #+
       # ggsubtitle("Contact Type")
      
      
      ggplotly(p)  %>% 
        config(displayModeBar = F)
    }) 
      
      output$outcome_bars <- renderPlotly({
        p <- datasetInput()$data %>% 
          filter(events %in% c('home_run', 'triple', 'double', 'single')) %>% 
          group_by(events) %>% 
          ggplot(aes(x = events)) +
          geom_bar() +
          theme_bw() +
          theme(axis.text.x = element_text(size=7, angle=15)) + 
          theme(axis.title.x = element_blank()) +
          theme(axis.title.y = element_blank()) +
          ggtitle("Frequencies for On-Base Outcomes") + 
          theme(plot.title = element_text(size = 10, face = 'bold'))
        #+
        # ggsubtitle("Contact Type")
        
        
        ggplotly(p)  %>% 
          config(displayModeBar = F)
      })
    
        
    session$onSessionEnded(stopApp)
})

