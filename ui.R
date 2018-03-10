#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readr)
library(plyr)
library(plotly)
library(shinythemes)

source('exploration.R')


# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("united"),

  # Application title
  titlePanel("Analysis of Anthony Rizzo's Offense (2015-2017)"),
  h5('Change the filters below to analyze how well Anthony Rizzo hits in certain situations. You can also scroll down to see more information about the plays you have filtered.'),
  # Sidebar with a slider input 
  fluidRow(
    sidebarPanel(
      style = "overflow-y:scroll; max-height: 700px",
      h2('Pitching Info'),
      hr(),
      h3('Game Scenario'),
      # selectInput('season',
      #             'Time of Year:',
      #             choices = c("Spring", "Summer", "Fall"),
      #             selected = c("Spring", "Summer", "Fall"),
      #             multiple = TRUE,
      #             selectize = FALSE),
      fluidRow(
        column(
          width = 6,
          selectInput("month",
                      "Month:",
                      choices = c("March", "April", "May", "June", "July", "August", "September", "October", "November"),
                      selected = c("March", "April", "May", "June", "July", "August", "September", "October", "November"),
                      multiple = TRUE,
                      selectize = FALSE,
                      width = '200px')
        ),
        column(
          width = 6,
          selectInput('year',
                      "Year:",
                      choices = seq(max(rizzo_df$game_year, na.rm = TRUE), min(rizzo_df$game_year, na.rm = TRUE), -1),
                      selected = seq(max(rizzo_df$game_year, na.rm = TRUE), min(rizzo_df$game_year, na.rm = TRUE), -1),
                      multiple = TRUE,
                      selectize = FALSE,
                      width = '200px')
        )
      ),
      
      fluidRow(
        column(
          width = 4,
          selectInput("balls",
                      "Balls:",
                      choices = c(0,1,2,3),
                      selected = c(0,1,2,3),
                      multiple = TRUE,
                      selectize = FALSE,
                      width = '200px')
        ),
        column(
          width = 4,
          selectInput("strikes",
                      "Strikes:",
                      choices = c(0,1,2),
                      selected = c(0,1,2),
                      multiple = TRUE,
                      selectize = FALSE,
                      width = '200px')
        ),
        column(
          width = 4,
          selectInput("outs",
                      "Outs:",
                      choices = c(0,1,2),
                      selected = c(0,1,2),
                      multiple = TRUE,
                      selectize = FALSE,
                      width = '200px')
        )
      ),
      fluidRow(
        column(
          width = 6,
          selectInput("homeaway",
                      "Top/Bottom:",
                      choices = c('Top', 'Bottom' = 'Bot'),
                      selected = c('Top', 'Bot'),
                      multiple = TRUE,
                      selectize = FALSE,
                      width = '200px',
                      size = 4)
        ),
        column(
          width = 6,
          selectInput("inning",
                      "Inning:",
                      choices = c(1,2,3,4,5,6,7,8,9),
                      selected = c(1,2,3,4,5,6,7,8,9),
                      multiple = TRUE,
                      selectize = FALSE,
                      width = '200px',
                      size = 4)  
        )
      ),
      # checkboxGroupInput("onbase",
      #              "Runners On Base:",
      #              choices = c("1st" = 1, "2nd" = 2, "3rd" = 3),
      #              selected = c(1,2,3),
      #              inline = TRUE),
      hr(),
      h3('Pitcher Info'),
      selectInput("pitcher_hand",
                  "Left/Right-Handed:",
                  choices = c("Right" = "R", "Left" = "L"),
                  selected = c("L","R"),
                  multiple = TRUE,
                  selectize = FALSE,
                  width = '200px'),
      hr(),
      h3('Pitch Selection'),
      fluidRow(
        column(
          width = 6,
          selectInput("pitch_name",
                      "Pitch Type",
                      choices = na.exclude(unique(rizzo_df$pitch_name)),
                      selected = na.exclude(unique(rizzo_df$pitch_name)),
                      multiple = TRUE,
                      selectize = TRUE,
                      #size = 3,
                      width = '200px')
        ),
        column(
          width = 6,
          sliderInput("release_speed",
                      "Velocity:",
                      min = min(rizzo_df$release_speed, na.rm = TRUE),
                      max = max(rizzo_df$release_speed, na.rm = TRUE),
                      value = c(min(rizzo_df$release_speed, na.rm = TRUE), max(rizzo_df$release_speed, na.rm = TRUE)),
                      width = '200px'),
          sliderInput("release_spin_rate",
                      "Spin Rate:",
                      min = min(rizzo_df$release_spin_rate, na.rm = TRUE),
                      max = max(rizzo_df$release_spin_rate, na.rm = TRUE),
                      value = c(min(rizzo_df$release_spin_rate, na.rm = TRUE), max(rizzo_df$release_spin_rate, na.rm = TRUE)),
                      width = '200px'),
          selectInput('strikezone',
                      'Strike Zone Location:',
                      choices = c(1,2,3,4,5,6,7,8,9,11,12,13,14),
                      selected = c(0,1,2,3,4,5,6,7,8,9,11,12,13,14),
                      selectize = FALSE,
                      multiple = TRUE,
                      width = '200px')
        )
      ),
      width = 3
    ),    
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        column(
          plotlyOutput("strikezonePlot", height = '200px'),
          width = 6
          ),
        # column(
        #   plotlyOutput('swing_contact_rates', height = '225px'),
        #   width = 6
        # )
        column(align="center",
          fluidRow(align='center',
            tags$b('Swing and Contact Rates', style='padding: 20px; font-face: bold; font-size: 16px')
          ),
          #NOTE------------think about replacing this with chart of swing rate vs contact rate
          fluidRow(align="center",
            column(
              textOutput('pitch_count'),
              width = 4
            ),
            column(
              textOutput('swing_count'),
              width = 4
            ),
            column(
              textOutput('contact_count'),
              width = 4
            )
          ),
          fluidRow(
            tags$b(textOutput('swing_rate'), style="padding:8px; font-size: 14px")
          ),
          fluidRow(
            tags$b(textOutput('contact_rate'), style="padding:8px; font-size: 14px")
          ),
          width = 6
        )
      ),
      hr(),
      fluidRow(
        column(
          plotlyOutput("launchPlot", height = '200px'),
          width = 6
        ),
        column(align="center",
          #textOutput('barrel_pct'),
          #NOTE -------------think about putting chart here of pie chart of contact types
          plotlyOutput('barrel_bars', height = '150px'),
          tags$b(textOutput('avg_barrel_pct'), style="font-size: 15px"),
          tags$b(textOutput('tango_barrel_rate'), style="font-size: 15px"),
          width = 6
        )
      ),
      hr(),
      fluidRow(
        column(
          plotlyOutput("spraychartPlot", height = '200px'),
          width = 6
        ),
        column(
          plotlyOutput('outcome_bars', height = '200px'),
          width = 6
        )
      ),
      #img(src = 'anthony-rizzo.png'),
      
      width = 6
    ),
    
    sidebarPanel(
      style = "overflow-y:scroll; max-height: 700px",
      h2('Outcomes Info'),
      hr(),
      h3('Contact Info'),
      selectInput('contact_info',
                  'Take/Swing/Contact',
                  choices = c('take', 'missed swing', 'foul', 'contact'),
                  selected = c('take', 'missed swing', 'foul', 'contact'),
                  multiple = TRUE,
                  selectize = FALSE,
                  width = '200px'),
      hr(),
      h3('Launch Info'),
      selectInput('ball_flight',
                  'Ball Flight',
                  choices = c('Barrel' = 6, 'Solid Contact' = 5, 'Flare/Burner' = 4, 'Poor Contact/Underneath' = 3, 'Poor Contact/Topped' = 2, 'Weak Contact' = 1, 'No Contact' = 0),
                  selected = c(0, 1, 2, 3, 4, 5, 6),
                  multiple = TRUE,
                  selectize = FALSE,
                  width = '200px'),
      hr(),
      h3('Field Placement'),
      selectInput("spray_angle_cat",
                  "Spray Angle:",
                  choices = c('Opposite Field', 'Straight-Away', 'Pulled', 'No Contact'),
                  selected = c('Opposite Field', 'Straight-Away', 'Pulled', 'No Contact'),
                  multiple = TRUE,
                  selectize = FALSE,
                  width = '200px'),
      # selectInput("infield_outfield",
      #             'Depth:',
      #             choices = c('Infield', 'Outfield', 'No Contact'),
      #             selected = c('Infield', 'Outfield', 'No Contact'),
      #             multiple = TRUE,
      #             selectize = FALSE,
      #             width = '200px'),
      width = 3
    )
  ),
  fluidRow(style = "overflow:scroll",
    DT::dataTableOutput("datatable"),
    width = 12
  )
))

