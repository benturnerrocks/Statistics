library(shiny)
library(tidyverse)
library(shinyWidgets)
library(RColorBrewer)
library(lubridate)


jury_data_adds <- read_csv("wrangledData.csv")

# Figure 1
# Explores the faceted plot by struck_by variable
#for time series plots, toggle stuck_by variable, race buttons, and year range selection
plot1_data <- jury_data_adds %>%
  filter(race %in% c("White", "Black")) %>%
  group_by(year, race, struck_by2) %>%
  summarize(tot = n()) %>%
  mutate(prop = tot / sum(tot)) %>%
  filter(struck_by2 %in% c("State", "Defense", "Chosen"))


sidebar_plot1 <- sidebarPanel(
  selectInput(inputId = "struck_by",
              label = "Filter by who struck the juror:",
              choices = c("State", "Defense","Chosen"),
              selected = "State"),
  sliderInput(inputId = "range", 
              label = "Show data for trials in the years:",
              value = c(min(plot1_data$year),max(plot1_data$year)), 
              min = min(plot1_data$year),
              max = max(plot1_data$year),
              step = 1,
              timeFormat = '%Y'),
  checkboxGroupInput(inputId = "race",
                     label = "Race of juror:",
                     choices = c("Black","White"),
                     selected = c("Black","White")
                    ))

plot1_text <- "Of the jurors eligible to be struck, above is a plot of the 
proportion of jurors struck by defense, state, or instead chosen to serve, 
taking into account whether the juror race was white or black."

plot1_takeaways1 <- "First, it appears the proportion of strike-eligible jurors
that are ultimately selected to be on the jury remains relatively constant over
time at roughly 0.50."
plot1_takeaways2 <- "Another important observation that comes from this is that black jurors are consistently
struck by the state at a higher rate than white jurors. From exploratory 
data analysis, it is clear there are significantly more black defendants than white defendants. 
The state’s strike trend might be due to the state trying to strike jurors
who are similar in race to the defendant. Given the sheer number of black defendants, the 
proportion of black jurors struck will be higher."

plot1_takeaways3 <- "A third observation from these plots is that white jurors are consistently
struck by the defense at a higher rate than black jurors. Similar to the state’s
strike trend over time, the defense’s strike trend may be explained by the defense
attempting to strike jurors that are dissimilar to the defendant’s race."


# Figure 2

plot2_title <- "Figure 2. Proportion of Jurors Struck by Race with and without Black Defendants"

plot2_data <- jury_data_adds %>%
  filter(strike_eligibility %in% c("State", "Both State and Defense"),
         race %in% c("Black", "White", "Unknown")) %>% 
  group_by(trial, race, defendant_race) %>%
  #for each trial, compute proportion of jurors struck by race, determine whether
  #any of the defendants was Black
  summarize(
    prop_struck_jurors = mean(struck_by %in% c("Struck by the state"), 
                              na.rm=TRUE
    ),
    black_defendant_present = "Black" %in% c(defendant_race,
                                             second_defendant_race,
                                             third_defendant_race,
                                             fourth_defendant_race),
    year = first(year)) %>%
  ungroup()

sidebar_plot2 <- sidebarPanel(
  checkboxGroupInput(inputId = "race2",
                     label = "Race of juror:",
                     choices = c("Black", "White", "Unknown"),
                     selected = c("Black","White", "Unknown")
  ),
  actionButton(inputId = "line_toggle",
               label = "Highlight Mean Strike Rates",
               style="color: #fff; background-color: #619CFF; 
                     border-color: black; border-radius: 10px; border-width: 2px"
              ),
  sliderInput(inputId = "range2", 
              label = "Show data for trials in the years:",
              value = c(min(plot2_data$year),max(plot2_data$year)), 
              min = min(plot2_data$year),
              max = max(plot2_data$year),
              step = 1,
              timeFormat = '%Y')
)

plot2_text <- "Fig. 2 above shows the distribution of trials based on the rate at which jurors of different 
              races were struck in those trials, and whether or not the trial had a Black defendant."

plot2_takeaways1 <- "Based on this visualization, it is clear that Black jurors are struck by 
                     the state at a much higher rate than either white or unknown race jurors
                     regardless of the year of the trial and the race of the defendant."

plot2_takeaways2 <- "On top of this, Black jurors are struck at a noticeably higher
                     rate in trials that had a Black defendant, while white jurors 
                     are actually struck at a slightly lower rate in trials with Black
                     defendants, suggesting a pattern of racial bias in which jurors
                     the state struck."

 
ui <- fluidPage(
  #make the background interesting
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "linear",
    direction = "bottom"),
  #create title and main panel for entire website page
  titlePanel("Visualizing Racial Bias in Juror Strike Rates"),
  mainPanel(
    #within website page create tabs for the two different plots, each of which will have a sidebar and main layout
    tabsetPanel(id="main", 
                tabPanel("Figure 1: Juror Strikes/Selections by Race Over Time", 
                         sidebarLayout(sidebar_plot1,
                                       mainPanel( 
                                         #main will have a vertical layout so we can have description underneath
                                         verticalLayout(
                                         plotOutput(outputId = "plot1", height = "300px"),
                                         #format text box for description/takeaways
                                         div(strong("Description:"),br(),
                                             plot1_text,br(),br(),
                                         strong("Takeaways:"),br(),
                                         #add bulleted list for takeaways
                                         tags$ul(
                                           tags$li(tags$span(plot1_takeaways1)),
                                           tags$li(tags$span(plot1_takeaways2)),
                                           tags$li(tags$span(plot1_takeaways3))), 
                                         style = "color:black; background-color:white; 
                                                   margin-top:20px; border: 2px solid blue; 
                                                   border-radius: 8px; font-size: medium; 
                                                   padding-top: 5px; padding-right: 5px; 
                                                   padding-bottom: 5px; padding-left: 5px;"))))),
                tabPanel("Figure 2: Juror Strike Rates by Defendant Race",
                         sidebarLayout(sidebar_plot2,
                                       mainPanel(
                                         verticalLayout(
                                           plotOutput(outputId = "plot2", height = "500px"),
                                           textOutput(outputId = "button"),
                                           div(strong("Description"), br(), plot2_text, br(),
                                               strong("Takeaways:"), br(),
                                               tags$ul(
                                                 tags$li(tags$span(plot2_takeaways1)),
                                                 tags$li(tags$span(plot2_takeaways2))), 
                                               style = "color:black; background-color:white; 
                                                   margin-top:20px; border: 2px solid blue; 
                                                   border-radius: 8px; font-size: medium; 
                                                   padding-top: 5px; padding-right: 5px; 
                                                   padding-bottom: 5px; padding-left: 5px;"))))))))
        

server <- function(input, output){
  output$plot1 <- renderPlot({
    ggplot(filter(plot1_data,
                  struck_by2 == input$struck_by,
                  race %in% input$race)) +
      geom_line(aes(x=year, y=prop, color=race)) +
      xlim(input$range) +
      ylim(0,1) +
      scale_color_manual(values = c("White" = "#0800e6", "Black" = "#e60000")) +
      labs(title="Jurors Struck or Chosen by Defense and State", x="Year",
           y="Proportion of Jurors", color="Juror Race")
  })
  
  output$plot2 <- renderPlot({
    race_selected <- length(input$race2) > 0
    #recalculate mean strike rates based on selected years
    plot2_data <- plot2_data %>% 
      filter(race %in% input$race2, year >= input$range2[1], year <= input$range2[2]) %>%
      group_by(race, black_defendant_present) %>%
      mutate(mean_strike_prop = mean(prop_struck_jurors))
    #plot with or without mean strike rate markers 
    if (input$line_toggle %% 2 == 0 && race_selected) {
      ggplot(filter(plot2_data, race %in% input$race2, 
                    year >= input$range2[1], year <= input$range2[2]), 
             aes(x = prop_struck_jurors, fill = black_defendant_present)) +
        geom_histogram(alpha = 0.5, position = "identity", 
                       color = "black", binwidth = 0.05) +
        facet_wrap(~race) +
        labs(title = plot2_title,
             x = "Proportion of Jurors Struck", y = "Number of Trials",
            color = "Presence of Black Defendant(s)",
            fill = "Presence of Black Defendant(s)")  +
        theme(legend.position = "bottom") + 
        geom_vline(aes(xintercept = mean_strike_prop, 
                       color = black_defendant_present), 
                   linetype = "dashed", size = 1) + 
        geom_point(aes(x = mean_strike_prop, y = 0, fill = black_defendant_present), 
                   shape = 24, size = 3)
    } else if (race_selected) {
      ggplot(filter(plot2_data, race %in% input$race2, 
                    year >= input$range2[1], year <= input$range2[2]), 
             aes(x = prop_struck_jurors, fill = black_defendant_present)) +
        geom_histogram(alpha = 0.5, position = "identity", 
                       color = "black", binwidth = 0.05) +
        facet_wrap(~race) +
        labs(title = plot2_title,
             x = "Proportion of Jurors Struck", y = "Number of Trials", 
             color = "Presence of Black Defendant(s)",
             fill = "Presence of Black Defendant(s)")  +
        theme(legend.position = "bottom")
    }
  })
}

shinyApp(ui = ui, server,options = list(height = 500))
