library(shiny)
library(tidyverse)
library(shinyWidgets)
library(RColorBrewer)
library(lubridate)
library(readr)
library(gganimate)

#pull the data
mergedData <- read_csv("merged_data.csv") 

#make the ml model
cinema_rforest <- readRDS("rforest_model_movie")


server <- function(input, output){
  
  # table displaying total number of movies and shows across all platforms
  output$EDAtable1 <- renderTable({
    mergedData %>%
      group_by(type) %>%
      summarize(numTitles = n()) %>%
      arrange(desc(numTitles))
  })
  
  # Line graph displaying total number of movies and shows across all platforms throughout
  # all release years
  output$EDAplot1 <- renderPlot({
    mergedData %>% 
      group_by(type, release_year) %>% 
      summarize(total = n()) %>%
      ggplot(aes(x = release_year, y = total, color = type)) +
      geom_line()
  })
  
  # table summarizing the top 10 directors who have directed the most movies/shows
  output$EDAtable2 <- renderTable({
    mergedData %>%
      group_by(name) %>%
      summarize(numTitles = n()) %>%
      slice_max(order_by = numTitles, n = 10)
  })
  
  # plot displaying the difference in average imdb scores vs tmdb scores over time
  output$EDAplot2 <- renderPlot({
    mergedData %>%
      group_by(release_year) %>%
      summarize(averageRating = mean(imdb_score,na.rm=T) - mean(tmdb_score,na.rm=T)) %>%
      ggplot(aes(x=release_year, y=averageRating)) +
      geom_point()
  })
  
  # A table for context for the previous boxplot displaying the total number of titles
  # under each streaming service
  output$EDAtable3 <- renderTable({
    mergedData %>%
      group_by(streaming_service) %>%
      summarize(numTitles = n()) %>%
      arrange(desc(numTitles))
  })
  
  # Boxplot displaying the average imdb scores of content on each streaming service
  output$EDAplot3 <- renderPlot({
    mergedData %>% 
      select(title, streaming_service, imdb_score) %>% 
      remove_missing() %>%
      group_by(streaming_service) %>%
      ggplot() +
      geom_boxplot(aes(x = streaming_service, y = imdb_score)) + 
      theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=1)) 
  })
  
  
  #plot for directors
  output$plot1 <- renderPlot({
    #base plot filter by certain directors and with inputted variable names
    basePlot <-
      ggplot(mergedData %>% filter(name %in% input$director, type %in% input$type),
             aes(x=!!input$x_var, y=!!input$y_var, color=name)) +
      geom_point() +
      geom_smooth(method = "lm",se = F)
    
    #adjust base plot if year range is changed
    if(input$x_var == "release_year") {
      basePlot + xlim(year(input$range))
    } else if(input$y_var == "release_year") {
      basePlot + ylim(year(input$range))
    } else {
      basePlot
    }
  })
  
  output$my_brush <- renderPrint({
    brushedPoints(mergedData %>% filter(name %in% input$director) %>% 
                    select(title, release_year, imdb_score, tmdb_score)%>%
                    distinct(), input$my_brush)
  })
  
  #plot the streaming services
  output$plot2 <- renderPlot({
    ggplot(mergedData %>% filter(streaming_service %in% input$services, type %in% input$type2)) +
      geom_line(aes(x=!!input$x_var2, y=!!input$y_var2, color=streaming_service), stat="summary_bin", binwidth=input$binwidth2)
    # +
    #   transition_time(release_year_date) +
    #   ease_aes('linear')
  })
  
  #plot the age certifications
  output$plot3 <- renderPlot({
    ggplot(mergedData %>% filter(age_certification %in% input$age)) +
      geom_line(aes(x=!!input$x_var3, y=!!input$y_var3, color=age_certification), stat="summary_bin", binwidth=input$binwidth3)
    
  })
  
  #our ml random forests model predictions
  output$prediction <- renderText({
    test <- data.frame(streaming_service=input$streaming_select,
                       name=input$name_select,
                       release_year=input$year_select,
                       runtime=input$runtime_input)
    prob <- predict(cinema_rforest_movie, newdata=test, type="prob")[1]
    pred <- ifelse(prob < 0.67, TRUE, FALSE)
    greatness <- ifelse(pred, "be great :)", "not be great :(")
    paste0("With the given characteristics, we predict that your movie would ", 
           greatness, ". The probability that your movie is great is: ", 1-prob, 
           ". The threshold to being great is 0.33.")
  })

}
