library(shiny)
library(tidyverse)
library(shinyWidgets)
library(RColorBrewer)
library(lubridate)
library(caret)
library(randomForest)
library(shinythemes)

#pull the data

mergedData <- read_csv("merged_data.csv")

#text analyses for plots (both for homepage and others)

titleText <- "Welcome to Streaming Central, your home for
all data streaming-related! Our goal is to show different
data trends across all streaming services with respect to many
different variables. Below is a series of plots and tables that
display some exploratory analysis"

EDAplot1Text <- "The plot above shows that over time, both movies
and TV shows have become more popular. It appears that there have
been significantly more movies than TV shows released per year. 
The table above to the right further emphasizes that there are more
movies than TV shows in our data set."

EDAplot2Textp1 <- "The plot above shows the difference between both of
the ratings websites (IMDB and TMDB) ratings over time. It appears the
difference between IMDB - TMDB scores has decreased, which means either
IMDB has given lower ratings over time or TMDB has given higher ratings
over time."

EDAplot2Textp2 <- "The table above to the right shows the top 10 directors
with the most movies/TV shows directed. Joseph Kane ranks 1st with 108 movies
directed."

EDAplot3Text <- "The plot above displays boxplots for the average IMDB scores
of content per each streaming service. The idea is to get a sense of the quality
of content each streaming service provides, thus giving users the ability to see
which streaming service might be worth subscribing to. The adjacent table shows
the total amount of content on each streaming service, providing context to the
average scores shown in each boxplot. For example, Amazon Prime looks to be one
of the less desirable streaming services with a lower average score of content,
the greatest range, and a large number of outliers with scores under 2.5. But
the service is saturated with over 10,000 movie/show options which may appeal
to viewers. Meanwhile, the popular Japanese Anime streaming service CrunchyRoll
looks to be the most consistently high-quality streaming service, with the
smallest range and high average movie/show rating, but the table shows that
there are only 318 content options."

plot_head <- "Overview:"
plot_end <- "Analysis:"

plot1_text <- "This directors tab allows you to examine trends pertaining to the 
directors across all streaming platforms. You can change the time period of 
movies’ release dates with the release_year double slider. "
plot1_takeaways1 <- "If the x-variable is set to the release year, the y-variable 
is set to the content’s IMDb score, and you select the 5 legendary movie directors 
Christopher Nolan, Akira Kurosawa, Jon Lasseter (Pixar movies), George Lucas 
(Star Wars), and Steven Spielberg, you can easily visualize the decline in rating 
of each in their later released movies. Even though their legacies span over 
different time periods, each one displays a worrying downward trend in ratings. 
Perhaps these directors’ first several movies were major hits and critics’ 
expectations were raised too high for their later movies. Or perhaps as time 
went on, critics simply became more critical of content. "


plot2_text <- "This tab allows you to examine trends of shows and movies over 
time with respect to streaming services. You can change the x and y axes to 
represent whichever variable you would like to examine."
plot2_takeaways1 <- "It appears Amazon Prime has lower average scores as compared 
to each of the other streaming services. This may be due to the fact that Amazon 
prime favors “quantity over quality” and would prefer to include shows even if 
they are relatively poorly rated."


plot3_text <- "This tab allows you to examine trends pertaining to the age 
certification of movies and shows. The bin slider allows you more easily visualize 
the trend line, especially when comparing more than 3 age certifications."
plot3_takeaways1 <- "One interesting trend that can be seen above is that R rated 
movies are generally rated lower than G, PG, and PG-13 movies. It's possible that 
since the lower-rated movies appeal to a larger audience, their average scores are higher."

tab4_text <- "You get to create a movie! Select which director you’d like to hire, 
which year the movie was released, which streaming service your movie is 
distributed to, and how long your movie will be. The prediction is based on a 
random forest classification model that has an overall accuracy of 84% 
(see github on front page for more info). It’s important to note the runtime variable 
has the most impact on whether a movie will be rated highly or not. In fact, it 
is significantly more powerful than the other three factors."

limit_text <- "It’s important to note the limitations of our data. First,
there are many movies/shows that have not received IMDB or TMDB scores. In
some parts of our analysis, we got rid of the media with no scores. It’s
possible this induced selection bias if the movies without scores would
have been rated higher or lower, especially in our statistical learning
model. Second, there are many movies and TV shows that are not available
on streaming services. This includes new releases and some others that have
never been released to streaming services. This limits the scope of our analysis
to only include the shows and movies that are available on these streaming services."

sources_text <- "We credit our data to Victor Soeiro on Kaggle, which uses ratings
from two movie databases: IMDB (Internet Movie Database) and TMBD (The Movie Database).
To get more information, you can visit the links below or go to our GitHub page to learn more."

have_fun_text <- "Check out the pages above to explore director trends, streaming
service trends, age certification trends, and statistical learning predictions!"

#some common things to limit duplication

xVarPanel <- #create a choice bar for picking x variable to look at
  function(id) {
    varSelectInput(inputId = id,
                   label = "Select the x variable:",
                   data = mergedData %>% select(where(is.numeric),-person_id),
                   selected = "release_year")
  }

yVarPanel <- #create a choice bar for picking y variable to look at
  function(id) {
    varSelectInput(inputId = id,
                   label = "Select the y variable:",
                   data = mergedData %>% select(where(is.numeric),-person_id),
                   selected = "imdb_score")
  }

typePanel <- 
  function(id) {
    checkboxGroupInput(inputId = id,
                       label = "Select type",
                       choices = c("MOVIE", "SHOW"),
                       selected = c("MOVIE", "SHOW"))
  }

sliderPanel <- #provide a slider to adjust the date range for an x-variable of releasedate
  function(id) {
    sliderInput(inputId = id, 
                label = "Show range of movie data over release years:",
                value = c(min(mergedData$release_year_date),max(mergedData$release_year_date)), 
                min = min(mergedData$release_year_date),
                max = max(mergedData$release_year_date),
                step = 1,
                timeFormat = '%Y')
  }

numericPanel <- #numeric input to control width of bins for x-axis
  function(id) {
    numericInput(inputId=id,
                 label="Select the bin width",
                 value=1,
                 max=30,
                 min=1,
                 step=1)
  }

animatePanel <- #inactive action button to toggle animation
  function(id) {
    actionButton(inputId = id,
                 label = "Animate",
                 style="color: #fff; background-color: #619CFF; 
                       border-color: black; border-radius: 10px; border-width: 2px"
    )
  }

# Director sidebar

plot1_title <- "Directors"

#creating the sidebar for the directors 
sidebar_directors <- sidebarPanel(
  #create an autocomplete selector for searching directors
  selectizeInput(inputId = "director",
                 label = "Input the directors you want to look at:",
                 choices = mergedData %>% select(name) %>% distinct(),
                 selected = "Joseph Kane", 
                 multiple = T,
                 options = NULL),
  xVarPanel("x_var"),
  yVarPanel("y_var"),
  typePanel("type"),
  sliderPanel("range"))

# streaming service sidebar

plot2_title <- "Streaming Services"

sidebar_services <- sidebarPanel(
  #create an autocomplete selector for searching services
  selectizeInput(inputId = "services",
                 label = "Input the streaming services you want to look at:",
                 choices = mergedData %>% select(streaming_service) %>% distinct(),
                 selected = "Netflix", 
                 multiple = T,
                 options = NULL),
  xVarPanel("x_var2"),
  yVarPanel("y_var2"),
  numericPanel("binwidth2"),
  typePanel("type2")
  )


# age certification sidebar

plot3_title <- "Age Certification"

sidebar_age <- sidebarPanel(
  selectizeInput(inputId = "age",
                 label = "Input the age certification you want to look at:",
                 choices = mergedData %>% select(age_certification) %>% distinct(),
                 selected = "R", 
                 multiple = T),
  xVarPanel("x_var3"),
  yVarPanel("y_var3"),
  numericPanel("binwidth3"))

#the machine learning section to predict good movies

graphic4_title <- "Prediction"

sidebar_ml <- sidebarPanel(
  selectInput(inputId="name_select",
              "Director",
              choices=unique(mergedData$name)
  ),
  selectInput(inputId="streaming_select",
              "Streaming Service",
              choices=unique(mergedData$streaming_service)
  ),
  selectizeInput(inputId="year_select",
                 "Year Choice",
                 choices=unique(mergedData$release_year)
  ),
  numericInput(inputId="runtime_input",
               "Runtime",
               value=90)
)

ui <- navbarPage(title="Streaming Central",
      tabPanel(title="Front Page",
            fluidPage(theme=shinytheme("sandstone"),
                    h1("Welcome to Streaming Central!"),
                    HTML('<center><img src="watching2.jpeg" width="400"></center>'),
                    HTML('<center>Welcome to Streaming Central, your home for
                      all streaming-related data! Our goal is to show different
                        data trends across all streaming services with respect to many
                        different variables. Below is a series of plots and tables that
                        display some exploratory analysis</center>'),
                    br(),
                    hr(),
                    br(),
                    h2("Figures 1a and 1b:"),
                    fluidRow(column(width = 8,
                                    plotOutput(outputId = "EDAplot1", height = "300px")
                                    ),
                             column(width = 1, offset = 1,
                                    tableOutput(outputId = "EDAtable1"))),
                    h3(plot_end),
                    p(EDAplot1Text),
                    hr(),
                    h2("Figures 2a and 2b:"),
                    fluidRow(column(width = 8,
                                    plotOutput(outputId = "EDAplot2", height = "300px"),
                                    h3(plot_end),
                                    p(EDAplot2Textp1),
                                    p(EDAplot2Textp2)
                    ),
                        column(width = 1, offset = 1,
                           tableOutput(outputId = "EDAtable2"))),
                    hr(),
                    h2("Figures 3a and 3b:"),
                    fluidRow(column(width = 8,
                                    plotOutput(outputId = "EDAplot3", height = "300px")
                    ),
                    column(width = 1, offset = 1,
                           tableOutput(outputId = "EDAtable3"))),
                    h3(plot_end),
                    p(EDAplot3Text),
                    br(),
                    hr(),
                    br(),
                    fluidRow(column(width = 6,
                                    h3("Limitations and Future Research:"),
                                    p(limit_text)
                    ),
                    column(width = 4, offset = 2,
                           h3("Sources:"),
                           p(sources_text),
                           HTML("
                           <p><a href='https://www.kaggle.com/victorsoeiro/datasets'>Kaggle Source</a>, 
                           <a href='https://github.com/
                                data-science-stat-220-s22/final-project-benturner-
                                teaganjohnson-kentahikino'>GitHub</a>, 
                                <a href='https://www.imdb.com/'>IMDB</a>, and 
                                <a href='https://www.themoviedb.org/?language=en-US'>TMDB</a>"),
                           h3("Have Fun!!"),
                           p(have_fun_text)
                           ))
            ) #end fluidPage
      ), #end tabPanel
      tabPanel(title=plot1_title,
        fluidPage(theme=shinytheme("sandstone"),
             sidebarLayout(sidebar_directors,
                 mainPanel(
                   #main will have a vertical layout so we can have description underneath
                   verticalLayout(
                     plotOutput(outputId = "plot1", height = "300px", 
                                brush=brushOpts(id="my_brush")),
                     h3("Drag over the graph to get point info!"),
                     verbatimTextOutput("my_brush"),
                     h3(plot_head),
                     p(plot1_text),
                     h3(plot_end),
                     tags$ul(
                       tags$li(plot1_takeaways1)
                     )
                 ) # end verticalLayout
            ) # end mainPanel
          ) # end sidebarLayout
        ) # end fluidPage
      ), # end tabPanel
      tabPanel(title=plot2_title,
          fluidPage(theme=shinytheme("sandstone"),
               sidebarLayout(sidebar_services,
                   mainPanel(
                     verticalLayout(
                         plotOutput(outputId = "plot2", height = "500px"),
                         textOutput(outputId = "button"),
                         h3(plot_head),
                         p(plot2_text),
                         h3(plot_end),
                         tags$ul(
                           tags$li(plot2_takeaways1)
                         )
                       ) # end verticalLayout
                     ) # end mainPanel
                   ) # end sidebarLayout
            ) # end fluidPage
       ), # end tabPanel
      tabPanel(title=plot3_title, 
          fluidPage(theme=shinytheme("sandstone"),
               sidebarLayout(sidebar_age,
                   mainPanel( 
                     verticalLayout(
                       plotOutput(outputId = "plot3", height = "300px"),
                       #format text box for description/takeaways
                       h3(plot_head),
                       p(plot3_text),
                       h3(plot_end),
                       tags$ul(
                         tags$li(plot3_takeaways1)
                       )
                       ) # end verticalLayout
                     ) # end mainPanel
                   ) # end sidebarLayout
                  ) # end fluidPage
               ), # end tabPanel
      tabPanel(title=graphic4_title,
          fluidPage(theme=shinytheme("sandstone"),
               sidebarLayout(sidebar_ml,
                 mainPanel(
                   verticalLayout(
                     h3("Will your movie be great?"),
                     textOutput(outputId="prediction"),
                     h3(plot_end),
                     p(tab4_text)
                     ) # end verticalLayout
                   ) # end mainPanel
                 ) # end sidebarLayout
              ) # end fluidPage
          ) # end tabPanel
      ) # end navbarPage
                                       







