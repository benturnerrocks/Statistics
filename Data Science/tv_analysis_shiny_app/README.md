June, Spring 2022, Stats 220, Katie St.Clair

Link: https://johnsont4.shinyapps.io/final-project-benturner-teaganjohnson-kentahikino/

List of Files:
dataCleaning.R
- is where we merged all the data csv files into one massive dataframe,
which is then joined to the credits csv files to get merged_data.csv

merged_data.csv
- see above

app.R
- is the file that runs the shinyApp

server.R
- File that hosts the shinyApp server

ui.R
- File that hosts the user interface for the shinyApp

ML_models_testing.Rmd (and rforest_model)
This file contains the setup of our random forest model used to predict a user’s input. We create the random forest model, then export it to the directory in a file called “rforest_model”).




Technical Report:

Movies and TV shows have been a large part of the entertainment industry for more than a century. In recent times, both (especially TV shows) have become exponentially more popular. This is in large part due to the massive growth of streaming services like Netflix and Amazon Prime. We were interested in seeking out trends with movies and TV shows along with their directors, release years, and any other interesting data points. To do this, we acquired 9 data sets from 9 different streaming services (Amazon Prime, Netflix, Disney+, Paramount+, Hulu, HBO Max, Dark Matter, Crunchyroll, and Rakuten). Below is a description of the technical work of our project.

The format for the data was the same for each streaming service. For each service, there was a “titles.csv” and “credits.csv”. The titles file contained information pertaining directly to the movie. The credits file contained information pertaining to the actors, the characters they played, and the directors for each show and movie. After downloading these datasets from Kaggle, we did some preliminary data wrangling and cleaning to ensure that (1) the dataset would be easier to use and (2) the dataset would be small enough to avoid memory limitations. We decided to only consider directors (rather than cast members) to avoid too large of a dataset (almost 400,000 rows if including cast members). After condensing the dataset, we bound the rows of all 9 data sets to get a data frame of 28,363 movies, each row representing a movie. We then joined this dataframe with the corresponding credits dataframe by movie id to get our final dataframe.

Our next step was to clean and wrangle the dataset. This included changing types of columns from numeric types to date types and from character types to logical types. 

We also added a new column to act as a classification for a statistical learning implementation. The goal of this statistical learning analysis was to predict whether a movie would be considered great based on _____ variables (whether a movie is top 10% or not). The cells in this column are “True” if that movie is in the top 15% of IMDB scores and “False” otherwise. Our final dataset had 21 variables including runtime, director, streaming service, and more. 

Using the machine learning model, we were able to predict “great” movies with an accuracy of 84% (sensitivity of 30%, specificity of 90%, precision of 25%). This model, specifically the model’s threshold, was chosen based on analyzing the metrics at incremented threshold values and choosing the threshold with the highest overall metrics (.67). See the ML_models_testing.Rmd file for a more detailed overview of our process.

The final dataset was then used to create an interactive Shiny website. We included a home page that includes some basic EDA, 3 pages that contain interactive time series plots, and a page where users can input their own information and receive feedback about whether their movie will be great or not.
