# Use an R runtime as a base image
FROM rocker/shiny:latest

# Make a directory in the container
RUN mkdir /home/shiny-app

# Copy the Shiny app files into the container
COPY app.R /home/shiny-app/app.R
COPY data.csv /home/shiny-app/data.csv

# Install R dependencies
RUN R -e "install.packages(c('shiny','dplyr', 'ggplot2', 'lubridate','magrittr'))"

# Expose the application port
EXPOSE 8000

# Command to run the Shiny app
CMD ["R", "-e", "shiny::runApp('/home/shiny-app/app.R')"]