# Base R Shiny image
FROM rocker/r-ver:4.2.0

# Install R dependencies
RUN R -e "install.packages(c('dplyr', 'tidyr','echarts4r','lubridate','bslib','tools','readxl'))"

# Copy the Shiny app code
COPY app.R /app/
# COPY data.Rda /app

WORKDIR /app

# Expose the application port
EXPOSE 3838

# Run the R Shiny app
CMD ["R", "-e", "shiny::runApp('./app.R', host='0.0.0.0', port=8080)"]
