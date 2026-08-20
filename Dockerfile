# Base R Shiny image
FROM rocker/r-ver:4.2.0

# Install R dependencies
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"

# Copy the Shiny app code
WORKDIR /app
COPY renv.lock renv.lock
RUN R -e "renv::restore()"
COPY app.R app.R
COPY data/data.Rdata data/data.Rdata

# Expose the application port
EXPOSE 8080

# Run the R Shiny app
CMD ["R", "-e", "shiny::runApp('./app.R', host='0.0.0.0', port=8080)"]

## voer uit lokaal:
## docker build -t rumigestapp .
## docker run -p 8080:8080 rumigestapp
