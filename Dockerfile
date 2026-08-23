# Base R Shiny image
FROM rocker/r-ver:4.5.1

RUN apt-get update && apt-get install -y wget curl software-properties-common

RUN apt-get update && \
    apt-get install -y libwebp-dev libwebp7 wget curl build-essential && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

RUN apt-get update && \
    apt-get install -y libmysqlclient21 && \
    rm -rf /var/lib/apt/lists/*

RUN apt-get update && \
    apt-get install -y unixodbc && \
    rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y \
    libuv1-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libgit2-dev \
    && rm -rf /var/lib/apt/lists/*

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
