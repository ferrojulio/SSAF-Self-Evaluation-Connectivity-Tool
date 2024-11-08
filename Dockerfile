# Use a base R image
FROM rocker/shiny:latest

# Install renv package
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org/')"

# Install system dependencies (libmysqlclient-dev, libudunits2-dev, libgdal-dev)
RUN apt-get update && apt-get install -y libmysqlclient-dev libudunits2-dev libgdal-dev

# Create and set the working directory
RUN mkdir /srv/shiny-server/myapp
WORKDIR /srv/shiny-server/myapp

# Copy the app code, renv.lock, and CSV file into the image
COPY app.R renv.lock .Renviron 202308_SS_Connectivity_EvalTool_StrataInfo2.csv ./

# Copy the contents of the www folder into the image
COPY www /srv/shiny-server/myapp/www

# Restore the R environment using renv
RUN R -e "renv::restore()"

# Set the Renviron environment variable to the path of the copied .Renviron
ENV R_ENVIRON /srv/shiny-server/myapp/.Renviron

# Expose the Shiny port
EXPOSE 3838

# Start the Shiny server
CMD ["R", "-e", "shiny::runApp(host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT')))"]
