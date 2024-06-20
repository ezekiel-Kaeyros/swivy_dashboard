FROM rocker/shiny

# Installation de l'openjdk
RUN apt-get update && apt-get install -y openjdk-8-jdk

# Installation des dépendances R spécifiées
RUN R -e "install.packages(c('plumber', 'mongolite', 'tidyr', 'dplyr', 'chron', 'purrr', 'stringr', 'lubridate'))"
RUN R -e "install.packages('plumber 1.2.2', type = 'source')"


# Make a directory in the container
WORKDIR /app

# Copy your files into the container
COPY . /app
COPY plumber.R /app/plumber.R


# Installation libglpk40
RUN apt-get update && apt-get install -y libglpk40

RUN apt-get update && apt-get install -y libsecret-1-0

# Installation des dépendances système pour les packages R
# Installation des dépendances système pour les packages R
RUN apt-get update && apt-get install -y libudunits2-dev libproj-dev libgdal-dev libgeos-dev libgsl-dev
# Installation de libgsl
# Expose the application port
EXPOSE 8180

# Run the R Shiny app
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 8180)"]
