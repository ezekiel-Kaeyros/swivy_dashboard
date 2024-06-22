# FROM rocker/r-ver:4.4.1

# # Installer les dépendances nécessaires
# RUN apt-get update && apt-get install -y \
#     libcurl4-openssl-dev \
#     libssl-dev \
#     libxml2-dev

# # Installer les packages R requis
# RUN R -e "install.packages(c('plumber', 'mongolite', 'tidyr', 'dplyr', 'chron', 'purrr', 'stringr', 'lubridate', 'ggplot2', 'plotly'))"

# # Définir le répertoire de travail
# WORKDIR /app

# # Copier le script Plumber et d'autres fichiers nécessaires
# COPY . .

# # Exposer le port sur lequel l'application s'exécute
# EXPOSE 8000

# # Commande pour exécuter l'application
# CMD ["Rscript", "app.R"]

FROM rocker/r-ver:latest

# install os dependencies
RUN apt-get update -qq
RUN apt-get install -y --no-install-recommends \
  git-core \
  libssl-dev \
  libcurl4-gnutls-dev \
  curl \
  libsodium-dev \
  libxml2-dev \
  && rm -rf /var/lib/apt/lists/*

# install pak alternatives to install.packages
RUN Rscript -e "install.packages('pak', repos = sprintf('https://r-lib.github.io/p/pak/stable'))"

# install latest plumber from github main branch
RUN Rscript -e "pak::pkg_install('rstudio/plumber@main')"
RUN R -e "install.packages(c( 'mongolite', 'tidyr', 'dplyr', 'chron', 'purrr', 'stringr', 'lubridate', 'ggplot2', 'plotly'))"


# install other R packages
RUN Rscript -e "pak::pkg_install(c('logger','tictoc', 'fs'))"

# setup workspace
COPY . /app

WORKDIR /app

ENTRYPOINT ["Rscript"]

CMD ["app.R"]

EXPOSE 8000