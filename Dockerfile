# Utiliser l'image de base R
FROM rocker/r-ver:4.4.1

# Installer les dépendances nécessaires
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

# Installer les packages R requis
RUN R -e "install.packages(c('plumber', 'mongolite', 'tidyr', 'dplyr', 'chron', 'purrr', 'stringr', 'lubridate', 'ggplot2', 'plotly'))"

# Définir le répertoire de travail
WORKDIR /app

# Copier le script Plumber et d'autres fichiers nécessaires
COPY . .

# Exposer le port sur lequel l'application s'exécute
EXPOSE 8000

# Commande pour exécuter l'application
CMD ["Rscript", "app.R"]
