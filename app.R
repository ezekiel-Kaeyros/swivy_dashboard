library(plumber)

# Charger le fichier plumber
r <- plumb("plumber.R")

# Démarrer le serveur sur le port 8000 (ou tout autre port de votre choix)
r$run(host = "0.0.0.0", port = 8000)
#plumb(file='plumber.R')$run(port = 8000)
