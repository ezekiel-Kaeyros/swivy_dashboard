library(plumber)

# Charger le fichier plumber
r <- plumb("plumber.R")

# Démarrer le serveur sur le port 8000 (ou tout autre port de votre choix)
r$run(host = "0.0.0.0",port = 8000)

#"eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpYXQiOjE3MTk0MTI1MTcsInVzZXJJRCI6MX0.8M-RlXmlgFL1PZc3aroIxoiCpagl3r78hpDRhLyarDk"
