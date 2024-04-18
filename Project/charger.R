charger <- function(matricule) {
  set.seed(matricule)
  mondata <- read.csv2("DevoirDH23.csv")[sample(290,195),-1]
}