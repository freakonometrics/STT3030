#EQM: retourne l'EQM entre deux vecteurs.
#pred: vecteurs de prédictions
#true: vecteur de vraies valeures
EQM <- function(pred,true){
  return (mean((pred-true)^2))
}


