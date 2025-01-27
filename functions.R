########################################################################
## ---------------------------------------------------------------------
##
## Data processing, analyses, and visualizations for 
## REEF Lionfish Trap Project
##
## sites-and-surveys.R

## FUNCTIONS TO WRITE OUT ANALYSES
sig_stars <- function(P){
  stars = P
  stars[P>0.1] = "" 
  stars[P<=0.1] = "." 
  stars[P<=0.05] = "*" 
  stars[P<=0.01] = "**" 
  stars[P<=0.001] = "***" 
  return(stars)
}

## FUNCTION TO OUTPUT PARAMETERS AND SIGNIFICANCE FROM LOG-TRANSFORMED GLM
coeftab_glm <- function(mod, response = "", model = "", dec_places = 3){
  #mod = lf_nb
  coefs = as.data.frame(coef(summary(mod)))
  out = data.frame(Effects = rownames(coefs))
  out$Est = round(exp(coefs$Estimate), dec_places)
  out$Est[1] = round(coefs$Estimate[1], dec_places)
  upCI  = round(exp(coefs$Estimate + 1.96 * coefs$`Std. Error`), dec_places)
  lowCI = round(exp(coefs$Estimate - 1.96 * coefs$`Std. Error`), dec_places)
  upCI[1]  = round(coefs$Estimate[1] + 1.96 * coefs$`Std. Error`[1], dec_places)
  lowCI[1] = round(coefs$Estimate[1] - 1.96 * coefs$`Std. Error`[1], dec_places)
  out$'95% CI' = paste(sprintf("%.3f", lowCI), "-", sprintf("%.3f", upCI), sep = "")
  out$test_stat = round(coefs[3], dec_places)
  out$P = coefs[4]
  names(out$test_stat)=names(coefs[3])
  P = coefs[,4]
  P = sprintf("%.3f", P)
  P[P<0.001] = "<0.001" 
  out$P = P
  out$Sig = sig_stars(P)
  out$Estimate = NULL; out$`Std. Error`=NULL
  if (response != "") out$Response = response
  if (model    != "") out$Model    = model
  return(out)
}
