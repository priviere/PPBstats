# Tikz figure
# cf here https://www.overleaf.com/project/5d1b33982e6ec5130bb4b3df
setwd("inst/bookdown/figures/")

vec_f = c(
  "decision-tree_agro-fully-replicated-design.tex",
  "decision-tree_agro-ibd-design.tex",
  "decision-tree_agro-regional-and-satellite-design.tex",
  "decision-tree_agro-row-column-design.tex",
  "decision-tree_all-head.tex",
  "decision-tree_all.tex",
  "decision-tree_Compare-different-varieties-evaluated-for-selection-in-different-locations_agro.tex",
  "decision-tree_Compare-different-varieties-evaluated-for-selection-in-different-locations_senso.tex",
  "decision-tree_Improve-the-prediction-of-a-target-variable-for-selection.tex",
  "decision-tree_Study-diversity-structure-and-identify-complementary-or-similar-parents-for-cross.tex",
  "decision-tree_Study-network-of-seed-circulation.tex",
  "decision-tree_Study-the-response-of-varieties-under-selection-over-several-environments.tex",
  "main-functions-agro.tex",
  "main-functions-agro-family-1.tex",
  "main-functions-agro-family-2.tex",
  "main-functions-agro-family-4-SR.tex",
  "main-functions-agro-family-4-HALF.tex",
  "main-functions-agro-family-5.tex",
  "main-functions-network.tex",
  "main-functions-organo.tex",
  "secondary-functions.tex"
)

for(f in vec_f){
  f = sub(".tex", "", f)
#  system(paste("pdflatex -no-file-line-error -interaction=nonstopmode ", f,".tex", sep = ""))
  system(paste("convert -density 300 -quality 100 ", f,".pdf ", f,".png", sep = ""))
  system("rm *.aux *.log")
}


setwd("../../../")

