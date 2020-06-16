# script to automatically render files from crontab (MacOS)
library(rmarkdown)

# pandoc location must be hard coded. Find with Sys.getenv("RSTUDIO_PANDOC")
Sys.setenv(RSTUDIO_PANDOC="/Applications/RStudio.app/Contents/MacOS/pandoc")

rmarkdown::render('web-summary.Rmd',output_file='web-summary.html')
setwd("docs")
rmarkdown::render('GA-model.Rmd',output_file='GA-model.html')
