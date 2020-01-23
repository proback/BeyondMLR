library(tidyverse)
library(bookdown)

setwd("~/BYSH PDF/")

#render entire book
render_book("index.Rmd", output_format = 'bookdown::pdf_book')

#preview chapter as pdf
preview_chapter("08-Introduction-to-Multilevel-Models.Rmd", output_format = 'bookdown::pdf_book')

#preview chapter as html
preview_chapter("07-Correlated-Data.Rmd", output_format = 'bookdown::gitbook')

#remove the file if knitting went wrong
bd <- "bookdown-bysh.Rmd"
if(file.exists(bd)) file.remove(bd)

##How to convert to a nice pdf format:
## - Put spaces after all #,##,### headers (#{2,}[a-z,A-Z]+)
## - Add \newcommands to preamble.tex
## - Add this to the top chunk:
##    if(knitr::is_html_output()){options(knitr.table.format = "html")} else {options(knitr.table.format = "latex")}
## - Add R inline chunk to define newcommands if knitr::is_html_output()
## - eqnarray shouldn't have empty lines in them
## - escape = F in tex tables
## - Can't have math expressions in \textrm (e.g. \textrm{age^2} should be changed to \textrm{age}^2)
## - Can't have line breaks in equation environments, should convert to eqnarray or align or some other

##ISSUES
## - Current undiagnosable issue in Chapter 6
##   - Got it to work, was an issue with the eqnarray environment, so I switched to align
## - A different issue with Chapter 7
##   - Fixed issue, it was with & in eqnarray, switched to alignedat for best results
##   - Current issue with \% needed for pdf, but then \ showing up in LaTeX
## - Chapter 8 also had issues with equation array for a cases.
##   - Resolved by changing from array environment to tabular environment. Note that cases environment from amsmath did not work here. 


##Notes on knitting:
##  - Best to use tinytex package from yihui for compatbility
##  - Updated preamble.tex for the Shaded environment.
##  - Make sure to set kableExtra to pdf mode and not have it set to html as an argument. 
##  - should update index.Rmd with the necessary packages at the top.


##Notes for Paul for PDF:
## - Most tables do not fit due to headers that are too wide, text needs to be abbreviated.
## - Quotes appear differently, and are very blockly and unclear under krantz style. Consider restyling or removing block quotes. 
## - some equations don't fit nicely.


#install packages
#install.packages("ggmosaic")
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("aloy/lmeresampler")