%.html: %.Rmd
	Rscript -e "library(knitr); knit2html(input='$<', output='$@');"

