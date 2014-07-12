default: PA1_template.html PA1_template.md

%.md: %.Rmd
	Rscript -e "library(knitr); knit(input='$<', output='$@');"

%.html: %.Rmd
	Rscript -e "library(knitr); knit2html(input='$<', output='$@');"

clean:
	rm PA1_template.html PA1_template.md figure/*.png

.PHONY: clean
