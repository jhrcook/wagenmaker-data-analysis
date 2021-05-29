help:
	@echo "available commands"
	@echo " - help           : print descriptions of make options"
	@echo " - install        : setup programming environment"
	@echo " - style          : run style guides over all code files"
	@echo " - build          : knit documents and build the website"
	@echo " - clear          : clear cache and website files"
	@echo " - clean_build    : clear and build"

install:
	Rscript -e "install.packages('renv')"
	Rscript -e "renv::restore()"

style:
	Rscript -e "styler::style()"

build: style
	Rscript -e "rmarkdown::render_site()"

clear:
	rm -r .mustashe docs

clean_build: clear style build
