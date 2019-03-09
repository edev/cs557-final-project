all: dir 
# latex biber latex2 dvi ps cp

dir:
	rm -rf build
	mkdir build
	cp -r bibliography.bib *.tex build
	cd build && pdflatex main.tex
	cd build && biber main
	cd build && pdflatex main.tex
	mv build/main.pdf CS_557_final_project_report.pdf

latex: *.tex
# latex -output-directory build main.tex
	pdflatex main.tex

biber:
	cp bibliography.bib build
	cd build && biber main

latex2:
	latex 
dvi: build/main.dvi
	cd build && dvips main.dvi

ps: build/main.ps
	cd build && ps2pdf main.ps

cp:
	cp build/main.pdf CS_557_final_project_report.pdf
