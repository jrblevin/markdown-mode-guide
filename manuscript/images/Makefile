all: title_page.png

title_page.png: title_page.tex
	xelatex title_page.tex
	convert -density 300 title_page.pdf -quality 100 title_page.png
	-rm title_page.{pdf,out,log,aux}

clean:
	rm title_page.png
