TARGETS=global-visibility.png title_page.png markup-hiding.png url-hiding.png

all: $(TARGETS)

global-visibility.png: global-visibility-all.png global-visibility-overview.png global-visibility-contents.png
	convert $^ -background transparent -splice 10x0+0+0 +append -chop 10x0+0+0 $@

title_page.png: title_page.tex
	xelatex title_page.tex
	convert -density 300 title_page.pdf -quality 100 title_page.png
	-rm title_page.{pdf,out,log,aux}

markup-hiding.png: markup-hiding-off.png markup-hiding-on.png
	convert $^ -background transparent -splice 10x0+0+0 +append -chop 10x0+0+0 $@

ham-mode.png: ham-mode-html.png ham-mode-markdown.png
	convert $^ -background transparent -splice 10x0+0+0 +append -chop 10x0+0+0 $@

url-hiding.png: url-hiding-off.png url-hiding-on.png
	convert $^ -background transparent -splice 10x0+0+0 +append -chop 10x0+0+0 $@

mathjax.png: mathjax-markdown.png mathjax-preview.png
	convert $^ -background transparent -splice 0x10+0+0 -append -chop 0x10+0+0 $@

inline-images.png: inline-images-off.png inline-images-on.png
	convert $^ -background transparent -splice 10x0+0+0 +append -chop 10x0+0+0 $@

clean:
	rm $(TARGETS)
