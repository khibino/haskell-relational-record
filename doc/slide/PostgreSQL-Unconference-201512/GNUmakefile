

targets = \
	Query.html


md_format = \
	markdown+pandoc_title_block+pipe_tables+table_captions+escaped_line_breaks+implicit_figures+strikeout+tex_math_dollars+latex_macros+fenced_code_blocks

math_opt = --latexmathml
#math_opt = --jsmath
#math_opt = --mathjax

slide_opts = \
	--self-contained --standalone --slide-level=2 \
	$(math_opt)
##	--incremental

%.html: %.md
	pandoc -f $(md_format) -t s5 -r markdown $(slide_opts) -o $@ $<

%.tex: %.md
	pandoc -f $(md_format) -t beamer -s --slide-level=2 -o $@ $<


%.dvi %.log %.aux: %.tex
	platex $<


%.pdf: %.dvi
	dvipdfmx $(@:.pdf=.dvi)


all: $(targets)

clean:
	$(RM) $(targets)
##	$(RM) *.dvi *.pdf
##	$(RM) *.aux *.log *.nav *.out *.snm *.toc *.vrb
