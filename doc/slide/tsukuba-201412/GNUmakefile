

targets = \
	DSL.html


md_format = \
	markdown+pandoc_title_block+pipe_tables+table_captions+escaped_line_breaks+implicit_figures+strikeout+tex_math_dollars+latex_macros+fenced_code_blocks

math_opt = --latexmathml
#math_opt = --jsmath
#math_opt = --mathjax

slide_opts = \
	--standalone --self-contained --slide-level=2 \
	$(math_opt)
##	--incremental

%.html: %.txt
	pandoc -f $(md_format) -t s5 $(slide_opts) -o $@ $<

%.tex: %.txt
	pandoc -f $(md_format) -t beamer -s --slide-level=2 -o $@ $<


%.dvi %.log %.aux: %.tex
	platex $<


%.pdf: %.dvi
	dvipdfmx $(@:.pdf=.dvi)


all: $(targets)

clean:
	$(RM) $(targets)
	$(RM) -r $(glad_imaged)
##	$(RM) *.dvi *.pdf
##	$(RM) *.aux *.log *.nav *.out *.snm *.toc *.vrb
