syntax: src/PhiToGraph/Syntax/Test
pygments: pygments/setup.py
pdf: doc/Syntax.pdf

all: syntax pygments pdf

clean:
	cd src && (make --makefile=PhiToGraph/Makefile clean; cd ../)

src/PhiToGraph/Syntax/Test: src/PhiToGraph/Syntax.cf
	cd src/ \
		&& bnfc -d PhiToGraph/Syntax.cf -p PhiToGraph --makefile=PhiToGraph/Makefile \
		&& make --makefile=PhiToGraph/Makefile \
		&& rm PhiToGraph/Syntax/Test.hs ; \
		cd ../

pygments/setup.py: src/PhiToGraph/Syntax.cf
	bnfc --pygments src/PhiToGraph/Syntax.cf

doc/Syntax.pdf: doc/Syntax.tex
	latexmk -output-directory=doc/ -pdflatex doc/Syntax.tex

doc/Syntax.tex: src/PhiToGraph/Syntax.cf
	bnfc --latex src/PhiToGraph/Syntax.cf -o doc/

.PHONY: syntax clean all pygments pdf
