DOCKER_IMAGE = latex-image
DOCKER_CONTAINER = latex-container
RUN = docker run --rm=true --name $(DOCKER_CONTAINER) --user="$(id -u):$(id -g)" -i -t -v `pwd`:/data $(DOCKER_IMAGE)
FILE = ragax

latex: 
	latexmk -xelatex -shell-escape $(FILE).tex
	bibtex ragax.aux
	latexmk -xelatex -shell-escape $(FILE).tex
	latexmk -xelatex -shell-escape $(FILE).tex

build: build-docker
	$(RUN) make

build-with-notes: build-docker
	sed -e 's/documentclass\[10pt\]/documentclass\[10pt,notes\]/' ragax.tex > ragax-with-notes.tex
	$(RUN) make FILE=ragax-with-notes

build-notes-only: build-docker
	sed -e 's/documentclass\[10pt\]/documentclass\[10pt,notes=only\]/' ragax.tex > ragax-notes-only.tex
	$(RUN) make FILE=ragax-notes-only

build-docker:
	docker build -t $(DOCKER_IMAGE) .

rm-docker:
	docker rm $(DOCKER_CONTAINER)