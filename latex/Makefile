DOCKER_IMAGE = latex-image
DOCKER_CONTAINER = latex-container
RUN = docker run --rm=true --name $(DOCKER_CONTAINER) --user="$(id -u):$(id -g)" -i -t -v `pwd`:/data $(DOCKER_IMAGE)
FILE = ragax

latex: 
	latexmk -xelatex -shell-escape $(FILE).tex
	bibtex $(FILE).aux
	latexmk -xelatex -shell-escape $(FILE).tex
	latexmk -xelatex -shell-escape $(FILE).tex

run: build-docker
	$(RUN) make

build-docker:
	docker build -t $(DOCKER_IMAGE) .

rm-docker:
	docker rm $(DOCKER_CONTAINER)