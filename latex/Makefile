DOCKER_IMAGE = latex-image
DOCKER_CONTAINER = latex-container

latex: 
	latexmk -xelatex -shell-escape demo.tex
	bibtex demo.aux
	latexmk -xelatex -shell-escape demo.tex
	latexmk -xelatex -shell-escape demo.tex
	mv demo.pdf ragax-ragalur-expressions.pdf

docker-run: docker-build
	docker run --rm=true --name $(DOCKER_CONTAINER) --user="$(id -u):$(id -g)" -i -t -v `pwd`:/data $(DOCKER_IMAGE) make

docker-build:
	docker build -t $(DOCKER_IMAGE) .

docker-rm:
	docker rm $(DOCKER_CONTAINER)