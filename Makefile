slide:
	(cd latex && make build)
	(cd latex && make build-with-notes)

regenerate:
	(cd golang && make regenerate)
	(cd golang && make js)
	(cd maxjs && make regenerate)
	