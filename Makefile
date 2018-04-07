pdf:
	(cd latex && make docker-run)

regenerate:
	(cd golang && make regenerate)
	(cd golang && make js)
	(cd maxjs && make regenerate)
	