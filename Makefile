pdf:
	(cd latex && make run)

regenerate:
	(cd golang && make regenerate)
	(cd golang && make js)
	(cd maxjs && make regenerate)
	