base_db_name = rsvp_site
docker_container = tippenein/rsvp-site
scp_path = root@brontasaur.us

ghci:
	stack ghci --ghci-options -fobject-code rsvp-site:lib

dev:
	stack build --fast --file-watch rsvp-site:lib --exec rsvp-site

db:
	psql -U postgres -tc "SELECT 1 FROM pg_database WHERE datname = '$(base_db_name)'" | grep -q 1 || psql -U postgres -c "CREATE DATABASE $(base_db_name)"

db_down:
	psql -U postgres -c "DROP DATABASE $(base_db_name)"

db_test:
	psql -U postgres -tc "SELECT 1 FROM pg_database WHERE datname = '$(base_db_name)_test'" | grep -q 1 || psql -U postgres -c "CREATE DATABASE $(base_db_name)_test"

db_user:
	createuser -P -s -e $(base_db_name)

db_reset: db_down db seed

seed:
	stack exec seed

publish:
	stack image container && \
	  docker push $(docker_container) && \
	  scp docker-compose.yml .env $(scp_path):/home/doc/
