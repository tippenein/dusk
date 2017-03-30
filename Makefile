base_db_name = rsvp_yesod

dev:
	stack build --fast --file-watch rsvp-yesod:lib --exec rsvp-yesod

db:
	psql -U postgres -tc "SELECT 1 FROM pg_database WHERE datname = '$(base_db_name)'" | grep -q 1 || psql -U postgres -c "CREATE DATABASE $(base_db_name)"
db_down:
	psql -U postgres -c "DROP DATABASE $(base_db_name)"
db_test:
	psql -U postgres -tc "SELECT 1 FROM pg_database WHERE datname = '$(base_db_name)_test'" | grep -q 1 || psql -U postgres -c "CREATE DATABASE $(base_db_name)_test"
