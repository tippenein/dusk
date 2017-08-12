.PHONY: check clean docs seed publish image_push deploy frontend

BUILD_IMAGE = "fpco/stack-build:lts-8.18"
IMAGE_NAME := tippenein/rsvp-site
EXE_NAME := rsvp-site
base_db_name = rsvp_site

FRONTEND_DIR = frontend

all: check frontend publish

frontend:
	$(MAKE) -C $(FRONTEND_DIR)

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

check:
	stack test --fast

clean:
	stack clean
	stack --docker clean

seed:
	stack exec seed

deploy: fat_image image_push publish

publish:
	scp app.env root@dusk.host:/home/doc/

image_push:
	docker push "$(IMAGE_NAME):latest"

fat_image:
	stack image container --stack-yaml=stack-docker.yaml
