.PHONY: check clean docs seed publish image deploy

BUILD_IMAGE = "fpco/stack-build:lts-8.18"
IMAGE_NAME := tippenein/rsvp-site
IMAGE_TAG := $(shell ./bin/image-tag)
EXE_NAME := rsvp-site
base_db_name = rsvp_site
scp_path = root@brontasaur.us

LOCAL_BINARY_PATH = $(shell stack path --local-install-root)
LINUX_BINARY_PATH = $(shell stack --docker path --local-install-root)

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

deploy: image publish

publish:
	scp app.env $(scp_path):/home/doc/

check:
	stack test --fast

clean:
	stack clean
	stack --docker clean

docs:
	stack haddock

image:
	stack --docker build
	./bin/build-image \
		$(BUILD_IMAGE) \
		$(LINUX_BINARY_PATH)/bin/$(EXE_NAME) \
		$(IMAGE_NAME) \
		$(IMAGE_TAG)
image_push:
	docker push "$(IMAGE_NAME):$(IMAGE_TAG)"
	docker push "$(IMAGE_NAME):latest"

fat_image:
	stack image container --stack-yaml=stack-docker.yaml
