
GID := $(shell id -g)
UID := $(shell id -u)
RESOLVER := $(shell yq -r .resolver stack.yaml)

deb: copy-deb stop

copy-deb: rebuild
	docker cp ff-deb_ff_1:/home/ff/deb/. ./dockers/debianizer/

build:
	EULER_GID=${GID} EULER_UID=${UID} STACK_RESOLVER=${RESOLVER} docker-compose -p ff-deb -f dockers/debianizer/docker-compose.yml up -d --build

rebuild: build-stop
	EULER_GID=${GID} EULER_UID=${UID} STACK_RESOLVER=${RESOLVER} docker-compose -p ff-deb -f dockers/debianizer/docker-compose.yml up -d --build

build-stop:
	EULER_GID=${GID} EULER_UID=${UID} docker-compose -p ff-deb -f dockers/debianizer/docker-compose.yml down

run:
	docker exec -ti ff-deb_ff_1 bash

stop:
	docker stop ff-deb_ff_1
