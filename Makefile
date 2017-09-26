

dev:
	docker-compose run game_of_life bash

build:
	mkdir -p build
	cd build; cmake -DCMAKE_BUILD_TYPE=Debug ..
	make  -C build

test:
	docker-compose start redis
	pytest

clean:
	rm -rf build

.PHONY: build dev test
