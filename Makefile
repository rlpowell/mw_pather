HASTEC_VERSION := $(shell hastec --version 2>/dev/null)

all:
ifdef HASTEC_VERSION
	hastec --make mw_pather.hs
	ghc --make mw_pather.hs
	@echo "\n\nRUN MAKE ON THE OTHER SIDE!\n\n"
else
	sudo chcon -t svirt_sandbox_file_t -R /home/rlpowell/src/mw_pather/
	rm -f source.zip
	zip source.zip Makefile index.html mw_pather.hs Dockerfile
	cp source.zip index.html mw_pather.js /home/rlpowell/public_html/media/public/mw_pather/
	sudo chcon -t httpd_user_content_t /home/rlpowell/public_html/media/public/mw_pather/
	@echo -e "\n\nRUN MAKE ON THE OTHER SIDE!\n\n"
endif

clean:
	rm -r main mw_pather.js mw_pather.o mw_pather.hi

docker_build:
	sudo docker kill mw_pather || true
	sudo docker rm mw_pather || true
	sudo docker rmi rlpowell/mw_pather || true
	sudo docker build -t rlpowell/mw_pather .

docker_run:
	sudo docker kill mw_pather || true
	sudo docker rm mw_pather || true
	sudo docker run --name mw_pather -t -i -v /home/rlpowell/src/mw_pather/:/srv/haste/mw_pather/:rw -p 0.0.0.0:24601:24601 rlpowell/mw_pather
	sudo docker attach mw_pather
