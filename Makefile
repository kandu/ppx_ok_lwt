PROJECT= ppx_ok_lwt

.PHONY: build, install clean

build:
	jbuilder build

install: build
	jbuilder install

uninstall: build
	jbuilder uninstall

clean:
	jbuilder clean

