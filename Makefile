DIR := ./chess/
ASSETS := ./assets/
SHARE := ~/.local/share/ocamlchess/
SHARE_ASSETS := $(SHARE)assets/

.PHONY: build clean install uninstall test doc indent

all: install

build:
	$(MAKE) -C $(DIR)

clean:
	$(MAKE) clean -C $(DIR)

install: build
	mkdir -p $(SHARE_ASSETS)
	cp -r $(ASSETS) $(SHARE)
	$(MAKE) install -C $(DIR)

uninstall:
	rm -rf $(SHARE)
	$(MAKE) uninstall -C $(DIR)

test:
	$(MAKE) test -C $(DIR)

doc:
	$(MAKE) doc -C $(DIR)

indent:
	sh tools/ocp-indent-all.sh
