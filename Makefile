DIR := chess

.PHONY: build clean install uninstall test doc indent

all: build install

build:
	$(MAKE) -C $(DIR)

clean:
	$(MAKE) clean -C $(DIR)

install:
	$(MAKE) install -C $(DIR)

uninstall:
	$(MAKE) uninstall -C $(DIR)

test:
	$(MAKE) test -C $(DIR)

doc:
	$(MAKE) doc -C $(DIR)

indent:
	sh tools/ocp-indent-all.sh
