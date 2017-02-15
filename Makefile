.PHONY: all clean

PROJECTS=test

TARGETS=$(foreach project,$(PROJECTS),$(project).native)

all:
	ocamlbuild $(TARGETS)

clean:
	ocamlbuild -clean

test:
	@ for target in $(TARGETS); do ./$(TARGET); done
