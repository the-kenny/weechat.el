NAME  := weechat
DESCRIPTION := Chat via WeeChat\'s relay protocol in Emacs

EMACS := emacs
BATCH := $(EMACS) -Q --batch --eval '(add-to-list '"'"'load-path ".")'

TESTS   := $(wildcard *-test.el)
EL 	:= $(wildcard weechat-*.el)
SOURCES := $(filter-out $(TESTS),$(EL))
DEPS    := ((s \"1.3.1\"))

DATE := $(shell date +%Y%m%d)

ifneq ($(wildcard .git),)
	GITVERSION ?= $(shell git describe --tags --dirty --always)
	VERSION ?= $(shell git describe --tags --exact-match 2> /dev/null | sed 's/^v//')
endif

ifeq ($(VERSION),)
	ifneq ($(wildcard version),)
		VERSION := $(shell cat version)
	else
		VERSION := $(DATE)
	endif
endif

PACKAGE := $(NAME)-$(VERSION)
TARBALL := $(PACKAGE).tar
PACKAGE_CONTENT := $(SOURCES) Makefile README.org README.html
PKG_EL := $(NAME)-pkg.el

.PHONY: all test doc package clean distclean
all: package
	$(info $(NAME) Version: $(VERSION))

test: $(TESTS)
	@$(BATCH) -l ert.el $(foreach file,$^,-l $(file)) -f ert-run-tests-batch-and-exit

clean:
	$(info Cleaning up)
	@$(RM) $(NAME)-pkg.el README.html README.txt
	@$(RM) -r $(PACKAGE)
	@$(RM) $(TARBALL)

distclean: clean
	@$(RM) -r $(TARBALL)

README.html: README.org
	$(info Creating documentation: $@)
	@$(BATCH) -l org.el --visit=$< -f org-export-as-html-batch

doc: README.html

$(PKG_EL):
	@echo "(define-package \"$(NAME)\" \"$(VERSION)\" \"$(DESCRIPTION)\" '$(DEPS))" > $@

$(TARBALL): $(PKG_EL) doc
	$(info Creating package tarball $(TARBALL))
	@mkdir -p $(PACKAGE)
	@cp -r $(PACKAGE_CONTENT) $(PKG_EL) $(PACKAGE)/
	@echo "$(VERSION)" > $(PACKAGE)/version
ifneq ($(GITVERSION),)
	@echo "$(GITVERSION)" > $(PACKAGE)/git-version
endif
	@tar cf $(TARBALL) $(PACKAGE)

package: $(TARBALL)
