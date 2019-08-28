SYSTEM := $(shell sh -c 'uname -s 2>/dev/null')
STACK_LOCAL_BIN_PATH := $(shell stack path | grep 'local-bin-path' | awk ' { print $$2 }')

# EXECUTABLE AND MAN PAGES

SOURCE_EXE = $(STACK_LOCAL_BIN_PATH)/today
TARGET_EXE = /usr/local/bin/today
TARGET_MAN = /usr/local/share/man/man1/today.1

install: $(TARGET_MAN)
.PHONY: install

$(TARGET_EXE): $(SOURCE_EXE)
	cp $< $@
.PHONY: $(TARGET_EXE)

$(TARGET_MAN): help2man $(TARGET_EXE)
	help2man --no-info $(TARGET_EXE) > $(TARGET_MAN)
.PHONY: $(TARGET_MAN)

$(SOURCE_EXE):
	stack install
.PHONY: $(SOURCE_EXE)

# TESTS

test: test-unit test-integration

test-unit:
	stack test
.PHONY: test-unit

test-integration: /usr/local/bin/bats
	bats test
.PHONY: test-integration

# CI

ci.test: ci.test-unit ci.test-integration

ci.test-unit:
	stack --no-terminal test
.PHONY: ci.test-unit

ci.test-integration: /usr/local/bin/bats
	bats -t test
.PHONY: ci.test-integration

ci.setup:
	stack --no-terminal setup
.PHONY: ci.setup

ci.install:
	stack --no-terminal install
.phony: ci.install

#################################################################
############################# TOOLS #############################
#################################################################

/usr/local/bin/bats:
ifeq ($(SYSTEM),Darwin)
ifneq ($(shell bats --version >/dev/null 2>&1 ; echo $$?),0)
	brew install bats-core
endif
else
ifneq ($(shell bats --version >/dev/null 2>&1 ; echo $$?),0)
	git clone https://github.com/bats-core/bats-core.git /tmp/bats
	cd /tmp/bats && ./install.sh /usr/local
	rm -rf /tmp/bats
endif
endif

help2man:
ifeq ($(SYSTEM),Darwin)
ifneq ($(shell help2man --version >/dev/null 2>&1 ; echo $$?),0)
	brew install help2man
endif
else
ifneq ($(shell help2man --version >/dev/null 2>&1 ; echo $$?),0)
	apt-get install --yes help2man
endif
endif
