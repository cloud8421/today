TARGET_EXE = /usr/local/bin/today
TARGET_MAN = /usr/local/share/man/man1/today.1

STACK_LOCAL_BIN_PATH := $(shell stack path | grep 'local-bin-path' | awk ' { print $$2 }')
SOURCE_EXE = $(STACK_LOCAL_BIN_PATH)/today

install: $(TARGET_MAN)
.PHONY: install

$(TARGET_EXE):
	stack install
	cp $(SOURCE_EXE) $(TARGET_EXE)
.PHONY: $(TARGET_EXE)

$(TARGET_MAN): $(TARGET_EXE)
	echo $@
	help2man --no-info $(TARGET_EXE) > $(TARGET_MAN)
.PHONY: $(TARGET_MAN)
