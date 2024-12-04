HS_FILES := $(wildcard src/*.hs)
OUT_DIR := out

HELPERS := "src/util/String.hs"

$(shell mkdir -p $(OUT_DIR))

.PHONY: all
all: run

run:
ifeq ($(FILE),)
	@for file in $(HS_FILES); do \
		name=$$(basename $$(basename $$file .hs)); \
		mkdir -p $(OUT_DIR)/$$name; \
		ghc -outputdir $(OUT_DIR)/$$name -o $(OUT_DIR)/$$name/$$name $$file $(HELPERS); \
	done
else
	mkdir -p $(OUT_DIR)/$$FILE
	ghc -outputdir $(OUT_DIR)/$$FILE -o $(OUT_DIR)/$$FILE/$(basename $(notdir $(FILE))) src/$(FILE).hs $(HELPERS)
	@echo ==========
	@$(OUT_DIR)/$$FILE/$(basename $(notdir $(FILE)))
endif
