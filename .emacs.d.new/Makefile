CURRENT_DATE = $(shell date +"%Y%m%d")
ELISP_DIR = $(shell pwd)/elisp


clean:
	rm -rf ./elpa

deps:
	@if [ ! -d $(ELISP_DIR) ]; then mkdir $(ELISP_DIR); fi
	@echo "installing moom..."
	@cd $(ELISP_DIR) && if [ ! -d $(ELISP_DIR)/moom ]; then git clone https://github.com/takaxp/moom.git; fi

install: deps
	EMACS_USER_DIRECTORY="$(shell pwd)" emacs -Q --batch -l ./init.el

reinstall: clean install
