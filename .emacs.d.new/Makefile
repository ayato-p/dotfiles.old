CURRENT_DATE = $(shell date +"%Y%m%d")
ELISP_DIR = $(shell pwd)/elisp


clean:
	rm -rf ./elpa

deps:
	@if [ ! -d $(ELISP_DIR) ]; then mkdir $(ELISP_DIR); fi
	@echo "installing frame-fns"
	@cd $(ELISP_DIR) && if [ ! -d $(ELISP_DIR)/frame-fns ]; then git clone https://github.com/emacsmirror/frame-fns.git; fi
	@echo "installing frame-cmds"
	@cd $(ELISP_DIR) && if [ ! -d $(ELISP_DIR)/frame-cmds ]; then git clone https://github.com/emacsmirror/frame-cmds.git; fi
	@echo "installing moom..."
	@cd $(ELISP_DIR) && if [ ! -d $(ELISP_DIR)/moom ]; then git clone https://github.com/takaxp/moom.git; fi
	@echo "installing undo-tree..."
	@cd $(ELISP_DIR) && if [ ! -d $(ELISP_DIR)/undo-tree ]; then git clone http://www.dr-qubit.org/git/undo-tree.git; fi

install: deps
	EMACS_USER_DIRECTORY="$(shell pwd)" emacs -Q --batch -l ./init.el

reinstall: clean install
