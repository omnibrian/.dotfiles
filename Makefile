CONFIG = dotbot.yaml
DOTBOT_DIR = .dotbot
DOTBOT_BIN = bin/dotbot
GITCONFIG_FILE = .gitconfig

submodule:
	git -C $(DOTBOT_DIR) submodule sync --quiet --recursive
	git submodule update --init --recursive $(DOTBOT_DIR)

dotbot:
	$(DOTBOT_DIR)/$(DOTBOT_BIN) -d $(shell pwd) -c $(CONFIG)
	@echo

git:
	@echo ">> adding global git config"
	git config -f $(GITCONFIG_FILE) -l | awk -F= '{ print $$1 " '"'"'" $$2 "'"'"'" }' | xargs -L 1 git config --global
	@echo -e "\033[0;33m>> don't forget to set git config -g user.name and user.email!!\033[0m"

install: dotbot git
	@echo
	@echo -e "\033[0;32m>> configuration has been installed\033[0m"
