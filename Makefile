CONFIG = dotbot.yaml
DOTBOT_DIR = .dotbot
DOTBOT_BIN = bin/dotbot
GITCONFIG_FILE = .gitconfig
NPM := $(shell command -v npm 2> /dev/null)
VSCODE := $(shell command -v code 2> /dev/null)
OS = $(shell uname -s)

submodules:
	git submodule update --init --recursive
	git -C $(DOTBOT_DIR) submodule sync --quiet --recursive

dotbot:
	$(DOTBOT_DIR)/$(DOTBOT_BIN) -d $(shell pwd) -c $(CONFIG)
	@echo

fzf:
ifeq (,$(wildcard ~/.fzf))
	@echo ">> installing fzf"
	git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
	~/.fzf/install --key-bindings --completion --no-update-rc
	@echo
else
	@echo ">> fzf already installed, skipping install"
	@echo
endif

git:
	@echo ">> adding global git config"
	git config -f $(GITCONFIG_FILE) -l | awk -F= '{ print $$1 " '"'"'" $$2 "'"'"'" }' | xargs -L 1 git config --global
	@echo -e "\033[0;33m>> don't forget to set git config --global user.name and user.email!!\033[0m"
	@echo

.PHONY: guake-load
guake-load:
ifeq ($(OS),Linux)
	@echo ">> loading guake config into dconf"
	dconf load /apps/guake/ < .config/guake.dconf
else
	@echo ">> skipping guake config for non-linux system"
	@echo
endif

guake-dump:
ifeq ($(OS),Linux)
	@echo ">> loading guake config into dconf"
	@echo ">> dumping guake config from dconf to file"
	dconf dump /apps/guake/ > .config/guake.dconf
else
	@echo ">> skipping guake dump for non-linux system"
	@echo
endif

yabai:
ifeq ($(OS),macOS)
	@echo ">> installing yabai and skhd"
	brew install koekeishiya/formulae/yabai
	brew install koekeishiya/formulae/skhd
	@echo ">> yabai installed"
	@echo
	@echo ">> To start yabai, run the following commands once dotbot has run:"
	@echo "    brew services start yabai"
	@echo "    brew services start skhd"
else
	@echo ">> skipping yabai install for non-macos system"
	@echo
endif

install: dotbot fzf git npm vscode
	@echo -e "\033[0;32m>> configuration has been installed\033[0m"
	@echo
	@echo ">> Guake configuration loading was skipped, to run manually:"
	@echo -e "    \033[1;37mmake guake-load\033[0m"
	@echo

dconf-dump: guake-dump
	@echo -e "\033[0;32m>> dconf configuration files have been updated\033[0m"

npm:
ifndef NPM
	@echo ">> skipping npm config, npm is not on PATH"
	@echo
else
	npm config set prefix '~/.local'
	@echo ">> npm configured to install to '~/.local'"
	@echo
endif

vscode:
ifndef VSCODE
	@echo ">> skipping vscode setup, code cli is missing"
	@echo
else
	@echo ">> installing vscode extensions"
	code --install-extension Tyriar.sort-lines --force
	code --install-extension dbaeumer.vscode-eslint --force
	code --install-extension eamodio.gitlens --force
	code --install-extension felipecaputo.git-project-manager --force
	code --install-extension formulahendry.auto-rename-tag --force
	code --install-extension golang.Go --force
	code --install-extension hashicorp.terraform --force
	code --install-extension ms-azuretools.vscode-docker --force
	code --install-extension ms-kubernetes-tools.vscode-kubernetes-tools --force
	code --install-extension ms-python.python --force
	code --install-extension ms-vscode-remote.remote-containers --force
	code --install-extension rebornix.ruby --force
	code --install-extension redhat.ansible --force
	code --install-extension redhat.vscode-yaml --force
	code --install-extension vscjava.vscode-java-pack --force
	code --install-extension vscode-icons-team.vscode-icons --force
	code --install-extension vscodevim.vim --force
	code --install-extension wholroyd.hcl --force
	code --install-extension wholroyd.jinja --force
	code --install-extension zhuangtongfa.material-theme --force
	@echo -e "\033[0;32m>> vscode extensions have been installed\033[0m"
endif
