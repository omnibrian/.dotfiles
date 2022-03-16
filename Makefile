CONFIG = dotbot.yaml
DOTBOT_DIR = .dotbot
DOTBOT_BIN = bin/dotbot
GITCONFIG_FILE = .gitconfig

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
	@echo ">> loading guake config into dconf"
	dconf load /apps/guake/ < .config/guake.dconf

guake-dump:
	@echo ">> dumping guake config from dconf to file"
	dconf dump /apps/guake/ > .config/guake.dconf

install: dotbot fzf git vscode
	@echo -e "\033[0;32m>> configuration has been installed\033[0m"
	@echo
	@echo ">> Guake configuration loading was skipped, to run manually:"
	@echo -e "    \033[1;37mmake guake-load\033[0m"
	@echo

dconf-dump: guake-dump
	@echo -e "\033[0;32m>> dconf configuration files have been updated\033[0m"

vscode:
	@echo ">> installing vscode extensions"
	code --install-extension Tyriar.sort-lines --force
	code --install-extension dbaeumer.vscode-eslint --force
	code --install-extension eamodio.gitlens --force
	code --install-extension felipecaputo.git-project-manager --force
	code --install-extension formulahendry.auto-rename-tag --force
	code --install-extension golang.Go --force
	code --install-extension hashicorp.terraform --force
	code --install-extension ms-azuretools.vscode-docker --force
	code --install-extension ms-python.python --force
	code --install-extension ms-vscode-remote.remote-containers --force
	code --install-extension rebornix.ruby --force
	code --install-extension redhat.vscode-yaml --force
	code --install-extension vscode-icons-team.vscode-icons --force
	code --install-extension vscodevim.vim --force
	code --install-extension wholroyd.hcl --force
	code --install-extension wholroyd.jinja --force
	code --install-extension zhuangtongfa.material-theme --force
	@echo -e "\033[0;32m>> vscode extensions have been installed\033[0m"
