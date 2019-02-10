 ## Install (actually just symlink) the dotfiles
.PHONY: dotfiles
dotfiles:
	for file in $(shell find $(CURDIR) -name ".*" -not -name ".git"); do \
		f=$$(basename $$file); \
		echo "linking $$file"; \
		ln -sfn $$file $(HOME)/$$f; \
	done;
