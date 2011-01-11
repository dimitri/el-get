(:name git-commit-mode
       :type git
       :url "git://github.com/rafl/git-commit-mode.git"
       :features git-commit
       :after (lambda ()
		(add-hook 'git-commit-mode-hook 'turn-on-flyspell)))

