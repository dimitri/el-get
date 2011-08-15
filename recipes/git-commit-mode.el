(:name git-commit-mode
       :description "Major mode for editing git commit messages"
       :type git
       :url "git://github.com/rafl/git-commit-mode.git"
       :features git-commit
       :after (lambda ()
		(add-hook 'git-commit-mode-hook 'turn-on-flyspell)))
