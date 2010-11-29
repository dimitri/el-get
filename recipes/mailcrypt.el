(:name mailcrypt
       :type cvs
       :url ":pserver:anonymous@mailcrypt.cvs.sourceforge.net:/cvsroot/mailcrypt"
       :module "mailcrypt"
       :build `(
                "autoconf"
                "./configure"
                ,(mapconcat 'shell-quote-argument
                            `("make"
                              ,(concat "EMACS=" invocation-directory invocation-name)
                              "INFOFILES=mailcrypt.info") " ")
                )
)

