(:name ack
       :type http
       :url "http://repo.or.cz/w/ShellArchive.git?a=blob_plain;hb=HEAD;f=ack.el"
       :description "Use ack where you might usually use grep."
       :build '("mv ShellArchive*ack.el ack.el")
       :features ack)
