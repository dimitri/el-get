(:name ack
       :type http
       :url "http://repo.or.cz/w/ShellArchive.git?a=blob_plain;hb=HEAD;f=ack.el"
       :build '("mv ShellArchive*ack.el ack.el")
       :features ack)
