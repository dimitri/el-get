;; Set a broken value for the session bus address
(setenv "DBUS_SESSION_BUS_ADDRESS"
        "unix:abstract=/tmpX/Xdbus-oBU7t7f9Pv,guid=X7bb736a5c60b6c3be8a1312800000064X")

;; Trigger a notification
(setq el-get-sources
      (list
       '(:name pkg :type builtin)))

(el-get 'sync 'pkg)
