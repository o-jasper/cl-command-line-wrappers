
(defsystem :iw-scan
  :depends-on (:destructuring-regex :regex :read-tab-listing :j-commandline :j-string-utils :j-seq-utils :j-general :alexandria)
  :description "Uses `iwlist wlan0 scanning` command to scan wireless
 networks. The iw-scan-continuous package has facilities for continuous
 scanning with a hook, and some hooks to start from.

NOTE: `sudo iwlist wlan0 scanning` it will only give a subset of the actual
 result! For more add to file /etc/sudoers: `<username> /usr/sbin/iwlist
 wlan0 scanning (TODO security implications?)"
  :serial t
  :author "Jasper den Ouden"
  :components ((:module "../src"
                 :components ((:file "iw-scan")))))
