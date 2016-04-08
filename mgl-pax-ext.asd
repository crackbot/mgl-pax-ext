
(asdf:defsystem :mgl-pax-ext
  :name "mgl-pax-ext"
  :description "Extensions for mgl-pax."
  :version "0.0.1"
  :author "Crackbot <thecrackbot@gmail.com>"
  :maintainer "Crackbot <thecrackbot@gmail.com>"
  :license "The MIT License (MIT)"
  :components ((:static-file "mgl-pax-ext.asd")
               (:file "package")
               (:file "utils")
               (:file "main"))
  :depends-on (:mgl-pax :docparser :alexandria))
