(defsystem :my-little-fts
  :name "my-little-fts"
  :description "My Little Full Text Search"
  :author "8c6794b6"
  :license "MIT"
  :depends-on (:alexandria
               :clack
               :lack
               :lass
               :plump
               :quri
               :spinneret
               :sqlite
               :trivial-rfc-1123)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "image")
   (:file "app")))
