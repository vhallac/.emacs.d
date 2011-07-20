;; TODO: Make this into an autoload
(require 'org2blog)

(setq org2blog/wp-blog-alist
      '(("Dysaster Zone"
         :url "http://dysasterzone.wordpress.com/xmlrpc.php"
         :username "vhallac"
         :default-title "Hello World"
         :default-categories ("org2blog" "emacs")
         :wp-latex nil
         :tags-as-categories nil)
        ("My Dysaster Zone"
         :url "http://dysasterzone.us.to/xmlrpc.php"
         :username "dysaster"
         :default-title "Hello World"
         :default-categories ("org2blog" "emacs")
         :wp-latex nil
         :tags-as-categories nil)))
