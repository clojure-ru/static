(ns static.config
  (:use [clojure.tools logging])
  (:import (java.io File)))


(let [defaults {:site-title "A Static Blog"
                :site-title-page " - Page %s" ; The extension for older pages
                :site-title-tag " - Tag %s" ; The extension for tags
                :site-description "Default blog description"
                :site-url "https://github.com/terhechte/static"
                :site-default-keywords "tag1 tag2 tag3 tag3"
                :site-author ""
                :in-dir "resources/"
                :out-dir "html/"
                :post-dir "archives/"
                :default-template "default.clj"
                :list-template "list.clj"
                :base-template "base.clj"
                :default-extension "html"
                :encoding "UTF-8"
                :posts-per-page 2
                :number-of-recent-posts 10
                :blog-as-index true
                :create-archives true
                :archives-title "Archives"
                :archives-title-month " - %s" ; for the individual month
                :date-format-post "E, d MMM yyyy" ; the output dateformat for posts
                :date-format-rss "E, d MMM yyyy HH:mm:ss Z" ; the date format for the rss feed
                :date-format-archive "MMMM yyyy" ; the output dateformat for archive links
                :rss-description-char-limit 120 ; if this is 0, include all content in the rss, otherwise limtit to the given amount of chars
                :emacs "/usr/bin/emacs"
                :emacs-config " -q "
                :org-export-command '(princ
                                      (org-no-properties
                                       (with-current-buffer
                                         (org-html-export-as-html nil nil nil t nil)
                                         (buffer-string))))
                }]
  
  (def config
    (memoize
     #(try 
        (let [config (apply hash-map (read-string (slurp (File. "config.clj"))))]
          ;;if emacs key is set make sure executable exists.
          (when (:emacs config)
            (if (not (.exists (File. (:emacs config))))
              (do (error "Path to Emacs not valid.")
                  (System/exit 0))))
          (merge defaults config))
        (catch Exception e (do 
                             (info "Configuration not found using defaults.")
                             defaults))))))

(defn set!-config [k v]
  (alter-var-root (find-var 'static.config/config) (fn [c] #(identity (assoc (c) k v)))))
  
