(ns static.core
  (:gen-class)
  (:require [watchtower.core :as watcher]
            [hiccup-bridge.core :as hicv]
            [clojure.walk :as walk]
            [net.cgrand.enlive-html :as enlive])
  (:use [clojure.tools logging cli]
        [clojure.java.browse]
        [ring.adapter.jetty]
        
        [ring.middleware.file]
        [ring.util.response]
        [hiccup core util page]
        
        [stringtemplate-clj core])

  (:use static.config :reload-all)
  (:use static.io :reload-all)
  (:import (java.io File)
           (java.net URL)
           (org.apache.commons.io FileUtils FilenameUtils)
           (java.text SimpleDateFormat)))

(defmacro define-template
  "wrap a template in the necessary calls to return correctly"
  [template-file & body]
  `((fn []
      ((enlive/template ~template-file [~'metadata ~'content]
                        ~@body
                        ) ~'metadata ~'content))))

(defn template-path
  "Return a template file descriptor for the html template name
   they're in public, so that one can just start a webserver there
   and modify the html with all the css and images in place"
  [name]
  (File. (str (static.io/dir-path :public) name)))

(defn setup-logging []
  (let [logger (java.util.logging.Logger/getLogger "")]
    (doseq [handler (.getHandlers logger)]
      (. handler setFormatter
         (proxy [java.util.logging.Formatter] []
           (format
             [record]
             (str "[+] " (.getLevel record) ": " (.getMessage record) "\n")))))))

(defmacro log-time-elapsed
  "Evaluates expr and logs the time it took.  Returns the value of expr."
  {:added "1.0"}
  [msg & expr]
  `(let [start# (. System (currentTimeMillis))
         ret# (do ~@expr)]
     (info (str ~msg " " (/ (double (- (. System (currentTimeMillis)) start#)) 1000.0) " secs"))
     ret#))

(declare tag-sidebar-list)

(defn parse-date 
  "Format date from in spec to out spec."
  [in out date]
  (.format (SimpleDateFormat. out) (.parse (SimpleDateFormat. in) date)))

(defn javadate-from-file
  "Returns an actual boxed date object for time calculations"
  [file]
  (.parse (SimpleDateFormat. "yyyy-MM-dd") (re-find #"\d*-\d*-\d*" (FilenameUtils/getBaseName (str file)))))

(defn date-from-file
  "parse a date from the url of the file"
  [file dateformat]
  (parse-date "yyyy-MM-dd" dateformat
              (re-find #"\d*-\d*-\d*" 
                       (FilenameUtils/getBaseName (str file)))))


(defn post-url 
  "Given a post file return its URL."
  [file]
  (let [name (FilenameUtils/getBaseName (str file))]
    (str (apply str (interleave (repeat \/) (.split name "-" 4))) "/")))

(defn site-url [f & [ext]]
  (-> (str f)
      (.replaceAll (dir-path :site) "")
      (FilenameUtils/removeExtension)
      (str "."
           (or ext
               (:default-extension (config))))))

(defn url-for-tag [tag]
  (str "/tags/" tag "/index.html"))

(defn project-sidebar-list
  "gives a list of all projects in the sidebar, with their respective url
  projects that have the option 'published:no' set will be ignored"
  []
  (filter #(< 0 (count %)) (map (fn [f]
         (let [[metadata content] (read-doc f)
               url (site-url f (:extension metadata))]
           ;(println url)
           ;(println (:options metadata))
           (if (= (:options metadata) "published:no")
             {}
             {:project (:title metadata) :link (str "/" url)})
         )) (list-files :site))))

(defn create-site-meta
  "creates the same structure as post-meta, but for sites"
  [metadata content]
  (let [url (:url metadata)]
    {:title (:title metadata)
     :content content
     :url url
     :date ""
     :footnotes []
     :id (hash url)
     :tags (:tags metadata)}))

(defn create-post-meta
  "creates a dictionary with information for a content item / post"
  [f]
  (let [url (post-url f)
        [metadata content] (static.io/read-doc f)
        date (date-from-file f (:date-format-post (config)))]
    ;(println "create-post-meta")
    {:title (:title metadata)
     :content @content
     :url url
     :date date
     :javadate (javadate-from-file f)
     :footnotes (:footnotes metadata)
     :id (hash url)
     :tags (:tags metadata)
     :description (or (:description metadata) "")}))

(defn recent-posts-sidebar []
  (let [posts (take (:number-of-recent-posts (config)) (reverse (list-files :posts)))]
    (map #(create-post-meta %) posts)))

(defn enhance-metadata
  "enhance the given metadata with additional information
   so that we don't have to compute this in the template"
  [m]
  ; this is certainly more complex than it should be. so much still to learn..
  (let [tagfn (fn [tags]
                (filter #(> (count %) 0)
                        (if (string? tags) (clojure.string/split tags #" " ) tags)))
        page-tags (tagfn (str (:tags m) " " (:keywords m)))
        site-tags (tagfn (:site-default-keywords (static.config/config)))
        merge-tags (vec (into #{} (if (> (count page-tags) 0) (apply conj site-tags page-tags) site-tags )))
        tagstring (clojure.string/join ", " merge-tags)]
    ; update with default-values for non-existing
    ; keywords are the combination of the site keywords and the site/post specific keywords
    (assoc (merge {:author (:site-author (static.config/config))
                   :site-title (:site-title (static.config/config))
                   :categories (tag-sidebar-list)
                   :projects (project-sidebar-list)} m)
      :tags tagstring)))

(def ^:dynamic metadata nil)
(def ^:dynamic content nil)

(defn template [page]
  (let [[m c] page
        template (if (:template m)
                   (:template m) 
                   (:default-template (static.config/config)))
        [type template-string] (if (= template :none)
                                 [:none c]
                                 (read-template template))]
    (cond (or (= type :clj)
              (= type :none))
          (binding [*ns* (the-ns 'static.core)
                    metadata m content c]

            ;(println "meta" metadata)

            (html (map #(eval %) template-string))
            )
          (= type :html)
          (let [m (->> m
                       (reduce (fn[h [k v]]
                                 (assoc h (name k) v)) {}))]
            (render-template template-string
                             (merge m {"content" c}))))))

(defn process-site 
  "Process site pages."
  []
  (dorun
   (map
    #(let [f %
           [metadata content] (read-doc f)]

       (if (empty? (force content))
         (warn (str "Empty Content: " f)))

       ;(println "metadata for" f "is" metadata)

       (write-out-dir
        (site-url f (:extension metadata))
        (template [(enhance-metadata (assoc metadata :type :site))
                   [(create-site-meta metadata (force content))]])))
    (list-files :site))))

;;
;; Create RSS Feed.
;;

(defn post-xml
  "Create RSS item node."
  [file]
  (let [[metadata content] (read-doc file)
        limit (:rss-description-char-limit (config))
        description (if (and (> limit 0) (> (count @content) limit))
                         (str (subs @content 0 limit) "… ")
                         @content)] 
    [:item 
     [:title (escape-html (:title metadata))]
     [:link  (str (URL. (URL. (:site-url (config))) (post-url file)))]
     [:pubDate (date-from-file file (:date-format-rss (config)))]
     [:description (escape-html description)]]))

(defn create-rss 
  "Create RSS feed."
  []
  (let [in-dir (File. (dir-path :posts))
        posts (take 10 (reverse (list-files :posts)))]
    (write-out-dir "rss-feed"
                   (html (xml-declaration "UTF-8")
                         (doctype :xhtml-strict)
                         [:rss {:version "2.0"} 
                          [:channel 
                           [:title (escape-html (:site-title (config)))]
                           [:link (:site-url (config))]
                           [:description
                            (escape-html (:site-description (config)))]
                           (pmap post-xml posts)]]))))

(defn create-sitemap
  "Create sitemap."
  []
  (write-out-dir 
   "sitemap.xml"
   (let [base (:site-url (config))] 
     (html (xml-declaration "UTF-8") 
           [:urlset {:xmlns "http://www.sitemaps.org/schemas/sitemap/0.9"}
            [:url [:loc base]]
            (map #(vector :url [:loc (str base %)]) 
                 (map post-url (list-files :posts)))
            (map #(vector :url [:loc (str base "/" %)]) 
                 (map site-url (list-files :site)))]))))

;;
;; Create Tags Page.
;;

(defn tag-map 
  "Create a map of tags and posts contining them. {tag1 => [url1 url2..]}"
  []
  (reduce 
   (fn[h v]
     (let [[metadata] (read-doc v)
           info [(post-url v) (:title metadata) v]
           tags (.split (:tags metadata) " ")]
       (reduce 
        (fn[m p] 
          (let [[tag info] p] 
            (if (nil? (m tag))
              (assoc m tag [info])
              (assoc m tag (conj (m tag) info)))))
        h (partition 2 (interleave tags (repeat info))))))
   (sorted-map)   
   (filter #(not (nil? (:tags (first (read-doc %))))) (list-files :posts))))

(defn tag-sidebar-list
  "create a list with all tags that link to the individual tag entries"
  []
  (map (fn [[tag posts]]
         {:tag tag :url (url-for-tag tag) :count (count posts)}) (tag-map)))

(defn create-tags 
  "Create and write tags page."
  []
  (dorun
   (map (fn [t]
          (let [[tag posts] t
                metadata {
                          :title (str (:site-title (config)) (format (:site-title-tag (config)) tag))
                          :template (:list-template (config))
                          :description (:site-description (config))
                          }
                enhanced-meta (enhance-metadata metadata)
                content (map #(create-post-meta (nth % 2)) (reverse posts))] ;the 2nd positon is the fp which we use to get all info from this post
            ;(println "content" content)
            (write-out-dir (url-for-tag tag)
                           (template [enhanced-meta [[tag content]]]))))
        (tag-map))))

(defn random-posts
  [amount]
  (map #(let [f %
          posturl (post-url f)
          [postmetadata _] (read-doc f)
          date (date-from-file f (:date-format-post (config)))]
      {:date date :url posturl :title (:title postmetadata)}) 
   (take amount (shuffle (list-files :posts)))))

;;
;; Create pages for latest posts.
;;

(defn pager
  "Return previous, next navigation links."
  [page max-index posts-per-page]
  (let [count-total (count (list-files :posts))
        older (str "/latest-posts/" (- page 1) "/")
        newer (str "/latest-posts/" (+ page 1) "/")]
    (cond
     (< count-total posts-per-page) nil
     (= page max-index) {:older older}
     (= page 0) {:newer newer}
     :default {:newer newer :older older})))

(defn create-latest-posts 
  "Create and write latest post pages."
  []
  (let [posts-per-page (:posts-per-page (config))
        posts (partition posts-per-page
                         posts-per-page
                         []
                         (reverse (list-files :posts)))
        pages (partition 2 (interleave (reverse posts) (range)))
        [_ max-index] (last pages)]
    (doseq [[posts page] pages]
      (let [pager-data (pager page max-index posts-per-page)
            title-extension (if (< page max-index) (format (:site-title-page (config)) (+ page 1)) "") ;don't add page extension for first page / index
            metadata {:title (str (:site-title (config)) title-extension) ; create metadata for page
                      :description (:site-description (config))
                      :template (:default-template (config))
                      :pager pager-data}
            enhanced-meta (enhance-metadata metadata)
            content (map #(create-post-meta %) posts)]
        (write-out-dir
         (str "latest-posts/" page "/index.html")
         (template
          [enhanced-meta content]))))))

;;
;; Create Archive For Year
;;

(defn write-posts-by-year [meta [year year-group]]
  (write-out-dir (str year "/index.html") (template [meta [[year year-group]]])))

(defn create-archives-for-year []
  (let [files (map create-post-meta (list-files :posts)) 
        sorted (reverse (sort-by :javadate files))
        annotated (map (fn [d] (let [[_ year month & rest] (clojure.string/split (:url d) #"/")]
                                 (assoc d :year year :month month))) sorted)
        grouped (reverse (vec (into (sorted-map) (group-by :year annotated))))
        meta (enhance-metadata {:title (:archives-title (config))
                                :template (:list-template (config))
                                :type :news-archive})]
    (dorun (map #(write-posts-by-year meta %) grouped))))

;;
;; Create Archive Pages.
;;

(defn post-count-by-mount 
  "Create a map of month to post count {month => count}"
  []
  (->> (list-files :posts)
       (reduce (fn [h v]
                 (let  [date (re-find #"\d*-\d*" 
                                      (FilenameUtils/getBaseName (str v)))]
                   (if (nil? (h date))
                     (assoc h date 1)
                     (assoc h date (+ 1 (h date)))))) {})
       (sort-by first)
       reverse))

(defn create-archives-one-page
  "Create and write archive pages
   Write one single page that lists everything"
  []
  (let [files (map #(create-post-meta %) (list-files :posts))
        sorted (reverse (sort-by :javadate files))
        annotated (map (fn [d] (let [[_ year month & rest] (clojure.string/split (:url d) #"/")]
                                 (assoc d :year year :month month))) sorted)
        grouped (reverse (vec (into (sorted-map) (group-by :year annotated))))
        meta (enhance-metadata {:title (:archives-title (config))
                                :template (:list-template (config))})]
    (write-out-dir (str (:post-dir (config)) "index.html") (template [meta grouped]))))

(defn create-archives-by-month
  "Create and write archive pages.
   Write a page for each month and a page listing all months"
  []
  ;;create main archive page.
  (let [meta (enhance-metadata {:title (:archives-title (config)) :template (:list-template (config))})
        content (map (fn [[mount count]]
                       {:title (str (parse-date "yyyy-MM" (:date-format-archive (config)) mount) "(" count ")") 
                        :url (str (:post-dir (config)) (.replace mount "-" "/") "/")
                        :date (parse-date "yyyy-MM" (:date-format-archive (config)) mount)
                        :content (str count)
                        :id (hash mount)
                        :keywords []
                        :footnotes []}
                       ) (post-count-by-mount))]
    (write-out-dir (str (:post-dir (config)) "index.html") (template [meta content])))
  
  ;;create a page for each month.
  (dorun
   (pmap
    (fn [month]
      (let [posts (->> (list-files :posts)
                       (filter #(.startsWith 
                                 (FilenameUtils/getBaseName (str %)) month))
                       reverse)
            metadata (enhance-metadata {:title (str (:archives-title (config)) (format (:archives-title-month (config)) month))
                              :template (:list-template (config))})
            content (map create-post-meta posts)]
        ;(println "content" content)
        (write-out-dir
         (str (:post-dir (config)) (.replace month "-" "/") "/index.html")
         (template [metadata content]))))
    (keys (post-count-by-mount)))))

(defn create-aliases 
  "Create redirect pages."
  ([]
     (doseq [post (list-files :posts)]
       (create-aliases post))
     (doseq [site (list-files :site)]
       (create-aliases site)))
  ([file]
     (let [doc (read-doc file)]
       (when-let [aliases (-> doc first :alias)]
         (doseq [alias (read-string aliases)]
           (write-out-dir
            alias
            (html [:html
                   [:head
                    [:meta {:http-equiv "content-type" :content "text/html; charset=utf-8"}]
                    [:meta {:http-equiv "refresh" :content (str "0;url=" (post-url file))}]]])))))))

(defn process-posts 
  "Create and write post pages."
  []
  (dorun
   (map
    #(let [f %
           [metadata content] (read-doc f)
           out-file (reduce (fn[h v] (.replaceFirst h "-" "/")) 
                            (FilenameUtils/getBaseName (str f)) (range 3))]
       (if (empty? @content)
         (warn (str "Empty Content: " f)))
       
       (write-out-dir 
        (str out-file "/index.html")
        (let [meta (enhance-metadata (assoc metadata :type :post :url (post-url f)))
              cont [(create-post-meta f)]]
          ;(println "content for post" (post-url f))
          ;(println meta)
          (template [meta cont]))
         ))
    (list-files :posts))))

(defn process-public 
  "Copy public from in-dir to out-dir."
  []
  (let [in-dir (File. (dir-path :public))
        out-dir (File. (:out-dir (config)))]
    (doseq [f (map #(File. in-dir %) (.list in-dir))]
      ; we ignore files in public that start with _ as these are
      ; html templates
      (if (.isFile f)
        (when (not (= \_ (first (FilenameUtils/getBaseName (str f)))))
          (FileUtils/copyFileToDirectory f out-dir))
        (FileUtils/copyDirectoryToDirectory f out-dir))
      )))

(defn load-base-template
  "load the base template, that contains template defaults"
  []
  (binding [*ns* (the-ns 'static.core)]
    (let [filepath (str (static.io/dir-path :templates)
                        (:base-template (static.config/config)))]
      (println filepath)
      (try
        (load-file filepath)
        (catch Exception e (do (println e) (info "Base template not found, continuing without."))))
      )))

(defn create 
  "Build Site."
  [] 
  (doto (File. (:out-dir (config)))
    (FileUtils/deleteDirectory)
    (.mkdir))

  ; Import the template helpers
  (load-base-template)

  (log-time-elapsed "Processing Public " (process-public))
  (log-time-elapsed "Processing Site " (process-site))

  (if (pos? (-> (dir-path :posts) (File.) .list count))
    (do 
      (log-time-elapsed "Processing Posts " (process-posts))
      (log-time-elapsed "Creating RSS " (create-rss))
      (log-time-elapsed "Creating Tags " (create-tags))
      
      (when (:create-archives (config))
        (log-time-elapsed "Creating Archives " (create-archives-one-page)))

      (log-time-elapsed "Creating Archives by Year" (create-archives-for-year)) 
      
      (log-time-elapsed "Creating Sitemap " (create-sitemap))
      (log-time-elapsed "Creating Aliases " (create-aliases))

      (when (:blog-as-index (config)) 
        ; Create the latest-post archives, i.e. create a index.html with n posts
        ; under latest-posts/ and link them together
        (log-time-elapsed "Creating Latest Posts " (create-latest-posts))

        ; copy the very last index.html over to / as the current index
        (let [max (apply max (map read-string (-> (:out-dir (config))
                                                  (str  "latest-posts/")
                                                  (File.)
                                                  .list)))]
          (FileUtils/copyFile 
           (File. (str (:out-dir (config)) 
                       "latest-posts/" max "/index.html")) 
           (File. (str (:out-dir (config)) "index.html"))))))))

(defn serve-static [req] 
  (let [mime-types {".clj" "text/plain"
                    ".mp4" "video/mp4"
                    ".ogv" "video/ogg"}]
    (if-let [f (file-response (:uri req) {:root (:out-dir (config))})] 
      (if-let [mimetype (mime-types (re-find #"\..+$" (:uri req)))] 
        (merge f {:headers {"Content-Type" mimetype}}) 
        f))))

(defn watch-and-rebuild
  "Watch for changes and rebuild site on change."
  []
  (watcher/watcher [(:in-dir (config))]
                   (watcher/rate 1000)
                   (watcher/on-change (fn [_]
                                        (info "Rebuilding site...")
                                        (try
                                          (create)
                                          (catch Exception e
                                            (warn (str "Exception thrown while building site! " e))))))))


(defn -main [& args]
  (let [[opts _ banner] (cli args
                             ["--build" "Build Site." :default false :flag true]
                             ["--tmp" "Use tmp location override :out-dir" :default false :flag true]
                             ["--jetty" "View Site." :default false :flag true]
                             ["--watch" "Watch Site and Rebuild on Change." :default false :flag true]
                             ["--rsync" "Deploy Site." :default false :flag true]
                             ["--help" "Show help" :default false :flag true])
        {:keys [build tmp jetty watch rsync help]} opts]

    (when help
      (println "Static")
      (println banner)
      (System/exit 0))

    (setup-logging)

    (let [out-dir (:out-dir (config))
          tmp-dir (str (System/getProperty "java.io.tmpdir") "/" "static/")]
      
      (when (or tmp
                (and (:atomic-build (config))
                     build))
        (let [loc (FilenameUtils/normalize tmp-dir)]
          (set!-config :out-dir loc)
          (info (str "Using tmp location: " (:out-dir (config))))))
      
      (cond build (log-time-elapsed "Build took " (create))
            watch (do (watch-and-rebuild)
                      (future (run-jetty serve-static {:port 8080}))
                      (browse-url "http://127.0.0.1:8080"))
            jetty (do (future (run-jetty serve-static {:port 8080}))
                      (browse-url "http://127.0.0.1:8080"))
            rsync (let [{:keys [rsync out-dir host user deploy-dir]} (config)]
                    (deploy-rsync rsync out-dir host user deploy-dir))
            :default (println "Use --help for options."))
      
      (when (and (:atomic-build (config))
                 build)
        (FileUtils/deleteDirectory (File. out-dir))
        (FileUtils/moveDirectory (File. tmp-dir) (File. out-dir))))
  
    (when-not watch
      (shutdown-agents))))
