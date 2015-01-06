(ns static.io
  (:use [clojure.tools logging]
        [clojure.java.shell :only [sh]]
        [cssgen]
        [hiccup core]
        [stringtemplate-clj core])
  (:require clojure.pprint)
  (:use static.config :reload-all)
  (:import (org.pegdown PegDownProcessor)
           (java.io File)
           (java.io InputStreamReader OutputStreamWriter)
           (org.apache.commons.io FileUtils FilenameUtils)))

(defn- split-file [content]
  (let [idx (.indexOf content "---" 4)]
    [(.substring content 4 idx) (.substring content (+ 3 idx))]))

(defn- prepare-metadata [metadata]
  (reduce (fn [h [_ k v]]
            (let [key (keyword (.toLowerCase k))]
              (if (not (h key))
                (assoc h key v)
                h)))
          {} (re-seq #"([^:#\+]+): (.+)(\n|$)" metadata)))

(defn- parse-markdown-footnotes [markdown]
  "since pegdown has no footnotes-support, we will ust do
   some replacement on the markdown itself in order to
   generate footnotes

   this is very limited footnotes support. right now
   the only markdown footnotes support is using [^name] in the text
   and at the bottom [^name]: text\n. Everything in one line.
   Additions welcome :)"
  (let [fo-reg #"\[\^[a-zA-Z0-9]*?\]\:.+\n"
        fos (re-seq fo-reg markdown)]
    (if (> (count fos) 0)
      (do
        ; create the footnotes
        (let [md1 (reduce (fn [[results markdown] footnote]
                            [(conj results {:text (last (clojure.string/split footnote #":" 2))
                                            :ref (last (re-find #"\[\^([0-9a-zA-Z]+)\]" footnote))
                                            :id (str "#fn" (last (re-find #"\d" footnote)))})
                             (-> markdown
                                ; replace the original footnote with nothing
                                (clojure.string/replace footnote "")
                                ; replace the text-footnote with an anchor/name
                                (clojure.string/replace (first (clojure.string/split footnote #":" 2))
                                         (format "<sup><a name='fn%s' href='#%s'>%s</a></sup>"
                                                 (last (re-find #"\d" footnote)); only the first number
                                                 (last (re-find #"\[\^([0-9a-zA-Z]+)\]" footnote))
                                                 (last (re-find #"\d" footnote)) 
                                                 )))]
                            ) [[] markdown] fos)]
          ;(clojure.pprint/pprint md1)
          ;(System/exit 0)
          md1
          )
        )
      [[] markdown] ; return no footnotes
      )))

(defn- read-markdown [file]
  (let [[metadata content]
        (split-file (slurp file :encoding (:encoding (config))))
        [footnotes content] (parse-markdown-footnotes content)]
    ;(println "mkd" content)
    ;(System/exit 0)
    [(assoc (prepare-metadata metadata) :footnotes footnotes)
     ;(delay (.markdownToHtml (PegDownProcessor. org.pegdown.Extensions/TABLES) content))
     (delay (.markdownToHtml (PegDownProcessor. (int (bit-or org.pegdown.Extensions/TABLES org.pegdown.Extensions/SMARTYPANTS org.pegdown.Extensions/AUTOLINKS org.pegdown.Extensions/FENCED_CODE_BLOCKS org.pegdown.Extensions/HARDWRAPS))) content))

     ]))

(defn- read-html [file]
  (let [[metadata content]
        (split-file (slurp file :encoding (:encoding (config))))]
    [(prepare-metadata metadata) (delay content)]))

(defn- slow-read-org 
  [file]
     (if (not (:emacs (config)))
       (do (error "Path to Emacs is required for org files.")
           (System/exit 0)))
     (let [metadata (prepare-metadata
                     (apply str
                            (take 500 (slurp file :encoding (:encoding (config))))))
           command [(:emacs (config))  "--batch" "--eval" (str
                                                           "(progn "
                                                           (apply str (map second (:emacs-eval (config))))
                                                           " (find-file \"" (.getAbsolutePath file) "\") "
                                                           (:org-export-command (config))
                                                           ")") (:emacs-config (config))]
           content (delay (:out (apply sh command)))]
       [metadata content])
     )

; We really need to parse each file once. So we memoize the results
(def read-org
  (memoize slow-read-org))


(defn- read-clj [file]
  (let [[metadata & content] (read-string
                              (str \( (slurp file :encoding (:encoding (config))) \)))
        evalcontent (binding [*ns* (the-ns 'static.core)]
                       (eval (last content)))]
    [metadata evalcontent]))

(defn- read-cssgen [file]
  (let [metadata {:extension "css" :template :none}
        content (read-string
                 (slurp file :encoding (:encoding (config))))
        to-css  #(clojure.string/join "\n" (doall (map css %)))]
    [metadata (delay (binding [*ns* (the-ns 'static.core)] (-> content eval to-css)))]))

(defn read-doc [f]
  (let [extension (FilenameUtils/getExtension (str f))]
    (cond (or (= extension "markdown") (= extension "md"))
          (read-markdown f)
          (= extension "md") (read-markdown f)
          (= extension "org") (read-org f)
          (= extension "html") (read-html f)
          (= extension "clj") (read-clj f)
          (= extension "cssgen") (read-cssgen f)
          :default (throw (Exception. "Unknown Extension.")))))

(defn dir-path [dir]
  (cond (= dir :templates) (str (:in-dir (config)) "templates/")
        (= dir :public) (str (:in-dir (config)) "public/")
        (= dir :site) (str (:in-dir (config)) "site/")
        (= dir :posts) (str (:in-dir (config)) "posts/")
        (= dir :examples) (str (:in-dir (config)) "examples/")
        :default (throw (Exception. "Unknown Directory."))))

(defn list-files [d]
  (let [d (File. (dir-path d))]
    (if (.isDirectory d)
      (sort
       (FileUtils/listFiles d (into-array ["markdown"
                                           "md"
                                           "clj"
                                           "cssgen"
                                           "org"
                                           "html"]) true)) [] )))

(def read-template
  (memoize
   (fn [template]
     (let [extension (FilenameUtils/getExtension (str template))]
       (cond (= extension "clj")
             [:clj
                (let [thecontent (-> (str (dir-path :templates) template)
                  (File.)
                  (#(str \( (slurp % :encoding (:encoding (config))) \) )))]
                  (read-string thecontent)
                  )
              ]
             :default
             [:html
              (load-template (dir-path :templates) template)])))
   )
  )

(defn write-out-dir [file str]
  (FileUtils/writeStringToFile
   (File. (:out-dir (config)) file) str (:encoding (config))))

(defn deploy-rsync [rsync out-dir host user deploy-dir]
  (let [cmd [rsync "-avz" "--delete" "--checksum" "-e" "ssh"
             out-dir (str user "@" host ":" deploy-dir)]]
    (info (:out (apply sh cmd)))))
