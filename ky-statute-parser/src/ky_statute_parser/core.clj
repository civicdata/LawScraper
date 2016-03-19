(ns ky-statute-parser.core
  (:require [clojure.data.json :as json]
            [clojure.data.xml :as xml])
  (:import [org.apache.pdfbox.pdmodel PDDocument]
           [org.apache.pdfbox.text PDFTextStripper TextPosition]
           [java.util ArrayList]
           [java.io StringWriter])
  (:gen-class))


  ;; This parser proceeds in the following basic steps.
  ;; 1. Read the pdf as contiguous chunks of text of a particular style
  ;;    These chunks look like :
  ;;    {:text "Some text" :left-padding float :style [:plain/:bold font-size] :lefts [left-padding-of-each-character]}
  ;;    There may be more than one chunk per line, particularly when part of the line is BOLD and another is not.
  ;; 1. Split the document into the front-matter[title and body] (size 12 font) and the end-matter[things like history, LRC notes...] (size 9.6)
  ;; 2. Parse the end matter
  ;; 3. Parse out the Title: everything that is :bold from the beginning of the doc until plain text is found
  ;; 4. Parse the body, the only remaining non-parsed text.


;;
;;      Wrapper around PDF Box.
;;        This section deals with reading the pdf.
;;

(defn get-style
  "Accepts a PDFBox TextPosition object, and returns [:bold/:plain font-size]"
  [position]
  (let [desc (-> position .getFont .getFontDescriptor)
        bold (if (or (.isForceBold desc)
                     (-> desc .getFontName (.contains "Bold")))
               :bold
               :plain)]
    [bold (.getFontSize position)]))

(defn pdf-extractor
  "Proxy object used to extract chunks (as described above) from a pdf.

  parsed is an atom of [] that chunks are conjed onto. Debug is a flag used for some logging.
  "
  [parsed debug]
  (proxy [PDFTextStripper]
      []
    (writeString
      ([text textPosition]
       (do
         (when debug (println "WriteStr2 :" text))
         (swap! parsed into
                (if (= (count text) (.size textPosition))
                  (let [parted (->> (map vector text textPosition)
                                     (partition-by (comp get-style second)))]
                    [(->> parted
                        (map (fn [x] {:text (apply str (map first x))
                                      :style (-> x first second get-style)
                                      :left-padding (-> x first second .getX)
                                      :lefts (mapv #(.getX (second %)) x)
                                      }))
                        (filter #(not (re-matches #"\s+" (:text %))))
                        )])
                [[{:text text :style (get-style (first textPosition))
                  :left-padding (-> textPosition first .getX)
                  :missmatched true}]]))))
      ([text]
       (do (when debug (println "WriteStr1 :" text))
           (swap! parsed conj {:text text}))))
  ))

(defn stateful-setters
  "Sets some PDFTextStripper preferences when merged with a set of parameters"
  [obj]
  {:drop-threshold                #(.setDropThreshold obj %)
   :indent-threshold            #(.setIndentThreshold obj %)
   :average-char-tolerance #(.setAverageCharTolerance obj %)
   :sort-by-position             #(.setSortByPosition obj %)
   :spacing-tolerance          #(.setSpacingTolerance obj %)})

(def state-pdf-tolerances
  "Defaults that I ended up selecting through search"
  {:indent-threshold 100
   :drop-threshold 2.75
   :sort-by-position true})

(defn get-meta
  "Gets some metadata from the pdf"
  [pdf-doc]
  (let [info (.getDocumentInformation pdf-doc)]
    {:author (.getAuthor info) :creation-date (.getCreationDate info)}))

(defn read-pdf
  "Does what it says on the tin. params is a map with keys corresponding to keys in stateful-setters"
  [filename params]
  (let [parsed (atom [])
        extractor (pdf-extractor parsed false)
        garbage (StringWriter.)
        ]
    (with-open [document (PDDocument/load (clojure.java.io/file filename))]
      (merge-with (fn [x y] (x y)) (stateful-setters extractor) params)
      (.writeText extractor document garbage)
      (reset! parsed {:contents @parsed :meta (get-meta document)})
      @parsed)))


;;
;; Utility methods
;; The following methods verify contracts during the parsing of the pdf, and throw errors as appropriate
;;


(defn verify
  [contract message data]
    (when-not (contract data)
      (throw (Exception. message )))
    data)

(defn verify-not
  [contract message data]
  (let [res (contract data)]
    (when res
      (throw (Exception. (str message res))))
    data))

(defn printret
  [x]
  (do (clojure.pprint/pprint x)
      x))

;;
;;
;; The following four functions handle the end matter.
;;
;;  gather-end-matter utilizes the other three functions to group the "headers" (like History, Effective, etc.) [concat-headers]
;;  to classify the headers [classify-end-line]
;;  to combine them all [gather-end-matter]
;;  


(defn classify-end-line
  [{:keys [text style left-padding] :as line-head}]
  (let [[bold-plain _] style
        history          #"8?History\s*[:.]?\s*"
        history-former   #"History\s*(?:of|for)\s*[fF]ormer\s*(.*)"
        hist-through     #"History\s*(?:through\s*)?([0-9]+)[:.]?\s*"
        hist-alt         #"History\s*for\s*alternate\s*version\s*of\s*KRS\s*([0-9.]+)\s*[:]?\s*"
        effective        #"Effective\s*(?:date\s*)?[:.]?\s*"
        effective-j      #"Effective\s*(?:date\s*)?[:.]?\s*J"
        catchline-r      #"Catc?hline\s+at\s+repeal\s*[:.]?\s*(.*)"
        catchline-e      #"Catchline\s+at\s+expiration\s*:\s*(.*)"
        catchline-o      #"Catchline\s+at\s+time\s+of\s+omission:\s*"
        catchline-r-enh  #"Catchline\s+at\s+repeal\s+of\s+former\s+KRS\s+([0-9A-Z.-]+)[:.]?\s*"
        lrc-note         #"Legislative\s+Research\s+(?:[cC]ommission\s*)?(?:[nN]ote)?\s*(.*)"
        former-cod       #"(?:F?ormerly\s*codified(?:,?in\s*part,?)?\s*(?:as)?\s*(.*)|Also\s*previously\s*codified\s*as\s*|Subsection\s*\([a-z0-9]+\)\s*formerly\s*codified)"
        also-former-cod  #"Also\s*previously\s*codified\s*(?:as\s*)?"
        note             #"N[oO][tT][eE][:.]?\s*"
        budget-ref       #"(\d+)\s*-\s*(\d+)\s*Budget\s*Reference[.]?\s*(.*)"
        enh-former-cod   #"KRS\s*([0-9A-Z.-]+)\s*formerly\s*codified\s*as\s*([0-9A-Z.-]+)?"
        renumbered       #"Subsequently\s*re.*"
        ineffective      #"Effective:\s*.*(History.*)"
        emphatic-dashes  #"[\s/.-]+"
        emphatic-section #"24A[.]120"
        repealeanacted   #"Subsequently\s*repealed,\s*reenacted,?\s*and\s*amended\s*as\s*KRS\s*([0-9A-Z.-]+)[.]?\s*"
        renumbered       #"Subsequently\s*renumbered\s*as\s*KRS\s*([0-9A-Z.-]+)[.]?\s*effective\s*,?\s*(.*)"
        ]
    (if-not (= :bold bold-plain)
      (condp re-matches text
        repealeanacted :>> (fn [[_ new-statute]] {:type :renumbered :new-number new-statute})
        renumbered     :>> (fn [[_ new-statute eff]] {:type :renumbered :new-number new-statute :effective eff})
      {:text text :child true})
      (condp re-matches text
        history            {:type :history}
        history-former :>> (fn [[_ old-title]] {:type :former-history :former-name old-title})
        hist-through   :>> (fn [[_ year]] {:type :archived-history :as-of-year year})
        hist-alt       :>> (fn [[_ sect]] {:type :alternate-history :reference sect})
        effective          {:type :effective}
        effective-j        {:type :effective :text "J"}
        catchline-r        {:type :catchline-at-repeal}
        catchline-e        {:type :catchline-at-expire}
        catchline-o        {:type :catchline-at-omission}
        catchline-r-enh    (fn [[_ id]] {:type :catchline-ar-repeal :formerly id})
        lrc-note           {:type :lrc-note}
        former-cod         {:type :former-codification}
        also-former-cod    {:type :former-codification}
        note               {:type :note}
        renumbered         {:type :renumbered}
        emphatic-dashes    {:text text :child true}
        emphatic-section   {:text text :child true}
        budget-ref     :>> (fn [[_ start end __]] {:type :budget-ref :start-year start :end-year end})
        enh-former-cod :>> (fn [[_ cod text]] (merge (if text {:text text} {}) {:type :former-codification :former-code cod}))
        ineffective    :>> (fn [[_ hist-line]] (assoc (classify-end-line {:text hist-line :style [:bold nil]}) :extra-type :effective-blank))
        (throw (Exception. (str "Unknown end-matter: " text))))
        )))


(defn gather-end-matter
  [{:keys [stack info] :as gathered} {:keys [type child text] :as input}]
  (cond
    (and (nil? stack) (nil? type)) (throw (Exception. (str "Orphaned end-matter cannot be incorporated, first element: " (:text input))))
    (and (nil? stack) type) {:info info :stack (merge-with str input {:text ""})}
    (and (stack :type) child) (update-in gathered [:stack :text] str text)
    (and stack type info) (if (info (stack :type))
                            (-> gathered
                                (update-in [:info (stack :type)] conj stack)
                                (assoc :stack (merge-with str input {:text ""})))
                            (-> gathered
                                (assoc-in [:info (stack :type)] [stack])
                                (assoc :stack (merge-with str input {:text ""}))))
    true (throw (Exception. "Orphaned end-matter cannot be incorporated"))
  ))


(defn concat-headers-bodies
  [endmatter]
  (let [compressor (fn compressor
                     ([a] a)
                     ([a m] {:left-padding (:left-padding a)
                              :style (:style a)
                              :lefts (into (:lefts a) (:lefts m))
                              :text (str (:text a) (:text m))}))]
    (->> endmatter
         (partition-by :style)
         (map #(reduce compressor %))
         )))


(defn handle-end-matter
  ([front]
   {:front front
    :end {}}
   )
  ([front end-matter]
   (let [finish-gather (fn [{:keys [info stack]}]
                         (if (info (stack :type))
                           (update info (stack :type) conj stack)
                           (assoc info (stack :type) [stack])))
        glue (fn [end] {:front front :end end})
        ]
  (->> end-matter
       flatten
       concat-headers-bodies
       (map classify-end-line)
       (reduce gather-end-matter {:info {}})
       finish-gather
       glue
       ))))




;;
;;  extract title splits the title off, and since the end is already handled, the body is all that is left.
;;
;;

(defn extract-title
  [{:keys [front end]}]
  (let [all-front (flatten front)
        [title-segments body-segments] (split-with #(= :bold (first (:style %))) all-front)]
    (verify (comp not empty?) "No Body/Title" body-segments)
    {:title (:text (apply merge-with str title-segments))
     :body body-segments
     :end end}
     ))


;; Two Contracts

(defn title-body-end-present
  [{:keys [title body end]}]
  (and title body end))

(defn bold-in-body
  [{:keys [body] :as inp}]
  (let [example (first (filter #(= :bold (first (:style %))) body))]
    (if example (:text example) false)))


;;
;;  There is a dash of bookkeeping involved in verifying that we're looking at a section
;;  involved in section-placer, and section-builder
;;  

(defn section-placer
  [line]
  (let [matched (re-matcher #"(?:\(([1-9][0-9]?)\)\s*)?(?:\(([a-z]?)\)\s*)?(?:([1-9][0-9]?)[.]\s*)?(?:([a-z])[.])?(?:(i+)[.])?\s*(.+)" line)]
    (.find matched)
    (for [ii (range 1 (inc (.groupCount matched)))]
      [(if (<= ii 2) (dec (.start matched ii)) (.start matched ii))  (.group matched ii)])))

(defn section-builder
  ([lefts [section-level [left-index text]]]
   (when-not (= section-level 5) (throw (Exception. "Incorrectly formatted section beginning")))
   {:text text :left-padding (lefts left-index)})
  ([lefts [section-level [left-index prefix]] & r]
   (if (nil? prefix)
     (apply section-builder lefts r)
     {:section-level (inc section-level) :section-prefix prefix :children-at (->> r (drop-while (fn [[_ [i t]]] (nil? t))) first ((fn [[_ [i t]]] (lefts i))))
      :subsections [(apply section-builder lefts r)]}
     )
   ))

(defn classify-body
  [{:keys [left-padding style text lefts]}]
  (->> text
       section-placer
       (map-indexed vector)
       (drop-while (fn [[_ [i t]]] (nil? t)))
       (apply section-builder lefts)
  ))


(defn merge-section
  [a b]
  (let [sla (:section-level a)
        slb (:section-level b)
        last-sub (last (filter (comp :subsections second) (map-indexed vector (:subsections a))))
        ]
    (when (nil? a) (throw (Exception. "Recursed too deeply in merge-section")))
    (if (= sla (dec slb))
      (update a :subsections conj b)
      (update-in a [:subsections (first last-sub)] merge-section b))))

(defn merge-text
  [{:keys [children-at subsections] :as a} {:keys [text left-padding] :as t}]
  (let [last-sub (peek subsections)]
    (cond
      (nil? last-sub) (update a :subsections conj {:text text})
      (:text last-sub) (update-in a [:subsections (dec (count (:subsections a))) :text] str text)
      (< left-padding (:children-at last-sub)) (update a :subsections conj {:text text})
      :else (update-in a [:subsections (dec (count (:subsections a)))] merge-text t)
      )))


(def next-logical-section
  (merge
   (->> (range 1 61)                    (map str)             (partition 2 1) (map (fn [[x y]] [[1 x] y])) (into {}))
   (->> (range (int \a) (inc (int \z))) (map (comp str char)) (partition 2 1) (map (fn [[x y]] [[2 x] y])) (into {}))
   (->> (range 1 61)                    (map str)             (partition 2 1) (map (fn [[x y]] [[3 x] y])) (into {}))
   (->> (range (int \a) (inc (int \z))) (map (comp str char)) (partition 2 1) (map (fn [[x y]] [[4 x] y])) (into {}))
   {[5 "i"] "ii" [5 "ii"] "iii" [5 "iii"] "iv" [5 "v"] "vi"}
   )  )

(def sane-section-starts
  [nil "1" "a" "1" "a" "i"])

(def base-body {:body {:section-level 0 :children-at 0 :subsections []}
                :last-sections [nil]
                :bad-parses []})

(defn section-prefix-array
  [{:keys [section-level section-prefix subsections]} seed]
  (let [[subsec] subsections
        ss (if (< (count seed) section-level) (conj seed nil) seed)]
    (if
      (:text subsec)
      (subvec (assoc ss section-level section-prefix) 0 (inc section-level))
      (assoc (section-prefix-array subsec ss) section-level section-prefix))))

(defn gather-body
  [{:keys [body last-sections bad-parses] :as doc} raw]
  (let [{:keys [section-level section-prefix text] :as input} (classify-body raw)
        section-type (cond
                       (nil? section-level) :text
                       (and (= section-level (count last-sections)) (= section-prefix (sane-section-starts section-level))) :new-subsection
                       (= section-level (count last-sections)) :skip-first-subsection
                       (and (< section-level (count last-sections)) (= section-prefix (next-logical-section [section-level (last-sections section-level)]))) :next-section
                       (> section-level (count last-sections)) :section-dive
                       (not= section-prefix (next-logical-section [section-level (last-sections section-level)])) :section-skip
                       :else :unknown-section-condition
                       )
        sane-parse (if (#{:section-dive :section-skip :unknown-section-condition :skip-first-subsection} section-type) false true)
        bp (if-not sane-parse (conj bad-parses section-type) bad-parses)
        tt (#{:text :section-dive :section-skip :unknown-section-condition :skip-first-subsection} section-type)
        bt (if tt {:text (raw :text) :left-padding (raw :left-padding)} input)
        sp (if (#{:new-subsection :next-section} section-type) (section-prefix-array input last-sections) last-sections)
        ]
    (cond
      tt (assoc doc :body (merge-text body bt) :last-sections sp :bad-parses bp)
      section-level (assoc doc :body (merge-section body input) :last-sections sp :bad-parses bp)
      true (throw (Exception. "Unknown body type"))
      )))

(defn parse-body
  [{:keys [title body end]}]
  (merge
   {:title title
    :end end}
   (reduce gather-body base-body body)))


(defn scrub-bold-type
  [lines]
  (map
   (fn [{:keys [style text] :as line}]
     (if
         (re-matches #"[:(., <]+" text)
       (assoc line :style [:plain (second style)])
       line)) lines))

(defn lift-effective-stamps
  [{:keys [title body end] :as input}]
  (let [effective-stamp? (fn [x] (and (= (first (:style x)) :bold)
                                      (re-matches #"\s*\(Effective\s*[A-Z][a-z]+\s*\d\d?,\s*\d+\s*\)\s*" (:text x))))
        caught (first (filter effective-stamp? body))
        body-remaining (filter (comp not effective-stamp?) body)]
    (if caught
      {:title title :body body-remaining :end (assoc end :effective [{:text (second (re-matches #"\s*\(Effective\s*([^)]*)\)\s*" (:text caught)))}])}
      input)))

(defn poorly-formatted-catchlines
  [{:keys [title body end] :as input}]
  (let [catchline? (fn [x] (if (and (= (first (:style x)) :bold)
                                (re-matches #"Catchline\s*at\s*repeal:\s*" (:text x))) true false ))
        [new-body catch catch-text :as parted] (partition-by catchline? body)
        ]
    (if catch
      {:title title :body new-body
       :end (assoc end :catchline-at-repeal [{:text (reduce str (map :text catch-text)) :type :catchline-at-repeal}])}
      input)
    )
  )

(defn parse-contents
  [contents]
  (let [line-style (fn [line] (-> line first :style))]
    (->> contents
         (filter (comp not empty?))
         (map scrub-bold-type)
         (partition-by (comp second line-style))
         (verify #(>= 2 (count %)) "Too many different fonts encountered")
         (apply handle-end-matter)
         extract-title
         lift-effective-stamps
         poorly-formatted-catchlines
         (verify title-body-end-present "Component missing")
         (verify-not bold-in-body "Body contains bold: ")
         parse-body
         )))

(defn district-statute
  [contents]
  (let [flattened (flatten contents)
        title? (re-matches #"(?:5|118B).\d+\s.*District.*" (:text (first flattened)))
        census? (some (fn [{:keys [text]}] (re-matches #"\s*-+CENSUS-+\s*.*" text)) flattened)]
    (if-not (and title? census?)
      false
      (let [title (-> flattened first :text)
            [body end] (split-with #(-> % :style first (= :bold) not) (drop 1 flattened))]
        (-> (handle-end-matter body end)
            (dissoc :front)
            (assoc :title title)
            (assoc :body {:section-level 0 :children-at 0
                          :subsections [{:text (clojure.string/join "\n" (map :text body))}]}))))))

(defn parse-pdf
  [filename]
  (let [parsed (read-pdf filename state-pdf-tolerances)]
    (if-let [district-case (district-statute (:contents parsed))]
      (assoc parsed :contents district-case)
      (update parsed :contents parse-contents)
      )))

;;
;;  Several functions to output xml based on json from stdin
;;
;;


;; Hiccup-style xml

;; [:law
;;  [:structure [:unit {:label "" :identifier "" :order_by "" :level ""}]]
;;  [:section_number "num"]
;;  [:catch_line "title"]
;;  [:order_by "order"]
;;  [:text [:section {:prefix ""} [:section sub]]]
;;  [:history "history"]
;;  [:metadata
;;   [:tag val]
;;   [:other-tag other-val]]
;;  [:tags [:tag "name"] [:tag "othername"]]
;;  ]

;; {:subchapter_link http://www.lrc.ky.gov/statutes/statute.aspx?id=3,
;;  :pdf_path full/481f842872483d1147468b2b1bdb351bcbf6ddad.aspx?id=3,
;;  :title_name TITLE I - SOVEREIGNTY AND JURISDICTION OF THE COMMONWEALTH,
;;  :title_index 1,
;;  :subchapter_name .100  Boundary with Virginia and West Virginia. ,
;;  :chapter_name CHAPTER 1 BOUNDARIES ,
;;  :pdf_md5 88092edd848cff52dfd6a96205e71054,
;;  :file_urls [http://www.lrc.ky.gov/statutes/statute.aspx?id=3],
;;  :subchapter_index 100}

(defn extract-structure
  [data]
  (let [[_ chap-num chap-name] (re-matches #"CHAPTER\s+(\d+)\s*-*\s*(.*)" (:chapter_name data))
        [_ title-num title-name] (re-matches #"TITLE\s+([IXVLCM]+)\s+-*\s*(.*)" (:title_name data))]
    [:structure
     [:unit {:label "title" :identifier title-num :order_by (:title_index data)} title-name]
     [:unit {:label "chapter" :identifier chap-num :order_by chap-num} chap-name]
     ]))

(defn extract-section-number
  [data]
  (let [[_ section-num catchline] (re-matches #"([^\s]+)\s*(.*)?" (get-in data [:contents :title]))]
    [:section_number section-num]
    ))

(defn extract-catch-line
  [data]
  (let [[_ section-num catchline] (re-matches #"([^\s]+)\s*(.*)?" (get-in data [:contents :title]))
        repeal-catch (->> data :end :catchline-at-repeal first :text)]
    (if-not (= "" catchline)
      [:catch_line catchline]
      (if repeal-catch [:catch_line repeal-catch]
      [:catch_line (str section-num " repealed")])
    )))

(defn extract-order-by
  [data]
  (let [[_ order-by] (re-matches #"[^.]+[.]([^\s]+)\s*(.*)?" (get-in data [:contents :title]))]
    [:order_by order-by]
    ))

(defn extract-section
  [{:keys [section-prefix text subsections]}]
  (if text
    text
    (into [:section {:prefix section-prefix}] (map extract-section subsections))))

(defn extract-text
  [data]
  (into [:text] (map extract-section (get-in data [:contents :body :subsections]))))



(defn extract-history
  [data]
  (if-let [history (get-in data [:contents :end :history])]
    [:history (apply str (map :text history))]
    [:history "None supplied"]))

  ;; {:type :history}
  ;; (fn [[_ old-title]] {:type :former-history :former-name old-title})
  ;; (fn [[_ year]] {:type :archived-history :as-of-year year})
  ;; (fn [[_ sect]] {:type :alternate-history :reference sect})
  ;; {:type :effective}
  ;; {:type :effective :text "J"}
  ;; {:type :catchline-at-repeal}
  ;; {:type :catchline-at-expire}
  ;; {:type :catchline-at-omission}
  ;; (fn [[_ id]] {:type :catchline-ar-repeal :formerly id})
  ;; {:type :lrc-note}
  ;; {:type :former-codification}
  ;; {:type :former-codification}
  ;; {:type :note}
  ;; {:type :renumbered}
  ;; {:text text :child true}
  ;; {:text text :child true}
  ;; (fn [[_ start end __]] {:type :budget-ref :start-year start :end-year end})
  ;; (fn [[_ cod text]] (merge (if text {:text text} {}) {:type :former-codification :former-code cod}))
  ;; (fn [[_ hist-line]] (assoc (classify-end-line {:text hist-line :style [:bold nil]}) :extra-type :effective-blank))


(defn extract-metadata-singular
  [key coll]
  (let [text (apply str (map :text coll))
        other-info (->> coll (map #(dissoc % :type :text)) (filter (comp not empty?)))]
    (concat [(when text [key text])]
     (when-not (empty? other-info)
       (mapcat (fn [x] (map (fn [[k v]] [(keyword (str (name key) "-" (name k))) v]) x)) other-info)))
    ))

(defn extract-metadata
  [data]
  (let [end-matter (get-in data [:contents :end])
        emk (keys (dissoc end-matter :history))
        sdf (java.text.SimpleDateFormat. "yyyy-MM-dd")
        cal ((data :meta) :creation-date)

        ]
    (.setCalendar sdf cal)
    (-> (mapcat #(extract-metadata-singular % (end-matter %)) emk)
        (conj :metadata)
        vec
        (conj [:pdf-author ((data :meta) :author)])
        (conj [:pdf-creation-date (when cal (.format sdf (.getTime cal)))])
        (conj [:pdf-download-date (data :download_time)])
        (conj [:original-link (-> data :file_urls first)])
        )
    ))

(defn extract-tags
  [data]
  [:tags
   [:tag "computer-parsed"]
   [:tag "unverified"]
   (when-not (empty? ((data :contents) :bad-parses)) [:tag "suspect-parse"])
   ])


;; [:law
;;  [:structure [:unit {:label "" :identifier "" :order_by "" :level ""}]]
;;  [:section_number "num"]
;;  [:catch_line "title"]
;;  [:order_by "order"]
;;  [:text [:section {:prefix ""} [:section sub]]]
;;  [:history "history"]
;;  [:metadata
;;   [:tag val]
;;   [:other-tag other-val]]
;;  [:tags [:tag "name"] [:tag "othername"]]
;;  ]

(defn extract-all
  [data]
  [:law
   (extract-structure data)
   (extract-section-number data)
   (extract-catch-line data)
   (extract-order-by data)
   (extract-text data)
   (extract-history data)
   (extract-metadata data)
   (extract-tags data)
   ]
  )

(defn unsafe-xml
  [json-str]
  (let [js (json/read-str json-str :key-fn keyword)
        pdf-name (js :pdf_path)
        parsed-pdf (parse-pdf pdf-name)
        data (merge parsed-pdf js)
        xml-out (subs
                 (xml/emit-str
                  (if (:parse-failed data)
                    (xml/sexp-as-element [:failed-parse
                                          [:pdf pdf-name]
                                          [:title (js :title_name)]
                                          [:chapter (js :chapter_name)]
                                          [:subchapter (js :subchapter_name)]])
                    (xml/sexp-as-element (extract-all data)))) 38)]
    xml-out
  ))

(defn -main [& args]
  (loop [line (read-line)]
    (when line
      (let [js (json/read-str line :key-fn keyword)
            pdf-path (js :pdf_path)
            [_ pdf-dir pdf-hash] (re-matches #"(.*)/full/(.+)[.]aspx[?]id=\d+" pdf-path)
            parsed-pdf (try (parse-pdf pdf-path) (catch Exception e {:parse-failed :true}) (catch Error e {:parse-failed true}))
            data (merge parsed-pdf js)
            xml-out (if (:parse-failed data)
                      (str "Failed parse: " pdf-path)
                        (try
                           (xml/emit-str
                            (if (:parse-failed data)
                              (xml/sexp-as-element [:failed-parse
                                                    [:pdf pdf-path]
                                                    [:title (js :title_name)]
                                                    [:chapter (js :chapter_name)]
                                                    [:subchapter (js :subchapter_name)]])
                              (xml/sexp-as-element (extract-all data))))
                          (catch Exception e
                            (str "Failed xml: " pdf-path))))
            failed (re-matches #"Failed.*" xml-out)
            ]
        (if failed
          (println line)
          ;; (println (str pdf-dir "/xml-files/" pdf-hash ".xml")))
          (spit (str pdf-dir "/xml-files/" pdf-hash ".xml") xml-out))
        (recur (read-line)))))
  )


(defn test-kenning
  [coll]
  (let [ft (fn [x]
             (print ".")
             (if
                 (try (parse-pdf x)
                      (catch Exception e false)
                      (catch StackOverflowError s false)) false x))]
    (filter ft coll)))

;; (def all-pdfs (map #(str "../../statepdfs/full/" (.getName %)) (drop 1 (file-seq (clojure.java.io/file "../../statepdfs/full")))))

;; (def failing-cases (atom []))

;; (reset! failing-cases (test-kenning all-pdfs))

(defn why-map
  [x]
  (try
    (parse-pdf x)
    (catch Exception e
      [x (.getMessage e)])
    (catch StackOverflowError e
      [x "Stackoverflow"])
    ))


;; (defn fail-matches
;;   [regex]
;;   (->> @failing-cases test-kenning (map why-map) (filter #(->> % second (re-matches regex)))))

;; (defn cluster
;;   [fails]
;;   (->> fails
;;        (group-by #(second (re-matches #"([^:]+):?.*" %)))
;;        ))

;; (swap! failing-cases test-kenning)

