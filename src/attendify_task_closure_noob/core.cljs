(ns attendify_task_closure_noob.core
  (:require [reagent.core :refer [atom]]
            [reagent.dom :refer [render]]
            [clojure.string :as str]
            [goog.labs.format.csv :refer [parse]]
            [clojure.spec.alpha :as s]
            [cljs.core.async :refer [put! chan <! >!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))


(defn validate-label [v]
  (-> v empty? not))
(defn is-number [x]
  (boolean (not (js/isNaN x))))

(s/def ::content
       (s/+
        (s/spec
         (s/cat :company validate-label :income is-number))))
(s/def ::nested
       (s/cat :head    (s/spec (s/tuple #(= "Company" %) #(= "Income" %)))
              :content ::content))

;; atom to store file contents
(defonce default-state {:isOpen false :file-name "" :file-size 0})
(defonce file-data (atom []))
(defonce app-state (atom default-state))

(def first-file
  (map
   (fn [e]
     (let [target (.-currentTarget e)
           file   (-> target .-files (aget 0))]
       (set! (.-value target) "")
       file))))

;; transducer to get text out of file object.
(def extract-result
  (map #(-> % .-target .-result parse js->clj)))

;; two core.async channels to take file array and then file and apply above transducers to them.
(defn handle-exception
  [ex]
  (js/alert (.-message ex))
  nil)

(def upload-reqs (chan 1 first-file))
(def file-reads (chan 1 extract-result handle-exception))

;; function to call when a file event appears: stick it on the upload-reqs channel (which will use the transducer to grab the first file)
(defn put-upload [e]
  (put! upload-reqs e))

;; sit around in a loop waiting for a file to appear in the upload-reqs channel, read any such file, and when the read is successful, stick the file on the file-reads channel.
(go-loop []
         (let [reader (js/FileReader.)
               file   (<! upload-reqs)]
           (swap! app-state assoc :file-name (.-name file) :file-size (.-size file))
           (set! (.-onload reader) #(put! file-reads %))
           (.readAsText reader file)
           (recur)))

;; sit around in a loop waiting for a string to appear in the file-reads channel and put it in the state atom to be read by reagent and rendered on the page.
(go-loop []
         (let [file (<! file-reads)]
           (try
             (cond
              (< (* 10 1024) (:file-size @app-state)) (throw (js/Error. "The file is too big"))
              (not (s/valid? ::nested file))          (throw (js/Error. "The file is invalid"))
              :else                                   (let [parsedFile (vec
                                                                        (map
                                                                         (fn [item]
                                                                           {:company (item 0) :income (item 1)})
                                                                         file))]
                                                        (reset! file-data parsedFile)))
             (catch js/Error e
               (reset! file-data [])
               (-> e .-message js/alert)))
           (recur)))

(defn on-item-click [e]
  (let [index (js/parseInt (-> e .-currentTarget .-dataset .-index))]
    (swap! app-state assoc :isOpen
           (if (= (get @app-state :isOpen) index)
             false
             index))))

(defn set-value-after-interaction [oldVal]
  (fn [index column]
    (let [isNumeric   (and (not= index 0) (= column :income))
          numToString (str oldVal) ;; for the "empty?" condition
          ]
      (cond
       (and isNumeric (not (empty? numToString)))              (js/parseFloat oldVal)
       (and isNumeric (empty? numToString))                    0
       (and (not isNumeric) (empty? oldVal))                   "-"
       :else                                                   oldVal))))
(defn handle-input-value [e]
  (let [eventType (.-type e)]
    (when (= eventType "submit")
          (.preventDefault e)
          (swap! app-state assoc :isOpen false)
          )
    )
  (fn [index column]
    (let [inputType (-> e .-currentTarget .-type)
          v         (-> e .-currentTarget .-value)]
      (reset! file-data
              (update-in @file-data [index column] #((set-value-after-interaction %) index column))))
    )
  )

(defn table-item [el text index column]
  [el
   (if (true? (= (get @app-state :isOpen) index))
     [:form
      {:on-submit #((handle-input-value %) index column)}
      [:input
       {:value     text
        :type      (if (and (= column :income) (= el :td))
                     "number"
                     "text")
        :on-blur   #((handle-input-value %) index column)
        :on-change (fn [e]
                     (let [v         (-> e .-currentTarget .-value)]
                       (reset! file-data
                               (update-in @file-data [index column] (fn [_] v)))))}]
      [:i.fas.fa-splotch
       {:on-click   on-item-click
        :data-index index}]]
     [:<>
      [:span text]
      [:i.fas.fa-pen
       {:on-click   on-item-click
        :data-index index}]])])

;; -------------------------
;; Views
(defn table [header content]
  (when header
        [:table.full-width.talign-center
         [:thead
          (let [{c :company i :income} header]
            [:tr
             [table-item :th c 0 :company]
             [table-item :th i 0 :income]])]
         [:tbody
          (map-indexed
           (fn [index {c :company i :income}]
             ^{:key index} [:tr
                            [table-item :td c (+ index 1) :company]
                            [table-item :td i (+ index 1) :income]])
           content)]]))
(defn upload-btn [file-name]
  [:span.upload-label
   [:label
    [:input.d-none
     {:type "file" :accept ".csv" :on-change put-upload}]
    [:i.fa.fa-upload.fa-lg]
    (if (empty? file-name)
      "click here to upload and render csv..."
      file-name)]
   (when file-name
         [:i.fa.fa-times.ml-2
          {:on-click (fn [e]
                       (reset! file-data [])
                       (reset! app-state default-state))}])])

(defn get-sum
  [data]
  (->> data
       (map
        (fn [{i :income}]
          (if (= i "")
            0
            (js/parseFloat i))))
       (reduce + 0)))

(defn home-page []
  (let [{file-name :file-name} @app-state
        [header & content]     @file-data
        sum                    (get-sum content)]
    [:div
     [upload-btn file-name]
     [:p "Total " sum]
     [:p "Average " (/ sum 2.0)]
     [table header content]]))

;; -------------------------
;; Initialize app

(defn mount-root []
  (render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
