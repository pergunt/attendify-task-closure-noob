(ns attendify_task_closure_noob.core
  (:require [reagent.core :refer [atom]]
            [reagent.dom :refer [render]]
            [clojure.string :as str]
            [cljs.core.async :refer [put! chan <! >!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

;; derived from https://mrmcc3.github.io/post/csv-with-clojurescript/
;; and based on reagent-frontend template.

;; dependencies from project.clj in addition to clojure, clojurescript, and reagent:
;; [org.clojure/core.async "0.2.395"]

;; atom to store file contents

(def file-data (atom []))
(def toggled-state (atom false))

;; transducer to stick on a core-async channel to manipulate all the weird javascript
;; event objects --- basically just takes the array of file objects or something
;; that the incomprehensible browser API creates and grabs the first one, then resets things.
(def first-file
  (map (fn [e]
         (let [target (.-currentTarget e)
               file (-> target .-files (aget 0))]
           (set! (.-value target) "")
           file))))

;; transducer to get text out of file object.
(def extract-result
  (map #(-> % .-target .-result js->clj)))

;; two core.async channels to take file array and then file and apply above transducers to them.
(def upload-reqs (chan 1 first-file))
(def file-reads (chan 1 extract-result))

;; function to call when a file event appears: stick it on the upload-reqs channel (which will use the transducer to grab the first file)
(defn put-upload [e]
  (put! upload-reqs e))

;; sit around in a loop waiting for a file to appear in the upload-reqs channel, read any such file, and when the read is successful, stick the file on the file-reads channel.
(go-loop []
         (let [reader (js/FileReader.)
               file (<! upload-reqs)]
           (set! (.-onload reader) #(put! file-reads %))
           (.readAsText reader file)
           (recur)))

;; input component to allow users to upload file.
(defn input-component []
  [:input {:type "file" :id "file" :accept ".txt" :name "file" :on-change put-upload}])

(defn on-item-click [e]
  (let [index (js/parseInt (-> e .-currentTarget .-dataset .-index))]
    (reset! toggled-state (if (= @toggled-state index)
                            false
                            index
                            ))
    )
  )

;; ----Add a multi-method
(defn table-item [el text index column]
  [el
   (if (true? (= @toggled-state index))
     [:<>
      [:input {
                :value text
                :type (if (and (= column :income) (= el :td))
                        "number"
                        "text"
                        )
                :on-blur (fn [e]
                           (let [inputType (-> e .-currentTarget .-type)
                                 v (-> e .-currentTarget .-value)
                                 ]
                             (when (and (= inputType "number") (empty? v))
                                   (reset! file-data (
                                     update-in @file-data [index column] (fn [oldVal] 0)
                                   )
                                 )
                               )
                             )
                           )
                :on-change (fn [e]
                             (let [v (-> e .-currentTarget .-value)
                                   inputType (-> e .-currentTarget .-type)
                                   ]
                               (reset! file-data (
                                   update-in @file-data [index column] (fn [oldVal] v)
                                   )
                                 )
                               )
                             )
                }
       ]
       [:span.toggle {
                :on-click on-item-click
                :data-index index
                } "close"
        ]
      ]
     [:<>
      [:span text]
      [:span.toggle {
               :on-click on-item-click
               :data-index index
               } "open"
       ]
      ]
     )
   ]
  )
;; -------------------------
;; Views
(defn table []
  (when (first @file-data)
    [:table.full-width.talign-center
     [:thead
      (let [{c :company i :income} (first @file-data)]
        [:tr
         [table-item :th c 0 :company]
         [table-item :th i 0 :income]
         ]
        )
      ]
     [:tbody
      (map-indexed (
         fn [index {c :company i :income}]
           ^{:key index} [:tr
                          [table-item :td c (+ index 1) :company]
                          [table-item :td i (+ index 1) :income]
                          ]
           ) (rest @file-data)
         )
      ]
     ]
    )
  )

;; sit around in a loop waiting for a string to appear in the file-reads channel and put it in the state atom to be read by reagent and rendered on the page.
(go-loop []
   (let [splitedArr (str/split (str/trim (<! file-reads)) "\n")]
     (reset! file-data
             (vec (map (fn [item]
                 (let [[company income] (str/split item " ")]
                   {:company company :income income}
                   )
                 )
                splitedArr
               ))
       )
     (recur)
 ))

(defn home-page []
  [:div
   [input-component]
   [:p "total " (->> (rest @file-data)
                    (map (fn [{i :income}]
                           (if (= i "")
                             0
                             (js/parseInt i)
                             )
                           ))
                    (reduce + 0)
                    )]
   [table]
   ])

;; -------------------------
;; Initialize app

(defn mount-root []
  (render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
