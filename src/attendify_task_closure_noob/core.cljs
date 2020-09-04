(ns attendify-task-closure-noob.core
    (:require
      [reagent.core :as r]
      [reagent.dom :as d]))

(def table-items '(
                   {:company "Ace" :income 1}
                   {:company "Acme" :income 55}
                   {:company "Evil" :income 22}
                   )
  )
;; -------------------------
;; Views
(defn -table [items]
  [:table.full-width.talign-center
   [:thead
    [:tr
     [:th "Company"]
     [:th "Income"]
     ]
    ]
   [:tbody
    (map
     (fn [{c :company i :income}]
       [:tr {:key c}
        [:td c]
        [:td i]
       ]
       )
     items)
    ]
   ]
  )

(defn home-page []
  [:div
   [:h2 "Welcome to my house"]
   (-table table-items)
  ]
  )

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
