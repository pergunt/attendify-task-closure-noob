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
(defn table [items]
  [:table.full-width.talign-center
   [:thead
    [:tr
     [:th "Company"]
     [:th "Income"]
     ]
    ]
   [:tbody
    (for [{c :company i :income} items]
      ^{:key c} [:tr [:td c] [:td i]]
      )
    ]
   ]
  )

(defn home-page []
  [:div
   [table table-items]
  ]
  )

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
