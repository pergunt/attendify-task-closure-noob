(ns attendify-task-closure-noob.prod
  (:require
    [attendify-task-closure-noob.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
