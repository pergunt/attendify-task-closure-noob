(ns ^:figwheel-no-load attendify-task-closure-noob.dev
  (:require
    [attendify-task-closure-noob.core :as core]
    [devtools.core :as devtools]))

(extend-protocol IPrintWithWriter
  js/Symbol
  (-pr-writer [sym writer _]
    (-write writer (str "\"" (.toString sym) "\""))))

(enable-console-print!)

(devtools/install!)

(core/init!)
