(ns main
  (:require [scanner]
            [clojure.pprint]))

(defn run [source]
  (let [tokens (scanner/scan-tokens source)]
    (clojure.pprint/pprint tokens)))


(defn run-file [file-path]
  (run (slurp file-path :encoding "utf-8")))


(defn run-prompt []
  (loop [_ nil]
    (print "> ")
    (flush)
    (when-let [line (read-line)]
      (run line)
      (recur nil))))


(defn -main [&[file-path :as args]]
  (println file-path args)
  (cond (> (count args) 1)
        (do (println "Usage: loxure [script]")
            (System/exit 64))
        (= (count args) 1)
        (run-file file-path)
        :else
        (run-prompt)))
