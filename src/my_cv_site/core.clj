(ns my-cv-site.core
  (:require [clojure.java.io :as c-io]
            [my-cv-site.html :as mcs-h])
  (:import (org.apache.pdfbox.pdmodel PDDocument)
           (FormattedReader))
  (:gen-class))

(defn replace-if [pred repl coll]
  (map #(if (pred %) (repl %) %) coll))

(defn printable-char? [c] (and (<= (int c) 127) (>= (int c) 32)))

(defn change-last [seq fn]
  (concat (butlast seq) [(fn (last seq))]))

(defn pdf->str [path]
  (with-open [fl (PDDocument/load path)]
    (let [htmler (new FormattedReader)]
      (.getText htmler fl))))

(defn str->sizes-map [st]
  (map (fn [%] [(ffirst %) (-> (->> % (map second)
                                      (mapcat identity)
                                      (replace-if (complement printable-char?) (fn [v] " ")))
                               clojure.string/join
                               .trim
                               (.replace "\n" " "))])
       (partition-by first
                     (map (fn [[ch sz]] [(-> sz .trim Float/parseFloat) ch])
                          (partition 2 (.split st "<ftsz>"))))))

(defn guess-hierarchy [sz-map & {:keys [tolerance] :or {tolerance 0}}]
  (loop [acc [{:font-size (ffirst sz-map) :text (-> sz-map first second) :children []}]
         stack [0 :children -1]
         szs (rest sz-map)]
    (if (empty? szs) acc
      (let [v (ffirst szs)
            n-s (loop [vv stack] (if (and (< 1 (count vv))
                                          (<= (- (get-in acc (concat (butlast (butlast vv)) [:font-size])) v) tolerance))
                                   (recur (butlast (butlast vv)))
                                   (change-last vv inc)))]
        (recur (assoc-in acc n-s {:font-size v :text (-> szs first second) :children []})
               (concat n-s [:children -1]) (rest szs))))))

(defn -main [& args]
  (->> "test.pdf"
       c-io/resource
       pdf->str
       str->sizes-map
       ((fn [m] (guess-hierarchy m :tolerance 1)))
       mcs-h/hier->site
       (spit (c-io/resource "out.html"))))
