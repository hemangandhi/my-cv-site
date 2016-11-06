(ns my-cv-site.core
  (:require [clojure.java.io :as c-io])
  (:import (org.apache.pdfbox.pdmodel PDDocument)
           (FormattedReader))
  (:gen-class))

(defn printable-char? [c] (and (<= (int c) 127) (>= (int c) 32)))

(defn change-last [seq fn]
  (concat (butlast seq) [(fn (last seq))]))

(defn pdf->str [path]
  (with-open [fl (PDDocument/load path)]
    (let [htmler (new FormattedReader)]
      (.getText htmler fl))))

(defn str->sizes-map [st]
  (map (fn [%] [(ffirst %) (-> (->> % (map second) (mapcat identity) (filter printable-char?))
                               clojure.string/join
                               .trim
                               (.replace "\n" " "))])
       (partition-by first
                     (map (fn [[ch sz]] [(-> sz .trim Float/parseFloat) ch])
                          (partition 2 (.split st "<ftsz>"))))))

(defn guess-hierarchy [sz-map]
  (loop [acc [{:font-size (ffirst sz-map) :text (-> sz-map first second) :children []}]
         stack [0 :children -1]
         szs (rest sz-map)]
    (if (empty? szs) acc
      (let [v (ffirst szs)
            n-s (loop [vv stack] (if (and (< 1 (count vv))
                                          (<= (- (get-in acc (concat (butlast (butlast vv)) [:font-size])) v) 0))
                                   (recur (butlast (butlast vv)))
                                   (change-last vv inc)))]
        (recur (assoc-in acc n-s {:font-size v :text (-> szs first second) :children []})
               (concat n-s [:children -1]) (rest szs))))))

(defn hier->html-ul [hier]
  (str "<li>" (clojure.string/join (map #(apply str (concat [(:text %)]
                                                            (if (empty? (:children %))
                                                              [""]
                                                              ["<ul>" (hier->html-ul (:children %)) "</ul>"])))
                                        hier)) "</li>"))

(defn guess-head [hier]
  (str "<title>" (get-in hier [0 :text]) "</title>"))

(defn wrap-body
  ([html] (str "<!DOCTYPE html><html><body>" html "</body></html"))
  ([body head] (str "<!DOCTYPE html><html><head>" head "</head><body>" body "</body></html>")))

(defn -main [& args]
  (->> "test.pdf"
       c-io/resource
       pdf->str
       str->sizes-map
       guess-hierarchy
       ((fn [h] (wrap-body (hier->html-ul h) (guess-head h))))
       (spit (c-io/resource "out.html"))))
