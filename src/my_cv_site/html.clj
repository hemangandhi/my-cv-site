(ns my-cv-site.html
  (:require [hiccup.core :as hic]))

(def bootstrap-include
  "<link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css\"/><script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js\"></script>")

(defn hier->html-ul [hier]
  (str "<ul>" (clojure.string/join (map #(apply str (concat ["<li>" (:text %)]
                                                            (if (empty? (:children %))
                                                              [""]
                                                              [(hier->html-ul (:children %))]) ["</li>"]))
                                        hier)) "</ul>"))

(defn guess-head [hier]
  (str "<title>" (get-in hier [0 :text]) "</title>"))

(defn wrap-body
  ([html] (str "<!DOCTYPE html><html><body>" html "</body></html"))
  ([body head] (str "<!DOCTYPE html><html><head>" head "</head><body>" body "</body></html>")))

(defn add-class [tag clas]
  (assoc-in tag [1 :class] clas))

(defn merge-kids [tag kids]
  (apply concat tag [kids]))

(defn hier->hic
  ([hier text->hic clas] (hier->hic hier text->hic clas 0))
  ([hier text->hic clas lv]
   (-> (text->hic hier)
       (add-class (clas lv))
       (merge-kids (map hier->hic (:childer hier))))))


