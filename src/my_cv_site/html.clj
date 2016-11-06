(ns my-cv-site.html
  (:require [hiccup.core :as hic]
            [hiccup.page :as hic-p]))

(def bootstrap-include
  {:css "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
   :jquery "https://code.jquery.com/jquery-3.1.1.min.js"
   :js "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"})

(defn guess-title [hier] (get-in hier [0 :text]))

(defn add-attr [tag attr va]
  (if (nil? va) tag
    (assoc-in tag [1 attr] va)))

(defn find-in
  ([struct key] (find-in struct key []))
  ([struct key acc]
   (when-let [gis-o (and (coll? struct) (get-in struct acc))]
     (when-let [gis (if (coll? gis-o) gis-o (= gis-o key))]
       (if (= gis-o key) acc
         (first (filter (complement nil?) (map #(->> % (conj acc) (find-in struct key))
                                               (if (vector? gis)
                                                 (range (count gis))
                                                 (keys gis))))))))))

(defn merge-kids
  ([tag where kids] (update-in tag where #(apply concat (butlast %) [kids])))
  ([tag kids] (apply concat tag [kids])))

(defn hier->hic
  ([hier text->hic clas] (map-indexed (fn [idx _] (hier->hic hier text->hic clas [idx])) (range (count hier))))
  ([hier text->hic clas lv]
   (-> (text->hic hier lv)
       (add-attr :class (clas lv))
       ((fn [tag]
         (apply merge-kids (concat [tag]
                                   (let [loc (find-in tag :deepest)]
                                     (println "Dbg " loc)
                                     (if (nil? loc) [] [(butlast loc)]))
                                   [(map-indexed (fn [idx va] (hier->hic hier
                                                                         text->hic
                                                                         clas
                                                                         (concat lv [:children idx])))
                                                 (:children hier))])))))))

(defn deduce-navbar-ul [hier]
  (let [sections-loc-start (if (<= 1 (count hier)) [0 :children] [])
        locs (map-indexed (fn [idx _] (conj sections-loc-start idx))
                          (get-in hier sections-loc-start))]
    (reduce (fn [state loc] (let [id (str "section" (:idx state))]
                              {:idx (inc (:idx state))
                               :hier (assoc-in (:hier state)
                                               (concat loc [:id]) id)
                               :navbar (conj (:navbar state)
                                             [:li [:a.page-scroll {:href (str "#" id)}
                                                                  (get-in state (concat [:hier] loc [:text]))]])
                               :sections (:sections state)}))
            {:idx 0 :hier hier
             :sections sections-loc-start
             :navbar [:ul.nav.navbar-nav.navbar-right]} locs)))

(defn make-navbar [hier]
  (update-in (deduce-navbar-ul hier) [:navbar]
             (fn [ul]
               [:nav#mainNav.navbar.navbar-default.navbar-fixed-top
                [:div.container-fluid
                 [:div.navbar-header
                  (vec (concat [:button.navbar-toggle.collapsed {:type "button" :data-toggle "collapse" :data-target "#navbar-div"}]
                               [[:span.sr-only "Navigation"]]
                               (repeat 3 [:span.icon-bar])))
                  [:a.navbar-brand.page-scroll {:href "#"} "My Interactive Resume"]]
                 [:div#navbar-div.collapse.navbar-collapse ul]]])))

(defn gen-main-body [hier]
  (hier->hic hier (fn [h lv] (if (> 2 (count lv)) [:section {:id (get-in h (conj lv :id))}
                                                   [:h1 (get-in h (conj lv :text))]
                                                   [:hr]
                                                   [:div.container :deepest]]
                                   [:p (get-in h (conj lv :text)) :deepest]))
             (fn [lv] nil)))

(defn hier->site [hier]
  (let [nv (make-navbar hier)
        body (->> nv :sections (concat [:hier]) (get-in nv) gen-main-body)
        head (concat [:head [:title (guess-title hier)]]
                     (hic-p/include-js (:js bootstrap-include))
                     (hic-p/include-js (:jquery bootstrap-include))
                     (hic-p/include-css (:css bootstrap-include)))]
    (println body)
    (println [:html head [:body (concat (:navbar nv) body)]])
    (hic-p/html5 [:html head [:body (concat (:navbar nv) body)]])))
