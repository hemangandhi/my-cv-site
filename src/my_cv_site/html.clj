(ns my-cv-site.html
  (:require [hiccup.util :as hic-u]
            [hiccup.page :as hic-p]))

(def bootstrap-include
  {:css "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
   :jquery "https://code.jquery.com/jquery-3.1.1.min.js"
   :js "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"})

(defn guess-title [hier] (get-in hier [0 :text]))

(defn add-attr [tag attr va]
  (if (nil? va) tag
    (assoc-in tag [1 attr] va)))

(defn hier->hic [hier writer]
  (if (map? hier)
    (writer hier hier->hic)
    (if (not (empty? hier))
      (map #(writer % hier->hic) hier)
      nil)))

(defn deduce-navbar-ul [hier]
  (let [sections-loc-start (if (>= 1 (count hier)) [0 :children] [])
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

(defn hier->str [hier]
  (-> hier :text clojure.string/join hic-u/to-str))

(defn gen-main-body [hier]
  (hier->hic hier (letfn [(w [h h->h]
                            (if (nil? (:id h))
                              [:p (concat (hier->str h) (h->h (:children h) w))]
                              [:section {:id (:id h)} [:h1 (hier->str h)] [:hr] [:div.container (h->h (:children h) w)]]))]
                           w)))

(defn hier->site [hier]
  (let [nv (make-navbar hier)
        body (->> nv :sections (concat [:hier]) (get-in nv) gen-main-body)
        head (concat [:head [:title (guess-title hier)]]
                     (hic-p/include-js (:jquery bootstrap-include))
                     (hic-p/include-js (:js bootstrap-include))
                     (hic-p/include-css (:css bootstrap-include)))
        full-thing [:html head (concat [:body] [(:navbar nv)] body)]]
    (hic-p/html5 full-thing)))
