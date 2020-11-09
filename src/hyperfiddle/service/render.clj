(ns hyperfiddle.service.render
  (:require [bidi.bidi :as bidi]
            [contrib.data :as data :refer [update-existing]]
            [contrib.reactive :as r]
            [hypercrud.transit :as hc-t]
            [hyperfiddle.api :as hf]
            [hyperfiddle.config :as config]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.runtime-impl :as runtime-impl]
            [hyperfiddle.service.auth :as auth]
            [hyperfiddle.service.routes :as routes]
            [hyperfiddle.state :as state]
            [hyperfiddle.ui.loading :as loading]
            [promesa.core :as p]
            [taoensso.timbre :as timbre]))

(defn omit-secure-keys [config]
  (-> config
      (select-keys config/SECURE-CLIENT-KEYS)
      (update :databases data/for-each select-keys config/SECURE-CLIENT-DATABASES-KEYS)
      (update-existing :auth0 select-keys config/SECURE-CLIENT-AUTH-KEYS)))

(deftype RT [io state-atom]
  hf/State
  (state [rt] state-atom)

  hf/HF-Runtime
  (io [rt] io)
  (request [rt pid request] (runtime-impl/hydrate-impl rt pid request))
  (set-route [rt pid route] (runtime-impl/set-route rt pid route false))
  (set-route [rt pid route force-hydrate] (runtime-impl/set-route rt pid route force-hydrate)))

(declare main-html)

(defn render [{:keys [config io route] :as context}]
  (let [{:keys [user-id email name]} (auth/subject context)
        state                        {::runtime/user-id       user-id
                                      ::runtime/user-email    email
                                      ::runtime/user-name     name
                                      ::runtime/auto-transact (data/map-values
                                                               (fn [hf-db]
                                                                 (if-some [auto-tx (:database/auto-transact hf-db)]
                                                                   auto-tx
                                                                   false))
                                                               (:databases config))
                                      ::runtime/partitions    {hf/root-pid {:route       route
                                                                            :is-branched true}}}
        rt                           (->RT io (r/atom (state/initialize state)))]
    (-> (runtime/bootstrap-data rt hf/root-pid runtime/LEVEL-NONE)
        (p/catch #(do (timbre/error %)
                      (or (:hyperfiddle.io/http-status-code (ex-data %))) 500))
        (p/then (fn [http-status-code]
                  {:http-status-code (or http-status-code 200)
                   :component        (main-html (assoc context :rt rt))})))))

(defn- main-html [{:keys [config rt]}]
  (let [{:keys [git/describe]} config]
    [:html {:lang "en"}
     [:head
      [:title "Hyperfiddle" #_(str (:hyperfiddle.route/fiddle (runtime/get-route rt hf/root-pid)))]
      [:link {:rel "icon" :type "image/png" :href "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAEGWlDQ1BrQ0dDb2xvclNwYWNlR2VuZXJpY1JHQgAAOI2NVV1oHFUUPrtzZyMkzlNsNIV0qD8NJQ2TVjShtLp/3d02bpZJNtoi6GT27s6Yyc44M7v9oU9FUHwx6psUxL+3gCAo9Q/bPrQvlQol2tQgKD60+INQ6Ium65k7M5lpurHeZe58853vnnvuuWfvBei5qliWkRQBFpquLRcy4nOHj4g9K5CEh6AXBqFXUR0rXalMAjZPC3e1W99Dwntf2dXd/p+tt0YdFSBxH2Kz5qgLiI8B8KdVy3YBevqRHz/qWh72Yui3MUDEL3q44WPXw3M+fo1pZuQs4tOIBVVTaoiXEI/MxfhGDPsxsNZfoE1q66ro5aJim3XdoLFw72H+n23BaIXzbcOnz5mfPoTvYVz7KzUl5+FRxEuqkp9G/Ajia219thzg25abkRE/BpDc3pqvphHvRFys2weqvp+krbWKIX7nhDbzLOItiM8358pTwdirqpPFnMF2xLc1WvLyOwTAibpbmvHHcvttU57y5+XqNZrLe3lE/Pq8eUj2fXKfOe3pfOjzhJYtB/yll5SDFcSDiH+hRkH25+L+sdxKEAMZahrlSX8ukqMOWy/jXW2m6M9LDBc31B9LFuv6gVKg/0Szi3KAr1kGq1GMjU/aLbnq6/lRxc4XfJ98hTargX++DbMJBSiYMIe9Ck1YAxFkKEAG3xbYaKmDDgYyFK0UGYpfoWYXG+fAPPI6tJnNwb7ClP7IyF+D+bjOtCpkhz6CFrIa/I6sFtNl8auFXGMTP34sNwI/JhkgEtmDz14ySfaRcTIBInmKPE32kxyyE2Tv+thKbEVePDfW/byMM1Kmm0XdObS7oGD/MypMXFPXrCwOtoYjyyn7BV29/MZfsVzpLDdRtuIZnbpXzvlf+ev8MvYr/Gqk4H/kV/G3csdazLuyTMPsbFhzd1UabQbjFvDRmcWJxR3zcfHkVw9GfpbJmeev9F08WW8uDkaslwX6avlWGU6NRKz0g/SHtCy9J30o/ca9zX3Kfc19zn3BXQKRO8ud477hLnAfc1/G9mrzGlrfexZ5GLdn6ZZrrEohI2wVHhZywjbhUWEy8icMCGNCUdiBlq3r+xafL549HQ5jH+an+1y+LlYBifuxAvRN/lVVVOlwlCkdVm9NOL5BE4wkQ2SMlDZU97hX86EilU/lUmkQUztTE6mx1EEPh7OmdqBtAvv8HdWpbrJS6tJj3n0CWdM6busNzRV3S9KTYhqvNiqWmuroiKgYhshMjmhTh9ptWhsF7970j/SbMrsPE1suR5z7DMC+P/Hs+y7ijrQAlhyAgccjbhjPygfeBTjzhNqy28EdkUh8C+DU9+z2v/oyeH791OncxHOs5y2AtTc7nb/f73TWPkD/qwBnjX8BoJ98VQNcC+8AAACGSURBVFgJY/wTuPg/wwACpgG0G2z1qANGQ2A0BAY8BFgoKQdY1scyIusnp1Ab8BAYdQDVQuBf24H9yOmBWDYjOQmHWMOJUUe1ECDGMmxqRh1AXkHEzszAwMtxAB6kD985MHCzw7mkMMhyAMuKKIpLQJgjR9PAgIfAaEE04FEw6oDREBgNAQDZwhcK3lLErwAAAABJRU5ErkJggg==\n"}]
      [:link {:rel "stylesheet" :href (bidi/path-for routes/routes ::hf/static-resource :build describe :resource-name "browser.css")}]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
      [:meta {:charset "UTF-8"}]
      [:script {:id "build" :type "text/plain"} describe]
      #_[:script {:id "tracking-fullstory" :type "text/javascript"} (slurp (io/resource "templates/fullstory.js"))]]

     [:body
      [:div {:id "root"}
       (loading/page)]
      (concat
       (map (fn [[id val]] [:script {:id id :type "application/transit-json"} val])
            {;;"params" (hc-t/encode (:CLIENT_PARAMS env))
             "hf-config" (-> config omit-secure-keys hc-t/encode)
             "state"  (hc-t/encode @(hf/state rt))})
       [[:script {:id "preamble" :src (bidi/path-for routes/routes ::hf/static-resource :build describe :resource-name "browser.js")}]
        [:script {:id "shared" :src (bidi/path-for routes/routes ::hf/static-resource :build describe :resource-name "shared.js")}]
        [:script {:id "main" :src (bidi/path-for routes/routes ::hf/static-resource :build describe :resource-name "main.js")}]])]]))
