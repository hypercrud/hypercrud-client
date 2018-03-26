(ns hyperfiddle.ide
  (:require [bidi.bidi :as bidi]
            [hypercrud.browser.auto-anchor :refer [system-link?]]
    #?(:cljs [hypercrud.browser.browser-ui :as browser-ui])
            [hypercrud.browser.core :as browser]
            [hypercrud.browser.routing :as routing]
            [hypercrud.browser.router :as router]
            [hypercrud.client.core :as hc]
    #?(:cljs [hypercrud.react.react-fragment :refer [react-fragment]])
    #?(:cljs [hypercrud.ui.navigate-cmp :as navigate-cmp])
    #?(:cljs [hypercrud.ui.stale :as stale])
            [hypercrud.util.core :refer [unwrap]]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.util.string :refer [safe-read-edn-string]]
            [hyperfiddle.foundation :as foundation]
            [hyperfiddle.io.hydrate-requests :refer [hydrate-one!]]
            [hyperfiddle.runtime :as runtime]
            [promesa.core :as p]
    #?(:cljs [reagent.core :as reagent])
            [taoensso.timbre :as timbre]

    ; pull in public ui deps
    ; todo these hc.ui.* should be reduced to one require e.g. [hypercrud.ui]
    #?(:cljs [hypercrud.react.react-fragment])
            [hypercrud.ui.auto-control]
    #?(:cljs [hypercrud.ui.result])

    ; pull in the entire ide app for reference from user-land
            [hyperfiddle.ide.actions]
            [hyperfiddle.ide.fiddles.domain]
            [hyperfiddle.ide.fiddles.fiddle-links.bindings]
    #?(:cljs [hyperfiddle.ide.fiddles.fiddle-links.renderer])
            [hyperfiddle.ide.fiddles.topnav]
    #?(:cljs [hyperfiddle.ide.fiddles.user-dashboard])
            [hyperfiddle.ide.util]))

(defn domain [rt hyperfiddle-hostname hostname]
  (let [domain-basis (if-let [global-basis @(runtime/state rt [::runtime/global-basis])]
                       (:domain global-basis)
                       (->> @(runtime/state rt [::runtime/partitions])
                            (some (fn [[_ partition]]
                                    (->> (:local-basis partition)
                                         (filter (fn [[k _]] (= foundation/domain-uri k)))
                                         seq)))))
        stage nil
        hf-domain-name (foundation/hostname->hf-domain-name hostname hyperfiddle-hostname)
        request (foundation/domain-request hf-domain-name rt)]
    (-> (hydrate-one! rt (into {} domain-basis) stage request)
        (p/then (fn [domain]
                  (if (nil? (:db/id domain))
                    ; terminate when domain not found
                    (throw (ex-info "Domain does not exist" {:hyperfiddle.io/http-status-code 404
                                                             :domain-name hf-domain-name}))
                    domain))))))

(defn ide-route [route]
  {:fiddle-id :hyperfiddle/topnav
   :request-params [#entity["$" (:fiddle-id route)]]})

(let [always-user (atom :user)
      constantly-nil (constantly nil)]
  ; ide is overloaded, these ide-context functions are exclusive to (top)
  ; despite being in the namespace (hyperfiddle.ide) which encompasses the union of target/user (bottom) and ide (top)
  (defn- *-ide-context [ctx ide-domain]
    {:pre [ide-domain]}
    (-> ctx
        (assoc :hypercrud.browser/page-on-click constantly-nil ; disable alt-nav up top
               :hypercrud.ui/display-mode always-user
               :target-domain (:hypercrud.browser/domain ctx) ; todo rename :target-domain to :hyperfiddle.ide/target-domain
               :user-profile @(runtime/state (:peer ctx) [:user-profile]))
        (update :hypercrud.browser/domain
                (fn [domain]
                  (-> (foundation/process-domain ide-domain)
                      (assoc-in [:domain/environment "$"] (:domain/fiddle-repo domain))))))))

(defn leaf-ide-context [ctx ide-domain]
  ; ide leaf-context does not have enough information to set hyperfiddle.ide/target-route
  ; this means ide popovers CANNOT access it
  (*-ide-context ctx ide-domain))

(defn page-ide-context [ctx ide-domain target-route]
  {:pre [target-route]}
  (-> (assoc ctx
        ::runtime/branch-aux {::foo "ide"}
        ; hyperfiddle.ide/target-route is ONLY available to inlined IDE (no deferred popovers)
        :target-route target-route)                         ; todo rename :target-route to :hyperfiddle.ide/target-route
      (*-ide-context ide-domain)))

(defn activate-ide? [x]
  ; To userland this "www" constant would require more complicated rules
  ; for mapping a hostname to a domain-ident
  (let [should-be-active (and (not (foundation/alias? x))
                              (not (= "www" x)))
        ; WWW needs a way to activate this. For now we can mirror the domain on www2
        explicitly-active false]
    (or explicitly-active should-be-active)))

(defn- *-target-context [ctx route]
  (assoc ctx
    :hypercrud.browser/page-on-click (let [branch-aux {:hyperfiddle.ide/foo "page"}]
                                       #?(:cljs (reactive/partial browser-ui/page-on-click (:peer ctx) nil branch-aux)))
    :hypercrud.ui/display-mode (runtime/state (:peer ctx) [:display-mode])
    :ide-active (activate-ide? (foundation/hostname->hf-domain-name ctx))
    :user-profile @(runtime/state (:peer ctx) [:user-profile])

    ; these target values only exists to allow the topnav to render in the bottom/user
    ; IF we MUST to support that, this should probably be done higher up for both ide and user at the same time
    ; and SHOULD ONLY be applied for ide within ide (other user fns don't get access to these values)
    :target-domain (:hypercrud.browser/domain ctx)          ; todo rename :target-domain to :hyperfiddle.ide/target-domain
    :target-route route                                     ; todo rename :target-route to :hyperfiddle.ide/target-route
    ))

(defn leaf-target-context [ctx route]
  (*-target-context ctx route))

(defn page-target-context [ctx route]
  (-> (assoc ctx ::runtime/branch-aux {::foo "user"})
      (*-target-context route)))

(defn ->bidi-consistency-wrapper [{:keys [handler route-params] :as ?r}]
  ; Bidi's interface is inconsistent and makes you understand two ways to identify a route
  ; "bidi/match" return val syntax, is the bad one
  ; Canonicalize on the "bidi/path-for" syntax
  (if ?r (apply conj [handler] (mapcat identity route-params))))

(defn bidi->hf [[handler & ?route-params :as ?r]]
  (if ?r
    (merge {:fiddle-id (router/-decode-fiddle-id handler)}  ; Bidi router names by fiddle/ident (not db/ident) so we wrap into lookup ref
           (let [ps (->> ?route-params                      ; bidi gives us alternating k/v
                         (partition-all 2)
                         (map vec)
                         sort                               ; order by keys for hyperfiddle, router should use kw or int
                         (mapv second)                      ; drop keys; hyperfiddle params are associative by index
                         )]
             (if (seq ps)
               {:request-params ps})))))

(defn bidi-match->path-for "adapt bidi's inconsistent interface" [[h & ps :as ?r]]
  (if ?r {:handler h :route-params ps}))

(defn abc []
  (map (comp keyword str) "abcdefghijklmnopqrstuvwxyz")     ; this version works in clojurescript
  #_(->> (range) (map (comp keyword str char #(+ % (int \a))))))

(defn ->bidi [{:keys [fiddle-id request-params] :as ?r}]
  (assert (not (system-link? fiddle-id)) "bidi router doesn't handle sys links")
  ; this is going to generate param names of 0, 1, ... which maybe doesn't work for all routes
  ; we would need to disallow bidi keywords for this to be valid. Can bidi use ints? I think not :(
  (if ?r (apply conj [fiddle-id] (mapcat vector (abc) request-params))))

(defn route-decode [rt s]
  {:pre [(string? s)]}
  (let [domain @(runtime/state rt [:hyperfiddle.runtime/domain])
        home-route (some-> domain :domain/home-route safe-read-edn-string unwrap)
        router (some-> domain :domain/router safe-read-edn-string unwrap)]
    (case s
      "/" (if router (bidi->hf home-route) home-route)          ; compat
      (or (if (= "/_/" (subs s 0 3)) (routing/decode (subs s 2) #_"include leading /"))
          (some-> router (bidi/match-route s) ->bidi-consistency-wrapper bidi->hf)
          (routing/decode s)))))

(defn route-encode [rt route]
  (let [domain @(runtime/state rt [:hyperfiddle.runtime/domain])
        home-route (some-> domain :domain/home-route safe-read-edn-string unwrap)
        router (some-> domain :domain/router safe-read-edn-string unwrap)]
    (or
      (if (system-link? (:fiddle-id route)) (str "/_" (routing/encode route)))
      (if (= (bidi->hf home-route) route) "/")
      (if router (apply bidi/path-for router (->bidi route)))
      (routing/encode route))))

(defn local-basis [global-basis route ctx]
  ;local-basis-ide and local-basis-user
  (let [{:keys [ide user]} global-basis
        basis (case (get-in ctx [::runtime/branch-aux ::foo])
                "page" (concat ide user)
                "ide" (concat ide user)
                "user" user)
        basis (sort basis)]                                 ; Userland api-fn should filter irrelevant routes
    (timbre/debug (pr-str basis))
    #_(determine-local-basis (hydrate-route route ...))
    basis))

; Reactive pattern obfuscates params
(defn api [route ctx]
  {:pre [route (not (string? route))]
   :post [#_(seq %)]}
  ; We can actually call into the foundation a second time here to get the ide-domain
  (let [ide-domain-q (foundation/domain-request "hyperfiddle" (:peer ctx))
        ide-domain (hc/hydrate-api (:peer ctx) (:branch ctx) ide-domain-q)]
    (case (get-in ctx [::runtime/branch-aux ::foo])
      "page" (concat [ide-domain-q]
                     (if route
                       (browser/request-from-route route (page-target-context ctx route)))
                     (if (and (activate-ide? (foundation/hostname->hf-domain-name ctx)) ide-domain)
                       (browser/request-from-route (ide-route route) (page-ide-context ctx ide-domain route))))
      "ide" (concat [ide-domain-q]
                    (if ide-domain
                      (browser/request-from-route route (leaf-ide-context ctx ide-domain))))
      "user" (if route
               (browser/request-from-route route (leaf-target-context ctx route))))))

#?(:cljs
   (defn view-page [?route ctx]
     (let [ide-active (activate-ide? (foundation/hostname->hf-domain-name ctx))
           ctx (assoc ctx :navigate-cmp (reagent/partial navigate-cmp/navigate-cmp (reagent/partial runtime/encode-route (:peer ctx))))]
       (react-fragment
         :view-page
         (when ide-active
           [stale/loading
            (stale/can-be-loading? ctx)
            @(hc/hydrate (:peer ctx) (:branch ctx) (foundation/domain-request "hyperfiddle" (:peer ctx)))
            (fn [e]
              ; todo inline staging area?
              [:div
               [:h3 "Fatal error"]
               [:pre (pr-str e)]])
            (fn [ide-domain]
              (let [ctx (-> (page-ide-context ctx ide-domain ?route)
                            (assoc :hypercrud.ui/ui-error browser-ui/ui-error-inline))]
                [browser/ui-from-route (ide-route ?route) ctx "topnav hidden-print"]))])
         (if ?route
           (let [class (str "hyperfiddle-user" (if ide-active " hyperfiddle-ide-user" ""))]
             ; This is different than foo=user because it is special css at root attach point
             [browser/ui-from-route ?route (page-target-context ctx ?route) class]))))))

#?(:cljs
   (defn view [route ctx]                                   ; pass most as ref for reactions
     (let [ide-domain (hc/hydrate-api (:peer ctx) (:branch ctx) (foundation/domain-request "hyperfiddle" (:peer ctx)))]
       (case (get-in ctx [::runtime/branch-aux ::foo])
         "page" (view-page route ctx)                       ; component, seq-component or nil
         ; On SSR side this is only ever called as "page", but it could be differently (e.g. turbolinks)
         ; On Browser side, also only ever called as "page", but it could be configured differently (client side render the ide, server render userland...?)
         "ide" [browser/ui-from-route route (leaf-ide-context ctx ide-domain)]
         "user" [browser/ui-from-route route (leaf-target-context ctx route)]))))
