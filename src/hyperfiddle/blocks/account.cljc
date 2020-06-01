(ns hyperfiddle.blocks.account
  (:require #?(:cljs [hyperfiddle.ui :as ui])
            [hyperfiddle.def :as hf-def]))

(hf-def/fiddle ::account
  :query
  '[:in $users
    :find
    (pull $users ?user
      [:db/id
       :user/name
       :user/email
       :user/last-seen
       :user/sub
       :user/picture
       :user/user-id
       *])
    .
    :where
    [(ground hyperfiddle.api/*subject*) ?user-id]
    [$users ?user :user/user-id ?user-id]]

  :renderer (hyperfiddle.blocks.account/Account val ctx props)

  :code
  (defmethod hyperfiddle.api/render #{:user/user-id
                                      :hyperfiddle.blocks/account}
    [ctx props]
    [:div.hyperfiddle-input-group
     [:div.input
      (pr-str (hypercrud.browser.context/data ctx))]])

  :css "
    img.avatar { border: 1px solid lightgray; border-radius: 50%; width: 80px; }
    img.avatar { float: left; margin-top: 1rem; margin-right: 1rem; }
    .-hyperfiddle-ide-user-settings h3 { margin-top: 1rem; }
   ")


#?(:cljs
   (defn Account [_ ctx _]
     (let [user @(:hypercrud.browser/result ctx)]
       [:div.container-fluid props
        [:div.p
         [ui/img (:user/picture user) ctx {:class "avatar"}]]

        [:h3 "Hello, " (:user/name user) "!"]

        [:div.p
         [:form {:action "/logout" :method "post"}
          (str "Last login was " (or (some-> (:user/last-seen user) .toLocaleDateString) "–") ".")

          [:button.btn.btn-link {:type  "submit"
                                 :style {:display        "inline"
                                         :padding        "0"
                                         :margin-left    ".25em"
                                         :font-size      "inherit"
                                         :vertical-align "inherit"}}
           "logout"]]
         ]

        [:div.p [:div {:style {:margin-bottom "1em"}}]]
        [ui/field [:user/user-id] ctx]])))
