(ns hyperfiddle.blocks.topnav
  (:require #?(:cljs [hyperfiddle.ui :as ui])
            [hyperfiddle.api :as hf]
            [hyperfiddle.blocks.account :as account]
            [hyperfiddle.blocks.view-mode-selector :refer [ViewModeSelector]]
            [hyperfiddle.def :as hf-def]))

(hf-def/fiddle ::topnav
               :query
               '[:in $users
                 :find
                 (pull $users ?user [:user/name])
                 .
                 :where
                 [(ground hyperfiddle.api/*subject*) ?user-id]
                 [$users ?user :user/user-id ?user-id]]
               :links
               {":hyperfiddle.blocks.topnav/topnav" [[~::account/account]]}

               :renderer (hyperfiddle.blocks.topnav/Topnav val ctx props)

               :css "")

#?(:cljs
   (defn Topnav [_ ctx _]
     [:nav {:role  :navigation
            :style {:padding         "0.25rem 1rem"
                    :display         :flex
                    :align-items     :center
                    :justify-content :space-between}}
      [ViewModeSelector]
      (when (-> (hf/domain (:runtime ctx)) (hf/database "$users"))
        (if (hyperfiddle.api/subject ctx)
          (let [{:keys [user/name]} (deref (:hypercrud.browser/result ctx))]
            [ui/link :hyperfiddle.blocks.account/account ctx (str "👤" (or name "Account")) {:style {:text-transform :capitalize}}])
          [:a {:href (hyperfiddle.ide/stateless-login-url ctx)} "👤 Sign in"]))]))
