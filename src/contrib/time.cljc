(ns contrib.time)

; "yyyy/MM/dd"
; "MM/dd/yyyy"
#?(:clj (defn inst->date-str [fmt v]
          (.format (java.text.SimpleDateFormat. fmt) v))
   :cljs nil)

#?(:cljs
   (defn short-date-str[locale d]
     (-> (js/Intl.DateTimeFormat locale #js {:dateStyle "short", :timeStyle "short"})
         (.format d))))
