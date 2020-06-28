(ns hyperfiddle.core                                        ; TODO rethink this file now that saas is dead
  (:require

    ; Userland API
    [hyperfiddle.api]
    #?(:cljs [hyperfiddle.ui])

    ; UI helpers
    #?(:cljs [contrib.loader])
    #?(:cljs [contrib.ui])

    hyperfiddle.foundation                                  ; load topnav fiddle defs
    ))

; WARNING:
; Do not import from within hyperfiddle namespaces.
; Import this only from main to avoid circular dependencies. Just list them all here.
