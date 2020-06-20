(ns hyperfiddle.core                                        ; TODO rethink this file now that saas is dead
  (:require

    ; Userland API
    [hyperfiddle.api]
    #?(:cljs [hyperfiddle.ui])

    ; UI helpers
    #?(:cljs [contrib.loader])
    #?(:cljs [contrib.ui])

    ; These things can hardcode hyperfiddle.ui, like userland
    #?(:cljs [hyperfiddle.ui.markdown-extensions])

    hyperfiddle.foundation                                  ; load topnav fiddle defs
    ))

; Circular dependency hack, do not alter! should not be dynamic
#?(:cljs (set! hyperfiddle.ui/markdown hyperfiddle.ui.markdown-extensions/markdown))

; WARNING:
; Do not import from within hyperfiddle namespaces.
; Import this only from main to avoid circular dependencies. Just list them all here.
