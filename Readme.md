Project status, 2021 June: closed beta, repo not available, public release eta 2021

# Hyperfiddle: CRUD apps as a function

Hyperfiddle is a Clojure/Script library for building rich CRUD apps out of nothing but functions. Hyperfiddle expresses sophisticated CRUD systems as **simply functions that compose, produce values, and run at the REPL**. This is made possible by our network-aware Clojure dialect that fully abstracts I/O effects at the programming language layer, uniting frontend and backend into one holistic asynchronous program. There is no divide between queries and views, query and view compose directly. The hyperfiddle macro will partition your AST across the client/server distributed system. There is no client/server dichotomy from the programmer's perspective. All IO and effects are managed.

![](https://pbs.twimg.com/media/EqarQ0xXIAMWj7R?format=jpg&name=medium) 
*Figure: Today, Hyperfiddle is about transactional CRUD apps like this*

* **High level**: Hyperfiddle makes it painless and easy to create sophisticated CRUD UIs with deep and dynamic data dependencies, without needing to think about data loading.
* **Managed I/O**: Hyperfiddle leverages a functional effect system to efficiently supervise queries, network, rendering, dom, and any other physical effects. 
* **Wicked fast**: We believe it to be possible to achieve IO that is perfect and physically ideal. Real-time, streaming, incremental, differental, no over-rendering. #functionalprogramming
* **Spec-driven UI**: We interpret clojure.spec annotations on your functions to drive semantic user interface widgets, out of the box with zero coding or configuration.
* **5k LOC**: Tight enough that you can dig in, see how it works, and contribute.

Our mission is to incubate our I/O technology and then abstract a layer further, into a future-of-coding programming experience that is ultimately accessible to regular people.

# Dependencies
* Datomic peer API or other Datomic-like immutable database supporting entity semantics and speculative transactions
* NO React.js dependency, no need for it (easily integrated if you need the ecosystem)

# Hyperfiddle is a reactive pure function

Copy paste this into your Clojure REPL and see the test pass:

```bash
git clone
git submodule update --init --recursive
clj
```
```Clojure
(require '[clojure.spec.alpha :as s]
         '[datascript.core :as d]
         '[hyperfiddle.api :as hf :refer [hfql]]
         '[hyperfiddle.photon :refer [defnode]]
         '[hyperfiddle.photon-dom :as dom]
         '[missionary.core :as m])

; database effects
#?(:clj (defn submissions [needle] (d/q ... hf/*$* needle)))
#?(:clj (defn shirt-sizes [gender needle] (d/q ... hf/*$* gender needle)))
#?(:clj (defn unique-email [email] true))

; specs are used to reflect a free UI that prompts for function parameters
(s/fdef submissions :args (s/cat :needle string?) :ret sequential?)
(s/fdef genders :args (s/cat) :ret sequential?)
(s/fdef shirt-sizes :args (s/cat :gender keyword?
                                 :needle string?) :ret sequential?)

; app as a dataflow function
(defnode page [needle]      ; defnode is like defn but reactive and distributed
  (hfql
    [{(submissions needle)
      [:db/id
       (:dustingetz/email ::hf/render render-email)
       {:dustingetz/gender 
        [:db/ident]}
       {(:dustingetz/shirt-size ::hf/options (shirt-sizes dustingetz/gender .))
        [:db/ident]}]}]))

(defnode render-email [email]
  ; ~@ transfers control flow to remote peer
  ~@(dom/div {:class (if ~@(unique-email email) "valid" "invalid")} email))
```
```Clojure
(require '[hyperfiddle.rcf :refer [tests]])
(tests
  (def !needle (atom "alice"))
  (def out (hf/run-dag (page ~(m/watch !needle))))
  @out := '{(user/submissions needle) ...}
  (reset! !needle "bob")
  @out := '{(user/submissions needle) ...})
```

Here you can see Hyperfiddle's layers:

1. `hfql`, a declarative notation for CRUD app structure and effect orchestration;
2. `defnode`, a network-aware Clojure dialect* (embedded in host Clojure, like core.async) that compiles to dataflow netcode, uniting frontend and backend into one program/AST;
3. [`missionary`](https://github.com/leonoel/missionary), a clojure/script functional effect system with primitives for referentially transparent IO actions, continuous signals and discrete streams, all with backpressure, cancellation, monadic structure and full dynamic control flow. Missionary also is a Clojure dialect and is inspired by core.async.

The key idea here is this that Hyperfiddle ASTs compile into a DAG which the macro will slice and partition over client/server system, shaking out dependencies to produce optimal network effects.

# References
* [2017 Jane Street. github.com/janestreet/incremental/src/incremental_intf.ml](https://github.com/janestreet/incremental/blob/c011b4ee9c4f603dc9aa40ba580b7665220fd43d/src/incremental_intf.ml)
* [2020 Noel. Missionary – A functional effect and streaming system for clojure and clojurescript](https://github.com/leonoel/missionary)
* [2014 Minsky Jane Street. Breaking down FRP](https://blog.janestreet.com/breaking-down-frp/)
* [2016 Minsky Jane Street. Seven Implementations of Incremental](https://www.youtube.com/watch?v=G6a5G5i4gQU) a nice overview of challenges essential to incremental computing
* [2018 Rosu Jane Street. Introduction to Incr_dom: Writing Dynamic Web Apps in OCaml](https://www.youtube.com/watch?v=h_e5pPKI0K4)
* [2018 Minsky Jane Street. Data Driven UIs, Incrementally](https://www.youtube.com/watch?v=R3xX37RGJKE) Good intro to incremental map, it's implementation and application
* [2009 Elliott. Push-pull functional reactive programming](http://conal.net/papers/push-pull-frp/)
* [2006 Uustalu, Vene. The Essence of Dataflow Programming](http://cs.ioc.ee/~tarmo/papers/essence.pdf) Best overview of various category theory positioning of dataflow, needs to be updated for free monad and cofree comonad.
