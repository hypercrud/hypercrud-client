Project status, 2021 May: closed beta, repo not available, public release eta 2021

# Hyperfiddle: CRUD apps as a function

Hyperfiddle is a Clojure/Script library for building rich CRUD apps out of nothing but functions. Hyperfiddle expresses sophisticated CRUD systems as **simply functions that compose, produce values, and run at the REPL**. This is made possible by our network-aware Clojure dialect that fully abstracts I/O effects at the programming language layer, uniting frontend and backend into one hollistic asynchronous program. There is no divide between queries and views, query and view compose directly. The hyperfiddle macro will partition your AST across the client/server distributed system. There is no client/server dichotemy from the programmer's perspective. All IO and effects are managed.

![](https://pbs.twimg.com/media/EqarQ0xXIAMWj7R?format=jpg&name=medium) 
*Figure: Today, Hyperfiddle is about transactional CRUD apps like this*

* **High level**: Hyperfiddle makes it painless and easy to create sophisticated CRUD UIs with deep and dynamic data dependencies, without needing to think about data loading.
* **Managed I/O**: Hyperfiddle leverages a functional effect system to efficiently supervise queries, network, rendering, dom, and any other physical effects. 
* **Wicked fast**: We believe it to be possible to achieve IO that is perfect and physically ideal. Real-time, streaming, incremental, differental, no over-rendering. #functionalprogramming
* **Spec-driven UI**: We interpret clojure.spec annotations on your functions to drive semantic user interface widgets, out of the box with zero coding or configuration.
* **Robust abstraction**: Functional programming makes your code more predictable, simpler to understand, and easier to debug. Cleanly separated layers let a technical business user make high level changes, while a software engineer can implement physical effects as needed.
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
         '[hyperfiddle.api :as hf]
         '[hyperfiddle.rcf :refer [tests]]
         '[missionary.core :as m])

; async database effects (supervised by missionary)
(defn submissions [needle] (m/sp (d/q ... (m/?! hf/*$*) needle)))
(defn shirt-sizes [gender needle] (m/sp (d/q ... (m/?! hf/*$*) gender needle)))

; app as a function
(defn main [needle]  
  (hf/q
    [{(submissions needle)
      [:db/id
       (:dustingetz/email ::hf/render (render-email id email))
       {:dustingetz/gender 
        [:db/ident]}
       {(:dustingetz/shirt-size 
           ::hf/options (shirt-sizes dustingetz/gender _))
        [:db/ident]}]}]))

(defn render-email [id email]
  (hf/dag
    ~@[:div.email
       {:class (if ~@(:dustingetz/is-admin (d/entity hf/*$* id)) "admin")} 
       email]))
```
```Clojure
(tests
  (def !needle (atom "alice"))
  (def app (hf/dag (main (m/watch !needle)))) ; hf/dag is a compiler
  (def out (run app))
  (time @out)
  ; 100ms initial render
  := {'(submissions needle) [...]}
  
  (reset! !needle "bob")
  ; 1ms incremental maintenance
  (time @out) 
  := {'(submissions needle) [...]}
  )
```

Here you can see Hyperfiddle's layers:

1. `hf/q`, a declarative notation for CRUD app structure and effect orchestration, coded in `hf/dag`;
2. `hf/dag`, a network-aware Clojure dialect* (embedded in host Clojure, like core.async) that compiles to differential dataflow netcode, uniting frontend and backend into one program/AST;
3. [`missionary`](https://github.com/leonoel/missionary), a clojure/script functional effect system with primitives for referentially transparent IO actions, continuous signals and discrete streams, all with backpressure, cancellation, monadic structure and full dynamic control flow. Missionary also is a Clojure dialect and is inspired by core.async.

*By "Clojure dialect" we mean like core.async go-blocks, which feel like Clojure but compile differently and have slightly different meaning.

# UI drivers

If you provide specs for your effects we will use them to reflect a free UI. We provide two UI drivers out of the box, dom and codemirror. 

```Clojure
(s/fdef submissions :args (s/cat ...) :ret (s/coll-of hf/ref?))
(s/fdef shirt-sizes :args (s/cat :gender hf/ref? :needle string?))

(tests
   (def !needle (atom "alice"))
   (def app (hf/dag (binding [hf/render hf/dom #_hf/codemirror]
                       (main (m/watch !needle)))))
   (def out (run app))
   (time @out) := {'(submissions needle) [...]}
  )
```

dom driver: (todo)

codemirror driver (we think of this as a "graph spreadsheet"): (todo)

Custom field renderer that arbitrarily queries the database from the view (!): (todo)

# How it works

Our insight is that CRUD apps have enough structural similarity between client and server that we can trace IO on the server (dataflow effects), stream the trace over network and replay those same effects on the client, thereby synchronizing the internal process state of the client/server peers across network. The shared structure forms a plan that enables the peers to anticipate each others’ needs. The plan is encoded as an AST and coordinated in advance, thus enabling any amount of dynamic decision making to be replicated and maintained in parallel without any communication, beyond the stream of traced dataflow effects, which are broadcasted as their outcome becomes known. The end result is a composable end-user programming model that approximates that of a single-process system without network.

Program ast:
```Clojure
(hf/dag 
  (reconcile-to-dom! js/document.body 
    [:div (cljs.pprint/pprint-str
            ; transfer from server to client
            ~@(d/entity hf/*db* e))]))
```

`hyperfiddle.viz` namespace will, given an async AST, produce the isomorphic flowcharts:

Program dag: (todo)

Process trace: (todo)

* CRUD apps form a dataflow signal "DAG" (database -> service -> network -> view -> dom)
* Both incremental rendering and incremental query can be expressed as a dag 
* (render and query have the same structure from a FP perspective, different only in "place")
* The dag is async and can hop network
* Same dag spans frontend and backend
* The dag is implicit from a corresponding lisp AST (this is pretty much a "free monad")
* The `hf/dag` macro compiles this programming language into an intermediate language
* At runtime a tracing interpreter for the intermediate language is reified
* The runtime streams the execution trace into a channel (websocket)
* There is an instruction and syntax to transfer control flow to the remote peer
* When a remote peer receives control flow transfer, it already has from the trace a snapshot of the dag "heap" (incremental memory of the computation history) and therefore can resume in midflight
* Websocket latency is down to like 8ms (120fps) so network transfer can be faster than a React.js render frame ... networks are now comparable to disk

# References
