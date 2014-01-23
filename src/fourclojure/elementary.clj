(ns fourclojure.elementary)

;; 1. Nothing but the Truth
(= true true)


;; 145. For the win
;;
;; Difficulty:	Elementary
;; Topics:	core-functions seqs
;; Clojure's for macro is a tremendously versatile mechanism for producing a
;; sequence based on some other sequence(s). It can take some time to understand
;; how to use it properly, but that investment will be paid back with clear,
;; concise sequence-wrangling later. With that in mind, read over these for
;; expressions and try to see how each of them produces the same result.

;(def r145 '(1 5 9 13 17 21 25 29 33 37))

(def r145 (range 1 40 4))

(= r145 (for [x (range 40)
            :when (= 1 (rem x 4))]
        x))

(= r145 (for [x (iterate #(+ 4 %) 0)
            :let [z (inc x)]
            :while (< z 40)]
        z))

(= r145 (for [[x y] (partition 2 (range 20))]
        (+ x y)))

;; 161. Subset and Superset
;;
;; Difficulty:	Elementary
;; Topics:	set-theory
;; Set A is a subset of set B, or equivalently B is a superset of A, if A is "contained" inside B. A and B may coincide.

(def r161 #{1 2})

(clojure.set/superset? r161 #{2})
(clojure.set/subset? #{1} r161)
(clojure.set/superset? r161 #{1 2})
(clojure.set/subset? #{1 2} r161)


;; 156. Map Defaults
;;
;; Difficulty:	Elementary
;; Topics:	seqs
;; When retrieving values from a map, you can specify default values in case the key is not found:
(= 2 (:foo {:bar 0, :baz 1} 2))
;; However, what if you want the map itself to contain the default values?
;; Write a function which takes a default value and a sequence of keys and constructs a map.

;; Mine
(defn r156 [default ks]
  (apply hash-map (reduce #(conj %1 %2 default) [] ks)))

;; Mine refined
(defn r156 [default ks]
  (reduce #(assoc %1 %2 default) {} ks))

;; Cool solution austintaylor ->
(defn r156 [default ks]
  (apply hash-map (interleave ks (repeat default))))

;; Shorter solution. zipmap is just what this exercise asks for
(defn r156 [default ks]
  (zipmap ks (repeat default)))

(= (r156 0 [:a :b :c]) {:a 0 :b 0 :c 0})
(= (r156 "x" [1 2 3]) {1 "x" 2 "x" 3 "x"})
(= (r156 [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]})


