(ns fourclojure.easy)

;; 24. Sum It All Up
;;
;; Difficulty:  Easy
;; Topics:	seqs
;; Write a function which returns the sum of a sequence of numbers.

(defn r24 [xs] (apply + xs))

(= (r24 [1 2 3]) 6)
(= (r24 (list 0 -2 5 5)) 8)
(= (r24 #{4 2 1}) 7)
(= (r24 '(0 0 -1)) -1)
(= (r24 '(1 10 3)) 14)


;; 25. Find the odd numbers
;;
;; Difficulty:	Easy
;; Topics:	seqs
;; Write a function which returns only the odd numbers from a sequence.

(defn r25 [xs]
  (filter #(= (mod % 2) 1) xs))

;; And obviously there is an odd? function, but it's just not called odd, but odd? because its a predicate...
(defn r25 [xs] (filter odd? xs))

(= (r25 #{1 2 3 4 5}) '(1 3 5))
(= (r25 [4 2 1 6]) '(1))
(= (r25 [2 2 4 6]) '())
(= (r25 [1 1 1 3]) '(1 1 1 3))


;; 23. Reverse a Sequence
;;
;; Difficulty:	Easy
;; Topics:	seqs core-functions
;; Write a function which reverses a sequence.
;;
;; Special Restrictions
;; reverse
;; rseq

(defn r23 [xs]
  (reduce #(conj %1 %2) '() xs))

;; A bit smarter, that anonymous function is the same as the function itself:
(defn r23 [xs]
  (reduce conj '() xs))

;; And lastly, with the knowledge of core:
(def r23 (partial into '()))

(= (r23 [1 2 3 4 5]) [5 4 3 2 1])
(= (r23 (sorted-set 5 7 2 7)) '(7 5 2))
(= (r23 [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])


;; 27. Palindrome Detector
;;
;; Difficulty:	Easy
;; Topics:	seqs
;; Write a function which returns true if the given sequence is a palindrome.
;; Hint: "racecar" does not equal '(\r \a \c \e \c \a \r)

(defn r27 [xs]
  (let [rs (reverse xs)
        mixed (partition 2 (interleave xs rs))]
    (every? (partial apply =) mixed)))

;; Smarter - Clojure can do equality in values, so (seq the xs for the string):
(defn r27 [xs] (= (seq xs) (reverse xs)))

(false? (r27 '(1 2 3 4 5)))
(true? (r27 "racecar"))
(true? (r27 [:foo :bar :foo]))
(true? (r27 '(1 1 3 3 1 1)))
(false? (r27 '(:a :b :c)))


;; 26. Fibonacci Sequence
;;
;; Difficulty:	Easy
;; Topics:	Fibonacci seqs
;; Write a function which returns the first X fibonacci numbers.

(defn r26 [n]
  (loop [i 2, n-1 1, n-2 1, fib [1 1]]
    (if (= i n)
      fib
      (let [res (+ n-1 n-2)]
        (recur (inc i) res n-1 (conj fib res))))))

(= (r26 3) '(1 1 2))
(= (r26 6) '(1 1 2 3 5 8))
(= (r26 8) '(1 1 2 3 5 8 13 21))


;; 38. Maximum value
;;
;; Difficulty:	Easy
;; Topics:	core-functions
;; Write a function which takes a variable number of parameters and returns the maximum value.
;; Special Restrictions
;; max
;; max-key

(defn r38 [& xs] (reduce #(if (> %1 %2) %1 %2) xs))

;; And the smart one:
(defn r38 [& xs] (last (sort xs)))

(= (r38 1 8 3 4) 8)
(= (r38 30 20) 30)
(= (r38 45 67 11) 67)


;; 29. Get the Caps
;;
;; Difficulty:	Easy
;; Topics:	strings
;; Write a function which takes a string and returns a new string containing only the capital letters.

(defn r29 [s]
  (let [upper-case? #(<= 65 (int %) 90)]
    (apply str (filter upper-case? (seq s)))))

;; The one before doesn't work in 4clojure, so with regex:
(defn r29 [s] (apply str (re-seq #"[A-Z]+" s)))

(= (r29 "HeLlO, WoRlD!") "HLOWRD")
(empty? (r29 "nothing"))
(= (r29 "$#A(*&987Zf") "AZ")


;; 32. Difficulty:	Easy
;; Topics:	seqs
;; Write a function which duplicates each element of a sequence.

(defn r32 [xs] (interleave xs xs))

(= (r32 [1 2 3]) '(1 1 2 2 3 3))
(= (r32 [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
(= (r32 [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
(= (r32 [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))


;; 48. Intro to some
;;
;; Difficulty:	Easy
;; Topics:
;; The some function takes a predicate function and a collection.
;; It returns the first logical true value of (predicate x) where x is an item in the collection.

(def r48 6)

(= r48 (some #{2 7 6} [5 6 7 8]))
(= r48 (some #(if (even? %) %) [5 6 7 8]))


;; 34. Implement range
;;
;; Difficulty:	Easy
;; Topics:	seqs core-functions
;; Write a function which creates a list of all integers in a given range.

(defn r34 [start end]
  (loop [i start, xs []]
    (if (= i end)
      xs
      (recur (inc i) (conj xs i)))))

;; Cool short solution:
(defn r34 [start end]
  (take (- end start) (iterate inc start)))

(= (r34 1 4) '(1 2 3))
(= (r34 -2 2) '(-2 -1 0 1))
(= (r34 5 8) '(5 6 7))


;; Flatten a Sequence
;;
;; Difficulty:	Easy
;; Topics:	seqs core-functions
;; Write a function which flattens a sequence.
;; Special Restrictions
;; flatten

(defn r28 [xss]
  (loop [xs xss rs []]
    (let [f (first xs)]
      (cond
       (empty? xs) rs
       (sequential? f) (recur (concat f (rest xs)) rs)
       :else (recur (rest xs) (conj rs f))))))

;; Short solution using map-cat recursion
(defn r28 [x]
  (if (coll? x)
    (mapcat r28 x)
    [x]))

(= (r28 '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
(= (r28 ["a" ["b"] "c"]) '("a" "b" "c"))
(= (r28 '((((:a))))) '(:a))


;; 42. Factorial Fun
;;
;; Difficulty:	Easy
;; Topics:	math
;; Write a function which calculates factorials.

(defn r42 [n] (apply * (range 1 (inc n))))

(= (r42 1) 1)
(= (r42 3) 6)
(= (r42 5) 120)
(= (r42 8) 40320)


;; 39. Interleave Two Seqs
;;
;; Difficulty:	Easy
;; Topics:	seqs core-functions
;; Write a function which takes two sequences and returns the first item from each, then the second item from each, then the third, etc.
;; Special Restrictions
;; interleave

(def r39 (partial mapcat vector))

(= (r39 [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
(= (r39 [1 2] [3 4 5 6]) '(1 3 2 4))
(= (r39 [1 2 3 4] [5]) [1 5])
(= (r39 [30 20] [25 15]) [30 25 20 15])


;; 30. Compress a Sequence
;;
;; Difficulty:	Easy
;; Topics:	seqs
;; Write a function which removes consecutive duplicates from a sequence.

(defn r30 [xs]
  (reduce (fn [acc x]
            (if (= x (last acc)) acc (conj acc x)))
          [] xs))

;; Smart solution using partition-by
(defn r30 [xs] (map first (partition-by identity xs)))

(= (apply str (r30 "Leeeeeerrroyyy")) "Leroy")
(= (r30 [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
(= (r30 [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))


;; 47. Contain Yourself
;;
;; Difficulty:	Easy
;; Topics:
;; The contains? function checks if a KEY is present in a given collection.
;; This often leads beginner clojurians to use it incorrectly with numerically
;; indexed collections like vectors and lists.

(def r47 4)

(contains? #{4 5 6} r47)
(contains? [1 1 1 1 1] r47)
(contains? {4 :a 2 :b} r47)
(not (contains? '(1 2 4) r47)) ; This one is exception in clj 1.5.1 but in 4clojure works...


;; 45. Intro to Iterate
;;
;; Difficulty:	Easy
;; Topics:	seqs
;; The iterate function can be used to produce an infinite lazy sequence.

(def r45 '(1 4 7 10 13))

(= r45 (take 5 (iterate #(+ 3 %) 1)))


;; 33. Replicate a Sequence
;;
;; Difficulty:	Easy
;; Topics:	seqs
;; Write a function which replicates each element of a sequence a variable number of times.

(defn r33 [xs n] (mapcat #(repeat n %) xs))

;; Another cool solution using for comprehension
(defn r33 [xs n] (for [x xs, i (range n)] x))

(= (r33 [1 2 3] 2) '(1 1 2 2 3 3))
(= (r33 [:a :b] 4) '(:a :a :a :a :b :b :b :b))
(= (r33 [4 5 6] 1) '(4 5 6))
(= (r33 [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
(= (r33 [44 33] 2) [44 44 33 33])


;; 40. Interpose a Seq
;;
;; Difficulty:	Easy
;; Topics:	seqs core-functions
;; Write a function which separates the items of a sequence by an arbitrary value.
;; Special Restrictions
;; interpose

(defn r40 [x xs] (butlast (mapcat vector xs (repeat x))))

;; Another cool one with interleave:
(defn r40 [x xs] (butlast (interleave xs (repeat x))))

(= (r40 0 [1 2 3]) [1 0 2 0 3])
(= (apply str (r40 ", " ["one" "two" "three"])) "one, two, three")
(= (r40 :z [:a :b :c :d]) [:a :z :b :z :c :z :d])


;; 31. Pack a Sequence
;;
;; Difficulty:	Easy
;; Topics:	seqs
;; Write a function which packs consecutive duplicates into sub-lists.

(defn r31 [xs] (partition-by identity xs))

(= (r31 [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
(= (r31 [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
(= (r31 [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))


;; 41. Drop Every Nth Item
;;
;; Difficulty:	Easy
;; Topics:	seqs
;; Write a function which drops every Nth item from a sequence.

(defn r41 [xs n] (flatten (partition (dec n) n [] xs)))

;; And now knowing about partition-all
(defn r41 [xs n] (flatten (partition-all (dec n) n xs)))

(= (r41 [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
(= (r41 [:a :b :c :d :e :f] 2) [:a :c :e])
(= (r41 [1 2 3 4 5 6] 4) [1 2 3 5 6])


;; 52. Intro to Destructuring
;;
;; Difficulty:	Easy
;; Topics:	destructuring
;; Let bindings and function parameter lists support destructuring.

(= [2 4] (let [[a b c d e f g] (range)] [c e]))


;; 49. Split a sequence
;;
;; Difficulty:	Easy
;; Topics:	seqs core-functions
;; Write a function which will split a sequence into two parts.
;; Special Restrictions
;; split-at

(defn r49 [n xs] [(take n xs) (drop n xs)])

;; Pro functional solution with juxt
(def r49 (juxt take drop))

(= (r49 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
(= (r49 1 [:a :b :c :d]) [[:a] [:b :c :d]])
(= (r49 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])


;; 51. Advanced Destructuring
;;
;; Difficulty:	Easy
;; Topics:	destructuring
;; Here is an example of some more sophisticated destructuring.

(= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] [1 2 3 4 5]] [a b c d]))


;; 83. A Half-Truth
;;
;; Difficulty:	Easy
;; Topics:
;; Write a function which takes a variable number of booleans.
;; Your function should return true if some of the parameters are true,
;; but not all of the parameters are true.
;; Otherwise your function should return false.

(defn r83 [& xs] (boolean (and (some false? xs) (some true? xs))))

;; Pro: = gives us all trues or all falses, and not= mixed trues and falses,
;; which is what we want...
(def r83 not=)

(= false (r83 false false))
(= true (r83 true false))
(= false (r83 true))
(= true (r83 false true false))
(= false (r83 true true true))
(= true (r83 true true true false))


;; 61. Map Construction
;;
;; Difficulty:	Easy
;; Topics:	core-functions
;; Write a function which takes a vector of keys and a vector of values and constructs a map from them.
;; Special Restrictions
;; zipmap

(defn r61 [ks xs] (apply hash-map (mapcat vector ks xs)))

;; Another interesting
(defn r61 [ks xs] (into {} (map vector ks xs)))

;; Cool one
(defn r61 [ks xs] (apply hash-map (interleave ks xs)))

(= (r61 [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
(= (r61 [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"})
(= (r61 [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})


;; 166. Comparisons
;;
;; Difficulty:	Easy
;; Topics:
;; For any orderable data type it's possible to derive all of the basic comparison
;; operations (<, ≤, =, ≠, ≥, and >) from a single operation (any operator but = or ≠ will work).
;; Write a function that takes three arguments, a less than operator for the data and two items to compare.
;; The function should return a keyword describing the relationship between the two items.
;; The keywords for the relationship between x and y are as follows:
;;     x = y → :eq
;;     x > y → :gt
;;     x < y → :lt

(defn r166 [f x y]
  (cond
   (f x y) :lt
   (f y x) :gt
   :else :eq))

(= :gt (r166 < 5 1))
(= :eq (r166 (fn [x y] (< (count x) (count y))) "pear" "plum"))
(= :lt (r166 (fn [x y] (< (mod x 5) (mod y 5))) 21 3))
(= :gt (r166 > 0 2))


;; 66. Greatest Common Divisor
;;
;; Difficulty:	Easy
;; Topics:
;; Given two integers, write a function which returns the greatest common divisor.

(defn r66 [a b]
  (cond
   (> a b) (recur (- a b) b)
   (< a b) (recur a (- b a))
   :else a))

(= (r66 2 4) 2)
(= (r66 10 5) 5)
(= (r66 5 7) 1)
(= (r66 1023 858) 33)


;; 81. Set Intersection
;;
;; Difficulty:	Easy
;; Topics:	set-theory
;; Write a function which returns the intersection of two sets.
;; The intersection is the sub-set of items that each set has in common.
;; Special Restrictions
;; intersection

(defn r81 [s1 s2]
  (clojure.set/difference (clojure.set/union s1 s2)
                          (clojure.set/difference s1 s2)
                          (clojure.set/difference s2 s1)))

;; And now the smart one
(def r81 (comp set filter))

(= (r81 #{0 1 2 3} #{2 3 4 5}) #{2 3})
(= (r81 #{0 1 2} #{3 4 5}) #{})
(= (r81 #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})


;; 62. Re-implement Iterate
;;
;; Difficulty:	Easy
;; Topics:	seqs core-functions
;; Given a side-effect free function f and an initial value x write a function
;; which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc.
;; Special Restrictions
;; iterate

(defn r62 [f x]
  (cons x (lazy-seq (r62 f (f x)))))

(= (take 5 (r62 #(* 2 %) 1)) [1 2 4 8 16])
(= (take 100 (r62 inc 0)) (take 100 (range)))
(= (take 9 (r62 #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))


;; 107. Simple closures
;;
;; Difficulty:	Easy Topics:	higher-order-functions math
;; Lexical scope and first-class functions are two of the most basic building
;; blocks of a functional language like Clojure. When you combine the two
;; together, you get something very powerful called lexical closures. With these,
;; you can exercise a great deal of control over the lifetime of your local
;; bindings, saving their values for use later, long after the code you're running
;; now has finished.
;; It can be hard to follow in the abstract, so let's build a simple closure.
;; Given a positive integer n, return a function (f x) which computes x^n. Observe
;; that the effect of this is to preserve the value of n for use outside the scope
;; in which it is defined.

(defn r107 [n] (fn [x] (int (Math/pow x n))))

(= 256 ((r107 2) 16),
       ((r107 8) 2))
(= [1 8 27 64] (map (r107 3) [1 2 3 4]))
(= [1 2 4 8 16] (map #((r107 %) 2) [0 1 2 3 4]))


;; 99. Product Digits
;;
;; Difficulty:	Easy Topics:	math seqs
;; Write a function which multiplies two numbers and returns the result as
;; a sequence of its digits.

(defn r99 [x y] (map #(Integer. (str %)) (str (* x y))))

(= (r99 1 1) [1])
(= (r99 99 9) [8 9 1])
(= (r99 999 99) [9 8 9 0 1])


;; 90. Cartesian Product
;;
;; Difficulty:	Easy Topics:	set-theory
;; Write a function which calculates the Cartesian product of two sets.
;;

(defn r90 [s1 s2] (set (for [x s1 y s2] [x y])))

(= (r90 #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
   #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
     ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
     ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]})

(= (r90 #{1 2 3} #{4 5})
   #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})

(= 300 (count (r90 (into #{} (range 10))
                  (into #{} (range 30)))))


;; 63. Group a Sequence
;;
;; Difficulty:	Easy Topics:	core-functions
;; Given a function f and a sequence s, write a function which returns a map. The
;; keys should be the values of f applied to each item in s. The value at each key
;; should be a vector of corresponding items in the order they appear in s.
;; Special Restrictions group-by

(defn r63 [f xs]
  (let [group (fn [m x]
                (let [k (f x)
                      s (get m k [])]
                  (assoc m k (conj s x))))]
    (reduce group {} xs)))

;; Very cool solution
(defn r63 [f xs]
  (apply merge-with concat
         (for [x xs] {(f x) [x]})))

(r63 #(> % 5) [1 3 6 8])

(= (r63 #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]})

(= (r63 #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
   {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]})

(= (r63 count [[1] [1 2] [3] [1 2 3] [2 3]])
   {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})


;; 122. Read a binary number
;;
;; Difficulty:	Easy Topics:
;; Convert a binary number, provided in the form of a string, to its numerical
;; value.
;;

;;   0   1   1
;; 2^2 2^1 2^0

(defn r122 [bstr]
  (let [pows (iterate #(* 2 %) 1)
        char->int #(Integer. (str %))
        rdigits (reverse (map char->int bstr))]
    (reduce #(+ %1 (apply * %2)) 0 (zipmap pows rdigits))))

;; If you know Java...:
(def r122 #(Integer/parseInt % 2))
;; In javascript it would be
;; (def r122 #(js/parseInt % 2))

(= 0     (r122 "0"))
(= 7     (r122 "111"))
(= 8     (r122 "1000"))
(= 9     (r122 "1001"))
(= 255   (r122 "11111111"))
(= 1365  (r122 "10101010101"))
(= 65535 (r122 "1111111111111111"))


;; 88. Symmetric Difference
;;
;; Difficulty:	Easy Topics:	set-theory
;; Write a function which returns the symmetric difference of two sets. The
;; symmetric difference is the set of items belonging to one but not both of the
;; two sets.
;;

(defn r88 [s1 s2] (clojure.set/union (clojure.set/difference s1 s2)
                                     (clojure.set/difference s2 s1)))

;; Collections style:
(defn r88 [s1 s2] (into (set (remove s1 s2)) (remove s2 s1)))

(= (r88 #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})

(= (r88 #{:a :b :c} #{}) #{:a :b :c})

(= (r88 #{} #{4 5 6}) #{4 5 6})

(= (r88 #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})


;; 143. dot product
;;
;; Difficulty:	Easy Topics:	seqs math
;; Create a function that computes the dot product of two sequences. You may
;; assume that the vectors will have the same length.
;;

(defn r143 [xs ys] (reduce + (map * xs ys)))

(= 0 (r143 [0 1 0] [1 0 0]))

(= 3 (r143 [1 1 1] [1 1 1]))

(= 32 (r143 [1 2 3] [4 5 6]))

(= 256 (r143 [2 5 6] [100 10 1]))


;; 126. Through the Looking Class
;;
;; Difficulty:	Easy Topics:	fun brain-teaser
;; Enter a value which satisfies the following:
;;

(def r126 Class)

(->> r126 class = and)

(let [x r126]
  (and (= (class x) x) x))


;; 135. Infix Calculator
;;
;; Difficulty:	Easy Topics:	higher-order-functions math
;; Your friend Joe is always whining about Lisps using the prefix notation for
;; math. Show him how you could easily write a function that does math using the
;; infix notation. Is your favorite language that flexible, Joe? Write a function
;; that accepts a variable length mathematical expression consisting of numbers
;; and the operations +, -, *, and /. Assume a simple calculator that does not do
;; precedence and instead just calculates left to right.
;;

(defn r135
  ([x f y] (f x y))
  ([x f y & xs] (apply r135 (f x y) xs)))

(r135 2 + 5)

(= 7  (r135 2 + 5))

(= 42 (r135 38 + 48 - 2 / 2))

(= 8  (r135 10 / 2 - 1 * 2))

(= 72 (r135 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))


;; 97. Pascal's Triangle
;;
;; Difficulty:	Easy Topics:
;; Pascal's triangle is a triangle of numbers computed using the following rules:
;; - The first row is 1.
;; - Each successive row is computed by adding together adjacent numbers in the
;;   row above, and adding a 1 to the beginning and end of the row.
;; Write a function which returns the nth row of Pascal's Triangle.
;;

;;
;; 1        [1]
;; 2       [1 1]
;; 3      [1 2 1]
;; 4     [1 3 3 1]
;; 5    [1 4 6 4 1]
;; 6  [1 5 10 10 5 1]  pascal[row, pos] = pascal[row-1, pos-1] + pascal[row-1, pos]
;; 7 [1 6 15 20 15 6 1]  pascal[row, pos] = pascal[row-1, pos-1] + pascal[row-1, pos]

(defn r97 [n]
  (letfn [(pascal [n p]
            (cond
             (or (= p 0) (= p (dec n))) 1
             :else (+ (pascal (dec n) (dec p)) (pascal (dec n) p))))]
  (for [x (range 0 n)] (pascal n x))))

(= (r97 1) [1])

(= (map r97 (range 1 6))
   [     [1]
        [1 1]
       [1 2 1]
      [1 3 3 1]
     [1 4 6 4 1]])

(= (r97 11)
   [1 10 45 120 210 252 210 120 45 10 1])


;; 118. Re-implement Map
;;
;; Difficulty:	Easy Topics:	core-seqs
;; Map is one of the core elements of a functional programming language. Given
;; a function f and an input sequence s, return a lazy sequence of (f x) for each
;; element x in s.
;;
;; Special Restrictions map map-indexed mapcat for

(defn r118 [f [x & xs :as s]]
  (if (empty? s)
    nil
    (cons (f x) (lazy-seq (r118 f xs)))))

;; Using when to make it more concise
(defn r118 [f [x & xs :as s]]
  (when s
    (cons (f x) (lazy-seq (r118 f xs)))))


(= [3 4 5 6 7]
   (r118 inc [2 3 4 5 6]))

(= (repeat 10 nil)
   (r118 (fn [_] nil) (range 10)))

(= [1000000 1000001]
   (->> (r118 inc (range))
        (drop (dec 1000000))
        (take 2)))


;; 95. To Tree, or not to Tree
;;
;; Difficulty:	Easy Topics:	trees
;; Write a predicate which checks whether or not a given sequence represents
;; a binary tree. Each node in the tree must have a value, a left child, and
;; a right child.
;;

(defn r95 [n]
  (or (nil? n)
      (and (sequential? n)
           (= 3 (count n))
           (r95 (second n))
           (r95 (nth n 2)))))

(= (r95 '(:a (:b nil nil) nil))
   true)

(= (r95 '(:a (:b nil nil)))
   false)

(= (r95 [1 nil [2 [3 nil nil] [4 nil nil]]])
   true)

(= (r95 [1 [2 nil nil] [3 nil nil] [4 nil nil]])
   false)

(= (r95 [1 [2 [3 [4 nil nil] nil] nil] nil])
   true)

(= (r95 [1 [2 [3 [4 false nil] nil] nil] nil])
   false)

(= (r95 '(:a nil ()))
   false)


;; 120. Sum of square of digits
;;
;; Difficulty:	Easy Topics:	math
;; Write a function which takes a collection of integers as an argument. Return
;; the count of how many elements are smaller than the sum of their squared
;; component digits. For example: 10 is larger than 1 squared plus 0 squared;
;; whereas 15 is smaller than 1 squared plus 5 squared.
;;

(defn r120 [xs]
  (let [char->int #(Integer. (str %))
        square #(* % %)
        digits-square-sum #(reduce + (map (comp square char->int) (str %)))]
    (reduce (fn [c n] (if (< n (digits-square-sum n)) (inc c) c))
            0
            xs)))


(= 8 (r120 (range 10)))

(= 19 (r120 (range 30)))

(= 50 (r120 (range 100)))

(= 50 (r120 (range 1000)))


;; 157. Indexing Sequences
;;
;; Difficulty:	Easy Topics:	seqs
;; Transform a sequence into a sequence of pairs containing the original elements
;; along with their index.
;;

(defn r157 [xs] (partition 2 (interleave xs (range))))

;; Another using map:
(defn r157 [xs] (map list xs (range)))


(= (r157 [:a :b :c]) [[:a 0] [:b 1] [:c 2]])

(= (r157 [0 1 3]) '((0 0) (1 1) (3 2)))

(= (r157 [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]])


;; 128. Recognize Playing Cards
;;
;; Difficulty:	Easy Topics:	strings game
;; A standard American deck of playing cards has four suits - spades, hearts,
;; diamonds, and clubs - and thirteen cards in each suit. Two is the lowest rank,
;; followed by other integers up to ten; then the jack, queen, king, and ace.
;; It's convenient for humans to represent these cards as suit/rank pairs, such as
;; H5 or DQ: the heart five and diamond queen respectively. But these forms are
;; not convenient for programmers, so to write a card game you need some way to
;; parse an input string into meaningful components. For purposes of determining
;; rank, we will define the cards to be valued from 0 (the two) to 12 (the ace)
;; Write a function which converts (for example) the string "SJ" into a map of
;; {:suit :spade, :rank 9}. A ten will always be represented with the single
;; character "T", rather than the two characters "10".


(defn r128 [[s r & card]]
  (let [suits {\D :diamond \H :heart \C :club \S :spade}
        ranks (zipmap "23456789TJQKA" (range))]
    {:suit (suits s) :rank (ranks r)}))

(= {:suit :diamond :rank 10} (r128 "DQ"))

(= {:suit :heart :rank 3} (r128 "H5"))

(= {:suit :club :rank 12} (r128 "CA"))

(= (range 13) (map (comp :rank r128 str)
                   '[S2 S3 S4 S5 S6 S7
                     S8 S9 ST SJ SQ SK SA]))


;; 100. Least Common Multiple
;;
;; Difficulty:	Easy Topics:	math
;; Write a function which calculates the least common multiple. Your function
;; should accept a variable number of positive integers or ratios.
;;

(defn r100 [& nums]
  (let [gcd (fn gcd [a b]
              (cond
               (> a b) (recur (- a b) b)
               (< a b) (recur a (- b a))
               :else a))
        lcm (fn [a b] (/ (* a b) (gcd a b)))]
    (reduce lcm nums)))

(== (r100 2 3) 6)

(== (r100 5 3 7) 105)

(== (r100 1/3 2/5) 2)

(== (r100 3/4 1/6) 3/2)

(== (r100 7 5/7 2 3/5) 210)


;; 147. Pascal's Trapezoid
;;
;; Difficulty:	Easy Topics:	seqs
;; Write a function that, for any given input vector of numbers, returns an
;; infinite lazy sequence of vectors, where each next one is constructed from the
;; previous following the rules used in Pascal's Triangle. For example, for [3 1 2],
;; the next row is [3 4 3 2].
;;

(defn r147 [row]
  (let [next-row (fn [xs]
                   (concat [(first xs)]
                           (map #(apply +' %) (partition 2 1 xs))
                           [(last xs)]))
        ys (next-row row)]
    (cons row (lazy-seq (r147 ys)))))

;; Smartpants solution (mapping the sum of [0 ...xs] and [xs... 0])
(defn r147 [xs] (iterate #(map +' (cons 0 %) (concat % [0])) xs))

(= (second (r147 [2 3 2])) [2 5 5 2])

(= (take 5 (r147 [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]])

(= (take 2 (r147 [3 1 2])) [[3 1 2] [3 4 3 2]])

(= (take 100 (r147 [2 4 2])) (rest (take 101 (r147 [2 2]))))


;; 96. Beauty is Symmetry
;;
;; Difficulty:	Easy Topics:	trees
;; Let us define a binary tree as "symmetric" if the left half of the tree is the
;; mirror image of the right half of the tree. Write a predicate to determine
;; whether or not a given binary tree is symmetric. (see To Tree, or not to Tree
;; for a reminder on the tree representation we're using).

(defn r96 [tree]
  (= tree ((fn flip [[v l r :as ft]]
             (when ft [v (flip r) (flip l)]))
           tree)))

(= (r96 '(:a (:b nil nil) (:b nil nil))) true)

(= (r96 '(:a (:b nil nil) nil)) false)

(= (r96 '(:a (:b nil nil) (:c nil nil))) false)

(= (r96 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
   true)

(= (r96 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
   false)

(= (r96 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [6 nil nil] nil]] nil]])
   false)


;; 173. Intro to Destructuring 2
;;
;; Difficulty:	Easy Topics:	Destructuring
;; Sequential destructuring allows you to bind symbols to parts of sequential
;; things (vectors, lists, seqs, etc.): (let [bindings* ] exprs*) Complete the
;; bindings so all let-parts evaluate to 3.

(= 3
  (let [[f n] [+ (range 3)]] (apply f n))
  (let [[[f n] b] [[+ 1] 2]] (f n b))
  (let [[f n] [inc 2]] (f n)))


;; 146. Trees into tables
;;
;; Difficulty:	Easy Topics:	seqs maps
;; Because Clojure's for macro allows you to "walk" over multiple sequences in
;; a nested fashion, it is excellent for transforming all sorts of sequences. If
;; you don't want a sequence as your final output (say you want a map), you are
;; often still best-off using for, because you can produce a sequence and feed it
;; into a map, for example.
;; For this problem, your goal is to "flatten" a map of hashmaps. Each key in your
;; output map should be the "path"1 that you would have to take in the original
;; map to get to a value, so for example {1 {2 3}} should result in {[1 2] 3}. You
;; only need to flatten one level of maps: if one of the values is a map, just
;; leave it alone.
;; 1 That is, (get-in original [k1 k2]) should be the same as (get result [k1 k2])

(defn r146 [mx]
  (into {} (for [[kx my] mx [ky vy] my] [[kx ky] vy])))

(= (r146 '{a {p 1, q 2}
         b {m 3, n 4}})
   '{[a p] 1, [a q] 2
     [b m] 3, [b n] 4})

(= (r146 '{[1] {a b c d}
         [2] {q r s t u v w x}})
   '{[[1] a] b, [[1] c] d,
     [[2] q] r, [[2] s] t,
     [[2] u] v, [[2] w] x})

(= (r146 '{m {1 [a b c] 3 nil}})
   '{[m 1] [a b c], [m 3] nil})


;; 153. Pairwise Disjoint Sets
;;
;; Difficulty:	Easy Topics:	set-theory
;; Given a set of sets, create a function which returns true if no two of those
;; sets have any elements in common1 and false otherwise. Some of the test cases
;; are a bit tricky, so pay a little more attention to them.
;; 1Such sets are usually called pairwise disjoint or mutually disjoint.
;;

(defn r153 [ss]
  (let [md? (comp empty? clojure.set/intersection)
        xss (for [x ss y ss :when (not (identical? x y))] [x y])]
    (reduce (fn [a [s1 s2]] (and a (md? s1 s2))) true xss)))

;; Smart solution (concat sets and uses distinct? to see that all the elements are different)
(defn r153 [ss] (apply distinct? (apply concat ss)))

(r153 #{#{'a 'b} #{'c 'd 'e}})

(= (r153 #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
   true)

(= (r153 #{#{:a :b :c :d :e}
           #{:a :b :c :d}
           #{:a :b :c}
           #{:a :b}
           #{:a}})
   false)

(= (r153 #{#{[1 2 3] [4 5]}
           #{[1 2] [3 4 5]}
           #{[1] [2] 3 4 5}
           #{1 2 [3 4] [5]}})
   true)

(= (r153 #{#{'a 'b}
           #{'c 'd 'e}
           #{'f 'g 'h 'i}
           #{''a ''c ''f}})
   true)

(= (r153 #{#{'(:x :y :z) '(:x :y) '(:z) '()}
           #{#{:x :y :z} #{:x :y} #{:z} #{}}
           #{'[:x :y :z] [:x :y] [:z] [] {}}})
   false)

(= (r153 #{#{(= "true") false}
           #{:yes :no}
           #{(class 1) 0}
           #{(symbol "true") 'false}
           #{(keyword "yes") ::no}
           #{(class '1) (int \0)}})
   false)

(= (r153 #{#{distinct?}
           #{#(-> %) #(-> %)}
           #{#(-> %) #(-> %) #(-> %)}
           #{#(-> %) #(-> %) #(-> %)}})
   true)

(= (r153 #{#{(#(-> *)) + (quote mapcat) #_ nil}
           #{'+ '* mapcat (comment mapcat)}
           #{(do) set contains? nil?}
           #{, , , #_, , empty?}})
   false)
