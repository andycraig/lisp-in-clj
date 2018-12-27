(ns lisp-in-clj.core
  (:require [clojure.test :refer [is]]))

(defn tokenise
  ;; Takes a string.
  [s]
  (-> s
      (clojure.string/replace "(" "( ")
      (clojure.string/replace ")" " )")
      (clojure.string/split #" ")))

(defn parse
  ;; Takes a vector of tokens.
  ;; Example:
  ;; ["(" "a" "(" "b" "c" ")" "d" ")"]
  ([col] (parse col 0 (empty col)))
  ([col i acc]
   (if (< i (count col))
     (let [c (nth col i)]
       (cond
         (= "(" c) (let [[n new-i] (parse col (inc i) (empty []))]
                     (parse col new-i (concat acc [n])))
         (= ")" c) [acc (inc i)]
         :else (parse col (inc i) (concat acc [c]))))
     acc)))

(def my-env
  {:add #(+ %1 %2)
   :sub #(- %1 %2)})

(defn my-eval
  ;; Takes a parsed vector.
  [x env]
  (cond
    (number? x) x
    (string? x) (get env x) ;; Symbol
    ;;TODO Conditional
    ;;TODO Definition
    :else (let
              [f (get env (keyword (first x)))
               args (map #(my-eval % env) (rest x))]
            (apply f args))))

(my-eval ["add" 1 2] my-env)
(my-eval ["add" ["add" 1 2] 3] my-env)
(parse ["(" "a" ")"])
(parse ["(" 1 ")"])
(parse ["(" "a" "(" "b" "(" "c" ")" ")" ")"])
(parse ["(" "a" "b" "(" "c" "d" ")" "e" ")"])

(concat 
 (concat (empty ["a"]) ["b"]) ["c"])

(conj ["a" "b"] ["c"])
(is (= ["a" "b"] (tokenise "a b")))
(is (= ["(" "a" "b" "c" ")"] (tokenise "(a b c)")))

(defn if-let-check
  [x]
  (if-let [y (nth (range 3) x)]
    y
    "oh no"))

(if-let-check 1)
(if-let-check 10000)
