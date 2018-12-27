(ns lisp-in-clj.core
  (:require [clojure.test :refer [is]]))

(defn tokenise
  ;; Takes a string.
  [s]
  (-> s
      (clojure.string/replace "(" "( ")
      (clojure.string/replace ")" " )")
      (clojure.string/split #" ")))

(defn process-token
  [s]
  (if (number? (read-string s))
    (read-string s)
    s))

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
         :else (parse col (inc i) (concat acc [(process-token c)]))))
     (first acc)))) ;;TODO Make use of first unnecessary


(def my-env
  {"add" #(+ %1 %2)
   "sub" #(- %1 %2)
   "car" #(first %) ;;TODO Fix
   "cdr" #(rest %) ;;TODO Fix
   "cons" #(conj %1 %2) ;;TODO Fix
   })

(defn my-eval
  ;; Takes a parsed vector.
  [x env]
  (cond
    (number? x) x
    (= "true" x) true
    (= "if" (first x)) (let [pred (second x)
                        conseq (nth x 2)
                        alt (nth x 3)]
                 (if (my-eval pred my-env)
                   (my-eval conseq my-env)
                   (my-eval alt my-env)))
    (= "define" (first x)) (assoc my-env (second x) (nth x 2))
    :else (let
              [f (get env (first x))
               args (map #(my-eval % env) (rest x))]
            (apply f args))))
