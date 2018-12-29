(ns lisp-in-clj.core)

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

(declare my-eval)

(defn make-lambda
  ;; Can handle:
  ;; (define inc (lambda (x) (add x 1)))
  ;; (define zero? (lambda (x) (if (eq x 0) 1 0)))
  ;; (define fac (lambda (x) (if (eq x 1) 1 (mul x (fac (sub x 1))))))
  [arg-names body env]
  (fn
    [& call-time-args]
    [body arg-names  call-time-args]
    (let [arg-pairs (interleave arg-names call-time-args)]
      (my-eval body (atom (apply assoc (cons @env arg-pairs)))))))

(defn my-eval
  ;; Takes a parsed vector.
  [x env]
  (cond
    (number? x) x
    (= "if" (first x)) (let [pred (second x)
                        conseq (nth x 2)
                        alt (nth x 3)]
                 (if (my-eval pred env)
                   (my-eval conseq env)
                   (my-eval alt env)))
    (string? x) (get @env x)
    (= "quote" (first x)) (second x)
    (= "lambda" (first x)) (make-lambda (second x) (nth x 2) env)
    (= "define" (first x)) ((constantly nil) (swap! env #(assoc % (second x) (my-eval (nth x 2) env))))
    :else (let
              ;; TODO f should be my-eval'd as well
              [f (get @env (first x))
               args (map #(my-eval % env) (rest x))]
            (apply f args))))

(defn repl
  [env-non-atom]
  (loop
      [env (atom env-non-atom)]
    (do
      (print ">>>")
      (flush)
      (let [s (clojure.string/trim (read-line))]
          (if (= "quit" s)
            (println "Goodbye!")
            (let [result (my-eval (parse (tokenise s)) env)]
              (do
                (println result)
                (recur env))))))))

(def my-env
  {
   "add" #(+ %1 %2)
   "sub" #(- %1 %2)
   "mul" #(* %1 %2)
   "eq" #(= %1 %2)
   })

(defn -main
  []
  (do
    (println "Welcome to a very simple Lisp REPL!")
    (println "I understand: if, define, lambda, quote, add, sub, mul, eq" )
    (println "Enter quit to stop.")
    (repl my-env)))
