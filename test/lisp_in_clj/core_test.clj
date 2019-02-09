(ns lisp-in-clj.core-test
  (:require [clojure.test :refer :all]
            [lisp-in-clj.core :refer :all]))

(deftest test-tokenise
  (testing "Test tokenise"
    (is (= ["(" "a" ")"] (tokenise "(a)")))))

(deftest test-parse
  (testing "Test parse"
    (is (= '("add" ("add" 1 2) 3)
           (parse ["(" "add" "(" "add" "1" "2" ")" "3" ")"])))))

(deftest test-my-eval-number
  (testing "Test parse number"
    (is (= (my-eval 1 (atom my-env))
           1))))

(deftest test-my-eval
  (testing "Test my-eval"
    (is (= (my-eval ["add" 1 2] (atom my-env))
           3))))

(deftest test-my-eval-if
  (testing "Test my-eval if"
    (is (= (my-eval ["if" ["eq" 3 3] 1 2] (atom my-env))
           1))))

(deftest test-my-eval-fn
  (testing "Test my-eval of function"
    (let [exp [["lambda" ["x" "y"] ["add" "x" "y"]] 1 2]]
      (is (= (my-eval exp (atom my-env))
             3)))))