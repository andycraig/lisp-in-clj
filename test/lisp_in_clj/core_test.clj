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
    (is (= (my-eval 1 my-env)
           1))))

(deftest test-my-eval
  (testing "Test my-eval"
    (is (= (my-eval ["add" 1 2] my-env)
           3))))

(deftest test-my-eval-if
  (testing "Test my-eval if"
    (is (= (my-eval ["if" "true" 1 2] my-env)
           1))))


