(ns bogo.tone-test
  (:require [clojure.test :refer :all]
            [bogo.tone :refer :all]))

(deftest test-add-tone-string
  (testing "Single-char vowel."
    (is (= (add-tone-string "a" :acute)) "á")
    (is (= (add-tone-string "nhan" :acute)) "nhán")
    (is (= (add-tone-string "ngang" :acute)) "ngáng"))
  (testing "Double-char vowel."
    (is (= (add-tone-string "hoa" :grave) "hoà"))
    (is (= (add-tone-string "quy" :grave) "quỳ"))
    (is (= (add-tone-string "mươn" :dot) "mượn"))
    (is (= (add-tone-string "thuôm" :dot) "thuộm")))
  (testing "Triple-char vowel."
    (is (= (add-tone-string "thiêu" :acute) "thiếu"))
    (is (= (add-tone-string "thuyên" :grave) "thuyền"))))

(deftest test-get-tone-char
  (testing "Simple tone"
    (is (= (get-tone-char \ả) :hook))
  (testing "No tone"
    (is (= (get-tone-char \a) :none))
    (is (= (get-tone-char \e) :none)))
  (testing "Consonants"
    (is (= (get-tone-char \w) :none))
    (is (= (get-tone-char \n) :none)))))
