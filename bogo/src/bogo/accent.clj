(ns bogo.accent
  (:require [clojure.set]
            [bogo.util :refer :all]))

(def keyword-accent
  (zipmap [:none :grave :acute :hook :tilde :dot] (range 6)))

(def accent-keyword
  (clojure.set/map-invert keyword-accent))

(defn get-vowel-family
  "Returns the family if the vowel is in one of the vowel families, nil
  otherwise."
  [chr]
  (first (filter #(in? chr %1) vowels)))

(defn add-accent-char
  "Adds an accent to a given char. Also remove any accent if the given accent is
  Accent.NONE"
  [chr accent]
  (get (get-vowel-family chr) (keyword-accent accent) chr))

(defn add-accent-string
  "Adds accent to a given string. Also removes any existing accent if accent is
  :none."
  [string accent]
  (let [{:keys [head vowel] _last :last} (separate string)]
    (apply str (map #(apply str %)
      [head
      (case (count vowel)
        ; single vowel
        1 [(add-accent-char (first vowel) accent)]
        ; For double vowel ending in [\y \i \u \o] with no last consonant, add
        ; the accent to the first vowel (cay, bai, bau, bao,...) else to the
        ; second one (qua, khoe, khoan,..).
        2 (if (and (in? (last vowel) [\y \i \u \o]) (empty? _last))
            [(add-accent-char (first vowel) accent) (last vowel)]
            [(first vowel) (add-accent-char (last vowel) accent)])
        ; Triple? Add to second-to-last if ending in [\o \i \u] (khoeo, khoai)
        ; else to last (chuyen, tuyet).
        3 (if (in? (last vowel) [\o \i \u])
          [(first vowel) (add-accent-char (nth vowel 1) accent) (last vowel)]
          (concat (butlast vowel) [(add-accent-char (last vowel) accent)]))
        vowel)
      _last]))))

(defn get-accent-char
  "Get the accent of a single char. The result can be one of
  [:none :grave :acute :hook :tilde :dot]."
  [chr]
  (get accent-keyword
    (.lastIndexOf
      (or (get-vowel-family chr) "")
      (str chr)) :none))

(defn get-last-accent-string
  "Get the accent of the last (right-most) character with accent.
  See get-accent-char for possible result values."
  [string]
  (first (drop-while #{:none} (reverse (map get-accent-char string)))))
