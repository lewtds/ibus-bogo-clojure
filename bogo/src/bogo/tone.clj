(ns bogo.tone
  (:require [clojure.set]
            [bogo.util :refer :all]))

(def keyword-tone
  (zipmap [:none :grave :acute :hook :tilde :dot] (range 6)))

(def tone-keyword
  (clojure.set/map-invert keyword-tone))

(defn get-vowel-family
  "Returns the family if the vowel is in one of the vowel families, nil
  otherwise."
  [chr]
  (first (filter #(in? chr %1) vowels)))

(defn add-tone-char
  "Adds an tone to a given char. Also remove any tone if the given tone is
  tone.NONE"
  [chr tone]
  (get (get-vowel-family chr) (keyword-tone tone) chr))

(defn add-tone-string
  "Adds tone to a given string. Also removes any existing tone if tone is
  :none."
  [string tone]
  (let [{:keys [head vowel] _last :last} (separate string)]
    (apply str (map #(apply str %)
      [head
      (case (count vowel)
        ; single vowel
        1 [(add-tone-char (first vowel) tone)]
        ; For double vowel ending in [\y \i \u \o] with no last consonant, add
        ; the tone to the first vowel (cay, bai, bau, bao,...) else to the
        ; second one (qua, khoe, khoan,..).
        2 (if (and (in? (last vowel) [\y \i \u \o]) (empty? _last))
            [(add-tone-char (first vowel) tone) (last vowel)]
            [(first vowel) (add-tone-char (last vowel) tone)])
        ; Triple? Add to second-to-last if ending in [\o \i \u] (khoeo, khoai)
        ; else to last (chuyen, tuyet).
        3 (if (in? (last vowel) [\o \i \u])
          [(first vowel) (add-tone-char (nth vowel 1) tone) (last vowel)]
          (concat (butlast vowel) [(add-tone-char (last vowel) tone)]))
        vowel)
      _last]))))

(defn get-tone-char
  "Get the tone of a single char. The result can be one of
  [:none :grave :acute :hook :tilde :dot]."
  [chr]
  (get tone-keyword
    (.lastIndexOf
      (or (get-vowel-family chr) "")
      (str chr)) :none))

(defn get-last-tone-string
  "Get the tone of the last (right-most) character with tone.
  See get-tone-char for possible result values."
  [string]
  (first (drop-while #{:none} (reverse (map get-tone-char string)))))
