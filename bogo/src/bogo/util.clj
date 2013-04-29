(ns bogo.util)

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def vowels
  ["aàáảãạ",
   "ăằắẳẵặ",
   "âầấẩẫậ",
   "eèéẻẽẹ",
   "êềếểễệ",
   "iìíỉĩị",
   "oòóỏõọ",
   "ôồốổỗộ",
   "ơờớởỡợ",
   "uùúủũụ",
   "ưừứửữự",
   "yỳýỷỹỵ"])

(defn in? [item collection]
  "Checks if an item is in a collection."
  (some #{item} collection))

(defn vowel?
  "Checks if a character is a vowel."
  [chr]
  (some #(in? chr %1) vowels))

(defn word-boundary?
  "Checks if a character is a word boundary character."
  [chr]
  (in? chr [\space \. \, \!]))

(defn separate
  "Separates a string into 3 parts - :head, :vowel and :last.
  Eg.
  > (separate \"chuyen\"
    {:last (n), :vowel (u y e), :head (c h)}"
  [string]
  (let
      [rstring (reverse string)
        [last-consonant stuff] (split-with (comp not vowel?) rstring)
        [vowel head] (let [[vowel- head-] (split-with vowel? stuff)]
                  (if (= (str (first head-) (last vowel-)) "qu")
                    [(butlast vowel-) (cons "u" head-)]
                    [vowel- head-]))]
    (zipmap [:head :vowel :last] (map reverse [head vowel last-consonant]))))
