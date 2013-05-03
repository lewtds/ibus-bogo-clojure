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

(def word-tokenizer-pattern
  "A regular expression that is the heart of 'separate'."
  (let [vowels-str (apply str vowels)]
    (re-pattern (format "(.*?(?:qu|g|gi)?)(?:(%s)(%s))?$"
      (str "[" vowels-str "]+")        ; vowel  - a
      (str "[^" vowels-str "]*?")))))  ; tail   - nh

(defn separate
  "Separates a string into 3 parts - :head, :vowel and :last.
  Eg.
  > (separate \"chuyen\"
    {:last (n), :vowel (u y e), :head (c h)}"
  [string]
  (zipmap [:head :vowel :last] (rest (re-find word-tokenizer-pattern string))))
