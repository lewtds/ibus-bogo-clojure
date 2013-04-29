(ns bogo.mark)

(def mark-affinity {
  :hat   {:family-a \â
          :family-o \ô
          :family-e \ê}

  :horn  {:family-o \ơ
          :family-u \ư}

  :breve {:family-a \ă}

  :bar   {:family-d \đ}

  :none  {:family-a \a
          :family-o \o
          :family-e \e
          :family-d \d
          :family-u \u}})

(defn get-family
  "Gets the mark family of a given char among :family-{a,e,o,u,d}."
  [chr]
  (case chr
    (\a \ă \â) :family-a
    (\e \ê)    :family-e
    (\o \ơ \ô) :family-o
    (\u \ư)    :family-u
    (\d \đ)    :family-d
    nil))

(defn add-mark-char
  "Adds mark to a single char."
  [chr mark]
  (get (get mark-affinity mark) (get-family chr) chr))

(defn add-mark-string
  "Adds a mark to a string. The mark can only be added if 'target' appears in the
  string."
  ; be careful not to be too rigid with target (ơ -> ô). Only family target perhaps?
  [string mark target]
  (let [idx (.lastIndexOf string (str target))]
    (if (not= idx -1)
      (apply str
        (concat
          (take idx string)
          [(add-mark-char (get string idx) mark)]
          (nthrest string (inc idx))))
      string)))
