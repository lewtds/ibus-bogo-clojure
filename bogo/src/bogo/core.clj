(ns bogo.core
  (:require [bogo.tone :refer :all]
            [bogo.mark :refer :all]
            [bogo.util :refer :all]))

(def simple-telex {
            "a" ["a^"]
            "o" ["o^"]
            "e" ["e^"]
            "w" ["u+" "o+" "a("]
            "d" ["dd"]
            "f" ["`"]
            "s" ["'"]
            "r" ["?"]
            "x" ["~"]
            "j" ["."]
        })

(defn parse-viqr
  "Parses an atomic VIQR-like string and returns a map indicating the necessary
  operation and arguments going with it."
  [string]
  (case (count string)
    1 {:action :add-tone
       :tone (case string
                  "`" :grave
                  "'" :acute
                  "?" :hook
                  "~" :tilde
                  "." :dot)}
    2 (if (in? (first string) [\a \o \e \u \d])
      {:action :add-mark
       :mark (case (last string)
                \^ :hat
                \+ :horn
                \( :breve
                \d :bar)
       :target (first string)}
      (case (first string)
        \+ {:action :append-char :char (last string)}
        {:action nil}))))

(defmacro apply-with-tone
  "doc-string"
  [string function]
  `(-> (add-tone-string ~string :none)
      ~function
      (add-tone-string (get-last-tone-string ~string))))

(defn execute-operation
  "Executes the operation parsed by `parse-viqr` on the given string."
  [string operation]
  (case (operation :action)
    :add-tone (add-tone-string string (operation :tone))
    ; Backup and reapply the tone in these two cases since it's likely to be
    ; misplaced in previous operations.
    :add-mark (apply-with-tone
      (add-tone-string string :none)
      (add-mark-string (operation :mark)
                       (operation :target)))
    :append-char (if (word-boundary? (operation :char))
      ; Don't reapply tone if the appended char is a word-boundary.
      (str string (operation :char))
      (apply-with-tone
        (add-tone-string string :none)
        (str (operation :char))))))

(defn get-transformation-list
  "Finds the list of possible transformations written in a VIQR-like syntax that
  a keypress can generate. Returns an empty list if no transformation is defined
  for that key."
  [chr input-method-map]
  (get input-method-map chr []))

(defn process-key
  "Processes a single keypress."
  [string chr]
  (let [result (reduce
      execute-operation
      string
      (map
        parse-viqr
        (get-transformation-list (str chr) simple-telex)))]
    (if (= result string)
      (execute-operation string (parse-viqr (str "+" chr)))
      result)))

(defn process-seq
  "Processes a key sequence."
  [key-sequence]
  (reduce
    (fn [string current-key]
      (let [[tail head] (map (comp (partial apply str) reverse) (split-with (complement word-boundary?) (reverse string)))]
        (str head (process-key tail current-key))
        ))
    ""
    (seq key-sequence)))
