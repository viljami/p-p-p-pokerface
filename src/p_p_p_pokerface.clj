(ns p-p-p-pokerface)

(def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14 })

(defn rank [[fst _]]
  (if (Character/isDigit fst)
    (Integer/valueOf (str fst))
    (replacements fst)))

(defn suit [[_ snd]] (str snd))

(defn amounts [hand] (vals (frequencies (map rank hand))))
(defn highest [hand] (apply max (amounts hand)))
(defn pair? [hand]
  (== 2 (highest hand)))

(def pair-hand ["2H" "2S" "4C" "5C" "7D"])

(defn three-of-a-kind? [hand]
  (== 3 (highest hand)))

(defn four-of-a-kind? [hand]
  (== 4 (highest hand)))

(defn flush? [hand]
  nil)

(defn full-house? [hand]
  nil)

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)


(def high-seven                   ["2H" "3S" "4C" "5C" "7D"])


(def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
(def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
(def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])

(def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
(def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
(def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
(def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
(def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
(def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
(def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])
