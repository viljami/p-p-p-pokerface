(ns p-p-p-pokerface)

(def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14 })

(defn rank [[fst _]]
  (if (Character/isDigit fst)
    (Integer/valueOf (str fst))
    (replacements fst)))

(defn suit [[_ snd]] (str snd))

(defn amounts [hand what] (vals (frequencies (map what hand))))
(defn highest [hand what] (apply max (amounts hand what)))
(defn pair? [hand] (== 2 (highest hand rank)))

(def pair-hand ["2H" "2S" "4C" "5C" "7D"])

(defn three-of-a-kind? [hand] (== 3 (highest hand rank)))

(defn four-of-a-kind? [hand] (== 4 (highest hand rank)))

(defn flush? [hand] (== 5 (highest hand suit)))

(defn full-house? [hand]
  (let [sortedResult (sort (amounts hand rank))]
    (= sortedResult (range (first sortedResult) (+ 2 (first sortedResult))))))

(defn two-pairs? [hand]
   (== 2 (last (sort (vals (frequencies (amounts hand rank)))))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))]
    (= ranks (range (first ranks) (+ 5 (first ranks))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  (apply max (map rank hand)))

(def checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]})

(def simpless #{[high-card? 0] [pair? 1] [two-pairs? 2]})

(defn value [hand]
  (apply max (map second
              (filter (fn [keyValue] ((first keyValue) hand)) checkers))))


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
