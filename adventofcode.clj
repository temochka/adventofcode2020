(ns advent-of-code
  (:import (java.lang Integer)))

(defn lines [file] (with-open [rdr (clojure.java.io/reader file)] (doall (line-seq rdr))))

(def input-1-1 (map #(Integer/parseInt %) (lines "1-1.txt")))
(def input-2-1 (lines "2-1.txt"))
(def input-3-1 (lines "3-1.txt"))
(def input-4-1 (lines "4-1.txt"))
(def input-4-2-example-invalid (lines "4-2-example-invalid.txt"))
(def input-4-2-example-valid (lines "4-2-example-valid.txt"))

(defn day-1-1 []
  (let [numbers (set input-1-1)
        x (some #(numbers (- 2020 %)) (seq numbers))
        y (- 2020 x)]
    (* x y)))

(defn day-1-2 []
  (let [numbers (set input-1-1)
        helper-fn (fn [sum] (some #(numbers (- sum %)) (seq numbers)))
        [x y] (some #(some->> (helper-fn (- 2020 %)) (vector %)) (seq numbers))
        z (- 2020 x y)]
    (* x y z)))

(defn day-2-1 []
  (->> input-2-1
       (map #(re-matches #"(\d+)[-](\d+) ([a-z])[:] ([a-z]+)" %))
       (filter
         (fn [[_ from to [ch] password]]
           (<= (Integer/parseInt from) ((frequencies password) ch 0) (Integer/parseInt to))))
       count))

(defn day-2-2 []
  (->> input-2-1
       (map #(re-matches #"(\d+)[-](\d+) ([a-z])[:] ([a-z]+)" %))
       (filter
         (fn [[_ pos-1 pos-2 [ch] password]]
           (= #{true false}
              (set [(= ch (.charAt password (dec (Integer/parseInt pos-1))))
                    (= ch (.charAt password (dec (Integer/parseInt pos-2))))]))))
       count))

(defn day-3-1 []
  (->> input-3-1
     (sequence
       (comp
         (map-indexed #(nth (cycle %2) (* %1 3)))
         (filter #(= \# %))))
     count))

(defn day-3-2 []
  (let [f (fn [dx dy]
            (->> input-3-1
               (sequence
                 (comp
                   (map-indexed #(when (= 0 (mod %1 dy)) %2))
                   (keep identity)
                   (map-indexed #(nth (cycle %2) (* %1 dx)))
                   (filter #(= \# %))))
               count))]
    (* (f 1 1)
       (f 3 1)
       (f 5 1)
       (f 7 1)
       (f 1 2))))

(defn day-4-1 []
  (->> input-4-1
       (partition-by #(nil? (seq %)))
       (map (fn [s] (set (mapcat #(re-seq #"[a-z]{3}(?=[:])" %) s))))
       (filter #(= #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"} (disj % "cid")))
       count))

(defn day-4-2 []
  (let [year #(some->> % (re-matches #"^\d{4}$") Integer/parseInt)
        validations {"byr" #(when-let [y (year %)] (<= 1920 y 2002))
                     "iyr" #(re-matches #"^20(1[0-9]|20)$" %)
                     "eyr" #(re-matches #"^20(2[0-9]|30)$" %)
                     "hgt" (fn [hgt]
                             (when-let [[_ n unit] (re-matches #"^(\d+)(in|cm)$" hgt)]
                               (case unit
                                 "cm" (<= 150 (Integer/parseInt n) 193)
                                 "in" (<= 59 (Integer/parseInt n) 76))))
                     "hcl" #(re-matches #"^\#[a-f0-9]{6}$" %)
                     "ecl" #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
                     "pid" #(re-matches #"[0-9]{9}" %)
                     "cid" (constantly true)}
        valid? (fn [passport] (every? #((val %) (passport (key %))) validations))]
    (->> input-4-1
         (partition-by #(nil? (seq %)))
         (map (fn [s] (into {} (map #(clojure.string/split % #":") (mapcat #(re-seq #"[a-z]{3}:[^\s]+" %) s)))))
         (filter #(= #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"} (disj (set (keys %)) "cid")))
         (filter valid?)
         count)))

(day-1-1)
(day-1-2)
(day-2-1)
(day-2-2)
(day-3-1)
(day-3-2)
(day-4-1)
(day-4-2)

(def input-5-1 (lines "5-1.txt"))

(defn day-5-1 []
  (let [parse (fn [l r [i & rst]]
                (if (= l r)
                  l
                  (case i
                    (\F \L) (recur l (- r (inc (quot (- r l) 2))) rst)
                    (\B \R) (recur (+ l (inc (quot (- r l) 2))) r rst))))
        pass-id (fn [pass]
                  (let [[row-pass col-pass] (partition-by (comp boolean #{\F \B}) pass)
                        row (parse 0 127 row-pass)
                        col (parse 0 7 col-pass)]
                    (+ (* row 8) col)))]
    (->> input-5-1
         (map pass-id)
         (apply max-key identity))))

(day-5-1)

(defn day-5-2 []
  (let [parse (fn [l r [i & rst]]
                (if (= l r)
                  l
                  (case i
                    (\F \L) (recur l (- r (inc (quot (- r l) 2))) rst)
                    (\B \R) (recur (+ l (inc (quot (- r l) 2))) r rst))))
        pass-id (fn [pass]
                  (let [[row-pass col-pass] (partition-by (comp boolean #{\F \B}) pass)
                        row (parse 0 127 row-pass)
                        col (parse 0 7 col-pass)]
                    (+ (* row 8) col)))
        booked-seats (->> input-5-1 (map pass-id) set)
        all-seats (set (range 40 801))]
    (clojure.set/difference all-seats booked-seats)))

(day-5-2)

