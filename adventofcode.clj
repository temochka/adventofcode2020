(ns advent-of-code
  (:require [clojure.set])
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

(def input-6-1 (lines "6-1.txt"))

(defn day-6-1 []
  (transduce
    (comp (partition-by empty?) (map #(count (set (apply concat %)))))
    +
    input-6-1))

(day-6-1)


(defn day-6-2 []
  (transduce
    (comp
      (partition-by empty?)
      (map (fn [answers]
             (->> answers
                  (apply concat)
                  frequencies
                  (keep #(when (= (count answers) (val %)) (key %)))
                  count))))
    +
    input-6-1))

(day-6-2)

(def input-7-1 (lines "7-1.txt"))
(def input-7-1-example (lines "7-1-example.txt"))

(defn day-7-1 []
  (let [index
        (->> input-7-1
             (map (juxt #(re-find #"^\w+ \w+" %) #(re-seq #"(\d+) (\w+ \w+)" %)))
             (mapcat (fn [[col subcols]] (map (fn [[_ _ subcol]] [subcol col]) subcols)))
             (reduce (fn [acc [subcol col]] (update acc subcol #(conj % col))) {}))
        traverse (fn f [col] (reduce clojure.set/union #{col} (into #{} (map #(f %)) (index col))))]
    (dec (count (traverse "shiny gold")))))


(day-7-1)

(def input-7-2-example (lines "7-2-example.txt"))

(defn day-7-2 []
  (let [index
        (->> input-7-1
             (map (juxt #(re-find #"^\w+ \w+" %) #(re-seq #"(\d+) (\w+ \w+)" %)))
             (mapcat (fn [[col subcols]] (map (fn [[_ n subcol]] [col [subcol (Integer/parseInt n)]]) subcols)))
             (reduce (fn [acc [col [subcol n]]] (update acc col #(assoc % subcol n))) {}))
        traverse (fn f [col n] (+ n (* n (reduce + (map #(f (key %) (val %)) (index col))))))]
    (dec (traverse "shiny gold" 1))))

(day-7-2)

(def input-8-1 (lines "8-1.txt"))

(defn day-8-1 []
  (let [memory (->> input-8-1
                    (map #(re-matches #"^(\w+) ([-+]\d+)$" %))
                    (mapv (fn [[_ inst n]] [inst (Integer/parseInt n)])))
        step (fn [{:keys [ir acc]}]
               (when-let [[inst arg] (nth memory ir)]
                 (case inst
                   "acc" {:ir (inc ir) :acc (+ acc arg)}
                   "jmp" {:ir (+ ir arg) :acc acc}
                   "nop" {:ir (inc ir) :acc acc})))]
    (loop [computer {:ir 0 :acc 0}
           instructions #{}]
      (if (instructions (:ir computer))
        (:acc computer)
        (recur (step computer) (conj instructions (:ir computer)))))))

(day-8-1)

(def input-8-1-example (lines "8-1-example.txt"))

(defn day-8-2 []
  (let [init-memory (->> input-8-1
                         (map #(re-matches #"^(\w+) ([-+]\d+)$" %))
                         (mapv (fn [[_ inst n]] [inst (Integer/parseInt n)])))
        flipped-memory (map (fn [[i n]] [({"nop" "jmp" "jmp" "nop"} i i) n]) init-memory)
        step (fn f [{:keys [ir acc stack memory] :as computer}]
               (when-not (stack ir)
                 (if-let [[inst arg] (nth memory ir nil)]
                     (f
                       (case inst
                         "acc" (assoc computer :ir (inc ir) :acc (+ acc arg) :stack (conj stack ir))
                         "jmp" (assoc computer :ir (+ ir arg) :stack (conj stack ir))
                         "nop" (assoc computer :ir (inc ir) :stack (conj stack ir))))
                   (when (= ir (count memory)) acc))))]
    (->> flipped-memory
         (map-indexed #(step {:memory (assoc init-memory %1 %2) :ir 0 :acc 0 :stack #{}}))
         (keep identity)
         first)))

(day-8-2)

(def input-9-1 (mapv bigint (lines "9-1.txt")))

(defn day-9-1 []
  (let [valid? (fn [[x & rest] n pairs]
                 (if (pairs x)
                   true
                   (when rest (recur rest n (conj pairs (- n x))))))] 

    (->> input-9-1
         (partition 26 1)
         (keep (fn [xs]
                 (let [preamble (take 25 xs)
                       n (first (drop 25 xs))]
                   (when-not (valid? preamble n #{}) n))))
         first)))

(day-9-1)

(defn day-9-2 []
  (let [invalid-number (day-9-1)]
    (->> (range (count input-9-1))
         (keep (fn [i]
                 (->> (drop i input-9-1)
                      (reduce (fn [[xs sum] x]
                                (cond
                                  (= (+ sum x) invalid-number) (reduced [(conj xs x) (+ sum x)])
                                  (< (+ sum x) invalid-number) [(conj xs x) (+ sum x)]
                                  :else (reduced nil)))
                              [[] 0]))))
         ffirst
         ((fn [xs] (+ (apply min xs) (apply max xs)))))))

(day-9-2)

(def input-10-1 (mapv #(Integer/parseInt %) (lines "10-1.txt")))

(defn day-10-1 []
  (->> input-10-1
       (cons 0)
       (cons (+ 3 (apply max input-10-1)))
       sort
       (partition 2 1)
       (map (fn [[a b]] (- b a)))
       frequencies
       vals
       (apply *)))

(day-10-1)

(defn day-10-2 []
  (let [in (->> input-10-1 sort)
        max-x (apply max in)
        cache (atom {})]
    (letfn [(with-cache [f & args]
              ((swap! cache update args #(or % (apply f args))) args))
            (count-arrangements [[x & rst] last-x]
              (cond
                (or (nil? x) (< max-x x)) 0N
                (= max-x x) 1N
                :else
                (->> rst
                     (take-while #(<= (- % x) 3))
                     (map-indexed (fn [i _] (with-cache count-arrangements (drop i rst) x)))
                     (reduce +))))]
      (count-arrangements (cons 0 in) 0))))

(day-10-2)

