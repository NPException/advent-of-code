(ns aoc-2021.day-19
  (:use [criterium.core])
  (:require [clojure.string :as str]
            [aoc-utils :as u]
            [clojure.set :as set]))

;; --- Day 19: Beacon Scanner --- https://adventofcode.com/2021/day/19

(def task-input (u/slurp-resource "inputs/aoc_2021/day-19.txt"))

(def test-input "--- scanner 0 ---\n404,-588,-901\n528,-643,409\n-838,591,734\n390,-675,-793\n-537,-823,-458\n-485,-357,347\n-345,-311,381\n-661,-816,-575\n-876,649,763\n-618,-824,-621\n553,345,-567\n474,580,667\n-447,-329,318\n-584,868,-557\n544,-627,-890\n564,392,-477\n455,729,728\n-892,524,684\n-689,845,-530\n423,-701,434\n7,-33,-71\n630,319,-379\n443,580,662\n-789,900,-551\n459,-707,401\n\n--- scanner 1 ---\n686,422,578\n605,423,415\n515,917,-361\n-336,658,858\n95,138,22\n-476,619,847\n-340,-569,-846\n567,-361,727\n-460,603,-452\n669,-402,600\n729,430,532\n-500,-761,534\n-322,571,750\n-466,-666,-811\n-429,-592,574\n-355,545,-477\n703,-491,-529\n-328,-685,520\n413,935,-424\n-391,539,-444\n586,-435,557\n-364,-763,-893\n807,-499,-711\n755,-354,-619\n553,889,-390\n\n--- scanner 2 ---\n649,640,665\n682,-795,504\n-784,533,-524\n-644,584,-595\n-588,-843,648\n-30,6,44\n-674,560,763\n500,723,-460\n609,671,-379\n-555,-800,653\n-675,-892,-343\n697,-426,-610\n578,704,681\n493,664,-388\n-671,-858,530\n-667,343,800\n571,-461,-707\n-138,-166,112\n-889,563,-600\n646,-828,498\n640,759,510\n-630,509,768\n-681,-892,-333\n673,-379,-804\n-742,-814,-386\n577,-820,562\n\n--- scanner 3 ---\n-589,542,597\n605,-692,669\n-500,565,-823\n-660,373,557\n-458,-679,-417\n-488,449,543\n-626,468,-788\n338,-750,-386\n528,-832,-391\n562,-778,733\n-938,-730,414\n543,643,-506\n-524,371,-870\n407,773,750\n-104,29,83\n378,-903,-323\n-778,-728,485\n426,699,580\n-438,-605,-362\n-469,-447,-387\n509,732,623\n647,635,-688\n-868,-804,481\n614,-800,639\n595,780,-596\n\n--- scanner 4 ---\n727,592,562\n-293,-554,779\n441,611,-461\n-714,465,-776\n-743,427,-804\n-660,-479,-426\n832,-632,460\n927,-485,-438\n408,393,-506\n466,436,-512\n110,16,151\n-258,-428,682\n-393,719,612\n-211,-452,876\n808,-476,-593\n-575,615,604\n-485,667,467\n-680,325,-822\n-627,-443,-432\n872,-547,-609\n833,512,582\n807,604,487\n839,-516,451\n891,-625,532\n-652,-548,-490\n30,-46,-14")

(defn parse-input
  [input]
  (->> (str/split input #"\s*--- scanner \d+ ---\s")
       (drop 1)
       (mapv #(mapv u/read-as-vector (str/split-lines %)))))

;; running tests on the inputs revealed that no two points within one scanner will create the same delta-id
(defn delta-entry
  [[a b]]
  [#{a b}
   (->> (map #(u/abs (- %1 %2)) a b)
        sort
        (map-indexed #(bit-shift-left %2 (* %1 11)))        ;; shift over by 11 bits (2028), since no delta can be larger than 2000
        (apply bit-or))])

(defn build-deltas
  [beacons]
  "Build a maps each possible beacon pair to a unique"
  (->> (u/combinations 2 beacons)
       (map delta-entry)
       (into {})))


(defn rotate
  [[x y z] ^long rot]
  (case rot
    0 [x, y, z]
    1 [y, (- x), z]
    2 [(- x), (- y), z]
    3 [(- y), x, z]
    4 [z, y, (- x)]
    5 [y, (- z), (- x)]
    6 [(- z), (- y), (- x)]
    7 [(- y), z, (- x)]
    8 [z, (- x), (- y)]
    9 [(- x), (- z), (- y)]
    10 [(- z), x, (- y)]
    11 [x, z, (- y)]
    12 [z, (- y), x]
    13 [(- y), (- z), x]
    14 [(- z), y, x]
    15 [y, z, x]
    16 [z, x, y]
    17 [x, (- z), y]
    18 [(- z), (- x), y]
    19 [(- x), z, y]
    20 [(- x), y, (- z)]
    21 [y, x, (- z)]
    22 [x, (- y), (- z)]
    23 [(- y), (- x), (- z)]))


(defn beacon-transforms
  [candidates a b]
  (let [[a1 a2] (seq a)
        [b1 b2] (seq b)]
    (->> (range 24)
         (mapcat (fn [rot]
                   [[rot (mapv - a1 (rotate b1 rot))]
                    [rot (mapv - a1 (rotate b2 rot))]
                    [rot (mapv - a2 (rotate b1 rot))]
                    [rot (mapv - a2 (rotate b2 rot))]]))
         (filter #(candidates %))
         set)))


(defn find-transform
  "Find rotation index and offsets between s1 and s2"
  [s1-beacons s2-beacons]
  (let [[k & more] (keys s1-beacons)
        candidates (beacon-transforms any? (s1-beacons k) (s2-beacons k))]
    (loop [[k & more] more
           candidates candidates]
      (if (or (not k) (= 1 (count candidates)))
        (first candidates)
        (recur more (beacon-transforms candidates (s1-beacons k) (s2-beacons k)))))))

(defn beacons-with-delta
  [scanner delta-id-set]
  (into {} (comp (filter #(delta-id-set (val %)))
                 (map (juxt val key)))
        scanner))

(defn apply-transform
  [rot delta [points delta-id]]
  (let [[a b] (seq points)]
    [#{(mapv + (rotate a rot) delta)
       (mapv + (rotate b rot) delta)}
     delta-id]))

(defn combine-scanners
  [s1 s2]
  (let [common-delta-ids (set/intersection (set (vals s1)) (set (vals s2)))]
    ;; there are 66 possible pairings for a list of 12 elements
    (when (>= (count common-delta-ids) 66)
      (when-let [[rot offset] (find-transform (beacons-with-delta s1 common-delta-ids)
                                              (beacons-with-delta s2 common-delta-ids))]
        [offset
         (->> (map #(apply-transform rot offset %) s2)
              (into s1))]))))


(defn construct-overlap
  [scanner others]
  (loop [[other & more] others]
    (when other
      (if-let [[offset combined] (combine-scanners scanner other)]
        [offset (cons combined (remove #{other} others))]
        (recur more)))))

(defn magic
  [input]
  (loop [[s & more] (map build-deltas (parse-input input))
         offsets []]
    (if more
      (let [[offset scanners] (construct-overlap s more)]
        (recur scanners (conj offsets offset)))
      [s offsets])))

(defn part-1
  [input]
  (let [[combined] (magic input)]
    (->> (keys combined)
         (mapcat seq)
         set
         count)))


(defn part-2
  [input]
  (let [[_ offsets] (magic input)]
    (->> (u/combinations 2 (cons [0 0 0] offsets))
         (map (fn [[[x1 y1 z1] [x2 y2 z2]]]
                (+ (u/abs (- x1 x2))
                   (u/abs (- y1 y2))
                   (u/abs (- z1 z2)))))
         (apply max))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 79
  (part-1 task-input)                                       ; => 491
  (quick-bench (part-1 task-input))

  ;; Part 2
  (part-2 test-input)                                       ; => 3621
  (part-2 task-input)                                       ; => 13374
  (quick-bench (part-2 task-input))

  )
