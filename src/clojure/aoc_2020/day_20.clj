(ns aoc-2020.day-20
  (:require [clojure.string :as string]
            [aoc-utils :as u]
            [clojure.set :as set]))

;; --- Day 20: Jurassic Jigsaw --- https://adventofcode.com/2020/day/20

(def task-input (u/slurp-resource "inputs/aoc_2020/day-20.txt"))

(def test-input "Tile 2311:\n..##.#..#.\n##..#.....\n#...##..#.\n####.#...#\n##.##.###.\n##...#.###\n.#.#.#..##\n..#....#..\n###...#.#.\n..###..###\n\nTile 1951:\n#.##...##.\n#.####...#\n.....#..##\n#...######\n.##.#....#\n.###.#####\n###.##.##.\n.###....#.\n..#.#..#.#\n#...##.#..\n\nTile 1171:\n####...##.\n#..##.#..#\n##.#..#.#.\n.###.####.\n..###.####\n.##....##.\n.#...####.\n#.##.####.\n####..#...\n.....##...\n\nTile 1427:\n###.##.#..\n.#..#.##..\n.#.##.#..#\n#.#.#.##.#\n....#...##\n...##..##.\n...#.#####\n.#.####.#.\n..#..###.#\n..##.#..#.\n\nTile 1489:\n##.#.#....\n..##...#..\n.##..##...\n..#...#...\n#####...#.\n#..#.#.#.#\n...#.#.#..\n##.#...##.\n..##.##.##\n###.##.#..\n\nTile 2473:\n#....####.\n#..#.##...\n#.##..#...\n######.#.#\n.#...#.#.#\n.#########\n.###.#..#.\n########.#\n##...##.#.\n..###.#.#.\n\nTile 2971:\n..#.#....#\n#...###...\n#.#.###...\n##.##..#..\n.#####..##\n.#..####.#\n#..#.#..#.\n..####.###\n..#.#.###.\n...#.#.#.#\n\nTile 2729:\n...#.#.#.#\n####.#....\n..#.#.....\n....#..#.#\n.##..##.#.\n.#.####...\n####.#.#..\n##.####...\n##..#.##..\n#.##...##.\n\nTile 3079:\n#.#.#####.\n.#..######\n..#.......\n######....\n####.#..#.\n.#...#.##.\n#.#####.##\n..#.###...\n..#.......\n..#.###...")


(def orientations
  [:BASE :R90 :R180 :R270 :FLIP_H :FLIP_V :FLIP_DIAG_1 :FLIP_DIAG_2])


(defn parse-tile
  [tile-input]
  (let [[_ id rows-input] (string/split tile-input #"Tile |:\n")]
    {:id          (u/parse-long id)
     :orientation :BASE
     :grid        (string/split-lines rows-input)}))

(defn parse-input
  [input]
  (->> (string/split input #"\n\n")
       (map parse-tile)
       set))


(defn pixel
  [{:keys [grid orientation]} x y]
  (let [end-index (dec (count grid))]
    (case orientation
      :BASE (get-in grid [y x])
      :R90 (get-in grid [(- end-index x) y])
      :R180 (get-in grid [(- end-index y) (- end-index x)])
      :R270 (get-in grid [x (- end-index y)])
      :FLIP_H (get-in grid [y (- end-index x)])
      :FLIP_V (get-in grid [(- end-index y) x])
      :FLIP_DIAG_1 (get-in grid [(- end-index x) (- end-index y)])
      :FLIP_DIAG_2 (get-in grid [x y]))))


(defn print-tile
  [tile]
  (let [size (count (:grid tile))]
    (->> (for [y (range size)
               x (range size)]
           [x y])
         (partition size)
         (mapv (fn [coords]
                 (println (apply str (map #(apply pixel tile %) coords))))))
    nil))


;; part-1

(defn get-edges
  [edges tile]
  (mapv
    (fn [edge]
      (->> edge
           (mapv #(apply pixel tile %))
           (apply str)))
    edges))

(defn all-orientation-edges
  [edges tile]
  (->> orientations
       (mapcat #(get-edges edges (assoc tile :orientation %)))
       set))

(defn count-matching-edges
  [tile-set edges tile]
  (let [other-tiles (disj tile-set tile)
        tile-edges (all-orientation-edges edges tile)]
    (->> other-tiles
         (map (partial get-edges edges))
         (map set)
         (mapcat (partial set/intersection tile-edges))
         set
         count)))


(defn build-edges
  [tile-set]
  (let [size (->> tile-set first :grid count)
        end-index (dec size)]
    [(vec (map-indexed (fn [i _] [i 0]) (range size)))
     (vec (map-indexed (fn [i _] [end-index i]) (range size)))
     (vec (map-indexed (fn [i _] [i end-index]) (range size)))
     (vec (map-indexed (fn [i _] [0 i]) (range size)))]))


(defn part-1
  [input]
  (let [tile-set (parse-input input)
        edges (build-edges tile-set)]
    (->> tile-set
         (map (juxt (partial count-matching-edges tile-set edges) :id))
         (filter #(= (first %) 2))
         (take 4)
         (map second)
         (apply *))))


;; part 2

(defn part-2
  [input]
  )


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 20899048083289
  (part-1 task-input)                                       ; => 15003787688423

  ;; Part 2
  (part-2 test-input)                                       ; =>
  (part-2 task-input)                                       ; =>

  )