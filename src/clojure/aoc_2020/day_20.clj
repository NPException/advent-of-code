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
    {:id          (parse-long id)
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
      :BASE        (get-in grid [y x])
      :R90         (get-in grid [(- end-index x) y])
      :R180        (get-in grid [(- end-index y) (- end-index x)])
      :R270        (get-in grid [x (- end-index y)])
      :FLIP_H      (get-in grid [y (- end-index x)])
      :FLIP_V      (get-in grid [(- end-index y) x])
      :FLIP_DIAG_1 (get-in grid [(- end-index x) (- end-index y)])
      :FLIP_DIAG_2 (get-in grid [x y]))))


(defn remove-tile
  [tile-set {:keys [id]}]
  (into #{} (remove #(= id (:id %))) tile-set))


(defn get-edge
  [tile edge]
  (->> edge
       (mapv #(apply pixel tile %))
       (apply str)))


;; part-1

(defn get-edges
  [edges tile]
  (mapv
    (partial get-edge tile)
    edges))

(defn all-orientation-edges
  [edges tile]
  (->> orientations
       (mapcat #(get-edges edges (assoc tile :orientation %)))
       set))

(defn count-matching-edges
  [tile-set edges tile]
  (let [other-tiles (remove-tile tile-set tile)
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
    [(vec (map-indexed (fn [i _] [i 0]) (range size)))      ;; TOP
     (vec (map-indexed (fn [i _] [end-index i]) (range size))) ;; RIGHT
     (vec (map-indexed (fn [i _] [i end-index]) (range size))) ;; BOTTOM
     (vec (map-indexed (fn [i _] [0 i]) (range size)))]))   ;; LEFT


(defn find-corner-tiles
  [tile-set edges]
  (->> tile-set
       (map (juxt (partial count-matching-edges tile-set edges) identity))
       (filter #(= (first %) 2))
       (take 4)
       (map second)))


(defn part-1
  [input]
  (let [tile-set (parse-input input)
        edges (build-edges tile-set)]
    (->> (find-corner-tiles tile-set edges)
         (map :id)
         (apply *))))


;; part 2

(defn matching-corner?
  [tile-set [top-edge right-edge bottom-edge left-edge] rotated-corner-tile]
  (let [corner-right (get-edge rotated-corner-tile right-edge)
        corner-bottom (get-edge rotated-corner-tile bottom-edge)]
    (->> tile-set
         (mapcat #(map (partial assoc % :orientation) orientations))
         (filter #(or (= corner-right (get-edge % left-edge))
                      (= corner-bottom (get-edge % top-edge))))
         ;; ensure that 2 tiles matched
         second
         some?)))


(defn rotate-corner-to-top-left
  "Returns possible variations of the given corner tile to make it the top-left corner"
  [tile-set edges corner-tile]
  (->> orientations
       (map #(assoc corner-tile :orientation %))
       (filter #(matching-corner? (remove-tile tile-set corner-tile) edges %))))


(defn find-matching-tile
  [tile-set tile src-edge target-edge]
  (let [src (get-edge tile src-edge)]
    (->> tile-set
         (mapcat #(map (partial assoc % :orientation) orientations))
         (filter #(= src (get-edge % target-edge)))
         first)))


(defn add-next-tile
  [tile-set tile-grid [top-edge right-edge bottom-edge left-edge]]
  (let [right-tile (last (last tile-grid))
        bottom-tile (first (last tile-grid))]
    (if-let [next-tile (find-matching-tile tile-set right-tile right-edge left-edge)]
      (assoc tile-grid (dec (count tile-grid))
                       (conj (last tile-grid) next-tile))
      (let [next-tile (find-matching-tile tile-set bottom-tile bottom-edge top-edge)]
        (conj tile-grid [next-tile])))))


(defn compose-tile-grid
  [tile-set edges corner-tiles]
  (let [top-left-tile (->> corner-tiles
                           (mapcat #(rotate-corner-to-top-left tile-set edges %))
                           (filter some?)
                           first)]
    (loop [tile-set (remove-tile tile-set top-left-tile)
           tile-grid [[top-left-tile]]]
      (if (empty? tile-set)
        tile-grid
        (let [new-tile-grid (add-next-tile tile-set tile-grid edges)]
          (recur (remove-tile tile-set (last (last new-tile-grid)))
                 new-tile-grid))))))


(defn merge-to-image
  [tile-grid]
  (let [tile-size (-> tile-grid first first :grid count (- 2))
        grid-size (* (count tile-grid) tile-size)]
    (->> (for [y (range grid-size)
               x (range grid-size)]
           [y x])
         (reduce
           (fn [image [y x :as coords]]
             (assoc-in image coords
                       (pixel (get-in tile-grid [(quot y tile-size)
                                                 (quot x tile-size)])
                              (inc (mod x tile-size))
                              (inc (mod y tile-size)))))
           (vec (repeat grid-size [])))
         (mapv #(apply str %)))))


(defn count-hashes
  [grid]
  (->> (apply concat grid)
       (filter #{\#})
       count))


(def monster
  ["                  # "
   "#    ##    ##    ###"
   " #  #  #  #  #  #   "])


(defn create-rule
  [hash-x hash-y]
  (fn [image-tile x y]
    (= \# (pixel image-tile (+ x hash-x) (+ y hash-y)))))


(def monster-at?
  "Args: [image-tile x y]
   Checks if a monster is at the given position."
  (->> (for [y (range (count monster))
             x (range (count (first monster)))
             :when (= \# (get-in monster [y x]))]
         (create-rule x y))
       (apply u/and-fn)))


(defn count-monsters
  ([image-tile]
   (->> orientations
        (map #(count-monsters image-tile %))
        (filter #(> % 0))
        first))
  ([image-tile orientation]
   (let [image-tile (assoc image-tile :orientation orientation)
         size (count (:grid image-tile))
         monster-width (count (first monster))
         monster-height (count monster)]
     (count
       (for [y (range (- size monster-height))
             x (range (- size monster-width))
             :when (monster-at? image-tile x y)]
         true)))))


(defn part-2
  [input]
  (let [tile-set (parse-input input)
        edges (build-edges tile-set)
        image (->> (find-corner-tiles tile-set edges)
                   (compose-tile-grid tile-set edges)
                   merge-to-image)
        image-tile {:grid        image
                    :orientation :BASE}]
    (- (count-hashes image)
       (* (count-monsters image-tile)
          (count-hashes monster)))))


(comment
  ;; Part 1
  (part-1 test-input)                                       ; => 20899048083289
  (part-1 task-input)                                       ; => 15003787688423

  ;; Part 2
  (part-2 test-input)                                       ; => 273
  (part-2 task-input)                                       ; => 1705

  )