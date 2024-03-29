(ns image-utils
  (:require
    [aoc-utils :as u]
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str])
  (:import (de.npe.imageutils GifWriter)
           (java.awt RenderingHints)
           (java.awt.image BufferedImage)
           (java.io BufferedOutputStream)
           (javax.imageio ImageIO)
           (javax.imageio.stream MemoryCacheImageOutputStream)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


(defn file
  "Create a File instance in the visualizations subfolder for the given path."
  [path]
  (doto (io/file "visualizations" path)
    (-> .getParentFile .mkdirs)))


(defn ^:private write-to-gif!
  "Writes a sequence of BufferedImages as a gif to the desired output.
  `out` is coerced to an OutputStream via clojure.java.io/output-stream.
  `delay-ms` the delay in ms between each frame. Note that delays have a resolution of 10 ms.
  `loop-limit` is the number of times the gif should run. 0 means loop forever. Max is 65536."
  [out delay-ms loop-limit images]
  (with-open [os         (io/output-stream out)
              image-os   (MemoryCacheImageOutputStream. (BufferedOutputStream. os (* 8 1024 1024)))
              gif-writer (GifWriter. image-os (int delay-ms) (int loop-limit))]
    (doseq [image images]
      (when-not (instance? BufferedImage image)
        (throw (ex-info "image is not a BufferedImage" {:image image})))
      (.writeToSequence gif-writer image))))


(defn bytes->rgb
  "Create integer RGB value from bytes RGB components"
  ^long
  ([[r g b]]
   (bytes->rgb r g b))
  ([^long r ^long g ^long b]
   (bit-or (bit-shift-left (bit-and r 0xFF) 16)
           (bit-shift-left (bit-and g 0xFF) 8)
           (bit-and b 0xFF))))

(defn floats->rgb
  "Create integer RGB value from float RGB components"
  ^long
  ([[r g b]]
   (floats->rgb r g b))
  ([^double r ^double g ^double b]
   (bit-or (bit-shift-left (bit-and (long (* r 255)) 0xFF) 16)
           (bit-shift-left (bit-and (long (* g 255)) 0xFF) 8)
           (bit-and (long (* b 255)) 0xFF))))


(defn rgb->bytes
  "Create bytes RGB components from single integer RGB"
  [^long rgb-int]
  [(bit-and (bit-shift-right rgb-int 16) 0xFF)
   (bit-and (bit-shift-right rgb-int 8) 0xFF)
   (bit-and rgb-int 0xFF)])

(defn floats->bytes
  "Create bytes RGB components from float RGB components"
  ([[r g b]]
   (floats->bytes r g b))
  ([^double r ^double g ^double b]
   [(long (* r 255.0))
    (long (* g 255.0))
    (long (* b 255.0))]))


(defn rgb->floats
  "Create float RGB components from single integer RGB"
  [^long rgb-int]
  [(/ (bit-and (bit-shift-right rgb-int 16) 0xFF) 255.0)
   (/ (bit-and (bit-shift-right rgb-int 8) 0xFF) 255.0)
   (/ (bit-and rgb-int 0xFF) 255.0)])

(defn bytes->floats
  "Create float RGB components from bytes RGB components"
  ([[r g b]]
   (bytes->floats r g b))
  ([^long r ^long g ^long b]
   [(/ r 255.0)
    (/ g 255.0)
    (/ b 255.0)]))



(defn lerp-hsvl
  "Linearly interpolates between two HSV/HSL colors."
  [[^double h1 ^double s1 ^double l1]
   [^double h2 ^double s2 ^double l2]
   ^double amount]
  (let [h (if (> (abs (- h1 h2)) 180)
            (if (> h1 h2)
              (u/lerp h1 (+ h2 360) amount)
              (u/lerp (+ h1 360) h2 amount))
            (u/lerp h1 h2 amount))
        s (u/lerp s1 s2 amount)
        l (u/lerp l1 l2 amount)]
    [(mod h 360) s l]))


(defn rgb-to-hsl
  "Takes an rgb tuple of floats, and converts it to an HSL tuple."
  [[^double r ^double g ^double b]]
  ; algorithm taken from https://www.rapidtables.com/convert/color/rgb-to-hsl.html
  (let [Cmax  (max r g b)
        Cmin  (min r g b)
        delta (- Cmax Cmin)
        H     (u/ifelse
                (zero? delta) 0.0
                (= Cmax r) (* 60.0 (-> (- g b) (/ delta) (mod 6) double))
                (= Cmax g) (* 60.0 (-> (- b r) (/ delta) (+ 2.0)))
                #_(= Cmax b) (* 60.0 (-> (- r g) (/ delta) (+ 4.0))))
        L     (/ (+ Cmax Cmin) 2)
        S     (if (zero? delta)
                0
                (/ delta
                   (- 1
                      (-> (* L 2) (- 1) (abs)))))]
    [H S L]))


(defn hsl-to-rgb
  "Takes an HSL tuple, and converts it to an rgb tuple of floats."
  [[^double h ^double s ^double l]]
  (let [c (* (- 1.0 (abs (- (* 2.0 l) 1.0))) s)
        x (* c (- 1.0 (abs (- ^double (mod (/ h 60.0) 2) 1.0))))
        m (- l (* c 0.5))]
    (mapv
      #(+ m ^double %)
      (cond
        (< h 60.0) [c x 0.0]
        (< h 120.0) [x c 0.0]
        (< h 180.0) [0.0 c x]
        (< h 240.0) [0.0 x c]
        (< h 300.0) [x 0.0 c]
        (< h 360.0) [c 0.0 x]))))


(defn scale-image
  [^BufferedImage image ^double factor]
  (let [width       (.getWidth image)
        height      (.getHeight image)
        int-factor? (== (int factor) factor)]
    (cond
      ;; no scaling
      (== factor 1.0)
      image
      ;; down-scale or integer upscale
      (or (< factor 1.0) int-factor?)
      (let [scale-image          (BufferedImage. (long (* width factor)) (long (* height factor)) BufferedImage/TYPE_INT_RGB)
            interpolation-method (if int-factor?
                                   RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR
                                   RenderingHints/VALUE_INTERPOLATION_BICUBIC)]
        (doto (.createGraphics scale-image)
          (.setRenderingHint RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)
          (.setRenderingHint RenderingHints/KEY_INTERPOLATION interpolation-method)
          (.scale factor factor)
          (.drawImage image 0 0 nil))
        scale-image)
      ;; non-integer upscale
      :else
      (let [double-factor (math/ceil (* factor 2))]
        ;; double image size with nearest-neighbour, then downsample
        (-> (scale-image image double-factor)
            (scale-image (/ factor double-factor)))))))


(defn ^:private guess-pixel-type
  [rgb]
  (cond
    ; plain int
    (int? rgb) :int
    ; sequence of bytes
    (and (sequential? rgb)
         (every? int? rgb)) :bytes
    ; sequence of floats
    (and (sequential? rgb)
         (every? float? rgb)) :floats))


; TODO: refactor so that all imaging functions takethe main piece of data in the first position (pixels, grid, data-seq, etc..)


(defn image-from-rgb
  "Takes `pixels` (a sequence of sequences, containing rgb color values).
  'pixel-type' can be one of :int :byte-tuple :float-tuple.
  It signals how every pixel is represented.
  :int -> every pixel is a single 24bit rgb value represented as an int
  :bytes -> every pixel is a tuple of three values ranging from 0-255
  :floats -> every pixel is a tuple of three values ranging from 0-1.
  If no `pixel-type` is provided, all rgb values are assumed to be identical with the first value.
  'scale' determines the upscale factor of the resulting image.
  Size of the image is determined by the size of the outer sequence for height,
  and the size of the first nested sequence for width."
  ^BufferedImage
  ([pixels]
   (image-from-rgb 1.0 pixels))
  ([scale pixels]
   (image-from-rgb (guess-pixel-type (ffirst pixels)) scale pixels))
  ([pixel-type scale pixels]
   (let [height (count pixels)
         width  (count (first pixels))
         rgb-fn (case pixel-type
                  :int (fn [^long x] (int x))
                  :bytes (fn [[r g b]] (bytes->rgb r g b))
                  :floats (fn [[r g b]] (floats->rgb r g b)))
         image  (BufferedImage. width height BufferedImage/TYPE_INT_RGB)]
     (loop [[row & more-rows] pixels
            y 0]
       (when row
         (loop [[pixel & more-pixels] row
                x 0]
           (when pixel
             (.setRGB image x y (rgb-fn pixel))
             (recur more-pixels (inc x))))
         (recur more-rows (inc y))))
     (scale-image image scale))))



(defn image-from-data
  "Takes a sequence of rows of datapoints. All rows must have the same length.
  `pixel-type` can be one of :int :byte-tuple :float-tuple. See `image-from-rgb` for details.
  `mapping-fn` will be called for each datapoint, and Is expected to return an rgb value
  that matches the `pixel-type`.
  If the arity without pixel-type is called, the pixel type of applying `mapping-fn` to the
  first data point will be used."
  ^BufferedImage
  ([mapping-fn data]
   (image-from-data mapping-fn 1.0 data))
  ([mapping-fn scale data]
   (image-from-rgb scale
     (mapv #(mapv mapping-fn %) data)))
  ([mapping-fn pixel-type scale data]
   (image-from-rgb pixel-type scale
     (mapv #(mapv mapping-fn %) data))))


(defn on-off-image-fn
  "Returns a simple image-creation function for true/false data sets.
  `scale`: multiplier for the final image size
  `on?`: function that takes a datapoint as an argument
  `on-rgb`: rgb value that is used for every datapoint for which `on?` returns logical true.
  `off-rgb`: rgb value that is used for every datapoint for which `on?` returns logical true."
  [scale on? on-rgb off-rgb]
  (let [mapping-fn #(if (on? %) on-rgb off-rgb)]
    (fn [data]
      (image-from-data mapping-fn scale data))))


; TODO: allow for unequal size ranges: color A->B = 0.0->0.5, color B->C = 0.5->0.75, color C->D = 0.75->1.0
(defn color-fade-mapping
  "Returns a pixel mapping-fn for use in `image-from-data`.
  It requires data to be floats between 0.0 and 1.0.
  Colors need to be in rgb float tuples.
  Will HSL-lerp between the colors according to the value."
  [colors]
  (let [hsl-colors (mapv rgb-to-hsl colors)]
    (if (= 2 (count hsl-colors))
      ;; shortcut for a single color pair
      (let [[c1 c2] hsl-colors]
        #(hsl-to-rgb (lerp-hsvl c1 c2 %)))
      ;; staged lerping for more colors
      (let [lerps        (->> (u/vpartition 2 1 hsl-colors)
                              (mapv (fn [[c1 c2]]
                                      #(hsl-to-rgb (lerp-hsvl c1 c2 %)))))
            lerps-count  (count lerps)
            last-section (dec lerps-count)]
        (fn [^double value]
          (let [lerp-section  (if (>= value 1.0)
                                last-section
                                (long (* value lerps-count)))
                lerp-fn       (lerps lerp-section)
                section-value (-> (* value lerps-count)
                                  (- lerp-section))]
            (lerp-fn section-value)))))))


(def color-fades
  "Predefined color transition functions for normalized data values."
  (reduce-kv
    (fn [m k colors]
      (let [float-colors (map rgb->floats colors)]
        (-> (assoc m k (color-fade-mapping float-colors))
            (assoc (keyword (str (name k) "-inverted")) (color-fade-mapping (reverse float-colors))))))
    {}
    {:thermal-cam  [0x000033 0xCC1919 0xFFE500 0xFFFFFF]
     :retro-height [0x219C90 0xE9B824 0xEE9322 0xD83F31]    ; https://colorhunt.co/palette/219c90e9b824ee9322d83f31
     :ocean        [0x0C356A 0x279EFF 0x40F8FF 0xD5FFD0]    ; https://colorhunt.co/palette/0c356a279eff40f8ffd5ffd0
     }))

(defn ^:private palette
  "Convenience function to take palette path param from colorhunt.co and print out clj conforming hex values.
  Those can be quickly copied into the color fades table above."
  [s]
  (->> (str/upper-case s)
       (partition 6)
       (map #(apply str "0x" %))
       (apply println)))


(defn greyscale-avg
  "Takes a single :int type pixel and converts it to greyscale by a simple average calculation."
  [^long pixel]
  (let [[^double r ^double g ^double b] (rgb->floats pixel)
        avg (/ (+ r g b)
               3.0)]
    [avg avg avg]))


(def ^{:private true, :const true, :tag 'double} one-third (/ 1.0 3.0))

(defn greyscale-LAB
  "Takes a single :int type pixel and converts it to greyscale by calculating
  the L* value of the color in LAB color space. (preserves perceived brightness better)"
  [^long pixel]
  ; taken from https://stackoverflow.com/a/689547
  (let [[^double r ^double g ^double b] (rgb->floats pixel)
        y  (+ (* 0.2126 (math/pow r 2.2))
              (* 0.7152 (math/pow g 2.2))
              (* 0.0722 (math/pow b 2.2)))
        L* (-> 116
               (* (math/pow y one-third))
               (- 16)
               (/ 100.0)
               (u/clampd 0.0 1.0))]
    [L* L* L*]))


(defn load-image-pixels
  "Loads an image from the given source (via io/input-stream) and returns a nested vector
  of RGB values with the desired pixel-type"
  [pixel-type source]
  (let [image  (ImageIO/read (io/input-stream source))
        width  (.getWidth image)
        height (.getHeight image)
        pixels (.getRGB image 0 0 width height (ints nil) 0 width)]
    (into []
      (comp
        (map (case pixel-type
               :int identity
               :bytes rgb->bytes
               :floats rgb->floats))
        (u/partitioning width))
      pixels)))


(defn write-png!
  [out ^BufferedImage image]
  (with-open [os (io/output-stream out)]
    (ImageIO/write image "png" os)))


(defprotocol GifRecorder
  (record-frame! [this data] "adds a frame to the gif, and returns data")
  (finish-recording! [this] "stops the recording and finalizes writing to disk"))


(defn start-gif-recorder!
  [out delay-ms loop-limit create-image-fn]
  ; the current frame is a promise of a vector, containing the image data
  ; and the promise for the next frame. If the promise returns nil, ends the recording.
  (let [first-frame   (promise)
        frame-atom    (atom first-frame)
        writer-thread (future
                        (write-to-gif! out delay-ms loop-limit
                          (->> @first-frame
                               (iterate #(deref (second %)))
                               (take-while some?)
                               (map first))))]
    (println "Start GIF recording")
    ;; create and return the recorder
    (reify GifRecorder
      (record-frame! [_ data]
        ;; prepare the image in a delay, just in case `swap!` is run multiple times because I misuse the recorder.
        (let [image (delay (create-image-fn data))]
          (swap! frame-atom
            (fn [frame]
              ;; if the atom is empty, recording has already finished
              (when frame
                (let [next-frame (promise)]
                  (deliver frame [@image next-frame])
                  next-frame)))))
        data)
      (finish-recording! [_]
        (swap! frame-atom
          (fn [frame]
            (when frame
              (deliver frame nil)
              nil)))
        ;; wait for writer to finish
        (print "Writing GIF... ")
        (flush)
        @writer-thread
        (println "Done!")))))


(defn record-as-gif!
  "Stores the given sequence of 2-dimensional data as a GIF.
  `delay-ms` will be 30 by default. `loop?` is true by default.
  If `loop?` is true, the last frame will be repeated for 5 seconds."
  ([output image-fn data-seq]
   (record-as-gif! output image-fn nil data-seq))
  ([output image-fn
    {:keys [delay-ms loop?] :or {delay-ms 30, loop? true}}
    data-seq]
   (print "Writing GIF... ")
   (flush)
   (write-to-gif! output delay-ms (if loop? 0 1)
     (->> data-seq
          (partition-all 2 1)
          (mapcat (fn [[data next-data]]
                    (if (and loop? (nil? next-data))
                      ;; repeat the last frame for 5 seconds
                      (let [gif-frame-delay (quot (int delay-ms) 10)
                            gif-fps         (/ 100.0 gif-frame-delay)]
                        (repeat (int (* 5 gif-fps)) (image-fn data)))
                      [(image-fn data)])))))
   (println "Done!")))


(defn find-min-max-grid-values
  "Takes a 2-dimensional grid of numeric values, and returns a tuple of doubles [min max]"
  [grid]
  (let [strip (apply concat grid)]
    [(double (apply min strip))
     (double (apply max strip))]))


(defn normalize-grid-values
  "Takes a 2-dimensional grid of numeric values, and normalizes them to values between 0.0 and 1.0.
  Optionally takes the known min and max values."
  ([grid]
   (let [[min-val max-val] (find-min-max-grid-values grid)]
     (normalize-grid-values grid min-val max-val 0.0)))
  ([grid ^double min-val ^double max-val ^double zero-range-default]
   (let [range-val (- max-val min-val)]
     (if (zero? range-val)
       ; edge case: all grid values are identical
       (let [height (count grid)
             width (count (first grid))]
         (->> (vec (repeat (* width height) zero-range-default))
              (u/vpartition width)
              (vec)))
       ; regular normalization of each value
       (mapv (fn [row]
               (mapv (fn [value]
                       (/ (- (double value) min-val) range-val))
                 row))
         grid)))))


; TODO: Provide convenience functions like to save heatmap images and gifs.
;       (See `aoc-2020.day-11/heatmap!` and `aoc-2020.day-11/heatmap-gifs!`)
;       Functions should take the `data-seq` to be used, the desired `output`,
;       the `heat-fn` (optional, increase heat on changing value by default),
;       and a `color-palette` keyword, which will be used to grab a mapping-fn from the color-fades table.

(defn heatmap
  "Turns a data sequence into a single heatmap grid.
  `heat-fn` - a 3-arg function which will receive a current value of the heatmap (`heat-start-val` if it's for the first data frame),
              a piece of data from the current frame, and value for the same piece of data in the previous frame (nil on first frame).
              The function will return the new value for the heatmap at that point."
  [heat-start-val heat-fn data-seq]
  (let [first-frame (first data-seq)
        width       (count (first first-frame))
        height      (count first-frame)
        heatstrip   (loop [heatstrip        (repeat (* width height) heat-start-val)
                           prev-frame-strip (repeat nil)
                           [frame & more] data-seq]
                      (if (nil? frame)
                        (vec heatstrip)
                        (let [frame-strip (mapcat identity frame)]
                          (recur
                            (mapv heat-fn heatstrip frame-strip prev-frame-strip)
                            frame-strip
                            more))))]
    (partition width heatstrip)))



(comment
  ;; 30x30 image with nine squares:
  ;; black  gray    white
  ;; red    green   blue
  ;; yellow magenta cyan
  (def pixels [[[0 0 0] [0.5 0.5 0.5] [1 1 1]]
               [[1 0 0] [0 1 0] [0 0 1]]
               [[1 1 0] [1 0 1] [0 1 1]]])

  (def img (image-from-rgb
             :floats
             10.0
             pixels))

  (write-png! "test.png" img)


  (defn rot [[a b c]]
    [c a b])

  (let [rec (start-gif-recorder! "test.gif" 500 0
              #(image-from-data identity :floats 50.0 %))]
    (->> pixels
         (record-frame! rec)
         (mapv rot)
         (record-frame! rec)
         (mapv rot)
         (record-frame! rec))
    (finish-recording! rec))


  (defmacro test-pixel-transform [algo-sym]
    (let [algo-name (name algo-sym)]
      `(->> (io/resource "images/pp.jpg")
            (load-image-pixels :int)
            (image-from-data ~algo-sym)
            (write-png! (str "target/pp-" ~algo-name ".png")))))

  (test-pixel-transform greyscale-avg)
  (test-pixel-transform greyscale-LAB)

  ;
  )
