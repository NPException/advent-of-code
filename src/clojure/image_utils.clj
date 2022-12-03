(ns image-utils
  (:require [clojure.java.io :as io]
            [clojure.math :as math])
  (:import (java.awt RenderingHints)
           (javax.imageio ImageIO)
           [javax.imageio.stream MemoryCacheImageOutputStream]
           [de.npe.imageutils GifWriter]
           [java.awt.image BufferedImage]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


(defn write-to-gif
  "Writes a sequence of BufferedImages as a gif to the desired output.
  `out` is coerced to an OutputStream via clojure.java.io/output-stream.
  `delay-ms` the delay in ms between each frame. Note that delays have a resolution of 10 ms.
  `loop-limit` is the number of times the gif should run. 0 means loop forever. Max is 65536."
  [out delay-ms loop-limit images]
  (with-open [os         (io/output-stream out)
              image-os   (MemoryCacheImageOutputStream. os)
              gif-writer (GifWriter. image-os (int delay-ms) (int loop-limit))]
    (doseq [image images]
      (when-not (instance? BufferedImage image)
        (throw (ex-info "image is not a BufferedImage" {:image image})))
      (.writeToSequence gif-writer image))))


(defn rgbi
  ^long
  ([[r g b]]
   (rgbi r g b))
  ([^long r ^long g ^long b]
   (bit-or (bit-shift-left (bit-and r 0xFF) 16)
           (bit-shift-left (bit-and g 0xFF) 8)
           (bit-and b 0xFF))))


(defn rgbf
  ^long
  ([[r g b]]
   (rgbf r g b))
  ([^double r ^double g ^double b]
   (bit-or (bit-shift-left (bit-and (long (* r 255)) 0xFF) 16)
           (bit-shift-left (bit-and (long (* g 255)) 0xFF) 8)
           (bit-and (long (* b 255)) 0xFF))))


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
                  :bytes (fn [[r g b]] (rgbi r g b))
                  :floats (fn [[r g b]] (rgbf r g b)))
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


(defn write-png
  [^BufferedImage image file]
  (ImageIO/write image "png" (io/file file)))


(defprotocol GifRecorder
  (record-frame! [this data] "adds a frame to the gif, and returns data")
  (finish-recording! [this] "stops the recording and finalizes writing to disk"))


(defn start-gif-recorder
  [out delay-ms loop-limit create-image-fn]
  ; the current frame is a promise of a vector, containing the image data
  ; and the promise for the next frame. If the promise returns nil, ends the recording.
  (let [first-frame   (promise)
        frame-atom    (atom first-frame)
        writer-thread (future
                        (write-to-gif out delay-ms loop-limit
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


(defn record-as-gif
  "Stores the given sequence of 2-dimensional data as a GIF.
  The GIF will loop, and show the last frame for 5 seconds."
  ([out delay-ms create-image-fn data-seq]
   (print "Writing GIF... ")
   (flush)
   (write-to-gif out delay-ms 0
     (lazy-cat
       (map create-image-fn (drop-last data-seq))
       (let [amount (int (* 5.0 (/ 1000.0 (int delay-ms))))]
         (repeat amount (create-image-fn (last data-seq))))))
   (println "Done!")
   data-seq))


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

  (write-png img "test.png")


  (defn rot [[a b c]]
    [c a b])

  (let [rec (start-gif-recorder "test.gif" 500 0
              #(image-from-data identity :floats 50.0 %))]
    (->> pixels
         (record-frame! rec)
         (mapv rot)
         (record-frame! rec)
         (mapv rot)
         (record-frame! rec))
    (finish-recording! rec))

  ;; TODO: visualize every iteration of 2020 day 11
  )

