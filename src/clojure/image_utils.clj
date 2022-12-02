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
  out is coerced to an OutputStream via clojure.java.io/output-stream."
  [out delay-ms loop? images]
  (with-open [os         (io/output-stream out)
              image-os   (MemoryCacheImageOutputStream. os)
              gif-writer (GifWriter. image-os delay-ms loop?)]
    (doseq [^BufferedImage image images]
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
     (map #(map mapping-fn %) data)))
  ([pixel-type mapping-fn scale data]
   (image-from-rgb pixel-type scale
     (map #(map mapping-fn %) data))))


(defn write-png
  [^BufferedImage image file]
  (ImageIO/write image "png" (io/file file)))


(comment
  ;; 30x30 image with nine squares:
  ;; black  gray    white
  ;; red    green   blue
  ;; yellow magenta cyan
  (def img (image-from-rgb
             :floats
             10.0
             [[[0 0 0] [0.5 0.5 0.5] [1 1 1]]
              [[1 0 0] [0 1 0] [0 0 1]]
              [[1 1 0] [1 0 1] [0 1 1]]]))

  (write-png img "test.png")

  ;; TODO: visualize every iteration of 2020 day 11
  )

