(ns image-utils
  (:require [clojure.java.io :as io])
  (:import [javax.imageio.stream MemoryCacheImageOutputStream]
           [de.npe.imageutils GifWriter]
           [java.awt.image BufferedImage]))


(defn write-to-gif
  "Writes a sequence of BufferedImages as a gif to the desired output.
  out is coerced to an OutputStream via clojure.java.io/output-stream."
  [out delay-ms loop? images]
  (with-open [os (io/output-stream out)
              image-os (MemoryCacheImageOutputStream. os)
              gif-writer (GifWriter. image-os delay-ms loop?)]
    (doseq [^BufferedImage image images]
      (.writeToSequence gif-writer image))))


(defn rgbi [^long r ^long g ^long b]
  (bit-or (bit-shift-left (bit-and r 0xFF) 16)
          (bit-shift-left (bit-and g 0xFF) 8)
          (bit-and b 0xFF)))


(defn rgbf [^double r ^double g ^double b]
  (bit-or (bit-shift-left (bit-and (long (* r 255)) 0xFF) 16)
          (bit-shift-left (bit-and (long (* g 255)) 0xFF) 8)
          (bit-and (long (* b 255)) 0xFF)))


(defn image-from-rgb
  "Takes a sequence of sequences, containing rgb color values.
  'pixel-type' can be one of :int :byte-tuple :float-tuple.
  It signals how every pixel is represented.
  :int -> every pixel is a single 24bit rgb value represented as an int
  :bytes -> every pixel is a tuple of three values ranging from 0-255
  :floats -> every pixel is a tuple of three values ranging from 0-1.
  'scale' determines the upscale factor of the resulting image.
  Size of the image is determined by the size of the outer sequence for height,
  and the size of the first nested sequence for width."
  [pixel-type scale pixels]
  (let [height (count pixels)
        width (count (first pixels))]
    ;; TODO
    ))


(comment
  (image-from-rgb
    :floats
    3.0
    [[[0 0 0] [0.5 0.5 0.5] [1 1 1]]
     [[1 0 0] [  0   1   0] [0 0 1]]
     [[1 1 0] [  1   0   1] [0 1 1]]])

  ;; TODO: visualize every iteration of 2020 day 11
  )

