(ns workshop-api.qr-labels
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn])
  (:import [net.glxn.qrgen.javase QRCode]
           [com.google.zxing EncodeHintType]
           [javax.imageio ImageIO]
           [java.awt.image BufferedImage]
           [java.awt Color Font Graphics2D RenderingHints]
           [java.awt.font TextAttribute])
  (:gen-class))

;;(def api-base "http://192.168.1.134:3000")  ;; Still needed for QR URL
(def api-base "https://alubirds.dev")  ;; Still needed for QR URL

(defn qr-img [url]
  (let [qr-bytes (-> (QRCode/from url)
                     (.to net.glxn.qrgen.core.image.ImageType/PNG)
                     (.withSize 100 100)
                     (.withHint EncodeHintType/MARGIN (Integer. 0))
                     .stream
                     .toByteArray)]
    (ImageIO/read (java.io.ByteArrayInputStream. qr-bytes))))

(defn wrap-text [text max-chars-per-line]
  (let [text (or text "No description")]  ;; Default to "No description" if nil
    (loop [words (str/split text #" ") current-line "" lines []]
      (if (empty? words)
        (if (empty? current-line) lines (conj lines current-line))
        (let [word (first words)
              new-line (if (empty? current-line) word (str current-line " " word))]
          (if (<= (count new-line) max-chars-per-line)
            (recur (rest words) new-line lines)
            (recur (rest words) word (conj lines current-line))))))))

(defn create-condensed-font
  [font-name size]
  (let [attributes {TextAttribute/WIDTH TextAttribute/WIDTH_CONDENSED
                    TextAttribute/FAMILY font-name
                    TextAttribute/SIZE (float size)}]
    (Font. ^java.util.Map attributes)))

(def templates {:lb-23-default {:typeface "Roboto"
                                :heading-size 40
                                :text-size 25
                                :line-chrs 18
                                :text-lines 4
                                :line-space 22
                                :label-x 202
                                :label-y 202
                                :coords {:qr-x 0 :qr-y 0
                                         :hd-x 100 :hd-y 70
                                         :tx-x 13 :tx-y 120}}
                :lb-23-20point {:typeface "Roboto"
                                :heading-size 40
                                :text-size 20
                                :line-chrs 20
                                :text-lines 5
                                :line-space 18
                                :label-x 202
                                :label-y 202
                                :coords {:qr-x 0 :qr-y 0
                                         :hd-x 100 :hd-y 70
                                         :tx-x 13 :tx-y 120}}
                :lb-23-18point {:typeface "Roboto"
                                :heading-size 40
                                :text-size 18
                                :line-chrs 30
                                :text-lines 6
                                :line-space 15
                                :label-x 202
                                :label-y 202
                                :coords {:qr-x 0 :qr-y 0
                                         :hd-x 100 :hd-y 70
                                         :tx-x 13 :tx-y 120}}})

;; expects a map as input with fields: :id, :label, :description
(defn generate-label [data & {:keys [template output-file]
                              :or {template :lb-23-default
                                   output-file "qr-image.png"}}]
  (let [{:keys [typeface heading-size text-size text-lines line-space
                label-x label-y line-chrs coords]} (get templates template)
        ;; use the location id to construct the qr code url
        url (str api-base "/api/inventory/location/" (:id data))
        qr-img (qr-img url)
        img (BufferedImage. label-x label-y BufferedImage/TYPE_INT_ARGB)
        g (.createGraphics img)
        desc-lines (wrap-text (:description data) line-chrs)]
    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (.setColor g Color/WHITE)
    (.fillRect g 0 0 label-x label-y)
    (.setColor g Color/BLACK)
    (.setFont g (Font. typeface Font/BOLD heading-size))
    ;; Use the location label for main heading
    (.drawString g (subs (:label data) 0 (min 20 (count (:label data))))
                 (:hd-x coords) (:hd-y coords))
    (.setFont g (create-condensed-font typeface text-size))
    ;; Use the wrapped location description for contents
    (doseq [[idx line] (map-indexed vector (take text-lines desc-lines))]
      (.drawString g line (:tx-x coords) (+ (:tx-y coords) (* idx line-space))))
    (.drawImage g qr-img (:qr-x coords) (:qr-y coords) nil)
    (.dispose g)
    (ImageIO/write img "png" (io/file output-file))
    (println "Generated label for" (:label data) "at" output-file "with url" url)))

(defn -main
  "Generate a QR code label from provided data (as EDN string)."
  [& args]
  (if (not= (count args) 2)
    (println "Usage: java -jar qr-labels.jar <edn-data> <output-file>\nExample: java -jar qr-labels.jar '{:label \"E1\" :description \"Electronics\" :id \"123\"}' /tmp/label.png")
    (let [edn-data (first args)
          output-file (second args)
          data (edn/read-string edn-data)] ;; Parse EDN string to map
      (println "Received data:" data)      ;; Debug
      (generate-label data :output-file output-file))))

