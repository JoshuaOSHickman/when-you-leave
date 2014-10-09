(ns when-you-leave.core
  (:require
    [figwheel.client :as fw]
    [om.core :as om :include-macros true]
    [sablono.core :as html :refer-macros [html]]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload
;; (defonce app-data (atom {}))

(println "Edits to this text should show up in your developer console.")

(def pink '(255, 20, 147))
(def blue '(0, 112, 184))
(def yellow '(255, 215, 0))

(defn rgb-to-hsv [r g b]
  (let [r' (/ r 255.0)
        g' (/ g 255.0)
        b' (/ b 255.0)
        cmax (max r' g' b')
        cmin (min r' g' b')
        delta (- cmax cmin)
        hue (cond (= cmax r') (* 60 (mod (/ (- g' b') delta) 6))
                  (= cmax g') (* 60 (+ 2 (/ (- b' r') delta)))
                  (= cmax b') (* 60 (+ 4 (/ (- r' g') delta))))
        saturation (if (= 0 delta) 0 (/ delta cmax))
        value cmax]
    [hue saturation value]))

(defn hsv-to-rgb [h s v]
  (let [c (* v s)
        x (* c (- 1 (Math/abs (- (mod (/ h 60) 2) 1))))
        m (- v c)
        [r' g' b'] (cond (<= 0 h 60) [c x 0]
                         (<= 60 h 120) [x c 0]
                         (<= 120 h 180) [0 c x]
                         (<= 180 h 240) [0 x c]
                         (<= 240 h 300) [x 0 c]
                         (<= 300 h 360) [c 0 x])]
    [(+ r' m), (+ g' m), (+ b' m)]))

(defn removing-saturation 
  "loop colors, dulling them (turning to grey) over n items, so n is grey"
  [colors n] 
  (let [hsvs (map (partial apply rgb-to-hsv) colors)
        saturations (map #(nth % 1) hsvs)
        min-sat (apply min saturations)
        new-saturations (into '(0) (range min-sat 0 (- (/ min-sat (dec n)))))
        new-colors-in-hsv (map #(assoc (vec %1) 1 %2) (cycle colors) new-saturations)]
    (map (partial apply hsv-to-rgb) new-colors-in-hsv)))

(defn average [xs] (/ (reduce + xs) (count xs)))

(defn dullify "have the r b g move closer to their average"
  [colors n]
  (let [true-average (average (flatten colors))
        weights (range 0 1 (/ 1 n))]
    (map (fn [color w] (map #(+ (* w true-average) (* (- 1.0 w) %)) color))
         (cycle colors) weights)))

(def colors (vec (dullify [pink blue yellow] 8)))

(defn hex-color [& vals]
  (apply str "rgb(" (conj (vec (take 5 (interleave (map Math/floor vals) (repeat ", ")))) ")")))

(defn widget [data]
  (om/component
   (let [words (.split "When you leave my colors fade to grey" #" ")]
     (html 
      [:div {:style 
             {:font-family "'Alegreya SC', serif", 
              :font-size "32px" 
              :background-color "black"
              :text-align "center"
              :margin "auto"
              :width "200px"}}
       (map 
        #(vector :p {:style {:color (apply hex-color %2)}} %1) 
        words
        colors)]))))

(om/root widget {} {:target js/document.body})

(fw/watch-and-reload
 :jsload-callback (fn []
                    ;; (stop-and-start-my app)
                    ))
