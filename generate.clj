(import java.lang.Math)
(require '[clojure.data :as data])
(require '[clojure.set :as st])

(def module 1)
(def diameters {:ep1 180
                :ep2 154.353360930517296
                :ep3 20.189402876134998
                :ep4 14.806325687239017})
(def carrier-rotations {:ep1 1
                        :ep2 -1
                        :ep3 2
                        :ep4 -2})
(def surrounding {:ep1 {:next :ep2}
                  :ep2 {:prev :ep1 :next :ep3}
                  :ep3 {:prev :ep2 :next :ep4}
                  :ep4 {:prev :ep3}})
(def min-gear-teeth 20)
(def min-gear-diameter (* module min-gear-teeth))
(def max-circle 210)
(def max-gear-diameter (- max-circle (* module 2)))  ; max diameter possible on a prusa i3 mk3s+
(def max-gear-teeth (/ max-gear-diameter module))
(def max-sun-teeth (- max-gear-teeth (* 2 min-gear-teeth)))
(def max-planet-teeth (int (Math/floor (/ (- max-gear-teeth min-gear-teeth) 2))))
(def min-ring-inner-teeth (* 3 min-gear-teeth))
(def min-ring-diameter-diff 15)

(def max-r4-teeth 20)

(defn epicycle-diameter
  [ep-key]
  (let [diameter (get diameters ep-key)]
    (fn [prospect]
      (assoc-in prospect [ep-key :diameter] diameter))))

(defn driver-prospects
  [ep-key]
  (let [prev-ep-key (get-in surrounding [ep-key :prev])
        next-ep-key (get-in surrounding [ep-key :next])
        prev-carrier-rotations (get carrier-rotations prev-ep-key)
        this-carrier-rotations (get carrier-rotations ep-key)
        carrier-rotations-to-sun (- this-carrier-rotations prev-carrier-rotations)]
    (fn [prospect]
      (eduction
       (mapcat (fn [driver-teeth]
                 (let [next-ring-teeth (get-in prospect [next-ep-key :ring :outer-teeth])
                       ;; the next ring's rotations are recorded relative to the next epicycle's sun,
                       ;; or if you prefer, to this epicycle's carrier (they are attached and rotate in unison)
                       next-ring-rotations (get-in prospect [next-ep-key :ring :rotations])
                       ;; remember that this is the _total_ number of driver rotations,
                       ;; over all of the carrier rotations, measured in the frame this epicycle's sun.
                       ;; this is also equal to the total number of planet-rotations in the fixed-sun frame.
                       driver-rotations (- this-carrier-rotations
                                           prev-carrier-rotations
                                           (/ (* next-ring-teeth
                                                 next-ring-rotations)
                                              driver-teeth))
                       planet-rotations-per-carrier-rotation (/ driver-rotations
                                                                carrier-rotations-to-sun)
                       ;; planet rotations = ((sun teeth / planet teeth) + 1) * carrier rotations around a fixed sun
                       sun-planet-teeth-ratio (clojure.lang.Numbers/toRatio
                                               (- planet-rotations-per-carrier-rotation 1))
                       sun-teeth-step (numerator sun-planet-teeth-ratio)
                       planet-teeth-step (denominator sun-planet-teeth-ratio)]
                   (when (and (<= sun-teeth-step max-sun-teeth)
                              (<= planet-teeth-step max-planet-teeth))
                     [(-> prospect
                          (assoc-in
                           [ep-key :driver]
                           {:teeth driver-teeth
                            :diameter (* driver-teeth module)
                            :carrier-rotations-to-sun carrier-rotations-to-sun
                            :rotations-to-sun driver-rotations})
                          (assoc-in
                           [ep-key :planets]
                           {:rotations driver-rotations
                            :rotations-per-carrier-rotation planet-rotations-per-carrier-rotation
                            :teeth-step planet-teeth-step})
                          (assoc-in
                           [ep-key :sun]
                           {:teeth-step sun-teeth-step}))]))))
       (range min-gear-teeth (inc max-gear-teeth))))))

(defn driver-prospects-fixed-ring
  [ep-key]
  (let [prev-ep-key (get-in surrounding [ep-key :prev])
        next-ep-key (get-in surrounding [ep-key :next])
        this-carrier-rotations (get carrier-rotations ep-key)]
    (fn [prospect]
      (eduction
       (mapcat (fn [driver-teeth]
                 (let [next-ring-teeth (get-in prospect [next-ep-key :ring :outer-teeth])
                       next-ring-rotations (get-in prospect [next-ep-key :ring :rotations])
                       ;; remember that this is the _total_ number of driver rotations,
                       ;; over all of the carrier rotations, measured in the frame of the carrier
                       driver-rotations (/ (* next-ring-teeth
                                              next-ring-rotations)
                                           driver-teeth)
                       ;; same as total driver rotations, but adjusted to be in the frame of the ring
                       planet-rotations-total (+ driver-rotations this-carrier-rotations)
                       planet-rotations-per-carrier-rotation (/ planet-rotations-total
                                                                this-carrier-rotations)
                       ;; planet rotations = -((ring teeth / planet teeth) - 1) * -carrier rotations
                       ;; with a fixed ring
                       ring-planet-teeth-ratio (clojure.lang.Numbers/toRatio
                                                (- 1 planet-rotations-per-carrier-rotation))
                       ring-inner-teeth-step (numerator ring-planet-teeth-ratio)
                       planet-teeth-step (denominator ring-planet-teeth-ratio)]
                   (when (and (<= ring-inner-teeth-step max-gear-teeth)
                              (<= planet-teeth-step max-planet-teeth))
                     [(-> prospect
                          (assoc-in
                           [ep-key :driver]
                           {:teeth driver-teeth
                            :diameter (* driver-teeth module)
                            :rotations driver-rotations})
                          (assoc-in
                           [ep-key :planets]
                           {:rotations planet-rotations-total
                            :teeth-step planet-teeth-step})
                          (assoc-in
                           [ep-key :ring]
                           {:inner-teeth-step ring-inner-teeth-step}))]))))
      (range min-gear-teeth (inc max-gear-teeth))))))

(defn max-idler-prospect
  [ep-key]
  (fn [prospect]
    (let [next-ep-key (get-in surrounding [ep-key :next])
          diameter (get diameters ep-key)
          planet-diameter (get-in prospect [ep-key :planets :diameter])
          sun-diameter (get-in prospect [ep-key :sun :diameter])
          driver-diameter (get-in prospect [ep-key :driver :diameter])
          driver-teeth (get-in prospect [ep-key :driver :teeth])
          driver-rotations (get-in prospect [ep-key :driver :rotations])
          next-ring-outer-diameter (get-in prospect [next-ep-key :ring :outer-diameter])
          driver-ring-side (+ driver-diameter next-ring-outer-diameter) ; missing 2x the idler
          planet-sun-side (+ planet-diameter sun-diameter)
          max-idler-diameter (int (/ (- (+ planet-sun-side diameter) driver-ring-side) 2))
          max-idler-teeth (int (/ max-idler-diameter module))]
      (first
       (transduce
        (comp
         (map (fn [idler-teeth]
                (let [idler-rotations (* (/ driver-teeth idler-teeth)
                                         (- driver-rotations))]
                  (assoc-in prospect
                            [ep-key :idler]
                            {:teeth idler-teeth
                             :diameter (* idler-teeth module)
                             :rotations idler-rotations}))))
         (take 1))
        conj
        (range max-idler-teeth (dec min-gear-teeth) -1))))))

(defn nice-factors
  [number-of-teeth]
  (into
   #{}
   (mapcat (fn [factor]
             (when (= (mod number-of-teeth factor) 0)
               [factor])))
   [3 4]))

(defn acceptable-numbers-of-planets
  [sun-teeth ring-teeth]
  (st/intersection (nice-factors sun-teeth) (nice-factors ring-teeth)))

(defn sun-planet-inner-ring-prospects
  [ep-key]
  (let [prev-ep-key (get-in surrounding [ep-key :prev])
        next-ep-key (get-in surrounding [ep-key :next])
        this-carrier-rotations (get carrier-rotations ep-key)
        prev-carrier-rotations (get carrier-rotations prev-ep-key 0)
        carrier-rotations-to-sun (- this-carrier-rotations prev-carrier-rotations)]
    (fn [prospect]
      (let [diameter (get-in prospect [ep-key :diameter])
            ;; _total_ planet rotations in the frame where the sun is fixed, remember
            driver-rotations (get-in prospect [ep-key :driver :rotations])
            driver-diameter (get-in prospect [ep-key :driver :diameter])
            planet-rotations (get-in prospect [ep-key :planets :rotations])
            next-ring-outer-diameter (get-in prospect [next-ep-key :ring :outer-diameter])
            sun-teeth-step (get-in prospect [ep-key :sun :teeth-step])
            planet-teeth-step (get-in prospect [ep-key :planets :teeth-step])]
        (eduction
         ;; valid planet/sun combinations
         (mapcat (fn [planet-teeth]
                   (let [multiple (/ planet-teeth planet-teeth-step)
                         sun-teeth (* sun-teeth-step multiple)]
                     (when (and (<= min-gear-teeth planet-teeth max-planet-teeth)
                                (<= min-gear-teeth sun-teeth max-sun-teeth))
                       [[planet-teeth sun-teeth]]))))
         ;; don't exceed maximum size for inner ring
         (mapcat (fn [[planet-teeth sun-teeth]]
                   (let [ring-inner-teeth (+ sun-teeth (* 2 planet-teeth))
                         planet-numbers (acceptable-numbers-of-planets sun-teeth ring-inner-teeth)]
                     (when (and
                            (<= ring-inner-teeth max-gear-teeth)
                            (> (count planet-numbers) 0))
                       [[planet-teeth sun-teeth ring-inner-teeth planet-numbers]]))))
         ;; make sure the diameters work out
         (mapcat (fn [[planet-teeth sun-teeth ring-inner-teeth planet-numbers]]
                   (let [planet-diameter (* planet-teeth module)
                         sun-diameter (* sun-teeth module)
                         driver-ring-side (+ driver-diameter next-ring-outer-diameter)
                         planet-sun-side (+ planet-diameter sun-diameter)]
                     (when (and (<= driver-ring-side
                                    (+ diameter planet-sun-side))
                                (<= planet-sun-side
                                    (+ diameter driver-ring-side))
                                (<= diameter
                                    (+ planet-sun-side driver-ring-side)))
                       [[planet-teeth sun-teeth ring-inner-teeth planet-numbers
                         planet-diameter sun-diameter]]))))
         (map (fn [[planet-teeth sun-teeth ring-inner-teeth planet-numbers
                    planet-diameter sun-diameter]]
                (let [ring-inner-diameter (* ring-inner-teeth module)
                      ;; again in the frame where the sun is fixed
                      ring-rotations (* carrier-rotations-to-sun
                                        (+ 1
                                           (/ sun-teeth ring-inner-teeth)))]
                  (-> prospect
                      (assoc-in [ep-key :sun] {:teeth sun-teeth
                                               :diameter sun-diameter})
                      (assoc-in [ep-key :planets] {:teeth planet-teeth
                                                   :diameter planet-diameter
                                                   :rotations planet-rotations
                                                   :numbers planet-numbers})
                      (assoc-in [ep-key :ring] {:inner-teeth ring-inner-teeth
                                                :inner-diameter ring-inner-diameter
                                                :rotations ring-rotations})))))
         (range planet-teeth-step (inc max-planet-teeth) planet-teeth-step))))))

(defn sun-planet-inner-ring-prospects-fixed-ring
  [ep-key]
  (let [prev-ep-key (get-in surrounding [ep-key :prev])
        next-ep-key (get-in surrounding [ep-key :next])
        this-carrier-rotations (get carrier-rotations ep-key)]
    (fn [prospect]
      (let [driver-rotations (get-in prospect [ep-key :driver :rotations])
            diameter (get-in prospect [ep-key :diameter])
            driver-diameter (get-in prospect [ep-key :driver :diameter])
            next-ring-outer-diameter (get-in prospect [next-ep-key :ring :outer-diameter])
            planet-rotations (get-in prospect [ep-key :planets :rotations])
            ring-inner-teeth-step (get-in prospect [ep-key :ring :inner-teeth-step])
            planet-teeth-step (get-in prospect [ep-key :planets :teeth-step])
            min-idler-diameter min-gear-teeth]
        (eduction
         ;; valid planet/ring combinations
         (mapcat (fn [planet-teeth]
                   (let [multiple (/ planet-teeth planet-teeth-step)
                         ring-inner-teeth (* ring-inner-teeth-step multiple)]
                     (when (and (<= min-gear-teeth planet-teeth max-planet-teeth)
                                (<= min-ring-inner-teeth ring-inner-teeth max-gear-teeth))
                       [[planet-teeth ring-inner-teeth]]))))
         ;; don't make the sun too big or small
         (mapcat (fn [[planet-teeth ring-inner-teeth]]
                   (let [sun-teeth (- ring-inner-teeth (* planet-teeth 2))
                         planet-numbers (acceptable-numbers-of-planets sun-teeth ring-inner-teeth)]
                     (when (and
                            (<= min-gear-teeth sun-teeth max-sun-teeth)
                            (> (count planet-numbers) 0))
                       [[planet-teeth ring-inner-teeth sun-teeth planet-numbers]]))))
         ;; make sure the diameters work out
         (mapcat (fn [[planet-teeth ring-inner-teeth sun-teeth planet-numbers]]
                   (let [planet-diameter (* planet-teeth module)
                         ring-inner-diameter (* ring-inner-teeth module)
                         sun-diameter (* sun-teeth module)
                         driver-ring-side (+ driver-diameter next-ring-outer-diameter (* 2 min-idler-diameter))
                         planet-sun-side (+ planet-diameter sun-diameter)]
                     (when (and (<= driver-ring-side
                                    (+ diameter planet-sun-side))
                                (<= planet-sun-side
                                    (+ diameter driver-ring-side))
                                (<= diameter
                                    (+ planet-sun-side driver-ring-side)))
                       [[planet-teeth ring-inner-teeth sun-teeth planet-numbers
                         planet-diameter ring-inner-diameter sun-diameter]]))))
         (map (fn [[planet-teeth ring-inner-teeth sun-teeth planet-numbers
                    planet-diameter ring-inner-diameter sun-diameter]]
                (let [input-rotations (+ 1 (/ ring-inner-teeth sun-teeth))]
                  (-> prospect
                      (assoc-in [ep-key :sun] {:teeth sun-teeth
                                               :diameter sun-diameter
                                               :input-rotations input-rotations})
                      (assoc-in [ep-key :planets] {:teeth planet-teeth
                                                   :diameter planet-diameter
                                                   :rotations planet-rotations
                                                   :numbers planet-numbers})
                      (assoc-in [ep-key :ring] {:inner-teeth ring-inner-teeth
                                                :inner-diameter ring-inner-diameter})))))
         (range planet-teeth-step (inc max-planet-teeth) planet-teeth-step))))))

(defn outer-ring-prospects
  [ep-key]
  (let [prev-ep-key (get-in surrounding [ep-key :prev])
        prev-ep-diameter (get diameters prev-ep-key)
        ;; the outer ring diameter must be small enough to engage the driver of the previous epicycle
        ;; that driver's size is unknown, but it's minimum size is min-gear-diameter. to engage the driver,
        ;; we also don't know the size of the inner ring on the previous epicycle, but it's maximum size
        ;; is max-gear-diameter. if this epicycle is situated 180 degrees from the previous epicycle's driver,
        ;; the outer ring's radius is at most the previous epicycle's radius (from center of this epicycle to
        ;; center of previous epicycle), plus the max gear radius (from center of previous epicycle to the
        ;; the inner ring of the previous epicycle), minus the minimum size for a driver, min-gear-diameter.
        ;; max-ring-outer-radius = prev-ep-radius + max-gear-radius - min-gear-diameter. doubling this gives
        ;; max-ring-outer-diameter = prev-ep-diameter + max-gear-diameter - 2(min-gear-diameter)
        ;; we are also limited by the max gear diameter here
        max-outer-ring-diameter (min
                                 (+ (- max-gear-diameter (* min-gear-diameter 2))
                                    prev-ep-diameter)
                                 max-gear-diameter)
        max-outer-ring-teeth (int (/ max-outer-ring-diameter module))]
    (fn [prospect]
      (let [ring-inner-diameter (get-in prospect [ep-key :ring :inner-diameter])
            min-outer-ring-diameter (max (* ring-inner-diameter 1.11)
                                         (+ ring-inner-diameter min-ring-diameter-diff))
            min-outer-ring-teeth (int (Math/ceil (/ min-outer-ring-diameter module)))]
        (eduction
         (map (fn [ring-outer-teeth]
                (-> prospect
                    (assoc-in [ep-key :ring :outer-teeth] ring-outer-teeth)
                    (assoc-in [ep-key :ring :outer-diameter] (* ring-outer-teeth module)))))
         (range min-outer-ring-teeth (inc max-outer-ring-teeth)))))))

(defn outer-ring-prospects-fixed-ring
  [ep-key]
  (fn [prospect]
    (let [ring-inner-diameter (get-in prospect [ep-key :ring :inner-diameter])
          ring-outer-diameter (int (Math/ceil
                                    (max (* ring-inner-diameter 1.11)
                                         (+ ring-inner-diameter min-ring-diameter-diff))))]
      (when (<= ring-outer-diameter max-circle)
        [(assoc-in prospect [ep-key :ring :outer-diameter] ring-outer-diameter)]))))

(defn fixed-sun-epicycle-check
  [ep-key]
  (let [prev-ep-key (get-in surrounding [ep-key :prev])
        this-carrier-rotations (get carrier-rotations ep-key)
        prev-carrier-rotations (get carrier-rotations prev-ep-key 0)
        carrier-rotations-to-sun (- this-carrier-rotations prev-carrier-rotations)]
   (fn [prospect]
     (let [epicycle (get prospect ep-key)
           ring-rotations (get-in epicycle [:ring :rotations])
           ring-inner-teeth (get-in epicycle [:ring :inner-teeth])
           sun-teeth (get-in epicycle [:sun :teeth])]
       (when-not (= ring-rotations
                    (* carrier-rotations-to-sun
                       (+ 1
                          (/ sun-teeth ring-inner-teeth))))
         (throw (ex-info "epicycle fails check" {:ep ep-key
                                                 :prospect prospect})))
       prospect))))

(defn epicycle4-prospects []
  (comp
   (map (fn [ring-outer-teeth]
          {:ep4 {:ring {:outer-teeth ring-outer-teeth
                        :outer-diameter (* ring-outer-teeth module)
                        :rotations (- (get carrier-rotations :ep4) (get carrier-rotations :ep3))}}}))
   (map (epicycle-diameter :ep4))))

(defn epicycle3-prospects []
  (comp
   (map (epicycle-diameter :ep3))
   (mapcat (driver-prospects :ep3))
   (mapcat (sun-planet-inner-ring-prospects :ep3))
   (mapcat (outer-ring-prospects :ep3))))

(defn epicycle2-prospects []
  (comp
   (map (epicycle-diameter :ep2))
   (mapcat (driver-prospects :ep2))
   (mapcat (sun-planet-inner-ring-prospects :ep2))
   (mapcat (outer-ring-prospects :ep2))))

(defn epicycle1-prospects []
  (comp
   (map (epicycle-diameter :ep1))
   (mapcat (driver-prospects-fixed-ring :ep1))
   (mapcat (sun-planet-inner-ring-prospects-fixed-ring :ep1))
   (map (max-idler-prospect :ep1))
   (mapcat (outer-ring-prospects-fixed-ring :ep1))))

(defn epicycle4-teeth-range []  (range min-gear-teeth (inc max-r4-teeth)))
(defn generate-choices []
  (->> (epicycle4-teeth-range)
       (transduce
        (comp (epicycle4-prospects)
              (epicycle3-prospects)
              (epicycle2-prospects)
              (epicycle1-prospects))
        conj))
)

(comment
  (def choices (generate-choices))
  (count choices)
  (count (filter (fn [prospect] (= (-> prospect :ep1 :ring :inner-teeth) 180)) choices))
  (count (filter (fn [prospect] (= (-> prospect :ep1 :sun :input-rotations) 4)) choices))
  (def narrowed (->> choices
                     (filter (fn [prospect] (= (-> prospect :ep1 :sun :input-rotations) 4)))
                     (filter (fn [prospect] (= (-> prospect :ep1 :ring :inner-teeth) 180)))))
  (count narrowed)
  (take 2 (data/diff (nth narrowed 0) (nth narrowed 1)))
)
