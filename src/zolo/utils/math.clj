(ns zolo.utils.math)

(defn average [numbers]
  (if-not (empty? numbers)
    (int
     (/ (apply + numbers)
        (count numbers)))))