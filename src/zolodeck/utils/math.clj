(ns zolodeck.utils.math)

(defn average [numbers]
  (int
   (/ (apply + numbers)
      (count numbers))))