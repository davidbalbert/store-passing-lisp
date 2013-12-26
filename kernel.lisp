(def nil? (lambda (x)
            (= x nil)))

(def atom? (lambda (x)
             (not (list? x))))

(def not (lambda (x)
           (if (= x nil)
             t
             nil)))

(defmacro unless (predicate yes no)
  `(if (not ,predicate)
     ,yes
     ,no))
