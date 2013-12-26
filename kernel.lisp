(defmacro defn (name arg-names & bodies)
  `(def ,name (lambda ,arg-names ,@bodies)))

(defn nil? (x) (= x nil))

(defn atom? (x) (not (list? x)))

(defn not (x)
      (if (= x nil)
        t
        nil))

(defmacro unless (predicate yes no)
  `(if (not ,predicate)
     ,yes
     ,no))
