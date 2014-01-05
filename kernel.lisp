(defmacro defn (name arg-names & bodies)
  `(def ,name (lambda ,arg-names ,@bodies)))

(defn list (& args) args)

(defn nil? (x) (= x nil))

(defn empty? (x) (= x nil))

(defn atom? (x) (not (list? x)))

(defn not (x)
      (if (= x nil)
        t
        nil))

(defmacro comment (& exprs)
  nil)

(defmacro unless (predicate yes no)
  `(if (not ,predicate)
     ,yes
     ,no))

(defmacro do (expr & exprs)
  `(let () ,expr ,@exprs))

(defn map (f coll)
      (unless (empty? coll)
        (cons (f (car coll)) (map f (cdr coll)))
        ()))

(defn filter (pred coll)
      (unless (empty? coll)
        (if (pred (car coll))
          (cons (car coll) (filter pred (cdr coll)))
          (filter pred (cdr coll)))
        ()))

(defn reduce (f acc coll)
      (unless (empty? coll)
        (reduce f (f acc (car coll)) (cdr coll))
        acc))

(defn comp (f g)
      (lambda (x) (f (g x))))

(defmacro let (bindings expr & exprs)
  ((lambda (names vals)
     `((lambda ,names ,expr ,@exprs) ,@vals))
   (map car bindings) (map (comp car cdr) bindings)))

(defmacro let* (bindings expr & exprs)
  (if (<= (length bindings) 1)
    `(let ,bindings ,expr ,@exprs)

    (let ((name (car (car bindings)))
          (value (car (cdr (car bindings)))))

         `((lambda (,name)
             (let* ,(cdr bindings) ,expr ,@exprs))
           ,value))))
