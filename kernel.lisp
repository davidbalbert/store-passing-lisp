(defmacro defun (name arg-names body & bodies)
  `(def ,name (lambda ,arg-names ,body ,@bodies)))

(defun list (& args) args)

(defun nil? (x) (= x nil))

(defun empty? (x) (= x nil))

(defun atom? (x) (not (list? x)))

(defun not (x)
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

(defun map (f coll)
  (unless (empty? coll)
    (cons (f (car coll)) (map f (cdr coll)))
    ()))

(defun filter (pred coll)
  (unless (empty? coll)
    (if (pred (car coll))
      (cons (car coll) (filter pred (cdr coll)))
      (filter pred (cdr coll)))
    ()))

(defun reduce (f acc coll)
  (unless (empty? coll)
    (reduce f (f acc (car coll)) (cdr coll))
    acc))

(defun comp (f g)
  (lambda (& args) (f (apply g args))))

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
