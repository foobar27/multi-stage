(ns towers.clojure.ast-test)

(smart-do [])
(->literal nil)

(smart-do [(->variable 'a)])
(->literal 1)

(smart-do [(->variable 'a) (->variable 'b)])
(->do (->variable 'a)
      (->variable 'b))


(smart-do [(->variable 'a) (->smart-do (->variable 'b) (->variable 'c))])
(->do (->variable 'a)
      (->variable 'b)
      (->variable 'b))

(smart-do [(smart-do (->variable 'a) (->variable 'b)) (->variable 'c)])
(->do (->variable 'a)
      (->variable 'b)
      (->variable 'c))

;; simple 1-argument case
(smart-let* [['a (->literal 1)]]
  (->literal 2))
(->let* [['a (->literal 1)]]
  (->literal 2))

;; implicit do, 1 binding
(smart-let* [['a (->literal 1)]]
  (smart-do [(->literal 2)]
            (->literal 3)))
(->let* [['a (->literal 1)]]
  (->literal 2)
  (->literal 3))

;; (let [a 1] a)
(smart-let* [['a (->literal 1)]]
  (->variable 'a))
(->literal 1)

;; (let [x 42 a 1] a)
(smart-let* [['x (->literal 42)]
             ['a (->literal 1)]]
  (->variable 'a))
(let [['x (->literal 42)]]
  (->literal 1))

;;  Undo let insertion: (let [x 4 y 5] (+ x y))
(smart-let* [['x (->literal 4)]
             ['y (->literal 5)]]
  (+ (->variable 'x)
     (->variable 'y)))
(+ (->literal 4)
   (->literal 5))

;; No body
(smart-let [['x ->literal 4]])

;; TODO more tests
