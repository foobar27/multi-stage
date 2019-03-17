(ns towers.pink.parser
  (:require [towers.base.ast]
            [towers.base.interpreter :refer [lift]]
            [towers.base.parser :refer [parse]]))

;; https://github.com/namin/pink/blob/master/pink.scm


(def src-pink-poly
  (parse
   (fn [maybe-lift eval exp env]
     (cond
       (number? exp) (maybe-lift exp)
       (symbol? exp) (env exp)
       (symbol? (first exp)) (let [f (first exp)
                                   args (rest exp)
                                   process-exp (fn [arg]
                                                 ((eval arg) env))
                                   process-unary (fn [operator]
                                                   (operator (process-exp (first args))))
                                   process-binary (fn [operator]
                                                    (operator (process-exp (first args))
                                                              (process-exp (first (rest args)))))]
                               (condp = f
                                 `+ (process-binary +)
                                 `- (process-binary -)
                                 `* (process-binary *)
                                 `/ (process-binary /)
                                 `= (process-binary =)
                                 `if (let [condition (first args)
                                           args (rest args)
                                           then (first args)
                                           args (rest args)
                                           else (first args)]
                                       (if (process-exp condition)
                                         (process-exp then)
                                         (process-exp else)))
                                 `let (let [binding (first args) ;; TODO support multiple arguments
                                            x-sym (first binding)
                                            x-exp (first (rest binding))
                                            body (first (rest args))]
                                        (let [x (process-binary x-exp)]
                                          ((eval body)
                                           #(if (= %1 x-sym)
                                              x
                                              (env %1)))))
                                 `lift (lift (process-exp (first args))) ;; TODO can we re-use process-unary?
                                 `run (run ;; TOOD can we re-use process-binary?
                                        (process-exp (first args))
                                        (process-exp (first (rest args))))
                                 `number? (process-unary number?)
                                 `symbol? (process-unary symbol?)
                                 `first (process-unary first)
                                 `rest (process-unary rest)
                                 `cons (maybe-lift (process-binary cons))
                                 `quote (process-unary maybe-lift)
                                 ;; f not predefined, try to look it up in the environment
                                 true ((env f) (process-exp (first args)))))
       ;; lambda function call
       true ((process-exp (first exp))
             (process-exp (first (rest exp)))) ;; TODO support multiple arguments
       ))))


(def src-pink-poly
  (parse
   (fn tie [eval] ;; TODO replace by multi-argument function
     (fn [l]
       (fn [exp]
         (fn [env]
           (cond
             (number? exp) ((first l) exp)
             (symbol? exp) (env exp)
             (symbol? (first exp)) (let [f (first exp) ;; TODO is it possible to use destructuring here => not yet supported by parser
                                         args (rest exp)
                                         process-exp (fn [arg]
                                                       (((eval l) arg) env))
                                         process-unary (fn [operator]
                                                         (let [[x] args]
                                                           (operator process-exp x)))
                                         process-binary (fn [operator]
                                                          (let [[x y] args]
                                                            (operator (process-exp x)
                                                                      (process-exp y))))]
                                     ;; TODO error handling (e.g. wrong number of arguments)
                                     (condp = f
                                       `+ (process-binary +)
                                       `- (process-binary -)
                                       `* (process-binary *)
                                       `= (process-binary =)
                                       ;; TODO use destructuring
                                       `if (let [[condition then else] args]
                                             (if (process-exp condition)
                                               (process-exp then)
                                               (process-exp else)))
                                       ;; TODO multi-argument functions
                                       ;; TODO do we need to port clambda?
                                       `fn* (let [[fn-name [fn-arg] fn-body] args]
                                              (fn f [x] ;; TODO do we need the name "f"?
                                                (((eval l) fn-body)
                                                 ;; construct new environment
                                                 (fn [y]
                                                   ;; TODO condp not supported in 'base' if we don't do macro expansion (which is required for self-applicability)
                                                   ;; TODO maybe we need a macro-feature in "base" for: condp, let and fn destructuring?
                                                   (condp = y
                                                     fn-name f
                                                     arg-name x
                                                     ;; else: default lookup
                                                     (env y))))))
                                       `let* (let [[[var-name var-exp] body] args
                                                   x (process-exp var-exp)]
                                               (fn [y]
                                                 (condp = y
                                                   arg-name x
                                                   ;; else: default lookup
                                                   (env y))))
                                       `lift (process-unary lift)
                                       ;; TODO unlift, lift-ref
                                       ;; TODO log
                                       `number? (process-unary number?)
                                       `symbol? (process-unary symbol?)
                                       `nil? (process-unary nil?)
                                       `list? (process-unary list?)
                                       `code? (process-unary code?)
                                       `first (process-unary first)
                                       `rest (process-unary rest)
                                       `cons (process-binary cons)
                                       ???
                                       )))))))))

;; TODO when to do macro expansion? In the translate function? Or in-between (if somebody dynamically constructs a let-form which needs to be transformed)



;; (define pink-poly-src
;;   (if (eq?  'unlift (car exp))   (((eval (cons (lambda _ e e) 0)) (cadr exp)) env)
;;       (if (eq?  'lift-ref (car exp)) (lift-ref (((eval l) (cadr exp)) env) (((eval l) (caddr exp)) env))
;;           (if (eq?  'log     (car exp))  (log (((eval l) (cadr exp)) env) (((eval l) (caddr exp)) env))
;;               (if (eq?  'quote  (car exp))   ((car l) (cadr exp))
;;                   (if (eq?  'run    (car exp))   (run (((eval l) (cadr exp)) env) (((eval l) (caddr exp)) env))
;;                       (if (eq?  'delta (car exp))
;;                         (let ev (((eval (cons (lambda _ e e) (cdr l))) (cadr exp)) env) (((ev l) (caddr exp)) env))
;;                         (if (eq?  'delta-eval  (car exp))
;;                           (let ev (((eval (cons (lambda _ e e) (cdr l))) (cadr exp)) env) (((((ev tie) eval) l) (caddr exp)) env))
;;                           ((env (car exp)) (((eval l) (cadr exp)) env)))))))))
;;   ((((eval l) (car exp)) env) (((eval l) (cadr exp)) env))
;;   )


;; (def ^:private src-last-index
;;   (parse
;;    ;; TODO replace by loop-recur (not yet supported by parser)
;;    (let [iter (fn [r i env]
;;                 (if (empty? env)
;;                   (if (= r -1)
;;                     (error) ;; TODO add meaningful error message and diagnostics
;;                     r)
;;                   (let [new-r (if (= e (first env))
;;                                 i
;;                                 r)]
;;                     (recur ( + i 1) (rest env)))))]
;;      (iter -1 0 env))))


;; (defn translate [e env]
;;   (cond
;;     (number? e) e
;;     (symbol? e) (->variable (last-index e))
;;     (list? e) (condp = (first e)
;;                 `quote (rest e)
;;                 `fn (let [[_ args body] e]
;;                       ))))

;; (define trans
;;   (lambda (e env)
;;           (cond
;;             ((number? e) e)
;;             ((symbol? e) `(var ,(last-index e env)))
;;             (((tagged? 'quote) e) (cadr e))
;;             (((tagged? 'lambda) e) `(lambda ,(trans (cadddr e) (append env (list (cadr e) (caddr e))))))
;;             (((tagged? 'let) e) `(let ,(trans (caddr e) env) ,(trans (cadddr e) (append env (list (cadr e))))))
;;             ((and (pair? e) (member (car e) '(if + - * eq? number? symbol? pair? null? code? cons car cdr run lift lift-ref log)))
;;              (cons (car e) (map (lambda (x) (trans x env)) (cdr e))))
;;             (else (map (lambda (x) (trans x env)) e)))))
