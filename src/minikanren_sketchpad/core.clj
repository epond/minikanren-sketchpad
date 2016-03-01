(ns minikanren-sketchpad.core
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic
        clojure.core.logic.pldb)
  (:require [clojure.core.logic.fd :as fd]))

(defn -main [& args]

  ; ===============================
  ; Day 1: Unified Theories of Code
  ; ===============================

  (db-rel mano x)
  (db-rel womano x)

  (def facts
    (db
      [mano :alan-turing]
      [womano :grace-hopper]
      [mano :leslie-lamport]
      [mano :alonzo-church]
      [womano :ada-lovelace]
      [womano :barbara-liskov]
      [mano :john-mccarthy]
      ))

  (db-rel vitalo p s)
  (db-rel turingo p y)

  (def facts
    (-> facts
        (db-fact vitalo :alan-turing :dead)
        (db-fact vitalo :grace-hopper :dead)
        (db-fact vitalo :leslie-lamport :alive)
        (db-fact vitalo :alonzo-church :dead)
        (db-fact vitalo :ada-lovelace :dead)
        (db-fact vitalo :barbara-liskov :alive)
        (db-fact vitalo :john-mccarthy :dead)
        (db-fact turingo :leslie-lamport :2013)
        (db-fact turingo :barbara-liskov :2008)
        (db-fact turingo :john-mccarthy :1971)
        ))

  ; membero is a relation that says its first argument is a member of the list in the second argument.
  (println "What are the members of the list [1 2 3]?"
           (run* [q] (membero q [1 2 3])))
  (println "What lists have 1 as a member (limit to 2 results)?"
           (run 2 [q] (membero 1 q)))

  (println "Who are the women?"
           (with-db facts
                    (run* [q]
                          (womano q))))
  (println "Which women are alive?"
           (with-db facts
                    (run* [q]
                          (womano q)
                          (vitalo q :alive))))
  (println "Which deceased people won the Turing Award?"
           (with-db facts
                    (run* [q]
                          (fresh [p y]
                                 (vitalo p :dead)
                                 (turingo p y)
                                 (== q [p  y])))))

  (println "conde succeeds for every goal that succeeds independently"
           (run* [q]
                 (conde
                   [(== q 1)]
                   [(== q 2) (== q 3)]
                   [(== q :abc)])))
  (println "conso relates the head and tail of a list to the whole list"
           (run* [q] (conso :a [:b :c] q)))

  (defn insideo [e l]
    (conde
      [(fresh [h t]
              (conso h t l)
              (== h e))]
      [(fresh [h t]
              (conso h t l)
              (insideo e t))]
      ))
  (println "insideo is equivalent to the built-in membero"
           (run* [q] (insideo q [:a :b :c])))
  (println "and can also be run backwards"
           (run* [q] (insideo :d [:a :b :c q])))


  ; ========================================
  ; Day 2: Mixing the Logical and Functional
  ; ========================================

  (defn matchinsideo [e l]
    (matche [l]
            ([[e . _]])
            ([[_ . t]] (matchinsideo e t))))                ; matche automatically creates fresh logic variables
  (println "matche lets us write insideo more concisely"
           (run* [q] (matchinsideo q [:a :b :c])))
  (println "and here it is working backwards"
           (run* [q] (insideo :d [:a :b :c q])))

  (defne defneinsideo [e l]
         ([_ [e . _]])
         ([_ [_ . t]] (defneinsideo e t)))
  (println "defne defines a function that uses patterns on its input arguments"
           (run* [q] (defneinsideo q [:a :b :c])))

  (println "core.logic map patterns must match exactly"
           (run* [q]
                 (fresh [m]
                        (== m {:a 1 :b 2})
                        (matche [m]
                                ([{:a 1}] (== q :found-a))
                                ([{:b 2}] (== q :found-b))
                                ([{:a 1 :b 2}] (== q :found-a-and-b))))))

  (println "We can use conde and featurec to include all branches in the solution"
           (run* [q]
                 (fresh [m a b]
                        (== m {:a 1 :b 2})
                        (conde
                                [(featurec m {:a a}) (== q [:found-a a])]
                                [(featurec m {:b b}) (== q [:found-b b])]
                                [(featurec m {:a a :b b}) (== q [:found-a-and-b a b])]))))

  (println "conda only looks for solutions in the first branch that has a successful first goal")
  (defn whicho [x s1 s2 r]
           (conda
                 [(all
                    (membero x s1)
                    (membero x s2)
                    (== r :both))]
                 [(all
                    (membero x s1)
                    (== r :one))]
                 [(all
                    (membero x s2)
                    (== r :two))]))
  (println (run* [q] (whicho :a [:a :b :c] [:d :e :c] q)))
  (println (run* [q] (whicho :d [:a :b :c] [:d :e :c] q)))
  (println (run* [q] (whicho :c [:a :b :c] [:d :e :c] q)))

  (defn conduinsideo [e l]
    (condu
      [(fresh [h t]
              (conso h t l)
              (== h e))]
      [(fresh [h t]
              (conso h t l)
              (conduinsideo e t))]
      ))
  (println "condu only makes the first committed choice" (run* [q] (conduinsideo q [:a :b :c :d])))


  ; =================================
  ; Day 3: Writing Stories with Logic
  ; =================================

  (println "What numbers are <= 1, restricting the search to a finite domain?"
           (run* [q]
                 (fd/in q (fd/interval 0 10))
                 (fd/<= q 1)))

  (println "What three numbers add up to 10?"
           (run* [q]
                (fresh [x y z]
                       (== q [x y z])
                       (fd/in x y z (fd/interval 1 10))
                       (fd/distinct [x y z])
                       (fd/< x y)
                       (fd/< y z)
                       (fd/eq
                         (= (+ x y z) 10)))))
  )