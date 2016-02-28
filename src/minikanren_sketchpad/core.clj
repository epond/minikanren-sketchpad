(ns minikanren-sketchpad.core
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic
        clojure.core.logic.pldb))

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

  ; conde succeeds for every goal that succeeds independently
  (println (run* [q]
                 (conde
                   [(== q 1)]
                   [(== q 2) (== q 3)]
                   [(== q :abc)])))
  ; conso relates the head and tail of a list to the whole list
  (println (run* [q] (conso :a [:b :c] q)))

  ; insideo is equivalent to the built-in membero
  (defn insideo [e l]
    (conde
      [(fresh [h t]
              (conso h t l)
              (== h e))]
      [(fresh [h t]
              (conso h t l)
              (insideo e t))]
      ))
  (println (run* [q] (insideo q [:a :b :c])))
  (println (run* [q] (insideo :d [:a :b :c q])))


  )