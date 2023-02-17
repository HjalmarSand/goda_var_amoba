Skip to content
Search or jump to…
Pull requests
Issues
Codespaces
Marketplace
Explore
 
@HjalmarSand 
ArvidNorinder
/
Gada-var-amoba
Private
Fork your own copy of ArvidNorinder/Gada-var-amoba
Code
Issues
Pull requests
Actions
Projects
Security
Insights
Gada-var-amoba/core.clj
@niklassanden
niklassanden Too many params
Latest commit 7b3b472 on Apr 22, 2021
 History
 1 contributor
194 lines (152 sloc)  6.9 KB

(ns amoebas.competition2021.Gada-var-amoba.core
	(:use amoebas.defs amoebas.lib amoebas.run amoebas.util)
)

(def MIN-FRIENDLIES-COUNT 18)
(def MIN-FUEL-TO-DIVIDE 8)
(def MIN-ENERGY-TO-DIVIDE 42)
(def EXCESS-ENERGY-TO-DIVIDE 70)
(def WEIGHTED-ENERGY-LIFE-SELECTOR 40)
(def TRAVELER-ENERGY-TO-REST 80)
(def FARMERS-PER-ENV 8)
(def FRIENDLIES-PER-ENV 25)

(defn next-position
    [position dir]
    
    (add-coordinates position (nth Neighbors dir))
)

(defn number-of-hits-until-death
    [health]

    (/ (+ health 3) 4)
)

(defn most-energy-least-life-selector-2
    [hs env]

    (last (sort-by #(let [target (:occupant (env %))]
                        (-
                            (:energy target)
                            (* (number-of-hits-until-death (:health target)) WEIGHTED-ENERGY-LIFE-SELECTOR)
                        )
                    ) hs))
)

(defn get-amoeba-density-normalized
    [max-friendlies max-enemies dir species env hostile-weight]

    (if (= max-enemies 0)
        (* (/ (count (friendlies species (Env-Sections dir) env)) (float max-friendlies)) 1.0)

        (- ;; +
            (* (/ (count (friendlies species (Env-Sections dir) env)) (float max-friendlies)) 1.0)
            (* (/ (count (hostiles species (Env-Sections dir) env)) (float max-enemies)) hostile-weight)
        )
    )
)

(defn get-fuel-normalized
    [max-total-fuel dir env]

    (if (= max-total-fuel 0)
        0

        (- (/ (:fuel (env (nth Neighbors dir))) (float max-total-fuel)))
    )
)

(defn is-traveler?
    "given a position of a friendly amoeba, determines whether it contains a traveler"
    [pos env]
    
    (nth (:data (:occupant (env pos))) 0)
)

(def Neighbors-Next-To-Neighbor {
    [1 1]    [[1 1] [1 0] [0 1]],
    [1 -1]   [[1 -1] [0 -1] [1 0]],
    [-1 1]   [[-1 1] [0 1] [-1 0]],
    [-1 -1]  [[-1 -1] [0 -1] [-1 0]]
})
(defn sign
    [x]
    (if (>= x 0) 1 -1)
)
(defn traveler-direction
    [filtered-dirs position]
    
    (let [
        neighbor [(sign (first position)) (sign (last position))]
        neighbors (Neighbors-Next-To-Neighbor neighbor)
        dirs (for [pos neighbors] (Neighbor-To-Dir pos))
        matching-dirs (filter #(some #{%} dirs) filtered-dirs)
    ]
        (if (empty? matching-dirs)
            0
            (first matching-dirs)
        )
    )
)

(declare create-amoeba-function)
(defn greedy-strategy
    [energy health species env data target-selector]

    (let
        [
            fuel-here (:fuel (env Here))

            friendlies-vec (friendlies species Environment env) 
            friendlies-count (count friendlies-vec) ;; kanske använd farmers-count istället (prestanda)
            farmers-count (count (filter #(not (is-traveler? % env)) friendlies-vec))

            hostile-weight (if (>= friendlies-count MIN-FRIENDLIES-COUNT) 5.0 -2.0) ;; 2.0 -2.0

            max-total-fuel (apply max (for [pos Neighbors] (:fuel (env pos))))
            max-friendlies (apply max (for [dir Dirs] (count (friendlies species (Env-Sections dir) env))))
            max-enemies (apply max (for [dir Dirs] (count (hostiles species (Env-Sections dir) env))))
            dirs-sorted (sort-by #(+
                                    (get-amoeba-density-normalized max-friendlies max-enemies % species env hostile-weight)
                                    (get-fuel-normalized max-total-fuel % env)
                                  ) Dirs
            )
            filtered-dirs (filter #(cell-empty? env (nth Neighbors %)) dirs-sorted)

            is-traveler (if (> max-enemies 0) false (nth data 0)) ;; Bli normal om en fiende syns
            position (nth data 1)

            neighboring-enemies (hostiles species Neighbors env)
            enemy-to-hit (target-selector neighboring-enemies env)
        ]

        (do
            ;; Just nu går vi och dividerar även om det är en fiende som kommer att vara i vår range efter förflyttningen
            (if (and (not (nil? enemy-to-hit)) (>= friendlies-count MIN-FRIENDLIES-COUNT))
                {:cmd :hit, :dir (Neighbor-To-Dir enemy-to-hit), :data [false position]}   ;; KAPOW! med eventuell defensiv 
                
                (if (empty? filtered-dirs)
                    {:cmd :rest, :data [is-traveler position]}
                    
                    (if is-traveler
                        (if (and (>= fuel-here 10) (< energy TRAVELER-ENERGY-TO-REST))
                            {:cmd :rest, :data [(if (>= farmers-count FARMERS-PER-ENV) is-traveler false) position]}

                            (let  [dir (traveler-direction filtered-dirs position)]
                            
                                {:cmd :move, :dir dir, :data [(if (>= farmers-count FARMERS-PER-ENV) is-traveler false) (next-position position dir)]}
                            )
                        )

                        (if (or
                                (>= fuel-here MIN-FUEL-TO-DIVIDE)
                                (>= energy EXCESS-ENERGY-TO-DIVIDE)
                            )
                            
                            (if (>= energy MIN-ENERGY-TO-DIVIDE)
                                (if (or (< farmers-count FARMERS-PER-ENV) (> max-enemies 0))
                                    {:cmd :divide, :dir (first filtered-dirs), 
                                    :function (create-amoeba-function most-energy-least-life-selector-2 false),
                                    :data [is-traveler position], :child-data [is-traveler (next-position position (first filtered-dirs))] }

                                    (if (>= friendlies-count FRIENDLIES-PER-ENV)
                                        
                                        {:cmd :rest, :data [is-traveler position]} ;; move and rest???

                                        {:cmd :divide, :dir (first filtered-dirs), 
                                        :function (create-amoeba-function most-energy-least-life-selector-2 true), 
                                        :data [is-traveler position], :child-data [true (next-position position (first filtered-dirs))] }
                                    )
                                    
                                )
                            
                                {:cmd :rest, :data [is-traveler position]}
                            )

                            {:cmd :move, :dir (first filtered-dirs), :data [is-traveler (next-position position (first filtered-dirs))] }
                        )
                    )
                )
            )
        )
    )
)

(defn create-amoeba-function
    [target-selector is-traveler]

    (fn [energy health species env data]

        (
            let [
                new-data (if (nil? data)
                    [is-traveler [0 0]]
                    data
                )
            ]

            (greedy-strategy energy health species env new-data target-selector)
        )
    )
)

(def Evam (create-amoeba-function most-energy-least-life-selector-2 false))
Footer
© 2023 GitHub, Inc.
Footer navigation
Terms
Privacy
Security
Status
Docs
Contact GitHub
Pricing
API
Training
Blog
About
Gada-var-amoba/core.clj at clojure · ArvidNorinder/Gada-var-amoba
