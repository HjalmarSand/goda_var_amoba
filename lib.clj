(ns amoebas.competition2021.Gada-var-amoba.lib2
	(:use amoebas.defs amoebas.lib amoebas.run)
)

(defn greedy-strategy-dense ;; dense greedy strategy, too dense (good for chaos fighting, bad for sustain)
    [energy health species env data]

    (let
        [
            max-total-fuel (apply max (for [dir Dirs] (total-fuel (Env-Sections dir) env)))
            max-friendlies (apply max (for [dir Dirs] (count (friendlies species (Env-Sections dir) env))))
            dirs-sorted (if (= max-total-fuel 0)
                (sort-by #(/ (count (friendlies species (Env-Sections %) env)) (float max-friendlies)) Dirs)

                (sort-by #(-
                    (* (/ (count (friendlies species (Env-Sections %) env)) (float max-friendlies)) 1.0)
                    (/ (total-fuel (Env-Sections %) env) (float max-total-fuel)))
                Dirs)
            )
            filtered-dirs (filter #(cell-empty? env (nth Neighbors %)) dirs-sorted)

            fuel-here (:fuel (env Here))

            neighboring-enemies (hostiles species Neighbors env)
            enemy-to-hit (most-energy-target-selector-2 neighboring-enemies species env)
        ]
    
        (do
            ;; Just nu går vi och dividerar även om det är en fiende som kommer att vara i vår range efter förflyttningen
            (if-not (nil? enemy-to-hit)
                {:cmd :hit, :dir (Neighbor-To-Dir enemy-to-hit)}   ;; KAPOW! med eventuell defensiv 
                
                (if (empty? filtered-dirs)
                    {:cmd :rest}
                    
                    (if (> fuel-here MIN-FUEL-TO-DIVIDE)
                        {:cmd :divide, :dir (first dirs-sorted)}
                        {:cmd :move, :dir (first dirs-sorted)}
                    )
                )
            )
        )
    )
)
