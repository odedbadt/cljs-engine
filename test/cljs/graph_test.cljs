; (ns ^:figwheel-always cljs-polytopes.graph-test
;   (:require
;     [cljs.test :refer-macros [deftest testing is run-tests]]
;     [cljs-polytopes.graph :as graph]
;     [figwheel.client :as fw]))

; (enable-console-print!)

; (deftest test-duplicate-and-transform-graph
;   (is (=
;   {
;     0 ["A" true [1 2]]
;     1 ["B" true [0 2]]
;     20 ["C" true [0 1]]
;     21 ["tA" true [22 23]]
;     22 ["tB" true [21 23]]
;     41 ["tC" true [21 22]]
;   }

;   (graph/duplicate-and-transform-graph
;   {
;     0 ["A" true [1 2]]
;     1 ["B" true [0 2]]
;     20 ["C" true [0 1]]
;   }
;   "t")
;   )))

; (deftest test-apply-word-rules
;  (is (=
;   (graph/apply-word-rules "A" [])
;   "A"
;   ))
;  (is (=
;   (graph/apply-word-rules "AA" [["AA" "B"]])
;   "B"
;   ))
;  (is (=
;   (graph/apply-word-rules "AAA" [["AA" "A"]])
;   "A"
;   ))

;  )


; (deftest test-pairinduction
;  (is (= [2 2] (graph/pair-induction [nil 2])))
;  (is (= [2 2] (graph/pair-induction [2 nil])))
;  (is (= [2 3] (graph/pair-induction [2 3]))))

; (deftest test-flatten-equivalence-table
;   (is (= {0 1, 2, 3}
;     (graph/flatten-equivalence-table {0 1, 2 3})))
;   (is (= {0 2, 1 2}
;     (graph/flatten-equivalence-table {0 1, 1 2})))
;   (is (= {0 2, 1 2, 3 5}
;     (graph/flatten-equivalence-table {0 1, 1 2, 3 5})))
;   (is (= {0 7, 1 7, 2, 7}
;     (graph/flatten-equivalence-table {0 1, 1 2, 2, 7}))))

; (defn cyclic-equals [l1 l2]
;   (if (= (count l1) (count l2))
;     (let [c (count l1)
;       l2_0 (first l2)
;       value-matches (comp (partial = l2_0) second)
;       indexed-l1 (map-indexed vector l1)
;       [offset _] (first (filter value-matches indexed-l1))
;       ]
;       (if offset
;         (every? identity (take c
;           (map = (drop offset (cycle l1)) l2))))
;       )))

; (deftest test-common-cycle
;   (is (cyclic-equals [[1 3] [2 2] [4 5] [6 6]]
;    (second
;      (graph/common-cycle [true [1 2 4 6]] [true [3 2 5 6]]))))
;   (is (cyclic-equals [[6 6] [1 7] [2 8] [4 4]]
;    (second
;      (graph/common-cycle [true [1 2 4 6]] [true [6 7 8 4]]))))
;   (is (cyclic-equals [[1 1] [5 5] [10 10]]
;    (second
;      (graph/common-cycle [true [10 1 nil]] [true [1 5 nil]]))))
;   (is (= nil
;    (graph/common-cycle [true [1 2 4 6 9]] [true [6 7 8 4]])))
;   (is (= nil
;    (graph/common-cycle [false [1 2 4 6]] [true [6 7 8 4]])))
;   (is (= nil
;    (graph/common-cycle [true [8 2 4 6]] [true [6 7 8 4]])))



;   )

; (deftest test-merge-vertices
;   (is (=
;     [{
;       0 ["A" true [1]]
;       1 ["B" true [0]]
;       3 ["B" true [0]]
;     }
;     {3 1 2 0}]
;     (graph/merge-vertices
;       [{
;         0 ["A" true [1]]
;         1 ["B" true [0]]
;         2 ["A" true [3]]
;         3 ["B" true [2]]
;         } {3 1 2 0}]
;         [2 0])
;     )
;   )
;   )

; (deftest test-transform-and-redue-graph
;  (is (= {
;   21 20
;   41 0
; }
; (get (graph/transform-and-reduce-graph
; {
;   0 ["A" true [20]]
;   20 ["BA" true [0]]
; }
; "B"
; [["BBA" "A"] ["AA" "A"]]) 1)

; )
;  )
;  (is
;    (= {
;     0  ["A" true [20]]
;     20 ["BA" true [0]]
;     21 ["BA" true [41]]
;     41 ["BBA" true [21]]
;   }
;   (get (graph/transform-and-reduce-graph
;   {
;     0 ["A" true [20]]
;     20 ["BA" true [0]]
;   }
;   "B"
;   [["BBA" "A"] ["AA" "A"]]) 0)

;   )
;    )
;  )

; (deftest test-transform-reduce-and-merge-graph
;   (is (=
;   {
;     0 ["A" true [1]]
;     1 ["B" true [0]]
;   }
;   (graph/transform-reduce-and-merge-graph [] {0 ["A" true [1]] 1 ["B" true [0]] } "") )
;   ))

; (defn -index-of [l x] (.indexOf (apply array l) x))

; (defn normalise-cycle [members]
;   (if (or (empty? members)
;     (some nil? members))
;   members
;   (let [l (count members)
;     minimal (apply min members)
;     argmin (-index-of members minimal)
;     ]
;     (vec (take l (drop argmin (cycle members)))))))

; (defn -normalise-graph-structure [graph-structure]
;   (into {}
;     (for [[k [formula cyclic members]] graph-structure]
;       [k [formula cyclic (normalise-cycle members)]]
;       )))

; (deftest test-process-graph__cube
;   (is (=
;     {0 ["P" true [1, 3, 5]],
;     1 ["bP" true [0, 4, 2]],
;     2 ["bbP" true [1, 10, 3]],
;     3 ["bbbP" true [0, 2, 21]],
;     4 ["aaP" true [1, 5, 10]],
;     5 ["aP" true [0, 21, 4]],
;     10 ["baaP" true [2, 4, 21]],
;     21 ["bbaaP" true [3, 10, 5]]}
;     (-normalise-graph-structure
;      (graph/process-graph "bbb"
;       {0 ["P" true [1, 3, 5]]
;       1 ["bP" true [2, 0, 4]]
;       2 ["bbP" true [3, 1, nil]]
;       3 ["bbbP" true [0, 2, nil]]
;       4 ["aaP" true [1, 5, nil]]
;       5 ["aP" true [4, 0, nil]] }
;       [["aaaa", ""] ["bbbb", ""] ["abP", "P"]] )))))

; (deftest test-process-graph__dodecahedron
;   (is (=
;         (-normalise-graph-structure
;           {
;            0 ["P" true [1 9 4]],
;            1 ["bP" true [0 2 18]],
;            225 ["abbaabbP" true [187 829 75]],
;            2 ["bbP" true [3 37 1]],
;            3 ["bbbP" true [2 4 74]],
;            4 ["bbbbP" true [0 7 3]],
;            36 ["baabbP" true [37 414 18]],
;            37 ["baabbbP" true [36 2 112]],
;            7 ["abbP" true [8 75 4]],
;            8 ["abbbP" true [7 9 187]],
;            9 ["abbbbP" true [0 17 8]],
;            74 ["abaabbP" true [75 112 3]],
;            75 ["abaabbbP" true [74 7 225]],
;            112 ["bbaabbP" true [74 829 37]],
;            17 ["aabbP" true [18 188 9]],
;            18 ["aabbbP" true [1 36 17]],
;            187 ["aabaabbP" true [188 225 8]],
;            188 ["aabaabbbP" true [414 187 17]],
;            829 ["bbaabaabbbP" true [225 414 112]],
;            414 ["baabaabbbP" true [829 188 36]]})
;        (-normalise-graph-structure
;          (graph/process-graph "aaaababababbbbbaaaaa"
;                               {0 ["P" true [4, 1, nil]]
;                                1 ["bP" true [0, 2, nil]]
;                                2 ["bbP" true [1, 3, nil]]
;                                3 ["bbbP" true [2, 4, nil]]
;                                4 ["bbbbP" true [3, 0, nil]]}
;                               [["bbbbP", "abP"] ["bbbbb", ""] ["aP", "P"]])))))


; (run-tests)

; ;; FW connection is optional in order to simply run tests,
; ;; but is needed to connect to the FW repl and to allow
; ;; auto-reloading on file-save
; (fw/start {
;  :websocket-url "ws://localhost:3449/figwheel-ws"
;            ;; :autoload false
;            :build-id "test"
;            })