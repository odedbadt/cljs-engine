(ns cljs-engine.shapes

  )

(def dodecahedron {
  :graph {
         0 [1 2 3]
         1 [0 4 9]
         2 [0 5 6]
         3 [0 7 8]
         4 [1 5 11]
         5 [2 4 12]
         6 [2 7 13]
         7 [3 6 14]
         8 [3 9 15]
         9 [1 8 10]
         10 [18 11 9]
         11 [16 10 4]
         12 [16 13 5]
         13 [17 12 6]
         14 [17 15 7]
         15 [18 14 8]
         16 [11 12 19]
         17 [13 14 19]
         18 [15 10 19]
         19 [18 17 16]
     }
   :coordinates [[ 0.24705928,  0.4766381 ,  0.30303984],
       [ 0.25771811,  0.4516372 ,  0.59976109],
       [ 0.9577531 ,  0.62589895,  0.44141287],
       [ 0.28058833,  0.43076192,  0.47830562],
       [ 0.37359114,  0.68586783,  0.67702713],
       [ 0.61101481,  0.09306838,  0.12749731],
       [ 0.70520045,  0.14806459,  0.11463999],
       [ 0.98811219,  0.10108484,  0.01119318],
       [ 0.24300478,  0.08382609,  0.49403568],
       [ 0.79515806,  0.01575166,  0.37396893],
       [ 0.7733236 ,  0.03286442,  0.0792101 ],
       [ 0.09219871,  0.25648626,  0.4098212 ],
       [ 0.95580973,  0.88799078,  0.82556765],
       [ 0.13379712,  0.82357617,  0.23083745],
       [ 0.87212817,  0.61829753,  0.58407831],
       [ 0.06001029,  0.64604954,  0.05688719],
       [ 0.3594946 ,  0.78900723,  0.67123488],
       [ 0.2876131 ,  0.65386749,  0.98819945],
       [ 0.89339847,  0.59496589,  0.30080013],
       [ 0.23299259,  0.94331295,  0.74571915]]
   })
