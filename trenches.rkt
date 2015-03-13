#lang racket

(provide trenches
         bounds
         bounds-utm
         plot-trenches)

(require json)
(require net/url)
(require net/uri-codec)
(require pict)
(require racket/draw)
(require data/utm)

;; (: sparql-url String)
(define sparql-url "http://rdf.muninn-project.org/sparql")

;; (: sparql-default-graph-uri String)
(define sparql-default-graph-uri "http://rdf.muninn-project.org/")

;; (: sparql-format String)
(define sparql-format "json")

;; (: sparql-timeout Number)
(define sparql-timeout 30000)

;; (: sparql-debug String)
(define sparql-debug "on")

;; (: trench-depth Number)
(define trench-depth -1)

;; (: make-sparql-query-url (-> String String))
(define (make-sparql-query-url query)
  (string-append sparql-url
                 "?"
                 "default-graph-uri="
                 (uri-encode sparql-default-graph-uri)
                 "&query="
                 (uri-encode query)
                 "&format="
                 (uri-encode sparql-format)
                 "&timeout="
                 (number->string sparql-timeout)
                 "&debug="
                 sparql-debug))

;; (: get/json (-> String JSExpr))
(define (get/json str)
  (call/input-url (string->url str)
                  get-pure-port
                  read-json))

;; (: make-objects-query (-> Real Real Real Real String))
(define (make-objects-query min-long min-lat max-long max-lat)
  (string-append "SELECT DISTINCT ?thing ?state "
    "{ "
    "  ?thing <http://geovocab.org/geometry#geometry> ?geoma . "
    "  ?thing <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>  <http://rdf.muninn-project.org/ontologies/military#MilitaryTrench> . "
    "  ?geoma <http://linkedgeodata.org/ontology/posSeq> ?SEQ . "
    "  ?SEQ <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/1999/02/22-rdf-syntax-ns#Seq> . "
    "  ?SEQ ?List ?node . "
    "  ?node <http://www.w3.org/2003/01/geo/wgs84_pos#lat> ?LAT . "
    "  ?node <http://www.w3.org/2003/01/geo/wgs84_pos#long> ?LONG . "
    " OPTIONAL {?thing <http://rdf.muninn-project.org/ontologies/graves#hasState> ?state . } "
    " FILTER (?LAT < "
    (number->string max-lat)
    " && ?LAT > "
    (number->string min-lat)
    " ) "
    " FILTER (?LONG < "
    (number->string max-long)
    "  && ?LONG > "
    (number->string min-long)
    " ) "
    "}"))

;; (: things (-> Real Real Real Real (Listof String)))
(define (things min-long min-lat max-long max-lat)
  (let* ([response (get/json (make-sparql-query-url (make-objects-query min-long min-lat max-long max-lat)))]
         [bindings (hash-ref (hash-ref response 'results) 'bindings)])
    (map (λ (binding)
            (hash-ref (hash-ref binding 'thing) 'value))
         bindings)))

;; (: make-trenches-query (-> String Real Real Real Real String))
(define (make-trenches-query trench min-long min-lat max-long max-lat)
  (string-append "SELECT ?List  ?LAT  ?LONG "
    "{ "
    "  <" trench "> <http://geovocab.org/geometry#geometry> ?geoma . "
    "  <" trench "> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>  <http://rdf.muninn-project.org/ontologies/military#MilitaryTrench> . "
    "  ?geoma <http://linkedgeodata.org/ontology/posSeq> ?SEQ . "
    "  ?SEQ <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/1999/02/22-rdf-syntax-ns#Seq> . "
    "  ?SEQ ?List ?node . "
    "  ?node <http://www.w3.org/2003/01/geo/wgs84_pos#lat> ?LAT . "
    "  ?node <http://www.w3.org/2003/01/geo/wgs84_pos#long> ?LONG . "
    "  FILTER (?LAT < "
    (number->string max-lat)
    " || ?LAT > "
    (number->string min-lat)
    " || ?LONG < "
    (number->string max-long)
    " || ?LONG > "
    (number->string min-long)
    " ) "
    "} "
    "ORDER BY ASC(?List)"))

;; (: trench (-> Real Real Real Real String (Listof (Vector Number Number))))
(define (trench min-long min-lat max-long max-lat)
  (λ (trench)
     (let* ([response (get/json (make-sparql-query-url (make-trenches-query trench min-long min-lat max-long max-lat)))]
            [bindings (hash-ref (hash-ref response 'results) 'bindings)])
     (map (λ (binding)
             (vector (string->number (hash-ref (hash-ref binding 'LONG) 'value))
                     (string->number (hash-ref (hash-ref binding 'LAT) 'value))))
          bindings))))

;; (: trenches (-> String Real Real Real Real (Listof (Listof (Vector Number Number)))))
(define (trenches min-long min-lat max-long max-lat)
  (let ([things (things min-long min-lat max-long max-lat)])
    (map (trench min-long min-lat max-long max-lat)
         things)))

;; (: bounds (-> (Listof (Listof (Vector Number Number)) (Vector Number Number Number Number))))
(define (bounds trenches)
  (let* ([flattened-trenches (flatten trenches)]
         [xs (map (λ (point)
                     (vector-ref point 0))
                  flattened-trenches)]
         [ys (map (λ (point)
                     (vector-ref point 1))
                  flattened-trenches)])
    (vector (apply min xs)
            (apply min ys)
            (apply max xs)
            (apply max ys))))

;; (: bounds-utm (-> (Listof (Listof (Vector Number Number))) (Vector Integer Integer Integer Integer)))
(define (bounds-utm trenches)
  (let* ([bounds-wgs84 (bounds trenches)]
         [min-long (vector-ref bounds-wgs84 0)]
         [min-lat  (vector-ref bounds-wgs84 1)]
         [max-long (vector-ref bounds-wgs84 2)]
         [max-lat  (vector-ref bounds-wgs84 3)]
         [min-utm  (utm min-long min-lat)]
         [max-utm  (utm max-long max-lat)])
    (vector (round (inexact->exact (vector-ref min-utm 0)))
            (round (inexact->exact (vector-ref min-utm 1)))
            (round (inexact->exact (vector-ref max-utm 0)))
            (round (inexact->exact (vector-ref max-utm 1))))))

;; (: plots-trenches (-> (Listof (Listof (Vector Number Number))) Pict))
(define (plot-trenches trenches)
  (let* ([bounds-utm-trenches (bounds-utm trenches)]
         [min-x (vector-ref bounds-utm-trenches 0)]
         [min-y (vector-ref bounds-utm-trenches 1)]
         [max-x (vector-ref bounds-utm-trenches 2)]
         [max-y (vector-ref bounds-utm-trenches 3)]
         [width (- max-x min-x)]
         [height (- max-y min-y)]
         [get-x (λ (point-wgs84)
                   (- max-x
                      (vector-ref (utm (vector-ref point-wgs84 0)
                                       (vector-ref point-wgs84 1))
                                  0)))]
         [get-y (λ (point-wgs84)
                   (- max-y
                      (vector-ref (utm (vector-ref point-wgs84 0)
                                       (vector-ref point-wgs84 1))
                                  1)))])
    (dc (λ (dc dx dy)
           (for ([trench trenches])
             (for ([point-wgs84 trench])
               (send dc
                     draw-point
                     (get-x point-wgs84)
                     (get-y point-wgs84)))))
        width height)))

;; (: plot-trenches->file (-> String (Listof (Listof (Vector Number Number))) Void))
(define (plot-trenches->file path trenches)
  (send (pict->bitmap
          (scale (colorize (linewidth 5 (plot-trenches trenches)) "red") 0.5))
        save-file
        path
        'png))
