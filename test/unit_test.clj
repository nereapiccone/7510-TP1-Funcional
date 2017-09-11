(ns unit_test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))


(def rule-query "hijo(X, Y) :- varon(X), padre(Y, X)")

(deftest is-a-rule-test 
  (testing "should be true"
    (is (= (selectOnlyRules rule-query) "hijo(X, Y) :- varon(X), padre(Y, X)"))
  )
)
;-----------------------------------------------------------------;
;-----------------------------------------------------------------;
(def query-with-dot "varon(pepe).")

(deftest hasDot-test 
  (testing "string has dot"
    (is (= (hasDot query-with-dot) true))
  )
)

(def query-with-no-dot "varon(pepe)")

(deftest hasDot-test-negative 
  (testing "string has dot shoud be false"
    (is (= (hasDot query-with-no-dot) false))
  )
)

;-----------------------------------------------------------------;
;-----------------------------------------------------------------;

(def db-list ["varon(juan)" "hijo(X, Y) :- varon(X), padre(Y, X)"])

(deftest rules-selection-test 
  (testing "selection of rulesfrom query"
    (is (= (rulesProcessing db-list) ["hijo(X, Y) :- varon(X), padre(Y, X)"]))
  )
)
;-----------------------------------------------------------------;
;-----------------------------------------------------------------;
(def database-with-dots "varon(juan).")

(deftest removeDots-test 
  (testing "removeDots should be true"
    (is (= (removeDots database-with-dots) ["varon(juan)"]))
  )
)

;-----------------------------------------------------------------;
;-----------------------------------------------------------------;
(def query "hijo(pepe, juan)")

(def rules ["hijo(X, Y) :- varon(X), padre(Y, X)"])

(deftest applyRule-test 
  (testing "apply rule should be true"
    (is (= (applyRule "hijo pepe juan" rules) "varon(pepe), padre(juan, pepe)"))
  )
)
;-----------------------------------------------------------------;
;-----------------------------------------------------------------;

(deftest getFirstWord-test
  (testing "get fist word of query" 
    (is (= (getFirstWord "hijo pepe juan") "hijo"))
  )
)
;-----------------------------------------------------------------;
;-----------------------------------------------------------------;

(deftest getNames-test
  (testing "get names of query" 
    (is (= (getNames "hijo pepe juan") ["pepe" "juan"]))
  )
)
;-----------------------------------------------------------------;
;-----------------------------------------------------------------;

(deftest getFactsFromRules-test
  (testing "get facts of query" 
    (is (= (getFactsFromRules rules) "varon(X), padre(Y, X)"))
  )
)
;-----------------------------------------------------------------;
;-----------------------------------------------------------------;

(deftest getVariablesFromRules-test
  (testing "get variables of query" 
    (is (= (getVariables "hijo" rules) ["X" "Y"]))
  )
)
;-----------------------------------------------------------------;
;-----------------------------------------------------------------;

(deftest getRelationship-test
  (testing "get relationship test" 
    (is (= (getRelationship "varon(X), padre(Y, X)" ["pepe" "juan"] ["X" "Y"]) "varon(pepe), padre(juan, pepe)"))
  )
)
;-----------------------------------------------------------------;
;-----------------------------------------------------------------;

(def string-without-space "hija(nerea)")

(deftest addSpace-test 
  (testing "should add space to string"
    (is (= (addSpace string-without-space) "hija (nerea)"))
  )
)
;-----------------------------------------------------------------;
;-----------------------------------------------------------------;
(def stringToStrip "hija (nerea)")

(deftest strip-test 
  (testing "string to strip"
    (is (= (strip stringToStrip ".,()") "hija nerea"))
  )
)
;-----------------------------------------------------------------;
;-----------------------------------------------------------------;
(def query-with-parentheses "varon(pepe)")

(deftest hasParentheses-test 
  (testing "string has parentheses"
    (is (= (hasParentheses query-with-parentheses) true))
  )
)
;-----------------------------------------------------------------;
;-----------------------------------------------------------------;
(def database-with-simple-fact "varon(juan)")

(deftest recognise-facts-test 
  (testing "recognise-facts should be true"
    (is (= (isAFactOrIsARule database-with-simple-fact) "isAFact varon"))
  )
)
;-----------------------------------------------------------------;
;-----------------------------------------------------------------;
(def database-with-simple-rule "hijo(X, Y) :- varon(X), padre(Y, X).")

(deftest recognise-rules-test 
  (testing "recognise-rules should be true"
    (is (= (isAFactOrIsARule database-with-simple-rule) "isARule hijo"))
  )
)
;-----------------------------------------------------------------;
;-----------------------------------------------------------------;
(def database-with-facts ["varon(juan)" "varon(pepe)" "varon(hector)" "varon(roberto)" "varon(alejandro)" "mujer(maria)"	"mujer(cecilia)"])
                        
(deftest recognise-facts-test 
  (testing "recognise-facts should be true"
    (is (= (isAFactOrIsARuleList database-with-facts) ["isAFact varon" "isAFact mujer" ]))
  )
)
;-----------------------------------------------------------------;
;-----------------------------------------------------------------;
(def database-with-rules ["hijo(X, Y) :- varon(X), padre(Y, X)"	"hija(X, Y) :- mujer(X), padre(Y, X)"])
                        
(deftest recognise-rules-test 
  (testing "recognise-rules should be true"
    (is (= (isAFactOrIsARuleList database-with-rules) ["isARule hijo" "isARule hija" ]))
  )
)