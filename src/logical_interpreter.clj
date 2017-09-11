(ns logical-interpreter
	(:require [clojure.string :as str]
		  	  [clojure.java.io :as io])
)

(require '[clojure.string :as str])
(import 'java.util.regex.Pattern)
		  	
;-----------------------------------------------------------------;
(defn hasParentheses
  "Check if the query has parentheses"
  [query] 
  (and (str/includes? query "(") (str/includes? query ")")  )
)

(defn hasDot
	"Check if the line has dot"
	[x]
	(str/includes? x ".")
)

(defn moreThanOneFact
	"Return true if exist more than one fact"
	[fact]
	(str/includes? fact "),")
)

(defn addSpace
  "Add space between string and parentheses"
  [query]
  (str/replace  query #"[(]" " (")
)

(defn isCorrect
	"Check if database is correct"
	[dbList]
	(every? true? (map hasDot dbList) )
)

(defn isValid
	"Check if the query is valid"
	[query]
	(if (hasParentheses query) true
	 false)
)

(defn convertToList
  "Convert database to list"
	[db]
	(remove empty? (str/split-lines db))
)

(defn isAFactOrIsARule
	"Decide if is a fact or if is a rule"
	[x]
	(if (= (first(re-seq #":-" x)) ":-") (str "isARule " (first(re-seq #"^[^(]+"  x))) 
	 (str "isAFact " (first(re-seq #"^[^(]+"  x))))
)

(defn isAFactOrIsARuleList
	"Generate a list with decision: isAFact or isARule"
	[databaseList]
	(distinct (map isAFactOrIsARule databaseList))
)

(defn strip [coll chars]
  "Delete some caracters"
  (apply str (remove #((set chars) %) coll))
)

(defn selectOnlyRules
	"If is not a rule, space"
	[x]
	(if (= (first(re-seq #":-" x)) ":-") x 
	    "")
)

(defn rulesProcessing
	"Select rules from database"
	[dbList]
	(remove empty? (distinct (map selectOnlyRules dbList)))
)

(defn analyseFact
	"Check if query is in database"
	[query dbList]
	(if (= (some #{query} dbList) query) true 
	    false)
)

(defn getFirstWord
  "Get first word of query"
  [query]
  (subs query 0 (str/index-of query " "))
)

(defn getNames
  "Get names after fact name"
  [query]
  (str/split (subs query ( +(str/index-of query " ") 1 ) ) #" ")
)

(defn getFactsFromRules
  "Get facts from rules"
  [rulesList]
  (subs (first rulesList) (+ (str/index-of (first rulesList) ":") 3))
)

(defn getVariablesFromRules
	"Return variables specify from rules"
	[rules fact]
	(if (= fact (subs rules 0 (str/index-of rules "(")))
	       (subs rules (+(str/index-of rules "(")1)   (str/index-of rules ")"))
	 nil
	)
)

(defn getVariables
  "Get variables of facts"
  [fact rulesList]
  (str/split (first (remove nil? (map #(getVariablesFromRules % fact) rulesList))) #", ")
)

(defn getRelationship
	"Return the correct relation"
	[factsFromRules factNames variables]	
	(if (> (count variables) 0) 
	    (do 
	        (getRelationship (str/replace factsFromRules (first variables) (first factNames))
	                                                       (rest factNames)  (rest variables))
	    ) 
	    factsFromRules)
)

(defn applyRule
	"Return the query with rule applied"
	[query rulesList]
	(let [fact (getFirstWord query)
		    factsFromRules (getFactsFromRules rulesList)
		    factNames (getNames query)
		    variables (getVariables fact rulesList)]
	   (getRelationship factsFromRules factNames variables)
	)
)

(defn pattern
  "Replace comma - semicolon"
  [comma semicolon]
  (str/replace comma (re-pattern (apply str (interpose "|" (map #(Pattern/quote %) (keys semicolon)))))  semicolon))


(defn checkFacts
	"If exist a lot of facts, split and analyse"
	[facts dbList]
	(let [listaFacts (str/split (pattern facts {"), " ");"}) #";")]
		(every? true? (map #(analyseFact % dbList) listaFacts))
	)
)


(defn analyseRule
	"Check if the rule is in database"
	[query dbList rulesList]
	(let [facts (applyRule (strip (addSpace query) ".,()") rulesList)]
  		(if (moreThanOneFact facts) (checkFacts facts dbList) 
  	    	(analyseFact facts dbList))
	)
)

(defn analyseQuery
	"Return true if query is fine"
	[query dbList rulesAndFactsList rulesList]
	(if (boolean (some #{(str "isAFact " (first(re-seq #"^[^(]+" query)))} rulesAndFactsList)) (analyseFact query dbList)    
	    (analyseRule query dbList rulesList))
)

(defn removeDots
  "Remove dots from database"
  [database]
  (str/split-lines (str/replace database #"[\t.]" ""))
)

(defn evaluate-query
	"Returns true if the rules and facts in database imply query, false if not. If either input can't be parsed, returns nil"
	[database query]
	(if (isValid query)
  	(let [ db (convertToList database) ]
      (if (isCorrect db) 
  	     (let [ dbList (removeDots database)
  		          rulesAndFactsList (isAFactOrIsARuleList dbList)
  		          rulesList (rulesProcessing dbList)
  			      ]
  			      (analyseQuery query dbList rulesAndFactsList rulesList)
  	     )
  		nil)
  	) 
	  nil )
)