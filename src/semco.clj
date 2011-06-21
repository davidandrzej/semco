;;
;; Topic semantic coherence score (Mimno et al, EMNLP 2011)
;;
(ns semco
  (:use [clojure.contrib.generic.math-functions :only (log)])
  (:use [clojure.set :only (intersection)])
  (:use [clojure.contrib.combinatorics :only (combinations)]))

;;
;; Topic representation (words and counts)
;;
(defrecord Topic [topic words wordset counts cocounts])

(defn make-topic
  "Helper function to construct Topic record"
  [topic words]
  (Topic. topic words (set words) {} {}))

(defn make-topics
  "Construct Topic records from a seq of word seqs"
  [topicwords]
  (map #(make-topic %1 %2) (-> topicwords count range) topicwords))
         
;;
;; Count document co-occurrence
;;
(defn update-counter [cts xs] (merge-with + cts (frequencies xs)))
(defn update-topic
  "Update topic counts given observed words in a single document"
  [words topic]
  (let [overlap (intersection (set words) (:wordset topic))]
    (assoc topic
      :counts (update-counter (:counts topic) overlap)
      :cocounts (update-counter (:cocounts topic)
                                (map set (combinations overlap 2))))))

(defn count-document
  "Given a docment, update all the topic counts"
  [topics document]
  (map (partial update-topic document) topics))

(defn get-counts
  "Get counts for all topics over all documents"
  [argtopics argdocuments]
  (loop [topics argtopics documents argdocuments]         
    (if (empty? documents) topics      
        (recur (count-document topics (first documents)) (rest documents)))))

;;
;; Given counts, compute semantic coherence score
;; 
(defn topic-words
  "Nested word pairs for a topic (order important due to eqn 1 from paper)"
  [topic]
  (for [word1 (:words topic)
        word2 (take-while (partial not= word1) (reverse (:words topic)))]
    [word1 word2]))

(defn score-pair
  "Semantic coherence score term for a single word pair"
  [topic [word1 word2]]
  (log (/ (-> topic :cocounts (get ,,, (set [word1 word2]) 0) inc double)
          (-> topic :counts (get ,,, word1)))))

(defn score-topic
  "Calculate semantic coherence score for a single topic"
  [topic]
  (reduce + (map (partial score-pair topic) (topic-words topic))))             

;;
;; All-in-one function
;;
(defn build-result
  "Construct final result: topic index, words, and semantic score"
  [topic]
  (assoc (select-keys topic [:topic :words]) :semco (score-topic topic)))
    
(defn semantic-coherence
  "Each topic is a seq of words, each document is a seq of words"
  [topicwords documents]
  (map build-result (get-counts (make-topics topicwords) documents)))
       
