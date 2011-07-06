
(ns semco
  "semco - calculate topic semantic coherence scores as described in
Mimno et al (EMNLP 2011)

David Andrzejewski (andrzejewski1@llnl.gov)

Optimizing Semantic Coherence in Topic Models. David Mimno, Hanna
Wallach, Edmund Talley, Miriam Leenders, Andrew McCallum. EMNLP,
2011, Edinburgh, Scotland."
  (:use [clojure.contrib.generic.math-functions :only (log)])
  (:use [clojure.set :only (intersection)])
  (:use [clojure.contrib.combinatorics :only (combinations)]))


;; Record used for topic representation (words and counts)
(defrecord Topic [topic words wordset counts cocounts])

(defn make-topic
  "Helper function to construct Topic record"
  [topic words]
  (Topic. topic words (set words) {} {}))

(defn make-topics
  "Construct Topic records from a seq of word seqs"
  [topicwords]
  (doall (map #(make-topic %1 %2) (-> topicwords count range) topicwords)))
         
(defn update-counter
  "Update counter cts with observed events xs" 
  [cts xs]
  (merge-with + cts (frequencies xs)))

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
  (doall (map (partial update-topic document) topics)))

(defn merge-topic
  "Merge two sets of observed counts for the same topic"
  [topic1 topic2]
  (assoc topic1
    :counts (merge-with + (:counts topic1) (:counts topic2))
    :cocounts (merge-with + (:cocounts topic1) (:cocounts topic2))))

(defn merge-topics
  "Merge two sets of observed counts for all topics"
  [topics1 topics2]
  (doall (map merge-topic topics1 topics2)))
  
(defn get-counts
  "Get counts for all topics over all docs (doall to avoid stackoverflow!)"
  [topics documents]
  (reduce merge-topics (pmap (partial count-document topics) documents)))

(defn topic-words
  "Nested word pairs for a topic (order important due to eq(1) from paper)"
  [topic]
  (for [word1 (:words topic)
        word2 (take-while (partial not= word1) (reverse (:words topic)))]
    [word1 word2]))

(defn score-pair
  "Semantic coherence score term for a single word pair"
  [topic [word1 word2]] ;; {:pre [(contains? (:counts topic) word1)]}
  (log (/ (-> topic :cocounts (get ,,, (set [word1 word2]) 0) inc double)
          (-> topic :counts (get ,,, word1 1))))) ;; shouldn't need this...

(defn score-topic
  "Calculate semantic coherence score for a single topic"
  [topic]
  (reduce + (map (partial score-pair topic) (topic-words topic))))             

(defn build-result
  "Construct final result map: topic index, words, and semantic score"
  [topic]
  (assoc (select-keys topic [:topic :words]) :semco (score-topic topic)))

(defn semantic-coherence
  "Externally called fucntion to do full semco analysis: each topic is
a seq of words, each document is a seq of words"
  [topicwords documents]
  (map build-result (get-counts (make-topics topicwords) documents)))
