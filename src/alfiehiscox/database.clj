(ns alfiehiscox.database
  (:require [next.jdbc :as jdbc]))

(def db {:dbtype "sqlite" :dbname "database.db"})
(def ds (jdbc/get-datasource db))

(defn add-list-link [{:keys [name url]}]
  (let [query "INSERT INTO list_links (name, url)
		         VALUES (?, ?)"]
    (jdbc/execute-one! ds [query name url])))

(defn add-all-links [links]
  (doseq [link links]
    (add-list-link link)))

(defn get-list-links []
  (let [query "SELECT * FROM list_links"]
    (jdbc/execute! ds [query])))

(defn clear-links []
  (let [query "DELETE FROM list_links"]
    (jdbc/execute! ds [query])))

(comment (count (get-list-links))
         (clear-links))

;; Book queries 

(defn book-exist? [title]
  (let [query "SELECT * FROM books WHERE title = ?"]
    (not (empty? (jdbc/execute! ds [query title])))))

(book-exist? "test")

(defn get-all-books []
  (let [query "select * from books group by title"]
    (jdbc/execute! ds [query])))

(defn get-n-books [n]
  (let [query "select distinct * from books limit ?"]
    (jdbc/execute! ds [query n])))

(book-exist? (:books/title (first (get-n-books 10))))

(defn add-book [{:keys [title author rating rating-count]}]
  (let [query "INSERT INTO books (title, author, rating, rating_count) VALUES (?, ?, ?, ?)"]
    (if (book-exist? title)
      nil
      (jdbc/execute! ds [query title author rating rating-count]))))

(defn add-all-books [books]
  (doseq [book books]
    (add-book book)))

(defn clear-books []
  (jdbc/execute! ds ["DELETE FROM books"]))

(def global_avg
  (let [books (get-all-books)
        ratings (map :books/rating books)]
    (/ (reduce + ratings) (count ratings))))

(def min-rating-count 10000)

(defn calculate_bayesian_average
  [book]
  (let [rating (:books/rating book)
        rating-count (:books/rating_count book)]
    (+ (* (/ rating-count (+ rating-count min-rating-count))
          rating)
       (* (/ min-rating-count (+ rating-count min-rating-count))
          global_avg))))

(count (get-all-books))

(->> (get-all-books)
     (sort-by calculate_bayesian_average)
     last)

(comment
  (count (get-all-books))
  (clear-books))

