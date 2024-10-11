(ns alfiehiscox.core
  (:require [net.cgrand.enlive-html :as html]
            [alfiehiscox.database :as db]
            [clojure.string :as str])
  (:import java.net.URL))

(def base "https://www.goodreads.com")

(defn fetch-url [url]
  (html/html-resource (URL. url)))

;; Getting titles from list pages
(defn get-list-titles [content]
  (map (fn [elem] {:link (str base (get-in elem [:attrs :href]))
                   :name (html/text elem)})
       (html/select content [:.listTitle])))

(defn get-next-page-link [content]
  (first (map #(get-in % [:attrs :href])
              (html/select content [:.next_page]))))

(get-list-titles (fetch-url (str base "/list/tag/fantasy")))

(get-next-page-link (fetch-url (str base "/list/tag/fantasy?page=100")))

(defn get-all-list-titles [limit sleep]
  (loop [url' (str base "/list/tag/fantasy")
         c 0]
    (Thread/sleep sleep)
    (if (or (nil? url')
            (>= c limit))
      nil
      (let [content (fetch-url url')
            titles (get-list-titles content)
            next-url (get-next-page-link content)]
        (db/add-all-links titles)
        (recur (str base next-url)
               (inc c))))))

(comment (get-all-list-titles 100 2000))

;; Getting Books from List Page 

(comment (get-all-book-info 0 0))

(def test-link (:list_links/url (second (db/get-list-links))))
(def test-content (fetch-url test-link))

(defn get-book-titles [content]
  (map html/text
       (html/select content [:.bookTitle :span])))

(defn get-book-authors [content]
  (map html/text
       (html/select content [:.authorName :span])))

(defn clean-rating [rating]
  (try
    (-> rating
        html/text
        str/trim
        (str/split #" avg")
        first
        (str/replace #"[a-zA-z]" "")
        str/trim
        Float/parseFloat)
    (catch Exception _ nil)))

(defn clean-rating-count [rating]
  (-> rating
      html/text
      str/trim
      (str/split #" — ")
      second
      (str/replace #"[a-zA-z,]" "")
      str/trim
      Integer/parseInt))

(defn get-book-rating [content]
  (map clean-rating
       (html/select content [:.minirating])))

(defn get-book-rating-count [content]
  (map clean-rating-count
       (html/select content [:.minirating])))

(defn get-book-info [content]
  (let [titles (get-book-titles content)
        authors (get-book-authors content)
        ratings (get-book-rating content)
        rating-counts (get-book-rating-count content)]
    (if (not (= (count titles) (count authors) (count ratings) (count rating-counts)))
      (throw (.Exception "unequal selects"))
      (loop [infos []
             [title & r_titles] titles
             [author & r_authors] authors
             [rating & r_ratings] ratings
             [rating-count & r_rating-counts] rating-counts]
        (if (nil? r_titles)
          infos
          (recur (conj infos {:title title
                              :author author
                              :rating rating
                              :rating-count rating-count})
                 r_titles
                 r_authors
                 r_ratings
                 r_rating-counts))))))

(defn get-all-book-info [limit sleep]
  (let [links (map :list_links/url (db/get-list-links))]
    (loop [links' links
           c 0]
      (Thread/sleep sleep)
      (if (or (empty? links')
              (>= c limit))
        nil
        (let [content (fetch-url (first links'))
              next-url (get-next-page-link content)
              book-info (get-book-info content)]
          (db/add-all-books book-info)
          (if (nil? next-url)
            (do (println (str "Finishing list: " (first links')))
                (recur (rest links') (inc c)))
            (do (println (str "Moving to next page: " (str base next-url)))
                (recur (conj (rest links') (str base next-url)) (inc c)))))))))

(comment (get-book-titles test-content)
         (get-book-authors test-content)
         (get-book-info test-content))

(comment (get-all-book-info 10000 500))
