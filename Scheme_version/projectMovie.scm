#lang racket

;; read-f: String -> List
;; Reads a CSV file and returns a list of lists, where each list represents a row in the CSV.
;; Skips the header line and splits each row by commas.
(define (read-f filename)
  (call-with-input-file filename
    (lambda (input-port)
      (let loop ((line (read-line input-port)))
        (cond 
          ((eof-object? line) '())  ; Return an empty list when EOF is reached
          ((string=? (string-trim line) "userId,movieId,rating,timestamp") 
           (loop (read-line input-port)))  ; Skip the header line
          (#t (cons (string-split line ",") (loop (read-line input-port)))))))) ) ; Process the data rows

;; convert-rating: List -> List
;; Converts a CSV row (userId, movieId, rating) into a list containing userId, movieId, and a boolean indicating 
;; whether the movie was liked based on the rating. If rating < 3.5, movie is considered disliked.
(define (convert-rating L)
  (list (string->number (car L)) 
        (string->number (cadr L)) 
        (< 3.5 (string->number (caddr L)))))  ; Return empty list if rating is invalid

;; Ratings: List
;; Generate a list of ratings by applying convert-rating to each row in the CSV file.
(define Ratings
  (map convert-rating
       (read-f "ratings.csv")))

;; add-rating: (List Number Number Boolean) List -> List
;; Adds a new rating to the list of users. If the user already exists, the rating is added to their liked or disliked movie lists.
(define (add-rating rating list-of-users)
  (let ((user-id (car rating))             ; Extract the user ID
        (movie-id (cadr rating))           ; Extract the movie ID
        (liked? (caddr rating)))           ; Extract the liked? value
    (define (update-user user)
      (if (= (car user) user-id)           ; Check if user ID matches
          (if liked?                       ; If the movie is liked
              (list (car user)            ; Keep the user ID
                    (cons movie-id (cadr user)) ; Add movie to liked movies
                    (caddr user))          ; Keep disliked movies unchanged
              (list (car user)            ; Keep the user ID
                    (cadr user)           ; Keep liked movies unchanged
                    (cons movie-id (caddr user)))) ; Add movie to disliked movies
          user))                           ; Return unchanged user if IDs don't match

    (define (find-user user-id users)
      (cond ((null? users) #f)             ; If the list is empty, return #f
            ((= (car (car users)) user-id) (car users)) ; If user found, return the user
            (else (find-user user-id (cdr users))))) ; Recur on the rest of the list

    (if (null? list-of-users)              ; If no users, create a new user
        (list (list user-id                ; Create new user entry
                    (if liked?            ; Initialize liked or disliked movies
                        (list movie-id) 
                        '())
                    (if liked? '()       ; Initialize disliked movies
                        (list movie-id))));; Handle existing users
        (let ((existing-user (find-user user-id list-of-users))) ; Check if the user already exists
          (if existing-user
              (map update-user list-of-users) ; Update the existing user
              (cons (list user-id            ; Add new user if not found
                          (if liked? 
                              (list movie-id) 
                              '())
                          (if liked? '() 
                              (list movie-id))) 
                    list-of-users)))))) ; Add to the front of the user list

;; add-ratings: List List -> List
;; Applies add-rating to each rating in the list of ratings, updating the list of users.
(define (add-ratings list-of-ratings list-of-users)
  ;; Filter out empty ratings before applying foldl
  (foldl add-rating list-of-users (filter (lambda (rating) (not (null? rating))) list-of-ratings)))

;; get-user: Number List -> List
;; Finds and returns a user from the list of users by their ID.
(define (get-user ID list-of-users)
  (define (find-user user-id users)
    (cond ((null? users) #f)                       ; If the list is empty, return #f
          ((= (car (car users)) user-id) (car users)) ; If user found, return the user
          (else (find-user user-id (cdr users)))))  ; Recur on the rest of the list

  (find-user ID list-of-users))                    ; Search for the user in the list

; Define the global list-of-users
(define users (add-ratings Ratings '())) 

;; similarity: Number Number -> Real
;; Computes the Jaccard similarity between two users based on their liked and disliked movies.
(define (similarity ID1 ID2)
  ;; Fetch the user data from the provided list-of-users (global variable)
  (define user1 (get-user ID1 users))
  (define user2 (get-user ID2 users))

  ;; Check if both users exist
  (if (or (not user1) (not user2))
      0.0  ;; If one of the users doesn't exist, return 0.0 (no similarity)
      (let* ((liked1 (cadr user1))          ; List of liked movies for user1
             (disliked1 (caddr user1))      ; List of disliked movies for user1
             (liked2 (cadr user2))          ; List of liked movies for user2
             (disliked2 (caddr user2))      ; List of disliked movies for user2
             
             ;; Define intersection function to get common elements in two lists
             (intersection (lambda (list1 list2)
                             (filter (lambda (x) (member x list2)) list1)))
             
             ;; Calculate intersections
             (liked-intersection (intersection liked1 liked2))
             (disliked-intersection (intersection disliked1 disliked2))
             
             ;; Define union function to merge lists with unique elements
             (union (lambda (list1 list2)
                      (append list1 (filter (lambda (x) (not (member x list1))) list2)))) )

        ;; Calculate similarity score based on Jaccard index and return as floating-point
        (/ (+ (length liked-intersection)
              (length disliked-intersection))
           (exact->inexact (length (union (union liked1 liked2) (union disliked1 disliked2))))))))

;; Example usage:
(similarity 33 88)
