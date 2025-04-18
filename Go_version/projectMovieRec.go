//Etudiant: Onel Valery Mezil
//Numero Etudiant: 300260630
// Project CSI2120/CSI2520
// Winter 2025
// Robert Laganiere, uottawa.ca

package main

import (
	"encoding/csv"
	"fmt"
	"log"
	"os"
	"runtime"
	"sort"
	"strconv"
	"sync"
	"time"
)

// movies with rating greater or equal are considered 'liked'
const R float64 = 3.5

// K: Minimum number of users who must like a movie for it to be recommended
// N: Number of top recommendations to return
var K int = 10
var N int = 20

// Define the Recommendation type
type Recommendation struct {
	userID     int     // recommendation for this user
	movieID    int     // recommended movie ID
	movieTitle string  // recommended movie title
	score      float32 // probability that the user will like this movie
	nUsers     int     // number of users who likes this movie
}

// get the probability that this user will like this movie
func (r Recommendation) getProbLike() float32 {
	return r.score / (float32)(r.nUsers)
}

// Define the User type
// and its list of liked items
type User struct {
	userID   int
	liked    []int // list of movies with ratings >= iLiked
	notLiked []int // list of movies with ratings < iLiked
}

// Utility functions for User struct
func (u User) getUser() int {
	return u.userID
}

func (u *User) setUser(id int) {
	u.userID = id
}

func (u *User) addLiked(id int) {
	u.liked = append(u.liked, id)
}

func (u *User) addNotLiked(id int) {
	u.notLiked = append(u.notLiked, id)
}

// Function to read the ratings CSV file and process each row.
// The output is a map in which user ID is used as key
func readRatingsCSV(fileName string) (map[int]*User, error) {
	// Open the CSV file.
	file, err := os.Open(fileName)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	// Create a CSV reader.
	reader := csv.NewReader(file)

	// Read first line and skip
	_, err = reader.Read()
	if err != nil {
		return nil, err
	}

	// creates the map
	users := make(map[int]*User, 1000)

	// Read all records from the CSV.
	records, err := reader.ReadAll()
	if err != nil {
		return nil, err
	}

	// Iterate over each record and convert the strings into integers or float.
	for _, record := range records {
		if len(record) != 4 {
			return nil, fmt.Errorf("each line must contain exactly 4 integers, but found %d", len(record))
		}

		// Parse user ID integer
		uID, err := strconv.Atoi(record[0])
		if err != nil {
			return nil, fmt.Errorf("error converting '%s' to userID integer: %v", record[0], err)
		}

		// Parse movie ID integer
		mID, err := strconv.Atoi(record[1])
		if err != nil {
			return nil, fmt.Errorf("error converting '%s' to movieID integer: %v", record[1], err)
		}

		// Parse rating float
		r, err := strconv.ParseFloat(record[2], 64)
		if err != nil {

			return nil, fmt.Errorf("error converting '%s' to rating: %v", record[2], err)
		}

		// checks if it is a new user
		u, ok := users[uID]
		if !ok {

			u = &User{uID, nil, nil}
			users[uID] = u
		}

		// ad movie in user list
		if r >= R {

			u.addLiked(mID)

		} else {

			u.addNotLiked(mID)
		}
	}

	return users, nil
}

// Function to read the movies CSV file and process each row.
// The output is a map in which user ID is used as key
func readMoviesCSV(fileName string) (map[int]string, error) {
	// Open the CSV file.
	file, err := os.Open(fileName)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	// Create a CSV reader.
	reader := csv.NewReader(file)

	// Read first line and skip
	_, err = reader.Read()
	if err != nil {
		return nil, err
	}

	// creates the map
	movies := make(map[int]string, 1000)

	// Read all records from the CSV.
	records, err := reader.ReadAll()
	if err != nil {
		return nil, err
	}

	// Iterate over each record and convert the strings into integers or float.
	for _, record := range records {
		if len(record) != 3 {
			return nil, fmt.Errorf("each line must contain exactly 3 entries, but found %d", len(record))
		}

		// Parse movie ID integer
		mID, err := strconv.Atoi(record[0])
		if err != nil {
			return nil, fmt.Errorf("error converting '%s' to movieID integer: %v", record[1], err)
		}

		// record 1 is the title
		movies[mID] = record[1]
	}

	return movies, nil
}

// checks if value is in the set
func member(value int, set []int) bool {

	for _, v := range set {
		if value == v {

			return true
		}
	}

	return false
}

// generator producing Recommendation instances from movie list
func generateMovieRec(wg *sync.WaitGroup, stop <-chan bool, userID int, titles map[int]string) <-chan Recommendation {

	outputStream := make(chan Recommendation)

	go func() {
		defer func() {
			wg.Done()
		}()
		defer close(outputStream)
		// defer fmt.Println("\nFin de generateMovieRec...")
		for k, v := range titles {
			select {
			case <-stop:
				return
			case outputStream <- Recommendation{userID, k, v, 0.0, 0}:
			}
		}
	}()

	return outputStream
}

// Filters out movies already seen by the user
func filterNotSeenByUser(input <-chan Recommendation, user *User) <-chan Recommendation {
	output := make(chan Recommendation)
	go func() {
		defer close(output)
		for rec := range input {
			if !member(rec.movieID, user.liked) && !member(rec.movieID, user.notLiked) {
				output <- rec
			}
		}
	}()
	return output
}

// Counts how many users liked each movie
func countMovieLikes(users map[int]*User) map[int]int {
	likeCounts := make(map[int]int)
	for _, user := range users {
		for _, movie := range user.liked {
			likeCounts[movie]++
		}
	}
	return likeCounts
}

// Filters movies that have been liked by at least K users
func filterByPopularity(input <-chan Recommendation, likeCounts map[int]int, K int) <-chan Recommendation {
	output := make(chan Recommendation)
	go func() {
		defer close(output)
		for rec := range input {
			if likeCounts[rec.movieID] >= K {
				rec.nUsers = likeCounts[rec.movieID]
				output <- rec
			}
		}
	}()
	return output
}

// Compute the Jaccard similarity score between two users based on liked and disliked movies
func computeS(user1 *User, user2 *User) float64 {
	// Convert the liked and disliked movies to sets (maps for fast lookup)
	likedSet1 := make(map[int]struct{})
	likedSet2 := make(map[int]struct{})
	dislikedSet1 := make(map[int]struct{})
	dislikedSet2 := make(map[int]struct{})

	for _, movieID := range user1.liked {
		likedSet1[movieID] = struct{}{}
	}
	for _, movieID := range user2.liked {
		likedSet2[movieID] = struct{}{}
	}

	for _, movieID := range user1.notLiked {
		dislikedSet1[movieID] = struct{}{}
	}
	for _, movieID := range user2.notLiked {
		dislikedSet2[movieID] = struct{}{}
	}

	// Calculate the intersection of the two sets (movies liked or disliked by both users)
	likedIntersection := 0
	for movieID := range likedSet1 {
		if _, exists := likedSet2[movieID]; exists {
			likedIntersection++
		}
	}
	dislikedIntersection := 0
	for movieID := range dislikedSet1 {
		if _, exists := dislikedSet2[movieID]; exists {
			dislikedIntersection++
		}
	}

	// Calculate the union of the two sets (all distinct movies liked or disliked by either user)
	likedUnion := len(likedSet1) + len(likedSet2) - likedIntersection
	dislikedUnion := len(dislikedSet1) + len(dislikedSet2) - dislikedIntersection

	// Calculate the total union of all sets (liked and disliked)
	totalUnion := likedUnion + dislikedUnion
	totalIntersection := likedIntersection + dislikedIntersection

	// If the total union is 0, return 0 (no common or distinct movies)
	if totalUnion == 0 {
		return 0
	}

	// Return the Jaccard similarity score
	return float64(totalIntersection) / float64(totalUnion)
}

// Worker function to compute recommendation scores
func computeScoreWorker(input <-chan Recommendation, output chan Recommendation, wg *sync.WaitGroup, user *User,
	users map[int]*User,
	movieLikes map[int]int,
	minLikes int,

) {

	defer wg.Done()

	for rec := range input {
		// Skip movies already viewed by user U
		if member(rec.movieID, user.liked) && member(rec.movieID, user.notLiked) {
			continue
		}

		// Get number of users who liked the movie
		nUsers, exists := movieLikes[rec.movieID]
		if !exists || nUsers < minLikes {
			continue
		}

		score := 0.0
		likeCount := 0

		// Iterate over users to compute score
		for vID, v := range users {
			if vID != user.userID && member(rec.movieID, v.liked) {
				score += computeS(user, v) // Compute Jaccard similarity S(U, V)
				likeCount++
			}
		}

		// Compute final probability P(U, M)
		if likeCount > 0 {
			rec.score = float32(score / float64(likeCount))
			output <- rec
		}
	}
}

// Compute scores using multiple workers
func computeScores(input <-chan Recommendation, numWorkers int, user *User, users map[int]*User, movieLikes map[int]int, k int) <-chan Recommendation {
	output := make(chan Recommendation)
	var wg sync.WaitGroup
	wg.Add(numWorkers)

	for i := 0; i < numWorkers; i++ {
		go computeScoreWorker(input, output, &wg, user, users, movieLikes, k)
	}

	go func() {
		wg.Wait()
		close(output)
	}()

	return output
}

func getTopNRecommendations(input <-chan Recommendation, N int) []Recommendation {
	var recommendations []Recommendation
	for rec := range input {
		recommendations = append(recommendations, rec)
	}

	sort.Slice(recommendations, func(i, j int) bool {
		return recommendations[i].score > recommendations[j].score
	})

	if len(recommendations) > N {
		recommendations = recommendations[:N]
	}

	return recommendations
}

// writeRecommendationsToFile writes recommendations in the desired format
func writeRecommendationsToFile(currentUser int, recommendations []Recommendation, nModule int, execTime time.Duration, nCpu int, userID int) error {
	fileName := fmt.Sprintf("recommendation_for_User_%d_with_%d_modules parallèles.txt", currentUser, nModule)

	file, err := os.Create(fileName)
	if err != nil {
		return err
	}
	defer file.Close()
	line01 := fmt.Sprintf("Number of CPUs:%d", nCpu)
	file.WriteString(line01)
	line02 := fmt.Sprintln("\nRecommendations for which user?\n")
	file.WriteString(line02)
	line03 := fmt.Sprintf("Recommendations for user # %d :\n", userID)
	file.WriteString(line03)

	for _, rec := range recommendations {
		title := rec.movieTitle
		line := fmt.Sprintf("Movie: %s, Score: %.4f\n", title, rec.score)
		_, err := file.WriteString(line)
		if err != nil {
			return err
		}
	}
	line1 := fmt.Sprintf("\n\nExecution time: %s", execTime)
	file.WriteString(line1)

	fmt.Println("Recommendations saved to", fileName)
	return nil
}

func main() {

	fmt.Println("Number of CPUs:", runtime.NumCPU()) // just curious

	// user to be considered
	var currentUser int
	nThread := 2
	fmt.Println("Recommendations for which user? ")
	fmt.Scanf("%d", &currentUser)

	// Call the function to read and parse the movies CSV file.
	titles, err := readMoviesCSV("C:/Users/monel/Uottawa/4eAnnee/Hiver 2025/CSI2520/Project/Dataset/ml-latest-small/movies.csv")
	if err != nil {
		log.Fatal(err)
	}

	// Call the function to read and parse the ratings CSV file.
	ratings, err := readRatingsCSV("C:/Users/monel/Uottawa/4eAnnee/Hiver 2025/CSI2520/Project/Dataset/ml-latest-small/ratings.csv")
	if err != nil {
		log.Fatal(err)
	}

	user, exists := ratings[currentUser]
	if !exists {
		log.Fatalf("User %d not found", currentUser)
	}

	likeCounts := countMovieLikes(ratings)

	// synchronization
	stop := make(chan bool)
	var wg sync.WaitGroup
	wg.Add(1)

	start := time.Now() // chrono

	// the sequence of filters
	recChannel := generateMovieRec(&wg, stop, currentUser, titles)
	filtered1 := filterNotSeenByUser(recChannel, user)
	filtered2 := filterByPopularity(filtered1, likeCounts, K)
	scored := computeScores(filtered2, nThread, user, ratings, likeCounts, 10) ///
	topN := getTopNRecommendations(scored, N)                                  // Get top N recommendations

	wg.Wait()
	close(stop) // stop all threads

	end := time.Now()

	fmt.Println("Top recommendations:")
	for _, rec := range topN {
		fmt.Printf("Movie: %s, Score: %.4f\n", rec.movieTitle, rec.score)
	}
	execTime := end.Sub(start)
	writeRecommendationsToFile(currentUser, topN, nThread, execTime, runtime.NumCPU(), currentUser)

	fmt.Printf("\n\nExecution time: %s", execTime)
}
