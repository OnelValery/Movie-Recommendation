// Project CSI2120/CSI2520
// Winter 2025/Java_version
// @author Onel Valery Mezil; uottawa.ca
//Numero Etudiant 300260630

import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Set;

// this is the (incomplete) class that will generate the recommendations for a user
public class RecommendationEngine {
	
	public ArrayList<Movie> movies;
	public ArrayList<User> users;
	public int K =10; 
    public double R= 3.5; 
    public int N= 20;

	
	// constructs a recommendation engine from files
	public RecommendationEngine(String movieFile, String ratingsFile) throws IOException, 
													NumberFormatException {
		
		movies= new ArrayList<>();
		users= new ArrayList<>();
		readMovies(movieFile);
		readUsers(ratingsFile);
	}
	
	// Reads the Movie csv file of the MovieLens dataset
	// It populates the list of Movies
    public void readMovies(String csvFile) throws IOException, 
													NumberFormatException {

        String line;
        String delimiter = ","; // Assuming values are separated by commas

		BufferedReader br = new BufferedReader(new FileReader(csvFile)); 
		// Read each line from the CSV file
		line = br.readLine();
		
		while ((line = br.readLine()) != null && line.length() > 0) {
			// Split the line into parts using the delimiter
			String[] parts = line.split(delimiter);
			String title;
			
			// parse the ID
			int movieID= Integer.parseInt(parts[0]);
			
			if (parts.length < 3)
				throw new NumberFormatException("Error: Invalid line structure: " + line);

			// we assume that the first part is the ID
			// and the last one are genres, the rest is the title
			title= parts[1];
			if (parts.length > 3) {
				
				for (int i=2; i<parts.length-1; i++)
					title+= parts[i];
			}
			
			movies.add(new Movie(movieID,title));
		}
		
			
    }

	// Reads the rating csv file of the MovieLens dataset
	// It populates the list of Users
    public void readUsers(String csvFile) throws IOException, 
													NumberFormatException {

        String line;
        String delimiter = ","; // Assuming values are separated by commas

		BufferedReader br = new BufferedReader(new FileReader(csvFile)); 
		// Read each line from the CSV file
		line = br.readLine();
		
		while ((line = br.readLine()) != null && line.length() > 0) {
			// Split the line into parts using the delimiter
			String[] parts = line.split(delimiter);
			int userID;
			int movieId;
			double rating;
			
			// parse the ID
			userID= Integer.parseInt(parts[0]);
			
			if (parts.length < 4)
				throw new NumberFormatException("Error: Invalid line structure: " + line);

			// we assume that the first part is the ID
			// and the last one are genres, the rest is the title
			movieId= Integer.parseInt(parts[1]);
			rating= Double.parseDouble(parts[2]);
			
			User currentUser=new User(userID);
			// Check if user already exists in the list
			User existingUser = userInTheList(currentUser);
			if (existingUser==null){
				  if (rating<R){
					currentUser.addDislikedMovie(getMovieById(movieId));
				  }
                  else{
					currentUser.addLikedMovie(getMovieById(movieId));
				  }
				  users.add(currentUser);
			}else{
				if (rating<R){
					existingUser.addDislikedMovie(getMovieById(movieId));
				  }
                  else{
					existingUser.addLikedMovie(getMovieById(movieId));
				  }
			}
			
		}
			
    }

	public Movie getMovie(int index) {
		return movies.get(index);
	}

	public User getUser(int index) {
		return users.get(index);
	}

	 // Method to get a movie by its movieId using a for loop
    public  Movie getMovieById( int movieId) {
        for (Movie movie : movies) {
            if (movie.getId() == movieId) {
                return movie;  // return the movie if found
            }
        }
        return null; // return null if movie is not found
    }

	// Function to return the user by giving their userID
    public User getUserByID( int userID) {
        for (User user : users) {
            if (user.getUserID() == userID) {
                return user;  // Return the user if userID matches
            }
        }
        return null;  // Return null if no user is found with the given userID
    }

	// Method to check if a user already exists in the list
    public User userInTheList(User currentUser) {
        // Check if currentUser is in the list of users
        for (User user : users) {
            if (user.equals(currentUser)) {
                return user;  // Return the user if found
            }
        }
        return null;  // Return null if the user is not found
    }
	
	public int getNumberOfMovies() {
		
		return movies.size();
	}

	public double computeS(User U1,User U2){
		int likedMoviesIntersection=calculateIntersectionSize(U1.getLikedMovies(),U2.getLikedMovies());
		int dislikedMoviesIntersection=calculateIntersectionSize(U1.getDislikedMovies(),U2.getDislikedMovies());
    
		return (likedMoviesIntersection+dislikedMoviesIntersection)/(double)calculateUnionSize(U1, U2);
	}
	 // Fonction qui calcule l'intersection de deux listes de films
	public int calculateIntersectionSize(ArrayList<Movie> list1, ArrayList<Movie> list2) {
        // Créer un ensemble à partir de la première liste de films
        Set<Movie> set1 = new HashSet<>(list1);

        // Créer un ensemble pour l'intersection
        Set<Movie> intersection = new HashSet<>(set1);

        // Retenir uniquement les éléments qui existent dans les deux ensembles
        intersection.retainAll(list2);

        // Retourner la taille de l'intersection
        return intersection.size();
    }

	// Fonction qui calcule |L(U1) ∪ L(U2) ∪ D(U1) ∪ D(U2)| en s'assurant qu'il n'y a pas de doublons
    public int calculateUnionSize(User user1, User user2) {
        // Créer un ensemble à partir de la liste des films aimés et non-aimés des deux utilisateurs
        Set<Movie> unionSet = new HashSet<>();

        // Ajouter les films aimés de U1
        unionSet.addAll(user1.getLikedMovies());

        // Ajouter les films aimés de U2
        unionSet.addAll(user2.getLikedMovies());

        // Ajouter les films non-aimés de U1
        unionSet.addAll(user1.getDislikedMovies());

        // Ajouter les films non-aimés de U2s
        unionSet.addAll(user2.getDislikedMovies());

        // La taille de l'ensemble d'union est le résultat
        return unionSet.size();
    }
	// Fonction pour écrire les  films recommandés dans le terminal
	public void DisplayRecommendationList(ArrayList<Recommendation> recommendationList){
		
        for (int i=0; i< recommendationList.size(); i++){
			String str= recommendationList.get(i).getMovie().getTitle()+" at "+
			recommendationList.get(i).getScore() + "[ "+recommendationList.get(i).getnUsers() + "]";
            System.out.println(str);
            //System.out.println("\n");
        }
    }

	// Fonction pour écrire les  films recommandés dans un fichier texte
    public void writeRecommendedMoviesToFile(ArrayList<Recommendation> recommendationList, String filename) {
        // Utilisation de BufferedWriter pour écrire dans le fichier
        try (BufferedWriter writer = new BufferedWriter(new FileWriter(filename))) {
            writer.write("Recommendation for user # "+ recommendationList.get(0).getUser().getUserID()+" : \n");
            for (int i=0; i<recommendationList.size();i++) {
				String str= recommendationList.get(i).getMovie().getTitle()+" at "+
				recommendationList.get(i).getScore() + "[ "+recommendationList.get(i).getnUsers() + "]";
                writer.write(str);
                writer.newLine();  // Ajouter une nouvelle ligne après chaque film
            }
            System.out.println("Recommended movies successfully written to " + filename);
        } catch (IOException e) {
            System.err.println("Error writing to file: " + e.getMessage());
        }
    }

	// Method to generate recommendations for user U
	public ArrayList<Recommendation> generateRecommendations(int userID) {

		User userU= getUserByID(userID);
		// Utilisation d'une HashMap pour stocker les films et leurs probabilités
        //Map<Movie, Double> movieProbabilities = new HashMap<>();
		
		ArrayList<Recommendation> PossibleRecommendationList= new ArrayList<>();
		

		if (getUserByID(userID)==null){
			System.out.println("This user doesn't exist");
			System.exit(0);
		}

		for (Movie movieM : movies) {
			int nUsers = 0;  // User who liked the movieM
			
		// If user U has already liked or dislike the movie, skip it
			if (userU.hasLikedMovie(movieM) || userU.hasDislikedMovie(movieM) ) {
				continue;
			}

		// Calculate L(M) - the list of users who liked the movie M
			ArrayList<User> usersWhoLikedM = new ArrayList<>();
			for (User userV : users) {
				if (userV.hasLikedMovie(movieM)) {
					usersWhoLikedM.add(userV);
				}
			}
 
			
			// If there are fewer than K users who liked the movie, skip it
			
			if (usersWhoLikedM.size() < K) {
				continue;
			}else{
				nUsers = usersWhoLikedM.size();
			}

			// Initialize score and the size of L(M)
			double scoreU_M = 0.0;
			int LM_size = 0;

			// For each user V who liked the movie M, compute the similarity and update the score
			for (User userV : usersWhoLikedM) {
				if (userV.getUserID() != userU.getUserID()) {
					double s_UV = computeS(userU, userV);
					scoreU_M += s_UV;
					LM_size++;
				}
			}

			// Calculate the probability P(U, M)
			double p_UM = scoreU_M / LM_size;
			
			// Ajouter le film et sa probabilité à la HashMap
            //movieProbabilities.put(movieM, p_UM);
			PossibleRecommendationList.add(new Recommendation(userU,movieM, p_UM, nUsers));
		}

		// Sort using a custom comparator to sort by score in descending order
        Collections.sort(PossibleRecommendationList, new Comparator<Recommendation>() {
            @Override
            public int compare(Recommendation rec1, Recommendation rec2) {
                // Sort in descending order (higher score comes first)
                return Double.compare(rec2.getScore(), rec1.getScore());
            }
        });

		ArrayList<Recommendation> recommendationList= new ArrayList<>();
		for(int i = 0; i < Math.min(N, PossibleRecommendationList.size()); i++){
               recommendationList.add(PossibleRecommendationList.get(i));
		}
        return recommendationList;
		
	}
	
	public static void main(String[] args) {
		
		try {
			RecommendationEngine rec= new RecommendationEngine(args[1], args[2]);
			int userID=Integer.parseInt(args[0]);
			String filename= "Recommendation_Movies_for_user_"+userID+".txt";
		    ArrayList<Recommendation> RecommendationList= rec.generateRecommendations(userID);
			
			//Display Recommendation for the chosen user on the terminal and a text fime
			String str1 ="Recommendation for user # "+ userID+" : \n";
			System.out.println(str1);
            rec.DisplayRecommendationList(RecommendationList);
			//System.err.println(RecommendationList.size());
		    rec.writeRecommendedMoviesToFile(RecommendationList, filename);
		
		   	 
        } catch (Exception e) {
            System.err.println("Error reading the file: " + e.getMessage());
        }
	}
}
