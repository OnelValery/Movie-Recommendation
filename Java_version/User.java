// Project CSI2120/CSI2520
// Winter 2025/Java_version
// @author Onel Valery Mezil; uottawa.ca
//Numero Etudiant 300260630
import java.util.ArrayList;
import java.util.Objects;


public class User {

    // Attributs
    private int userID;                      // Identifiant unique de l'utilisateur
    private ArrayList<Movie> likedMovies;           // Liste des films aimés par l'utilisateur
    private ArrayList<Movie> dislikedMovies;        // Liste des films non aimés par l'utilisateur

    // Constructeur
    public User(int  userID) {
        this.userID = userID;
        this.likedMovies = new ArrayList<>();    // Initialisation de la liste des films aimés
        this.dislikedMovies = new ArrayList<>(); // Initialisation de la liste des films non aimés
    }

    // Getters et Setters
    public int getUserID() {
        return userID;
    }

    public void setUserID(int userID) {
        this.userID = userID;
    }

    public ArrayList<Movie> getLikedMovies() {
        return likedMovies;
    }

    public ArrayList<Movie> getDislikedMovies() {
        return dislikedMovies;
    }

    // Méthode pour ajouter un film à la liste des films aimés
    public void addLikedMovie(Movie movie) {
        if (!likedMovies.contains(movie)) {
            likedMovies.add(movie);
        }
    }

    // Méthode pour ajouter un film à la liste des films non aimés
    public void addDislikedMovie(Movie movie) {
        if (!dislikedMovies.contains(movie)) {
            dislikedMovies.add(movie);
        }
    }

    // Méthode pour retirer un film de la liste des films aimés
    public void removeLikedMovie(Movie movie) {
        likedMovies.remove(movie);
    }

    // Méthode pour retirer un film de la liste des films non aimés
    public void removeDislikedMovie(Movie movie) {
        dislikedMovies.remove(movie);
    }

    // Override equals() and hashCode() to compare users by userID
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        User user = (User) obj;
        return userID == user.userID;
    }

    @Override
    public int hashCode() {
        return Objects.hash(userID);
    }



    // Méthode toString pour afficher les informations de l'utilisateur
    @Override
    public String toString() {
        return "User{" +
               "userID='" + userID + '\'' +
               ", likedMovies=" + likedMovies +
               ", dislikedMovies=" + dislikedMovies +
               '}';
    }

    // Méthode pour vérifier si l'utilisateur a aimé un film
    public boolean hasLikedMovie(Movie movie) {
        return likedMovies.contains(movie);
    }

    // Méthode pour vérifier si l'utilisateur n'a pas aimé un film
    public boolean hasDislikedMovie(Movie movie) {
        return dislikedMovies.contains(movie);
    }

    
}
