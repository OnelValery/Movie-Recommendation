// Project CSI2120/CSI2520
// Winter 2025/Java_version
// @author Onel Valery Mezil; uottawa.ca
//Numero Etudiant 300260630
public class Recommendation {
    
    // Attributs
    private User user;    // L'utilisateur pour lequel la recommandation est faite
    private Movie movie;   // Le film recommandé
    private double score;   // La probabilité que l'utilisateur aime ce film
    private int nUsers;     // Le nombre d'utilisateurs ayant aimé ce film
    
    // Constructeur
    public Recommendation(User user, Movie movie, double score, int nUsers) {
        this.user = user;
        this.movie = movie;
        this.score = score;
        this.nUsers = nUsers;
    }
    
    // Getters et Setters
    public User getUser() {
        return user;
    }
    
    public void setUser(User user) {
        this.user = user;
    }
    
    public Movie getMovie() {
        return movie;
    }
    
    public void setMovie(Movie movie) {
        this.movie = movie;
    }
    
    public double getScore() {
        return score;
    }
    
    public void setScore(double score) {
        this.score = score;
    }
    
    public int getnUsers() {
        return nUsers;
    }
    
    public void setnUsers(int nUsers) {
        this.nUsers = nUsers;
    }
    
    // Méthode toString pour afficher les informations de la recommandation
    @Override
    public String toString() {
        return "Recommendation{" +
               "user='" + user + '\'' +
               ", movie='" + movie + '\'' +
               ", score=" + score +
               ", nUsers=" + nUsers +
               '}';
    }


    // Méthode pour vérifier si l'utilisateur a une probabilité élevée d'aimer le film
    public boolean isHighScore(double threshold) {
        return this.score >= threshold;
    }

    
}
