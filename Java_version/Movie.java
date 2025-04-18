// Project CSI2120/CSI2520
// Winter 2025/Java_version
// @author Onel Valery Mezil; uottawa.ca
//Numero Etudiant 300260630
public class Movie {

    // Attributs
    private String title;      // Title of the movie
    private int id;   

    // Constructor
    public Movie(int id, String title) {
        this.title = title;
        this.id = id;
        
    }
    
    // Getters et Setters
    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }


    // Méthode toString pour afficher les informations du film
    @Override
    public String toString() {
        return "["+id+"]:"+title;
    }

}

