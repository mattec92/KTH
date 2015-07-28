package se.mattec.id2212.project.model;

import java.io.Serializable;
import javax.persistence.Entity;
import javax.persistence.Id;

/**
 * A representation of an article in the web shop.
 *
 * @author Mattias
 */
@Entity
public class Article implements Serializable {

    @Id
    private String name;

    private int quantity;

    /**
     * Creates an empty article
     */
    public Article() {
    }

    /**
     * Creates an article.
     *
     * @param name The name of the article.
     * @param quantity The quantity of the article.
     */
    public Article(String name, int quantity) {
        this.name = name;
        this.quantity = quantity;
    }

    /**
     * Creates a copy of the provided article, but with another quantity.
     *
     * @param article The article to copy.
     * @param quantity The new quantity.
     */
    public Article(Article article, int quantity) {
        this.name = article.getName();
        this.quantity = quantity;
    }

    /**
     * Returns the name of the article.
     *
     * @return The name.
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the name of the article.
     *
     * @param name The name to set.
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Returns the quantity of the article.
     *
     * @return The quantity.
     */
    public int getQuantity() {
        return quantity;
    }

    /**
     * Sets the quantity of the article.
     *
     * @param amount The quantity to set.
     */
    public void setQuantity(int amount) {
        this.quantity = amount;
    }

}
