package se.mattec.id2212.project.view;

import java.util.ArrayList;
import java.util.HashMap;
import javax.ejb.EJB;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import javax.inject.Named;
import se.mattec.id2212.project.controller.ShopBean;
import se.mattec.id2212.project.model.Article;

/**
 * A manager linking between the JSF user interface and the application logic in
 * ShopBean.
 *
 * @author Mattias
 */
@Named(value = "shopManager")
@ManagedBean
@SessionScoped
public class ShopManager {

    @EJB
    private ShopBean shopBean;

    private HashMap<String, String> inventoryQuantities;
    private HashMap<String, String> basketQuantities;

    private String errorMessage;

    /**
     * Creates the ShopManager. Initiates quantity HashMaps.
     */
    public ShopManager() {
        inventoryQuantities = new HashMap<>();
        basketQuantities = new HashMap<>();
    }

    /**
     * Adds an article to the shopping basket.
     *
     * @param article The article to add.
     */
    public void addToBasket(Article article) {
        int quantity = getQuantityForArticle(article, inventoryQuantities);

        try {
            shopBean.addToBasket(article, quantity);

            errorMessage = null;
        }
        catch (Exception ex) {
            errorMessage = ex.getMessage();
        }
    }

    /**
     * Removes an article from the shopping basket.
     *
     * @param article The article to remove.
     */
    public void removeFromBasket(Article article) {
        int quantity = getQuantityForArticle(article, basketQuantities);

        try {
            shopBean.removeFromBasket(article, quantity);

            errorMessage = null;
        }
        catch (Exception ex) {
            errorMessage = ex.getMessage();
        }
    }

    /**
     * Returns all articles available.
     *
     * @return A list of articles.
     */
    public ArrayList<Article> getArticles() {
        return shopBean.getArticles();
    }

    /**
     * Returns all articles in the shopping basket.
     *
     * @return A list of articles.
     */
    public HashMap<String, Article> getBasket() {
        return shopBean.getBasket();
    }

    /**
     * Performs a checkout. Removes all articles from the basket.
     */
    public void checkout() {
        shopBean.checkout();

        errorMessage = null;
    }

    /**
     * Empties the shopping basket. Returns the articles to the shop inventory.
     */
    public void emptyBasket() {
        shopBean.emptyBasket();

        errorMessage = null;
    }

    /* HELPERS */
    private int getQuantityForArticle(Article article, HashMap<String, String> map) {
        String quantityString = map.get(article.getName());

        if (quantityString != null && quantityString.isEmpty() == false) {
            int quantity;

            try {
                quantity = Integer.parseInt(quantityString);

                return quantity;
            }
            catch (NumberFormatException e) {
            }
        }

        return 0;
    }

    /* GETTERS AND SETTERS */
    /**
     * Returns the HashMap between quantity inputTexts and articles for the
     * inventory.
     *
     * @return A HashMap for storing quantities.
     */
    public HashMap<String, String> getInventoryQuantities() {
        return inventoryQuantities;
    }

    /**
     * Returns the HashMap between quantity inputTexts and articles for the
     * shopping basket.
     *
     * @return A HashMap for storing quantities.
     */
    public HashMap<String, String> getBasketQuantities() {
        return basketQuantities;
    }

    /**
     * Returns the error message String. Used to display error messages,
     *
     * @return A String containing (possibly) an error message.
     */
    public String getErrorMessage() {
        return errorMessage;
    }

}
