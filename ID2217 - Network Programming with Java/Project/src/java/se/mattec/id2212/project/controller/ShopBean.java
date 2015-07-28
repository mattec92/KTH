package se.mattec.id2212.project.controller;

import java.util.ArrayList;
import java.util.HashMap;
import javax.ejb.LocalBean;
import javax.ejb.Stateful;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import se.mattec.id2212.project.model.Article;

/**
 * Contains application logic for a user session in the web shop.
 *
 * @author Mattias
 */
@Stateful
@LocalBean
public class ShopBean {

    @PersistenceContext(unitName = "WebShopPU")
    private EntityManager em;

    private HashMap<String, Article> basket;

    /**
     * Creates a ShopBean with an empty shopping basket.
     */
    public ShopBean() {
        basket = new HashMap<>();
    }

    /**
     * Adds an article to the shopping basket.
     *
     * @param article The article to add.
     * @param quantity The quantity to add.
     * @throws Exception If quantity is less or equal to zero or if the quantity
     * is larger than the stock of the article.
     */
    public void addToBasket(Article article, int quantity) throws Exception {
        if (quantity <= 0) {
            throw new Exception("Invalid quantity.");
        }

        if (quantity > article.getQuantity()) {
            throw new Exception("Not enough items in stock.");
        }

        Article articleFromBasket = basket.get(article.getName());

        if (articleFromBasket == null) {
            articleFromBasket = new Article(article, quantity);
        }
        else {
            int totalAmnount = articleFromBasket.getQuantity() + quantity;
            articleFromBasket.setQuantity(totalAmnount);
        }

        basket.put(article.getName(), articleFromBasket);

        int newQuantity = article.getQuantity() - quantity;
        article.setQuantity(newQuantity);
        em.merge(article);
    }

    /**
     * Removes an article from the shopping basket.
     *
     * @param articleFromBasket The article to remove.
     * @param quantity The quantity to remove.
     * @throws Exception If quantity is less or equal to zero or if the quantity
     * is larger than the stock of the article.
     */
    public void removeFromBasket(Article articleFromBasket, int quantity) throws Exception {
        if (quantity <= 0) {
            throw new Exception("Invalid quantity.");
        }

        if (quantity > articleFromBasket.getQuantity()) {
            throw new Exception("Not enough items in basket.");
        }

        Article article = getArticle(articleFromBasket.getName());

        int basketQuantity = articleFromBasket.getQuantity() - quantity;
        articleFromBasket.setQuantity(basketQuantity);

        if (basketQuantity <= 0) {
            basket.remove(articleFromBasket.getName());
        }
        else {
            basket.put(articleFromBasket.getName(), articleFromBasket);
        }

        int newQuantity = article.getQuantity() + quantity;
        article.setQuantity(newQuantity);
        em.merge(article);
    }

    /**
     * Simulates a checkout. Basket is cleared.
     */
    public void checkout() {
        basket.clear();
    }

    /**
     * Empties basket. Articles are returned to the shop stock.
     */
    public void emptyBasket() {
        for (Article articleInBasket : basket.values()) {
            Article article = getArticle(articleInBasket.getName());
            int newQuantity = article.getQuantity() + articleInBasket.getQuantity();
            article.setQuantity(newQuantity);
            em.merge(article);
        }
        basket.clear();
    }

    /**
     * Returns all articles in the basket. The article name is used as key of
     * the HashMap.
     *
     * @return A HashMap containing the articles.
     */
    public HashMap<String, Article> getBasket() {
        return basket;
    }

    /**
     * Returns all articles in the database.
     *
     * @return A list of articles.
     */
    public ArrayList<Article> getArticles() {
        Query query = em.createQuery("SELECT a FROM Article a", Article.class);

        return new ArrayList<>(query.getResultList());
    }

    private Article getArticle(String name) {
        return em.find(Article.class, name);
    }

}
