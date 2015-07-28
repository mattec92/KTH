package se.mattec.id2212.project.controller;

import java.util.ArrayList;
import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import se.mattec.id2212.project.model.Article;
import se.mattec.id2212.project.model.Account;

/**
 * Contains application logic to handle the administration of the web shop.
 *
 * @author Mattias
 */
@Stateless
@LocalBean
public class AdminBean {

    @PersistenceContext(unitName = "WebShopPU")
    private EntityManager em;

    /**
     * Adds units to the total quantity of an item.
     *
     * @param article The item to increase quantity of.
     * @param quantity The quantity to add.
     * @throws Exception If quantity is less or equal to zero.
     */
    public void addUnits(Article article, int quantity) throws Exception {
        if (quantity <= 0) {
            throw new Exception("Invalid quantity.");
        }

        int newQuantity = article.getQuantity() + quantity;
        article.setQuantity(newQuantity);
        em.merge(article);
    }

    /**
     * Removes units to the total quantity of an item
     *
     * @param article The item to decrease quantity of.
     * @param quantity The quantity to remove.
     * @throws Exception If quantity is less or equal to zero.
     */
    public void removeUnits(Article article, int quantity) throws Exception {
        if (quantity <= 0) {
            throw new Exception("Invalid quantity.");
        }

        int newQuantity = article.getQuantity() - quantity;

        if (newQuantity >= 0) {
            article.setQuantity(newQuantity);
        }
        else {
            article.setQuantity(0);
        }

        em.merge(article);
    }

    /**
     * Adds a new article to the database.
     *
     * @param name Name of the article to add.
     * @param quantity Quantity of the article to add.
     * @throws Exception If name is not provided or quantity is less or equal to
     * zero.
     */
    public void addArticle(String name, int quantity) throws Exception {
        if (name != null && name.isEmpty()) {
            throw new Exception("Name must be supplied.");
        }

        if (quantity <= 0) {
            throw new Exception("Invalid quantity.");
        }

        Article article = getArticle(name);

        if (article == null) {
            article = new Article(name, quantity);
        }
        else {
            int newQuantity = article.getQuantity() + quantity;
            article.setQuantity(newQuantity);
        }

        em.merge(article);
    }

    /**
     * Removes an an article from the database.
     *
     * @param article The article to remove.
     */
    public void removeArticle(Article article) {
        Article managedArticle = em.merge(article);
        em.remove(managedArticle);
    }

    /**
     * Bans an account.
     *
     * @param account The account to ban.
     */
    public void banAccount(Account account) {
        account.setIsBanned(true);
        em.merge(account);
    }

    /**
     * Unbans an account.
     *
     * @param account The account to unban.
     */
    public void unbanAccount(Account account) {
        account.setIsBanned(false);
        em.merge(account);
    }

    /**
     * Returns all accounts in the database.
     *
     * @return A list of accounts.
     */
    public ArrayList<Account> getAccounts() {
        Query query = em.createQuery("SELECT a FROM Account a", Account.class);

        return new ArrayList<>(query.getResultList());
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

    /**
     * Adds a few articles to the database.
     */
    public void initArticles() {
        em.merge(new Article("Cool gnome", 10));
        em.merge(new Article("Happy gnome", 20));
        em.merge(new Article("Sad gnome", 50));
    }

}
