package se.mattec.id2212.project.view;

import java.util.ArrayList;
import java.util.HashMap;
import javax.ejb.EJB;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import javax.inject.Named;
import se.mattec.id2212.project.controller.AdminBean;
import se.mattec.id2212.project.model.Article;
import se.mattec.id2212.project.model.Account;

/**
 * A manager linking between the JSF user interface and the application logic in
 * AdminBean.
 *
 * @author Mattias
 */
@Named("AdminManager")
@ManagedBean
@SessionScoped
public class AdminManager {

    @EJB
    private AdminBean adminBean;

    private HashMap<String, String> inventoryQuantities;

    private String errorMessage;

    /**
     * Creates the AdminManager.
     */
    public AdminManager() {
        inventoryQuantities = new HashMap<>();
    }

    /**
     * Adds additional units to an article in the web store.
     *
     * @param article The article to add units to.
     */
    public void addUnits(Article article) {
        int quantity = getQuantityForArticle(article);

        try {
            adminBean.addUnits(article, quantity);

            errorMessage = null;
        }
        catch (Exception ex) {
            errorMessage = ex.getMessage();
        }
    }

    /**
     * Removes units from an article in the web store.
     *
     * @param article The article to remove units from.
     */
    public void removeUnits(Article article) {
        int quantity = getQuantityForArticle(article);

        try {
            adminBean.removeUnits(article, quantity);

            errorMessage = null;
        }
        catch (Exception ex) {
            errorMessage = ex.getMessage();
        }
    }

    /**
     * Adds a new article to the web store.
     *
     * @param name The name of the unit to add.
     * @param quantity The quantity of the unit to add.
     */
    public void addArticle(String name, int quantity) {
        try {
            adminBean.addArticle(name, quantity);

            errorMessage = null;
        }
        catch (Exception ex) {
            errorMessage = ex.getMessage();
        }
    }

    /**
     * Removes an article from the web store.
     *
     * @param article The article to remove.
     */
    public void removeArticle(Article article) {
        adminBean.removeArticle(article);

        errorMessage = null;
    }

    /**
     * Bans an account.
     *
     * @param account The account to ban.
     */
    public void banAccount(Account account) {
        adminBean.banAccount(account);

        errorMessage = null;
    }

    /**
     * Unbans an account.
     *
     * @param account The account to unban.
     */
    public void unbanAccount(Account account) {
        adminBean.unbanAccount(account);

        errorMessage = null;
    }

    /**
     * Returns all accounts in the web store.
     *
     * @return A list of accounts.
     */
    public ArrayList<Account> getAccounts() {
        return adminBean.getAccounts();
    }

    /**
     * Returns all articles in the web store.
     *
     * @return A list of articles.
     */
    public ArrayList<Article> getArticles() {
        return adminBean.getArticles();
    }

    /**
     * Initiates the web store with some articles.
     */
    public void initArticles() {
        adminBean.initArticles();

        errorMessage = null;
    }

    /* HELPERS */
    private int getQuantityForArticle(Article article) {
        String quantityString = inventoryQuantities.get(article.getName());

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
     * Returns the HashMap between quantity inputTexts and articles.
     *
     * @return A HashMap for storing quantities.
     */
    public HashMap<String, String> getInventoryQuantities() {
        return inventoryQuantities;
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
