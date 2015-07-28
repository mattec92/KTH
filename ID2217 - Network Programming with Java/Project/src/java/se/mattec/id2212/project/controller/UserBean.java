package se.mattec.id2212.project.controller;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import se.mattec.id2212.project.model.Account;

/**
 * Contains application logic for authentication in the web shop.
 *
 * @author Mattias
 */
@Stateless
@LocalBean
public class UserBean {

    @PersistenceContext(unitName = "WebShopPU")
    private EntityManager em;

    /**
     * Registers an account with the web shop.
     *
     * @param username The username to register.
     * @param password The password to register.
     * @param isAdmin If the account is an admin account or not.
     * @return The account registered with the web shop.
     * @throws Exception If the account is already registered.
     */
    public Account register(String username, String password, boolean isAdmin) throws Exception {
        Account account = getAccount(username);

        if (account != null) {
            throw new Exception("Account already registered.");
        }

        Account newAccount = new Account(username, password, isAdmin);
        em.persist(newAccount);

        return newAccount;
    }

    /**
     * Authenticates with the web shop.
     *
     * @param username The username to log on to.
     * @param password The password to the account.
     * @return The account logged on to.
     * @throws Exception If the account doesn't exist, if the password is
     * incorrect or if the account is banned.
     */
    public Account login(String username, String password) throws Exception {
        Account account = getAccount(username);

        if (account == null) {
            throw new Exception("Account does not exist.");
        }

        if (account.getPassword().equals(password) == false) {
            throw new Exception("Password is not correct.");
        }

        if (account.isIsBanned()) {
            throw new Exception("Account is banned.");
        }

        return account;
    }

    private Account getAccount(String username) {
        return em.find(Account.class, username);
    }

}
