package se.mattec.id2212.project.view;

import javax.ejb.EJB;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import javax.inject.Named;
import se.mattec.id2212.project.controller.UserBean;
import se.mattec.id2212.project.model.Account;

/**
 * A manager linking between the JSF user interface and the application logic in
 * UserBean.
 *
 * @author Mattias
 */
@Named(value = "userManager")
@ManagedBean
@SessionScoped
public class UserManager {

    @EJB
    private UserBean userBean;

    private String loginError;
    private String registerError;

    private boolean isLoggedIn;
    private boolean isLoggedInAsAdmin;

    /**
     * Creates the UserManager with login status initiated to false.
     */
    public UserManager() {
        isLoggedInAsAdmin = false;
        isLoggedIn = false;
    }

    /**
     * Tries to log in to the web shop.
     *
     * @param username The username to log in with.
     * @param password The password to validate.
     */
    public void login(String username, String password) {
        Account loggedInAccount = null;

        try {
            loggedInAccount = userBean.login(username, password);
        }
        catch (Exception ex) {
            loginError = ex.getMessage();
        }

        if (loggedInAccount != null) {
            isLoggedIn = true;

            if (loggedInAccount.isIsAdmin()) {
                isLoggedInAsAdmin = true;
            }
        }
    }

    /**
     * Tries to register a new user with the web shop.
     *
     * @param username The username to register.
     * @param password The password to go with the account.
     * @param isAdmin If the user is admin or not.
     */
    public void register(String username, String password, boolean isAdmin) {
        Account loggedInAccount = null;

        try {
            loggedInAccount = userBean.register(username, password, isAdmin);
        }
        catch (Exception ex) {
            registerError = ex.getMessage();
        }

        if (loggedInAccount != null) {
            isLoggedIn = true;

            if (loggedInAccount.isIsAdmin()) {
                isLoggedInAsAdmin = true;
            }
        }
    }

    /**
     * Logs the current user out by resetting the login status.
     */
    public void logout() {
        isLoggedInAsAdmin = false;
        isLoggedIn = false;
        loginError = null;
        registerError = null;
    }

    /* GETTERS AND SETTERS */
    /**
     * Returns the error message string for the login view.
     *
     * @return A String (possibly) containing an error.
     */
    public String getLoginError() {
        return loginError;
    }

    /**
     * Returns the error message string for the register view.
     *
     * @return A String (possibly) containing an error.
     */
    public String getRegisterError() {
        return registerError;
    }

    /**
     * Returns if the user is logged in as an admin or not.
     *
     * @return True if user is an admin, false otherwise.
     */
    public boolean isIsLoggedInAsAdmin() {
        return isLoggedInAsAdmin;
    }

    /**
     * Returns if the user is logged in or not.
     *
     * @return True if user is logged in, false otherwise.
     */
    public boolean isIsLoggedIn() {
        return isLoggedIn;
    }

}
