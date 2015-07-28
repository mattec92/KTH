package se.mattec.id2212.project.model;

import java.io.Serializable;
import javax.persistence.Entity;
import javax.persistence.Id;

/**
 * A representation of an account in the web shop.
 *
 * @author Mattias
 */
@Entity
public class Account implements Serializable {

    @Id
    private String username;

    private String password;

    private boolean isAdmin;

    private boolean isBanned;

    /**
     * Creates an empty account.
     */
    public Account() {
    }

    /**
     * Creates an account.
     *
     * @param username The username of the account.
     * @param password The password of the account.
     * @param isAdmin If the account is an admin account or not.
     */
    public Account(String username, String password, boolean isAdmin) {
        this.username = username;
        this.password = password;
        this.isAdmin = isAdmin;
        this.isBanned = false;
    }

    /**
     * Returns the username of the account.
     *
     * @return The username.
     */
    public String getUsername() {
        return username;
    }

    /**
     * Sets the username of the account.
     *
     * @param username The username to set.
     */
    public void setUsername(String username) {
        this.username = username;
    }

    /**
     * Returns the password of the account.
     *
     * @return The password.
     */
    public String getPassword() {
        return password;
    }

    /**
     * Sets the password of the account.
     *
     * @param password The password to set.
     */
    public void setPassword(String password) {
        this.password = password;
    }

    /**
     * Returns if the account is an admin user or not.
     *
     * @return True if the account is an admin user, otherwise false.
     */
    public boolean isIsAdmin() {
        return isAdmin;
    }

    /**
     * Sets if the account is an admin user or not.
     *
     * @param isAdmin If the account should be an admin user or not.
     */
    public void setIsAdmin(boolean isAdmin) {
        this.isAdmin = isAdmin;
    }

    /**
     * Returns if the account is banned or not.
     *
     * @return True if the account is banned, otherwise false.
     */
    public boolean isIsBanned() {
        return isBanned;
    }

    /**
     * Sets if the account is banned or not.
     *
     * @param isBanned If the account is banned or not.
     */
    public void setIsBanned(boolean isBanned) {
        this.isBanned = isBanned;
    }

}
