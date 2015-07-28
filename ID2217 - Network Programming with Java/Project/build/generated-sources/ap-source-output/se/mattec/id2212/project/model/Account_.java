package se.mattec.id2212.project.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value="EclipseLink-2.5.2.v20140319-rNA", date="2014-12-29T22:17:45")
@StaticMetamodel(Account.class)
public class Account_ { 

    public static volatile SingularAttribute<Account, String> username;
    public static volatile SingularAttribute<Account, Boolean> isAdmin;
    public static volatile SingularAttribute<Account, String> password;
    public static volatile SingularAttribute<Account, Boolean> isBanned;

}