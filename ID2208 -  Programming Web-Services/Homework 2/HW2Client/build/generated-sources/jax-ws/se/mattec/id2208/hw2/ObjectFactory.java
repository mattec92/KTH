
package se.mattec.id2208.hw2;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the se.mattec.id2208.hw2 package. 
 * <p>An ObjectFactory allows you to programatically 
 * construct new instances of the Java representation 
 * for XML content. The Java representation of XML 
 * content can consist of schema derived interfaces 
 * and classes representing the binding of schema 
 * type definitions, element declarations and model 
 * groups.  Factory methods for each of these are 
 * provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {

    private final static QName _GetItinearies_QNAME = new QName("http://hw2.id2208.mattec.se/", "getItinearies");
    private final static QName _GetItineariesResponse_QNAME = new QName("http://hw2.id2208.mattec.se/", "getItineariesResponse");
    private final static QName _LoginResponse_QNAME = new QName("http://hw2.id2208.mattec.se/", "loginResponse");
    private final static QName _Login_QNAME = new QName("http://hw2.id2208.mattec.se/", "login");
    private final static QName _UnauthorizedException_QNAME = new QName("http://hw2.id2208.mattec.se/", "UnauthorizedException");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: se.mattec.id2208.hw2
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link LoginResponse }
     * 
     */
    public LoginResponse createLoginResponse() {
        return new LoginResponse();
    }

    /**
     * Create an instance of {@link GetItinearies }
     * 
     */
    public GetItinearies createGetItinearies() {
        return new GetItinearies();
    }

    /**
     * Create an instance of {@link GetItineariesResponse }
     * 
     */
    public GetItineariesResponse createGetItineariesResponse() {
        return new GetItineariesResponse();
    }

    /**
     * Create an instance of {@link UnauthorizedException }
     * 
     */
    public UnauthorizedException createUnauthorizedException() {
        return new UnauthorizedException();
    }

    /**
     * Create an instance of {@link Login }
     * 
     */
    public Login createLogin() {
        return new Login();
    }

    /**
     * Create an instance of {@link Flight }
     * 
     */
    public Flight createFlight() {
        return new Flight();
    }

    /**
     * Create an instance of {@link Itineary }
     * 
     */
    public Itineary createItineary() {
        return new Itineary();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link GetItinearies }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://hw2.id2208.mattec.se/", name = "getItinearies")
    public JAXBElement<GetItinearies> createGetItinearies(GetItinearies value) {
        return new JAXBElement<GetItinearies>(_GetItinearies_QNAME, GetItinearies.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link GetItineariesResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://hw2.id2208.mattec.se/", name = "getItineariesResponse")
    public JAXBElement<GetItineariesResponse> createGetItineariesResponse(GetItineariesResponse value) {
        return new JAXBElement<GetItineariesResponse>(_GetItineariesResponse_QNAME, GetItineariesResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LoginResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://hw2.id2208.mattec.se/", name = "loginResponse")
    public JAXBElement<LoginResponse> createLoginResponse(LoginResponse value) {
        return new JAXBElement<LoginResponse>(_LoginResponse_QNAME, LoginResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Login }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://hw2.id2208.mattec.se/", name = "login")
    public JAXBElement<Login> createLogin(Login value) {
        return new JAXBElement<Login>(_Login_QNAME, Login.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link UnauthorizedException }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://hw2.id2208.mattec.se/", name = "UnauthorizedException")
    public JAXBElement<UnauthorizedException> createUnauthorizedException(UnauthorizedException value) {
        return new JAXBElement<UnauthorizedException>(_UnauthorizedException_QNAME, UnauthorizedException.class, null, value);
    }

}
