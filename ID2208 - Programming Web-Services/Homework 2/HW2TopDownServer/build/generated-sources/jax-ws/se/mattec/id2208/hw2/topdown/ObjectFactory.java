
package se.mattec.id2208.hw2.topdown;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the se.mattec.id2208.hw2.topdown package. 
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

    private final static QName _BookTicketResponse_QNAME = new QName("http://topdown.hw2.id2208.mattec.se/", "bookTicketResponse");
    private final static QName _IssueTicketResponse_QNAME = new QName("http://topdown.hw2.id2208.mattec.se/", "issueTicketResponse");
    private final static QName _UnauthorizedException_QNAME = new QName("http://topdown.hw2.id2208.mattec.se/", "UnauthorizedException");
    private final static QName _BookTicket_QNAME = new QName("http://topdown.hw2.id2208.mattec.se/", "bookTicket");
    private final static QName _IssueTicket_QNAME = new QName("http://topdown.hw2.id2208.mattec.se/", "issueTicket");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: se.mattec.id2208.hw2.topdown
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link IssueTicket }
     * 
     */
    public IssueTicket createIssueTicket() {
        return new IssueTicket();
    }

    /**
     * Create an instance of {@link BookTicket }
     * 
     */
    public BookTicket createBookTicket() {
        return new BookTicket();
    }

    /**
     * Create an instance of {@link ExceptionBean }
     * 
     */
    public ExceptionBean createExceptionBean() {
        return new ExceptionBean();
    }

    /**
     * Create an instance of {@link BookTicketResponse }
     * 
     */
    public BookTicketResponse createBookTicketResponse() {
        return new BookTicketResponse();
    }

    /**
     * Create an instance of {@link IssueTicketResponse }
     * 
     */
    public IssueTicketResponse createIssueTicketResponse() {
        return new IssueTicketResponse();
    }

    /**
     * Create an instance of {@link Flight }
     * 
     */
    public Flight createFlight() {
        return new Flight();
    }

    /**
     * Create an instance of {@link Ticket }
     * 
     */
    public Ticket createTicket() {
        return new Ticket();
    }

    /**
     * Create an instance of {@link Itineary }
     * 
     */
    public Itineary createItineary() {
        return new Itineary();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BookTicketResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://topdown.hw2.id2208.mattec.se/", name = "bookTicketResponse")
    public JAXBElement<BookTicketResponse> createBookTicketResponse(BookTicketResponse value) {
        return new JAXBElement<BookTicketResponse>(_BookTicketResponse_QNAME, BookTicketResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link IssueTicketResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://topdown.hw2.id2208.mattec.se/", name = "issueTicketResponse")
    public JAXBElement<IssueTicketResponse> createIssueTicketResponse(IssueTicketResponse value) {
        return new JAXBElement<IssueTicketResponse>(_IssueTicketResponse_QNAME, IssueTicketResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ExceptionBean }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://topdown.hw2.id2208.mattec.se/", name = "UnauthorizedException")
    public JAXBElement<ExceptionBean> createUnauthorizedException(ExceptionBean value) {
        return new JAXBElement<ExceptionBean>(_UnauthorizedException_QNAME, ExceptionBean.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BookTicket }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://topdown.hw2.id2208.mattec.se/", name = "bookTicket")
    public JAXBElement<BookTicket> createBookTicket(BookTicket value) {
        return new JAXBElement<BookTicket>(_BookTicket_QNAME, BookTicket.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link IssueTicket }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://topdown.hw2.id2208.mattec.se/", name = "issueTicket")
    public JAXBElement<IssueTicket> createIssueTicket(IssueTicket value) {
        return new JAXBElement<IssueTicket>(_IssueTicket_QNAME, IssueTicket.class, null, value);
    }

}
