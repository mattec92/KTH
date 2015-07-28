
package se.mattec.id2208.hw2;

import java.net.MalformedURLException;
import java.net.URL;
import javax.xml.namespace.QName;
import javax.xml.ws.Service;
import javax.xml.ws.WebEndpoint;
import javax.xml.ws.WebServiceClient;
import javax.xml.ws.WebServiceException;
import javax.xml.ws.WebServiceFeature;


/**
 * This class was generated by the JAX-WS RI.
 * JAX-WS RI 2.2.6-1b01 
 * Generated source version: 2.2
 * 
 */
@WebServiceClient(name = "FlightReservationServiceBottomUp", targetNamespace = "http://hw2.id2208.mattec.se/", wsdlLocation = "http://localhost:8080/FlightReservationServiceBottomUp/FlightReservationServiceBottomUp?wsdl")
public class FlightReservationServiceBottomUp_Service
    extends Service
{

    private final static URL FLIGHTRESERVATIONSERVICEBOTTOMUP_WSDL_LOCATION;
    private final static WebServiceException FLIGHTRESERVATIONSERVICEBOTTOMUP_EXCEPTION;
    private final static QName FLIGHTRESERVATIONSERVICEBOTTOMUP_QNAME = new QName("http://hw2.id2208.mattec.se/", "FlightReservationServiceBottomUp");

    static {
        URL url = null;
        WebServiceException e = null;
        try {
            url = new URL("http://localhost:8080/FlightReservationServiceBottomUp/FlightReservationServiceBottomUp?wsdl");
        } catch (MalformedURLException ex) {
            e = new WebServiceException(ex);
        }
        FLIGHTRESERVATIONSERVICEBOTTOMUP_WSDL_LOCATION = url;
        FLIGHTRESERVATIONSERVICEBOTTOMUP_EXCEPTION = e;
    }

    public FlightReservationServiceBottomUp_Service() {
        super(__getWsdlLocation(), FLIGHTRESERVATIONSERVICEBOTTOMUP_QNAME);
    }

    public FlightReservationServiceBottomUp_Service(WebServiceFeature... features) {
        super(__getWsdlLocation(), FLIGHTRESERVATIONSERVICEBOTTOMUP_QNAME, features);
    }

    public FlightReservationServiceBottomUp_Service(URL wsdlLocation) {
        super(wsdlLocation, FLIGHTRESERVATIONSERVICEBOTTOMUP_QNAME);
    }

    public FlightReservationServiceBottomUp_Service(URL wsdlLocation, WebServiceFeature... features) {
        super(wsdlLocation, FLIGHTRESERVATIONSERVICEBOTTOMUP_QNAME, features);
    }

    public FlightReservationServiceBottomUp_Service(URL wsdlLocation, QName serviceName) {
        super(wsdlLocation, serviceName);
    }

    public FlightReservationServiceBottomUp_Service(URL wsdlLocation, QName serviceName, WebServiceFeature... features) {
        super(wsdlLocation, serviceName, features);
    }

    /**
     * 
     * @return
     *     returns FlightReservationServiceBottomUp
     */
    @WebEndpoint(name = "FlightReservationServiceBottomUpPort")
    public FlightReservationServiceBottomUp getFlightReservationServiceBottomUpPort() {
        return super.getPort(new QName("http://hw2.id2208.mattec.se/", "FlightReservationServiceBottomUpPort"), FlightReservationServiceBottomUp.class);
    }

    /**
     * 
     * @param features
     *     A list of {@link javax.xml.ws.WebServiceFeature} to configure on the proxy.  Supported features not in the <code>features</code> parameter will have their default values.
     * @return
     *     returns FlightReservationServiceBottomUp
     */
    @WebEndpoint(name = "FlightReservationServiceBottomUpPort")
    public FlightReservationServiceBottomUp getFlightReservationServiceBottomUpPort(WebServiceFeature... features) {
        return super.getPort(new QName("http://hw2.id2208.mattec.se/", "FlightReservationServiceBottomUpPort"), FlightReservationServiceBottomUp.class, features);
    }

    private static URL __getWsdlLocation() {
        if (FLIGHTRESERVATIONSERVICEBOTTOMUP_EXCEPTION!= null) {
            throw FLIGHTRESERVATIONSERVICEBOTTOMUP_EXCEPTION;
        }
        return FLIGHTRESERVATIONSERVICEBOTTOMUP_WSDL_LOCATION;
    }

}
