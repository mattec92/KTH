/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package se.mattec.id2208.hw2.topdown;

import javax.ejb.Stateless;
import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebService;

/**
 *
 * @author mcederlund
 */
@WebService(serviceName = "FlightReservationServiceTopDown", portName = "FlightReservationServiceTopDownPort", endpointInterface = "se.mattec.id2208.hw2.topdown.FlightReservationServiceTopDown", targetNamespace = "http://topdown.hw2.id2208.mattec.se/", wsdlLocation = "WEB-INF/wsdl/FlightReservationServiceTopDown/FlightReservationServiceTopDown.wsdl")
@Stateless
public class FlightReservationServiceTopDown {

    public FlightReservationServiceTopDown() {
        TicketService.init();
    }

    public java.lang.String bookTicket(
            @WebParam(name = "authToken") java.lang.String authToken, 
            @WebParam(name = "itineary") se.mattec.id2208.hw2.topdown.Itineary itineary, 
            @WebParam(name = "cardNumber")java.lang.String cardNumber) throws UnauthorizedException {
        if (!authToken.equals(TicketService.SECRET_TOKEN)) {
            throw new UnauthorizedException("Not authorized", null);
        }

        if (cardNumber != null && !cardNumber.isEmpty()) {
            String ticketNumber = TicketService.bookTicket(itineary, cardNumber);

            return ticketNumber;
        }

        return null;
    }
    
    public se.mattec.id2208.hw2.topdown.Ticket issueTicket(
            @WebParam(name = "authToken") java.lang.String authToken, 
            @WebParam(name = "ticketNumber")java.lang.String ticketNumber) throws UnauthorizedException {
        if (!authToken.equals(TicketService.SECRET_TOKEN)) {
            throw new UnauthorizedException("Not authorized", null);
        }

        Ticket ticket = TicketService.getTicket(ticketNumber);

        return ticket;
    }

}
