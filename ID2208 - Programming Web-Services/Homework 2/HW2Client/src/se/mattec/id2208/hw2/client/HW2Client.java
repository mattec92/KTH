/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package se.mattec.id2208.hw2.client;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import se.mattec.id2208.hw2.Flight;
import se.mattec.id2208.hw2.Itineary;
import se.mattec.id2208.hw2.UnauthorizedException;
import se.mattec.id2208.hw2.UnauthorizedException_Exception;
import se.mattec.id2208.hw2.topdown.Ticket;

/**
 *
 * @author mcederlund
 */
public class HW2Client {

    private static final String SECRET_TOKEN = "SecretTokenThatAllowsAccess";
    private static final String USERNAME = "mattias";
    private static final String PASSWORD = "mattias";
    private static final String DEPARTURE_CITY = "Stockholm";
    private static final String DESTINATION_CITY = "Paris";
    private static final String DATE = "2015-02-06";
    
    private static String savedTicketNumber = null;

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        if (testAuth()) {
            System.out.println("testAuth succeded");
        } 
        else {
            System.out.println("testAuth failed");
        }
        if (testFindItineary()) {
            System.out.println("testFindItineary succeded");
        } 
        else {
            System.out.println("testFindItineary failed");
        }
        if (testBookItineary()) {
            System.out.println("testBookItineary succeded");
        } 
        else {
            System.out.println("testBookItineary failed");
        }
        if (testIssueTicket()) {
            System.out.println("testIssueTicket succeded");
        } 
        else {
            System.out.println("testIssueTicket failed");
        }
        if (testFull()) {
            System.out.println("testFull succeded");
        } 
        else {
            System.out.println("testFull failed");
        }
    }

    public static boolean testAuth() {
        try {
            boolean okAuth = login(USERNAME, PASSWORD) != null;
            
            boolean failAuth = false;
            try {
            login("fail", "fail");
            }
            catch (UnauthorizedException_Exception e) {
                failAuth = true;
            }
            
            return okAuth && failAuth;
        } catch (Exception ex) {
            Logger.getLogger(HW2Client.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        return false;
    }

    public static boolean testFindItineary() {
        try {
            boolean okIntineary = getItinearies(SECRET_TOKEN, DEPARTURE_CITY, DESTINATION_CITY, DATE).isEmpty() == false;
            boolean failedAuth = false;
            try {
                 getItinearies("NotSecretToken", DEPARTURE_CITY, DESTINATION_CITY, DATE);
            }
            catch (UnauthorizedException_Exception e) {
                failedAuth = true;
            }
            boolean failDepartureIntineary = getItinearies(SECRET_TOKEN, "NotOkCity", DESTINATION_CITY, DATE).isEmpty();
            boolean failDestinationIntineary = getItinearies(SECRET_TOKEN, DEPARTURE_CITY, "NotOkCity", DATE).isEmpty();
            boolean failDateIntineary = getItinearies(SECRET_TOKEN, DEPARTURE_CITY, DESTINATION_CITY, "NotOkDate").isEmpty();
            
            return okIntineary && failedAuth && failDepartureIntineary && failDestinationIntineary && failDateIntineary;
        } catch (Exception ex) {
            Logger.getLogger(HW2Client.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        return false;
    }

    public static boolean testBookItineary() {
        try {
            se.mattec.id2208.hw2.topdown.Itineary itineary = new se.mattec.id2208.hw2.topdown.Itineary();
            itineary.setDepartureCity(DEPARTURE_CITY);
            itineary.setDestinationCity(DESTINATION_CITY);
            
            List<se.mattec.id2208.hw2.topdown.Flight> flights = new ArrayList<>();
            
            se.mattec.id2208.hw2.topdown.Flight flight = new se.mattec.id2208.hw2.topdown.Flight();
            flight.setDepartureCity(DEPARTURE_CITY);
            flight.setDestinationCity(DESTINATION_CITY);
            flight.setDate(DATE);
            flight.setPrice(1000);
            
            itineary.getFlights().addAll(flights);
                    
            savedTicketNumber = bookTicket(SECRET_TOKEN, itineary, "12345");
            boolean okBooking = savedTicketNumber != null;
            boolean failedBooking = bookTicket(SECRET_TOKEN, itineary, null) == null;
            
            return okBooking && failedBooking;
            
        } catch (Exception ex) {
            Logger.getLogger(HW2Client.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        return false;
    }

    public static boolean testIssueTicket() {
        try {
            boolean okTicket = issueTicket(SECRET_TOKEN, savedTicketNumber) != null;
            boolean failedTicket = issueTicket(SECRET_TOKEN, "unknownnumber") == null;
            
            return okTicket && failedTicket;
            
        } catch (Exception ex) {
            Logger.getLogger(HW2Client.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        return false;
    }

    public static boolean testFull() {
        try {
            String token = login(USERNAME, PASSWORD);

            System.out.println(token);

            List<Itineary> itinearies = getItinearies(token, DEPARTURE_CITY, DESTINATION_CITY, DATE);

            for (Itineary itineary : itinearies) {
                System.out.println("------------------------------");
                System.out.println("Itineary for trip from " + itineary.getDepartureCity() + " to " + itineary.getDestinationCity() + ":");

                int price = 0;
                for (Flight flight : itineary.getFlights()) {
                    System.out.println(flight.getDate() + " - From " + flight.getDepartureCity() + " to " + flight.getDestinationCity());

                    price += flight.getPrice();
                }

                System.out.println("Price: $" + price);
            }

            if (!itinearies.isEmpty()) {
                String ticketNumber = bookTicket(token, convertItineary(itinearies.get(0)), "12345");

                if (ticketNumber != null) {

                    System.out.println("Ticket booked with ticketNumber: " + ticketNumber);

                    Ticket ticket = issueTicket(token, ticketNumber);

                    if (ticket != null) {
                        System.out.println("Ticket issued with ticketnumber: " + ticket.getTicketNumber() + ", cardNumber: " + ticket.getCardNumber());

                        return true;
                    }
                }
            }

        } catch (Exception ex) {
            Logger.getLogger(HW2Client.class.getName()).log(Level.SEVERE, null, ex);
        }

        return false;
    }

    public static se.mattec.id2208.hw2.topdown.Itineary convertItineary(Itineary itineary) {
        se.mattec.id2208.hw2.topdown.Itineary convertedItineary = new se.mattec.id2208.hw2.topdown.Itineary();
        convertedItineary.setDepartureCity(itineary.getDepartureCity());
        convertedItineary.setDestinationCity(itineary.getDestinationCity());

        for (Flight flight : itineary.getFlights()) {
            convertedItineary.getFlights().add(convertFlight(flight));
        }

        return convertedItineary;
    }

    public static se.mattec.id2208.hw2.topdown.Flight convertFlight(Flight flight) {
        se.mattec.id2208.hw2.topdown.Flight convertedFlight = new se.mattec.id2208.hw2.topdown.Flight();
        convertedFlight.setDepartureCity(flight.getDepartureCity());
        convertedFlight.setDestinationCity(flight.getDestinationCity());
        convertedFlight.setDate(flight.getDate());
        convertedFlight.setPrice(flight.getPrice());

        return convertedFlight;
    }

    private static String login(java.lang.String username, java.lang.String password) throws UnauthorizedException_Exception {
        se.mattec.id2208.hw2.FlightReservationServiceBottomUp_Service service = new se.mattec.id2208.hw2.FlightReservationServiceBottomUp_Service();
        se.mattec.id2208.hw2.FlightReservationServiceBottomUp port = service.getFlightReservationServiceBottomUpPort();
        return port.login(username, password);
    }

    private static java.util.List<se.mattec.id2208.hw2.Itineary> getItinearies(java.lang.String authToken, java.lang.String departureCity, java.lang.String destinationCity, java.lang.String date) throws UnauthorizedException_Exception {
        se.mattec.id2208.hw2.FlightReservationServiceBottomUp_Service service = new se.mattec.id2208.hw2.FlightReservationServiceBottomUp_Service();
        se.mattec.id2208.hw2.FlightReservationServiceBottomUp port = service.getFlightReservationServiceBottomUpPort();
        return port.getItinearies(authToken, departureCity, destinationCity, date);
    }

    private static String bookTicket(java.lang.String authToken, se.mattec.id2208.hw2.topdown.Itineary itineary, java.lang.String cardNumber) throws se.mattec.id2208.hw2.topdown.UnauthorizedException {
        se.mattec.id2208.hw2.topdown.FlightReservationServiceTopDown_Service service = new se.mattec.id2208.hw2.topdown.FlightReservationServiceTopDown_Service();
        se.mattec.id2208.hw2.topdown.FlightReservationServiceTopDown port = service.getFlightReservationServiceTopDownPort();
        return port.bookTicket(authToken, itineary, cardNumber);
    }

    private static Ticket issueTicket(java.lang.String authToken, java.lang.String ticketNumber) throws se.mattec.id2208.hw2.topdown.UnauthorizedException {
        se.mattec.id2208.hw2.topdown.FlightReservationServiceTopDown_Service service = new se.mattec.id2208.hw2.topdown.FlightReservationServiceTopDown_Service();
        se.mattec.id2208.hw2.topdown.FlightReservationServiceTopDown port = service.getFlightReservationServiceTopDownPort();
        return port.issueTicket(authToken, ticketNumber);
    }

}
