/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package se.mattec.id2208.hw3.client;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import javax.inject.Named;
import se.mattec.id2208.hw3.server.models.Flight;
import se.mattec.id2208.hw3.server.models.Itineary;
import se.mattec.id2208.hw3.server.models.Ticket;

/**
 *
 * @author Mattias
 */
@Named(value = "flightReservationService")
@ManagedBean
@SessionScoped
public class FlightReservationService {

    private FlightsReservationRESTClient flightsReservationRESTClient;

    private String secretToken;

    Itineary itineary;

    private String itineariesRepresentation = "";

    private String ticketNr;

    private String ticketRepresentation = "";

    public FlightReservationService() {
        flightsReservationRESTClient = new FlightsReservationRESTClient();
    }

    public void login(String username, String password) {
        String loginResponse = flightsReservationRESTClient.login(username, password);
        
        if (loginResponse != null) {
            secretToken = loginResponse;
        }
        else {
            secretToken = "";
        }
    }

    public void getItinearies(String departureCity, String destinationCity, String date) {
        itineary = flightsReservationRESTClient.getItineary(Itineary.class, secretToken, departureCity, destinationCity, date);
        
        if (itineary != null) {
            itineariesRepresentation = itinearyToString(itineary);
        }
        else {
            itineariesRepresentation = "";
        }
    }

    public void bookTicket(String cardNr) {
        ticketNr = flightsReservationRESTClient.bookTicket(itineary, secretToken, cardNr);
    }

    public void getTicket(String ticketNr) {
        Ticket ticket = flightsReservationRESTClient.getTicket(Ticket.class, secretToken, ticketNr);

        if (ticket != null) {
            ticketRepresentation = "TicketNr: " + ticket.getTicketNumber() + "\n";
            ticketRepresentation += "CardNr: " + ticket.getCardNumber() + "\n";
            ticketRepresentation += itinearyToString(ticket.getItineary());
        }
        else {
            ticketRepresentation = "";
        }
    }

    public void changeCardNumber(String ticketNr, String cardNr) {
        flightsReservationRESTClient.changeCardNumber(cardNr, secretToken, ticketNr);
    }

    public void cancelBooking(String ticketNr) {
        flightsReservationRESTClient.cancelBooking(secretToken, ticketNr);
    }

    //Helpers
    private String itinearyToString(Itineary itineary) {
        String itinearyString = "";//------------------------------\n";
        itinearyString += "Itineary for trip from " + itineary.getDepartureCity() + " to " + itineary.getDestinationCity() + ": ";

        int price = 0;
        for (Flight flight : itineary.getFlights()) {
            itinearyString += flight.getDate() + " - From " + flight.getDepartureCity() + " to " + flight.getDestinationCity() + " ";

            price += flight.getPrice();
        }

        itinearyString += "Price: $" + price;

        return itinearyString;
    }

    //Getters
    public String getSecretToken() {
        return secretToken;
    }

    public void setSecretToken(String secretToken) {
        this.secretToken = secretToken;
    }

    public String getItineariesRepresentation() {
        return itineariesRepresentation;
    }

    public void setItineariesRepresentation(String itineariesRepresentation) {
        this.itineariesRepresentation = itineariesRepresentation;
    }

    public String getTicketNr() {
        return ticketNr;
    }

    public void setTicketNr(String ticketNr) {
        this.ticketNr = ticketNr;
    }

    public String getTicketRepresentation() {
        return ticketRepresentation;
    }

    public void setTicketRepresentation(String ticketRepresentation) {
        this.ticketRepresentation = ticketRepresentation;
    }

}
