/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package se.mattec.id2208.hw3.client;

import java.util.List;
import javax.ws.rs.ClientErrorException;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.GenericType;
import se.mattec.id2208.hw3.server.models.Itineary;

/**
 * Jersey REST client generated for REST resource:FlightReservationService
 * [flightsReservationService]<br>
 * USAGE:
 * <pre>
 *        FlightsReservationRESTClient client = new FlightsReservationRESTClient();
 *        Object response = client.XXX(...);
 *        // do whatever with response
 *        client.close();
 * </pre>
 *
 * @author Mattias
 */
public class FlightsReservationRESTClient {
    private WebTarget webTarget;
    private Client client;
    private static final String BASE_URI = "http://localhost:8080/HW3Server";

    public FlightsReservationRESTClient() {
        client = javax.ws.rs.client.ClientBuilder.newClient();
        webTarget = client.target(BASE_URI).path("flightsReservationService");
    }

    public <T> T getItineary(Class<T> responseType, String token, String departureCity, String destinationCity, String date) throws ClientErrorException {
        WebTarget resource = webTarget;
        resource = resource.path(java.text.MessageFormat.format("getItineary/{0}/{1}/{2}/{3}", new Object[]{token, departureCity, destinationCity, date}));
        return resource.request(javax.ws.rs.core.MediaType.APPLICATION_XML).get(responseType);
    }

    public List<Itineary> getItinearies(String token, String departureCity, String destinationCity, String date) throws ClientErrorException {
        WebTarget resource = webTarget;
        resource = resource.path(java.text.MessageFormat.format("getItinearies/{0}/{1}/{2}/{3}", new Object[]{token, departureCity, destinationCity, date}));
        return resource.request(javax.ws.rs.core.MediaType.APPLICATION_XML).get(new GenericType<List<Itineary>>(){});
    }

    public String login(String username, String password) throws ClientErrorException {
        WebTarget resource = webTarget;
        if (username != null) {
            resource = resource.queryParam("username", username);
        }
        if (password != null) {
            resource = resource.queryParam("password", password);
        }
        resource = resource.path("login");
        return resource.request(javax.ws.rs.core.MediaType.TEXT_PLAIN).get(String.class);
    }

    public void changeCardNumber(Object requestEntity, String token, String ticketNr) throws ClientErrorException {
        webTarget.path(java.text.MessageFormat.format("changeCardNumber/{0}/{1}", new Object[]{token, ticketNr})).request(javax.ws.rs.core.MediaType.APPLICATION_XML).put(javax.ws.rs.client.Entity.entity(requestEntity, javax.ws.rs.core.MediaType.APPLICATION_XML));
    }

    public String bookTicket(Object requestEntity, String token, String cardNumber) throws ClientErrorException {
        return webTarget.path(java.text.MessageFormat.format("bookTicket/{0}/{1}", new Object[]{token, cardNumber})).request(javax.ws.rs.core.MediaType.APPLICATION_XML).post(javax.ws.rs.client.Entity.entity(requestEntity, javax.ws.rs.core.MediaType.APPLICATION_XML), String.class);
    }

    public void cancelBooking(String token, String ticketNr) throws ClientErrorException {
        webTarget.path(java.text.MessageFormat.format("cancelBooking/{0}/{1}", new Object[]{token, ticketNr})).request().delete();
    }

    public <T> T getTicket(Class<T> responseType, String token, String ticketNumber) throws ClientErrorException {
        WebTarget resource = webTarget;
        if (token != null) {
            resource = resource.queryParam("token", token);
        }
        if (ticketNumber != null) {
            resource = resource.queryParam("ticketNumber", ticketNumber);
        }
        resource = resource.path("getTicket");
        return resource.request(javax.ws.rs.core.MediaType.APPLICATION_XML).get(responseType);
    }

    public void close() {
        client.close();
    }
    
}
