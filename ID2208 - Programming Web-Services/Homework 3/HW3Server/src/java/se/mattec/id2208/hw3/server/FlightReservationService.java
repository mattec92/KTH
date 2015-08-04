/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package se.mattec.id2208.hw3.server;

import java.util.List;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.UriInfo;
import javax.ws.rs.Path;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import se.mattec.id2208.hw3.server.models.Itineary;
import se.mattec.id2208.hw3.server.models.Ticket;

/**
 *
 * @author Mattias
 */
@Path("flightsReservationService")
public class FlightReservationService
{

    @Context
    private UriInfo context;

    public FlightReservationService()
    {
    }

    @GET
    @Path("login")
    @Produces(MediaType.TEXT_PLAIN)
    public String login(
            @QueryParam("username") String username,
            @QueryParam("password") String password)
    {
        if (FlightsDataService.getInstance().authorize(username, password))
        {
            return FlightsDataService.SECRET_TOKEN;
        }

        throw new WebApplicationException(Response.Status.UNAUTHORIZED);
    }

    @GET
    @Path("getItinearies/{token}/{departureCity}/{destinationCity}/{date}")
    @Produces(MediaType.APPLICATION_XML)
    public List<Itineary> getItinearies(
            @PathParam("token") String token,
            @PathParam("departureCity") String departureCity,
            @PathParam("destinationCity") String destinationCity,
            @PathParam("date") String date)
    {
        if (!token.equals(FlightsDataService.SECRET_TOKEN))
        {
            throw new WebApplicationException(Response.Status.UNAUTHORIZED);
        }

        List<Itineary> itinearies = FlightsDataService.getInstance().getIntinearies(departureCity, destinationCity, date);

        return itinearies;
    }

    @GET
    @Path("getItineary/{token}/{departureCity}/{destinationCity}/{date}")
    @Produces(MediaType.APPLICATION_XML)
    public Itineary getItineary(
            @PathParam("token") String token,
            @PathParam("departureCity") String departureCity,
            @PathParam("destinationCity") String destinationCity,
            @PathParam("date") String date)
    {
        if (!token.equals(FlightsDataService.SECRET_TOKEN))
        {
            throw new WebApplicationException(Response.Status.UNAUTHORIZED);
        }

        List<Itineary> itinearies = FlightsDataService.getInstance().getIntinearies(departureCity, destinationCity, date);

        if (!itinearies.isEmpty()) {
            return itinearies.get(0);
        }
        
        return null;
    }

    @POST
    @Path("bookTicket/{token}/{cardNumber}")
    @Produces(MediaType.APPLICATION_XML)
    @Consumes(MediaType.APPLICATION_XML)
    public String bookTicket(
            @PathParam("token") String token,
            @PathParam("cardNumber") String cardNumber,
            Itineary itineary)
    {
        if (!token.equals(FlightsDataService.SECRET_TOKEN))
        {
            throw new WebApplicationException(Response.Status.UNAUTHORIZED);
        }

        if (cardNumber != null && !cardNumber.isEmpty())
        {
            return FlightsDataService.getInstance().bookTicket(itineary, cardNumber);
        }

        throw new WebApplicationException(Response.Status.BAD_REQUEST);
    }

    @GET
    @Path("getTicket")
    @Produces(MediaType.APPLICATION_XML)
    public Ticket getTicket(
            @QueryParam("token") String token,
            @QueryParam("ticketNumber") String ticketNumber)
    {
        if (!token.equals(FlightsDataService.SECRET_TOKEN))
        {
            throw new WebApplicationException(Response.Status.UNAUTHORIZED);
        }

        return FlightsDataService.getInstance().getTicket(ticketNumber);
    }

    @PUT
    @Path("changeCardNumber/{token}/{ticketNr}")
    @Consumes(MediaType.APPLICATION_XML)
    public void changeCardNumber(
            @PathParam("token") String token,
            @PathParam("ticketNr") String ticketNr,
            String cardNr)
    {
        if (!token.equals(FlightsDataService.SECRET_TOKEN))
        {
            throw new WebApplicationException(Response.Status.UNAUTHORIZED);
        }
        
        FlightsDataService.getInstance().changeCardNumber(ticketNr, cardNr);
    }

    @DELETE
    @Path("cancelBooking/{token}/{ticketNr}")
    public void cancelBooking(
            @PathParam("token") String token,
            @PathParam("ticketNr") String ticketNr)
    {
        if (!token.equals(FlightsDataService.SECRET_TOKEN))
        {
            throw new WebApplicationException(Response.Status.UNAUTHORIZED);
        }
        
        FlightsDataService.getInstance().deleteTicket(ticketNr);
    }

}
