package se.mattec.id2208.hw2.topdown;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 *
 * @author mcederlund
 */
public class TicketService {

    public static final String SECRET_TOKEN = "SecretTokenThatAllowsAccess";

    private static Map<String, Ticket> tickets;

    public TicketService() {
    }

    public static void init() {
        tickets = new HashMap<>();
    }

    public static String bookTicket(se.mattec.id2208.hw2.topdown.Itineary itineary, String cardNumber) {
        Ticket ticket = new Ticket();
        ticket.setItineary(itineary);
        ticket.setCardNumber(cardNumber);
        ticket.setTicketNumber(String.valueOf((long) (Math.random() * Long.MAX_VALUE)));

        tickets.put(ticket.getTicketNumber(), ticket);

        return ticket.getTicketNumber();
    }

    public static Ticket getTicket(String ticketNumber) {
        return tickets.get(ticketNumber);
    }

}
