package se.mattec.id2208.hw3.server.models;

import javax.xml.bind.annotation.XmlRootElement;

/**
 *
 * @author Mattias
 */
@XmlRootElement
public class Ticket {

    private String ticketNumber;
    private Itineary itineary;
    private String cardNumber;

    public Ticket() {
    }

    public Ticket(String ticketNumber, Itineary itineary, String cardNumber) {
        this.ticketNumber = ticketNumber;
        this.itineary = itineary;
        this.cardNumber = cardNumber;
    }

    public String getTicketNumber() {
        return ticketNumber;
    }

    public void setTicketNumber(String ticketNumber) {
        this.ticketNumber = ticketNumber;
    }

    public Itineary getItineary() {
        return itineary;
    }

    public void setItineary(Itineary itineary) {
        this.itineary = itineary;
    }

    public String getCardNumber() {
        return cardNumber;
    }

    public void setCardNumber(String cardNumber) {
        this.cardNumber = cardNumber;
    }

}
