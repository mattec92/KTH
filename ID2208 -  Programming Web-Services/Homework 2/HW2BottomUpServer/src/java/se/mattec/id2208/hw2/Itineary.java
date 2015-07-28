package se.mattec.id2208.hw2;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Mattias
 */
public class Itineary {

    private String departureCity;
    private String destinationCity;

    private List<Flight> flights;

    public Itineary(String departureCity, String destinationCity, List<Flight> flights) {
        this.departureCity = departureCity;
        this.destinationCity = destinationCity;
        this.flights = flights;
    }

    public String getDepartureCity() {
        return departureCity;
    }

    public void setDepartureCity(String departureCity) {
        this.departureCity = departureCity;
    }

    public String getDestinationCity() {
        return destinationCity;
    }

    public void setDestinationCity(String destinationCity) {
        this.destinationCity = destinationCity;
    }

    public List<Flight> getFlights() {
        return flights;
    }

    public void setFlights(List<Flight> flights) {
        this.flights = flights;
    }
    
    public int getPrice() {
        int price = 0;
        
        if (flights != null) {
            for (Flight flight : flights) {
                price += flight.getPrice();
            }
        }
        
        return price;
    }

}
