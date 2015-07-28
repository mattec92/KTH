package se.mattec.id2208.hw2;

/**
 *
 * @author Mattias
 */
public class Flight {

    private String departureCity;
    private String destinationCity;

    private String date;

    private int price;

    public Flight(String departureCity, String destinationCity, String date, int price) {
        this.departureCity = departureCity;
        this.destinationCity = destinationCity;
        this.date = date;
        this.price = price;
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

    public String getDate() {
        return date;
    }

    public void setDate(String date) {
        this.date = date;
    }

    public int getPrice() {
        return price;
    }

    public void setPrice(int price) {
        this.price = price;
    }

}
