package se.mattec.id2208.hw2;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 *
 * @author mcederlund
 */
public class FlightsDataService {

    public static final String SECRET_TOKEN = "SecretTokenThatAllowsAccess";

    private static Map<String, String> users;
    private static List<Flight> flights;

    public FlightsDataService() {
    }

    public static void init() {
        generateUsers();
        generateFlights();
    }

    private static void generateUsers() {
        users = new HashMap<>();

        users.put("mattias", "mattias");
        users.put("asd", "asd");
        users.put("qwe", "qwe");
    }

    private static void generateFlights() {
        flights = new ArrayList<>();

        for (int i = 0; i < 100; i++) {
            String departureCity = "";
            String destinationCity = "";
            String date = "";
            int price = (int) (Math.random() * 500) + 200;

            switch (((int) (Math.random() * 5))) {
                case 0:
                    departureCity = "Stockholm";
                    break;
                case 1:
                    departureCity = "London";
                    break;
                case 2:
                    departureCity = "Oslo";
                    break;
                case 3:
                    departureCity = "Paris";
                    break;
                case 4:
                    departureCity = "Berlin";
                    break;
            }

            switch (((int) (Math.random() * 5))) {
                case 0:
                    destinationCity = "Stockholm";
                    break;
                case 1:
                    destinationCity = "London";
                    break;
                case 2:
                    destinationCity = "Oslo";
                    break;
                case 3:
                    destinationCity = "Paris";
                    break;
                case 4:
                    destinationCity = "Berlin";
                    break;
            }

            switch (((int) (Math.random() * 5))) {
                case 0:
                    date = "2015-02-04";
                    break;
                case 1:
                    date = "2015-02-05";
                    break;
                case 2:
                    date = "2015-02-06";
                    break;
                case 3:
                    date = "2015-02-07";
                    break;
                case 4:
                    date = "2015-02-08";
                    break;
            }

            if (!departureCity.equals(destinationCity)) {
                flights.add(new Flight(departureCity, destinationCity, date, price));
            }
        }
    }

    public static List<Itineary> getIntinearies(String departureCity, String destinationCity, String date) {
        List<Itineary> itinearies = new ArrayList<>();

        //Find direct flights
        for (Flight flight : flights) {
            if (flight.getDepartureCity().equals(departureCity)
                    && flight.getDestinationCity().equals(destinationCity)
                    && (date == null || flight.getDate().equals(date))) {
                List<Flight> flightsOfItineary = new ArrayList<>();
                flightsOfItineary.add(flight);
                Itineary itineary = new Itineary(departureCity, destinationCity, flightsOfItineary);
                itinearies.add(itineary);
            }
        }

        //Find flights with 1 stop
        for (Flight flight : flights) {
            if (flight.getDepartureCity().equals(departureCity)
                    && (date == null || flight.getDate().equals(date))) {
                List<Flight> flightsOfItineary = new ArrayList<>();
                flightsOfItineary.add(flight);
                for (Flight flightTwo : flights) {
                    if (flightTwo.getDepartureCity().equals(flight.getDestinationCity())
                            && flightTwo.getDestinationCity().equals(destinationCity)
                            && ((date == null && flightTwo.getDate().compareTo(flight.getDate()) >= 0) || flightTwo.getDate().equals(date))) {
                        flightsOfItineary.add(flightTwo);
                        Itineary itineary = new Itineary(departureCity, destinationCity, flightsOfItineary);
                        itinearies.add(itineary);
                        break;
                    }
                }
            }
        }

        return itinearies;
    }

    public static boolean authorize(String username, String password) {
        return users.containsKey(username) && password.equals(users.get(username));
    }

}
