package se.mattec.id2208.hw2;

import java.util.List;
import javax.jws.WebService;
import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.ejb.Stateless;

/**
 *
 * @author Mattias
 */
@WebService(serviceName = "FlightReservationServiceBottomUp")
@Stateless()
public class FlightReservationServiceBottomUp {

    public FlightReservationServiceBottomUp() {
        FlightsDataService.init();
    }

    /**
     * Authorizes the user for future use of the service.
     *
     * @param username Username for user account
     * @param password Password for user account
     * @return Authorization token used in other operations
     */
    @WebMethod(operationName = "login")
    public String login(@WebParam(name = "username") String username,
            @WebParam(name = "password") String password) throws UnauthorizedException {
        if (FlightsDataService.authorize(username, password)) {
            return FlightsDataService.SECRET_TOKEN;
        }

        throw new UnauthorizedException("Not authorized");
    }

    /**
     * Returns an itineary for provided details.
     *
     * @param authToken Authorization token for granting access to operation.
     * @param departureCity Departure city
     * @param destinationCity Destination city
     * @return Itineary for provided details
     * @throws java.lang.Exception If unauthorized
     */
    @WebMethod(operationName = "getItinearies")
    public List<Itineary> getItinearies(@WebParam(name = "authToken") String authToken,
            @WebParam(name = "departureCity") String departureCity,
            @WebParam(name = "destinationCity") String destinationCity,
            @WebParam(name = "date") String date) throws UnauthorizedException {
        if (!authToken.equals(FlightsDataService.SECRET_TOKEN)) {
            throw new UnauthorizedException("Not authorized");
        }

        return FlightsDataService.getIntinearies(departureCity, destinationCity, date);
    }

}
