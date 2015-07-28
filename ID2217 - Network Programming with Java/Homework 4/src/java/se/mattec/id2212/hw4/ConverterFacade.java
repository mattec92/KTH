package se.mattec.id2212.hw4;

import java.io.Serializable;
import java.util.List;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;

@Stateless
public class ConverterFacade implements Serializable {

    @PersistenceContext(unitName = "CurrencyConverterPU")
    private EntityManager em;

    public ConverterFacade() {
    }

    public List<Currency> listCurrencies() {
        Query query = em.createQuery("SELECT c FROM Currency c", Currency.class);
        List<Currency> currenciesFromDB = query.getResultList();

        /*
        if (currenciesFromDB == null || currenciesFromDB.isEmpty()) {
            em.persist(new Currency("SEK", 1.0));
            em.persist(new Currency("USD", 7.5));
            em.persist(new Currency("EUR", 9.0));
            em.persist(new Currency("GBP", 11.0));

            currenciesFromDB = query.getResultList();
        }
        */

        return currenciesFromDB;
    }

    public double convert(double fromAmount, String fromCurrency, String toCurrency) {
        double fromRate = 0;
        double toRate = 0;

        for (Currency currency : listCurrencies()) {
            if (currency.getName().equals(fromCurrency)) {
                fromRate = currency.getRate();
            }
            if (currency.getName().equals(toCurrency)) {
                toRate = currency.getRate();
            }
        }

        return fromAmount * (fromRate / toRate);
    }

}
