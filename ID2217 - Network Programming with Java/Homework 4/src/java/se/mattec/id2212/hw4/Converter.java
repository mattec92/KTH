package se.mattec.id2212.hw4;

import java.io.Serializable;
import java.util.List;
import javax.ejb.EJB;
import javax.faces.bean.ManagedBean;
import javax.inject.Named;

@Named(value = "converter")
@ManagedBean
public class Converter implements Serializable {

    @EJB
    private ConverterFacade converterFacade;

    private String fromCurrency;
    private String toCurrency;
    private double amount;
    private double convertedAmount = -1;

    public Converter() {
    }

    public void convert() {
        convertedAmount = converterFacade.convert(amount, fromCurrency, toCurrency);
    }

    public List<Currency> getCurrencies() {
        return converterFacade.listCurrencies();
    }

    public String getFromCurrency() {
        return fromCurrency;
    }

    public void setFromCurrency(String fromCurrency) {
        this.fromCurrency = fromCurrency;
    }

    public String getToCurrency() {
        return toCurrency;
    }

    public void setToCurrency(String toCurrency) {
        this.toCurrency = toCurrency;
    }

    public double getAmount() {
        return amount;
    }

    public void setAmount(double amount) {
        this.amount = amount;
    }

    public double getConvertedAmount() {
        return convertedAmount;
    }

    public void setConvertedAmount(double convertedAmount) {
        this.convertedAmount = convertedAmount;
    }

}
