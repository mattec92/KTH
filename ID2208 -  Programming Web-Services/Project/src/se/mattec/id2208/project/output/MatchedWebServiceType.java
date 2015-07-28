//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.11 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2015.02.24 at 07:58:49 PM CET 
//


package se.mattec.id2208.project.output;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for MatchedWebServiceType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="MatchedWebServiceType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="OutputServiceName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="InputServiceName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="MacthedOperation" type="{http://www.kth.se/ict/id2208/Matching}MatchedOperationType" maxOccurs="unbounded"/&gt;
 *         &lt;element name="WsScore" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "MatchedWebServiceType", propOrder = {
    "outputServiceName",
    "inputServiceName",
    "macthedOperation",
    "wsScore"
})
public class MatchedWebServiceType {

    @XmlElement(name = "OutputServiceName", required = true)
    protected String outputServiceName;
    @XmlElement(name = "InputServiceName", required = true)
    protected String inputServiceName;
    @XmlElement(name = "MacthedOperation", required = true)
    protected List<MatchedOperationType> macthedOperation;
    @XmlElement(name = "WsScore")
    protected double wsScore;

    /**
     * Gets the value of the outputServiceName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getOutputServiceName() {
        return outputServiceName;
    }

    /**
     * Sets the value of the outputServiceName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setOutputServiceName(String value) {
        this.outputServiceName = value;
    }

    /**
     * Gets the value of the inputServiceName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getInputServiceName() {
        return inputServiceName;
    }

    /**
     * Sets the value of the inputServiceName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setInputServiceName(String value) {
        this.inputServiceName = value;
    }

    /**
     * Gets the value of the macthedOperation property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the macthedOperation property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getMacthedOperation().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link MatchedOperationType }
     * 
     * 
     */
    public List<MatchedOperationType> getMacthedOperation() {
        if (macthedOperation == null) {
            macthedOperation = new ArrayList<MatchedOperationType>();
        }
        return this.macthedOperation;
    }

    /**
     * Gets the value of the wsScore property.
     * 
     */
    public double getWsScore() {
        return wsScore;
    }

    /**
     * Sets the value of the wsScore property.
     * 
     */
    public void setWsScore(double value) {
        this.wsScore = value;
    }

}
