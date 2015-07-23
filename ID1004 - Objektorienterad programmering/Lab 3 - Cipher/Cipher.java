
/**
 * Abstrakt superklass för olika chiffer.
 * @author fki@kth.se
 */
public abstract class Cipher {

  /**
   * Håller krypteringsnyckeln.
   */
  protected final long key;

  /**
   * Konstruktor med nyckel. Subklassers konstruktorer bör anropa
   * denna konstruktor för att installera nyckeln.
   * @param key Nyckeln som chiffret använder.
   */
  public Cipher(long key) {
    this.key = key;
  }

  /**
   * Krypterar ett tecken och returnerar det.
   * @param plainChar Ett tecken i klartext, endast A-Z
   * @return Det krypterade tecknet, endast A-Z
   */
  public abstract char encryptChar(char plainChar);

  /**
   * Dekrypterar ett tecken och returnerar det.
   * @param cryptoChar Ett tecken i kryptotext, endast A-Z
   * @return Tecknet i klartest, A-Z
   */
  public abstract char decryptChar(char cryptoChar);

}

