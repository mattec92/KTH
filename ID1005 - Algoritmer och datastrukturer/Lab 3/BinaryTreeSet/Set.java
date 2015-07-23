// Set.java

/**********************************************************************

Gränssnitet Set definierar en mängd med obegränsad kapacitet.

En mängd är en samling element, där ett element inte kan förekomma mer
än en gång. Element i mängden ändras inte på något sätt - de är där som
de är.

******************************* ***************************************/

// package fjava.collection.set;

import java.util.*;  // Iterable, Iterator


public interface Set<E> extends Iterable<E>
{
	// isEmpty returnerar true om mängden är tom, annars false
	boolean isEmpty ();


	// size returnerar antalet element i mängden
	int size ();


    // contains returnerar true om mängden innehåller ett element som
    // är likadant som ett givet element, annars false.
    boolean contains (E element);


	// add lägger till ett givet element till mängden. Om ett sådant
	// element redan finns i mängden, så gör metoden ingenting.
	void add (E element);


    // remove tar bort det element i mängden som är likadant som
    // ett givet element. Om ett sådant element inte finns i mängden,
    // så gör metoden ingenting.
	void remove (E element);


	// clear tar bort alla element från mängden
	void clear ();


	// isSubsetOf returnerar true om mängden är en delmängd av en given
	// mängd, annars false
	boolean isSubsetOf (Set<E> set);


	// union returnerar unionen av mängden och en given mängd.
	Set<E> union (Set<E> set);


	// intersection returnerar snittet av mängden och en given mängd.
	Set<E> intersection (Set<E> set);


	// difference returnerar differensen av mängden och en given mängd.
	Set<E> difference (Set<E> set);


	// iterator returnerar en iterator till mängden.
	// Den returnerade iteratorn kan användas för iteration genom
	// mängden, och för borttagning av element från mängden.
	// Medan iteratorn används för iteration genom mängden, ska
	// mängden inte ändras på något sätt (förutom möjligen genom
	// själva iteratorn).
	Iterator<E> iterator ();
}
