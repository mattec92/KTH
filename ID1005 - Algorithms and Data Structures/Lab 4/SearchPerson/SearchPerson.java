import java.util.*;
import java.io.*;

public class SearchPerson {
	//Klassen Person, innehåller information om en person.
	private static class Person implements Comparable<Person> {
		String name;
		int age;
		String city;
		public Person(String name, int age, String city) {
			this.name = name;
			this.age = age;
			this.city = city;
		}
		//CompareTo - bestämmer hur Persons ska jämföras med varandra (Via namn).
		public int compareTo(Person person2) {
			return this.name.compareTo(person2.name);

		}
		//toString - returnerar en String-representation av personen.
		public String toString() {
			return "Name/Age/City: " + name + "/" + age + "/" + city;
		}
	}

	public static void main(String[] arg) throws IOException {
		HashTable<String, Person> reg = new HashTable<String, Person>(100);
		reg.put("Mattias", new Person("Mattias", 19, "Stockholm"));
		reg.put("Pelle", new Person("Pelle", 12, "Blajstan"));
		reg.put("Kalle", new Person("Kalle", 33, "Göteborg"));
		reg.put("Linus", new Person("Linus", 21, "Stockoholm"));
		reg.put("Lisa", new Person("Lisa", 3, "Hemma"));
		reg.put("Emma", new Person("Emma", 54, "Malmö"));
//		System.out.println(reg.contains("Mattias"));
//		System.out.println(reg.get("Kalle"));
		reg.put("Kalle", new Person("Kalle", 23, "Leksand"));
//		System.out.println(reg.get("Kalle"));
		reg.toFile();

		BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
		System.out.println("Kommandon: \nadd: Lägg till en person till registret.\nremove: Tar bort en person ur registret.\n" +
							"get: Visar information om en person.\nfile: Skriver registret till en fil.");
		while (true) {
			System.out.println("********************************");
			String in = reader.readLine();
			if (in.equalsIgnoreCase("add")) {
				System.out.println("Ange info om personen med syntax: namn/ålder/stad");

				String[] info = reader.readLine().split("/");
				reg.put(info[0], new Person(info[0], Integer.parseInt(info[1]), info[2]));
			}
			else if (in.equalsIgnoreCase("remove")) {
				System.out.println("Ange namn på personen du vill ta bort");
				reg.remove(reader.readLine());
			}
			else if (in.equalsIgnoreCase("get")) {
				System.out.println("Ange namn på personen du vill ha info om.");
				System.out.println(reg.get(reader.readLine()));
			}
			else if (in.equalsIgnoreCase("file")) {
				System.out.println("Skriver registret till fil.");
				reg.toFileSorted();
			}
		}
	}
}