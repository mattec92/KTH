import java.util.regex.*;
import java.util.Scanner;

public class EmailCheck {

    public static void main(String[] args) {
    	Scanner sc = new Scanner(System.in);
    	String in = "";
    	while (true) {
	    	Pattern p = Pattern.compile("^[a-zA-Z0-9!#$%*/?|^{}`~&'+\\-=_]+(\\.[a-zA-Z0-9!#$%*/?|^{}`~&'+\\-=_]+)?@[a-z0-9]+((\\.[a-z0-9]+)+)?(.com|.org|.net|.nu|.se)+$");
			Matcher m = p.matcher(sc.nextLine());
			System.out.println(m.find());
    	}
    }


}