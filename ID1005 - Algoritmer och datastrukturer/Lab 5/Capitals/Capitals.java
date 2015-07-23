public class Capitals {
	public static void main(String[] arg) {
		WeightedDirectedGraph g = new WeightedDirectedGraph();
		//Lägg till städer
		String[]    citys = {"Stockholm", "Göteborg", "Malmö", "Västerås", "Luleå"};
        for (int i = 0; i < citys.length; i++) {
            g.addCorner (citys[i]);
        }
        //Lägg till vägar mellan städer
        g.addRoute("Stockholm", "Malmö", 100);
        g.addRoute("Stockholm", "Västerås", 40);
        g.addRoute("Göteborg", "Västerås", 70);
        g.addRoute("Göteborg", "Malmö", 20);
        g.addRoute("Göteborg", "Stockholm", 60);
        g.addRoute("Stockholm", "Göteborg", 65);
//       	g.addRoute("Malmö", "Luleå", 200);
//       	g.addRoute("Västerås", "Luleå", 150);
       	System.out.println(g);
       	System.out.println("Breadth first search:");
       	System.out.println(g.hasRouteBreadth("Stockholm", "Malmö"));
       	System.out.println(g.hasRouteBreadth("Stockholm", "Västerås"));
       	System.out.println(g.hasRouteBreadth("Stockholm", "Göteborg"));
       	System.out.println(g.hasRouteBreadth("Västerås", "Stockholm"));
       	System.out.println("\nDepth first search:");
       	System.out.println(g.hasRouteDepth("Stockholm", "Malmö"));
       	System.out.println(g.hasRouteDepth("Stockholm", "Västerås"));
       	System.out.println(g.hasRouteDepth("Stockholm", "Göteborg"));
       	System.out.println(g.hasRouteDepth("Västerås", "Stockholm"));
	}
}