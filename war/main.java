
public class main {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		//checking for deck
		Deck deck = new Deck(2, Deck.DESCENDING);
		System.out.println(deck);
		//testing for shuffle function
		System.out.println("--------------------------\nAfter Shuffle\n");
		deck.shuffle();
		System.out.println(deck);
		
	}

}
