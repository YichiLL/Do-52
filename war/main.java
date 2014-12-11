
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
		System.out.println("Draw 1: "+ deck.draw());
		System.out.println("Draw 2: "+ deck.draw());
		//create player
		Set hand = deck.draw_hand(10);
		MyPlayer p1 = new MyPlayer();
		p1.drawCard(hand);
		int i = p1.selectCard();
		System.out.println("p1 selected "+ p1.playCard(i));
		///////////////war///////////////
		/*
		boolean play = 1;
		int playerCount = 2;
		Player[] players = new Player[2];
		
		for(int i = 0; i < playerCount; i++){
			players[i] = new MyPlayer();
		}
		//setup
		for(int i = 0; i<deck.size()/2; i ++){
			players[0].drawCard(deck.draw());
		}
		while(play){
			
		}*/
		
	}

}
