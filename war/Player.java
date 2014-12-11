public class Player {
	static final int hand_size = 5; 
	public Card[] hand = new Card[hand_size];
	public String name;
	
	public Player(String name, Card[] hand) {
		this.name = name;
		this.hand = hand;
	}	
	
	public void drawCard(Card card){
		for(int i = 0; i < hand_size; i++){
			if(hand[i] == null){
				hand[i] = card;
				break;
			}
		}
	}
	
	public Card playCard(int i){
		Card c = hand[i];
		hand[i] = null;
		return c;
	}
}
