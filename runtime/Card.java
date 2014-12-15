public class Card {
	static final int SPADE = 4;
	static final int HEART = 3;
	static final int CLUB = 2;
	static final int DIAMOND = 1;
	static final String[] cardNames = {
		"One", "Two", "Three", "Four", "Five",
		"Six", "Seven", "Eight", "Nine", "Ten",
		"Jack", "Queen", "King"
	};
	static final String[] suitNames = {
		"Spade", "Heart", "Clover", "Diamond"
	};
	static final int JACK = 10;
	static final int QUEEN = 11;
	static final int KING = 12;
	static final int ACE = 0;
	
	protected int cardNumber;
	int val;
	protected int suit;
	int suitVal;
	
	/*example, in a game of big 2, the card 2 of Spades will have
	cardNumber = 2
	val = 13
	suit = 1
	suitVal = 4 */
	public Card(int num, int value, int suitNum, int suitValue){
		cardNumber = num;
		val = value;
		suit = suitNum;
		suitVal = suitNum;
				
	}
	
	//interface of the card
	public String toString(){
		return cardNames[cardNumber] + " of " + suitNames[suit];
	}
	
	

}
