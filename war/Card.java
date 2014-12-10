package war;

public class Card {
	static final int SPADE = 0;
	static final int HEART = 1;
	static final int CLOVER = 2;
	static final int DIAMOND = 3;
	
	static final String[] cardNames = {
		"One", "Two", "Three", "Four", "Five",
		"Six", "Seven", "Eight", "Nine", "Ten",
		"Jack", "Queen", "King"
	};
	
	static final String[] suitNames = {
		"Spade", "Heart", "Clover", "Diamond"
	};
	
	private int cardNumber;
	private int val;
	private int suit;
	private int suitVal;
	
	/*example, in a game of big 2, the card 2 of Spades will have
	cardNumber = 2
	val = 13
	suit = 1
	suitVal = 4 */
	public Card(int num, int value, int suitNum, int suitValue){
		cardNumber = num;
		val = value;
		suit = suitNum;
		suitVal = suitValue;
				
	}
	
	//interface of the card
	public String toString(){
		return cardNames[cardNumber] + " of " + suitNames[suit] + " | Val: " + val;
	}
	
	


}
