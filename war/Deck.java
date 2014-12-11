import java.util.Random;


public class Deck {
	static final int ASCENDING = 0;
	static final int DESCENDING = 1;
	private Card[] deck;
	private int total_cards = 52; // Cards in deck
	
	public Deck(int maxCard, int order){
		int minCard;
		if (order == ASCENDING){
			minCard = (maxCard + 1) % 13;
			
		}else{
			minCard = maxCard - 1;
			if(minCard<0)
				minCard+= 13;
		}

		deck = new Card[52];
		for(int i = 0; i<52; i++){
			
			int num = i/4;
			int value;
			
			if(order == ASCENDING){
				value = num - minCard;
			}
			else{
				value = minCard - num;
			}
			int suitNum = i%4;
			int suitVal = 0;
			if (value<0)
				value+=13;
			deck[i] = new Card(num, value, suitNum, suitVal);
		}
	}
	
	public String toString(){
		String s = "";
		for (Card card: deck){
			s += card.toString() + "\n";
		}
		return s;
	}

	public void shuffle(){
		Random rnd = new Random();
		for (int i = 0; i < 52; i++){
			swap(deck, i, rnd.nextInt(i));
		} 
	}
	
	private void swap(Card[] d, int i, int j){
		Card tmp = d[i];
        d[i] = d[j];
        d[j] = tmp;
	}
	
	public Card draw(){
		Random rnd = new Random();
		int index = 0;
		do{
			index = rnd.nextInt( 52 );
		}while(deck[index] == null);
		
		total_cards--;
		Card tmp = deck[index];
		deck[index] = null;
		return tmp;
	}
	
	public Card[] draw_hand(){
		int hand_size = 5;
		Card[] hand = new Card[hand_size];
		for(int i = 0; i < hand_size; i++ ){
			hand[i] = draw();
		}
		return hand;
	}
}


