
public class Deck {
	static final int ASCENDING = 0;
	static final int DESCENDING = 1;
	private Card[] deck;
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
}


