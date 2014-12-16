import java.util.ArrayList;
public class Deck extends Set{

	static final int DECKSIZE = 52;
	
	public Deck(int maxCard, boolean ascend){
		super(null);
		int minCard;
		if (ascend){
			minCard = (maxCard + 1) % 13;
			
		}else{
			minCard = maxCard - 1;
			if(minCard<0)
				minCard+= 13;
		}
		ArrayList<Card> deck = new ArrayList<Card>();
		for(int i = 0; i<52; i++){
			
			int num = i/4;
			int value;
			
			if(ascend){
				value = num - minCard;
			}
			else{
				value = minCard - num;
			}
			int suitNum = i%4;
			int suitVal = 0;
			if (value<0)
				value+=13;
			deck.add(new Card(num, value, suitNum, suitVal));
		}
		super.set = deck;
		

	}
	
	public void arrangeSuit(int first, int second, int third, int fourth){
		for(Card c : super.set){
			if(c.suit == first){
				c.suitVal = 4;
			}else if(c.suit== second){
				c.suitVal = 3;
			}else if(c.suit == third){
				c.suitVal = 2;
			} else{
				c.suitVal = 1;
			}
			
		}
	}
	

	

}


