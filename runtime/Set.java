import java.util.ArrayList;
import java.util.Random;


public class Set {
	protected ArrayList<Card> set;
	public Set(ArrayList<Card> setOfCards){
		set = setOfCards;
	}
	
	public void shuffle(){
		Random rnd = new Random();
		for (int i = 0; i < set.size(); i++){
			swap(set, i, rnd.nextInt(set.size()));
		} 
	}
	
	private void swap(ArrayList<Card> deck, int i, int j){
		Card tmp = deck.get(i);
		deck.set(i, deck.get(j));
		deck.set(j, tmp);
	}
//draws out a card on top of the deck 
	public Card draw(){
		return set.remove(0);
	}
//draws out the top n cards	
	public ArrayList<Card> draw_hand(int n){
		ArrayList<Card>hand = new ArrayList<Card>();
		for(int i = 0; i < n; i++ ){
			hand.add(draw());
		}
		return hand;
	}
	
	public String toString(){
		String s = "";
		for (Card card: set){
			s += card.toString() + "\n";
		}
		return s;
	}
}
