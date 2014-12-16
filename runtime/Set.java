import java.util.ArrayList;
import java.util.Random;
import java.util.Scanner;


public class Set {
	
	protected ArrayList<Card> set;
	
	final static int TOP = 0;
	final static int BOTTOM = 1;
	final static int APPEND = 0;
	final static int PREPEND = 1;
	

	public Set(ArrayList<Card> setOfCards){
		set = setOfCards;
	}
	
	public Set(){
		set = new ArrayList<Card>();
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
	/* ==========================Prepend===========================/
	 * sourceSet: source
	 * topOrBottom : indicates topOrBottom card from source set
	 * targetSet: Set that the Card from the source will be put on the top of
	 */
	public static void prepend(Set sourceSet, int topOrBottom, Set targetSet){
		if(sourceSet.size()==0)
			return;
		if(topOrBottom==TOP){
			targetSet.set.add(0,sourceSet.set.remove(0));
		}else if(topOrBottom==BOTTOM){
			targetSet.set.add(0,sourceSet.set.remove(sourceSet.size()-1));
		}else{
			System.out.println("error");
		}
	}
	/* ==========================append===========================/
	 * sourceSet: source
	 * topOrBottom : indicates topOrBottom card from source set
	 * targetSet: Set that the Card from the source will be put on the bottom of
	 */
	public static void append(Set sourceSet, int topOrBottom, Set targetSet){
		if(sourceSet.size()==0)
			return;
		if(topOrBottom==TOP){
			targetSet.set.add(sourceSet.set.remove(0));
		}else if(topOrBottom==BOTTOM){
			targetSet.set.add(sourceSet.set.remove(sourceSet.size()-1));
		}else{
			System.out.println("error");
		}
	}

//draws out the top n cards	
	public Set draw_hand(int n){
		if(size()<n)
			return null;
		ArrayList<Card> hand = new ArrayList<Card>();
		for(int i = 0; i < n; i++ ){
			hand.add(draw());
		}
		Set newSet = new Set(hand);
		return newSet;
	}
//draws out a card on top of the deck 
	public Card draw(){
		if(size()==0)
			return null;
		return set.remove(0);
	}
//remove a specific card
	public Card draw(int n){
		if(size()==0)
			return null;
		if (size()<n)
			return set.remove(size());
		return set.remove(n);
	}
	public String toString(){
		String s = "";
		for (int i = 0; i< set.size(); i++){
			s += (i+1)+":"+ set.get(i).toString() + "\n";
		}
		return s;
	}
	public Set select(int n){
		Set set = new Set();
		ArrayList<Card> chosenCards = set.set;
		
		//makes sure # cards selected is less than set size
		int min = Math.min(size(), n);
		int input = 0;
		for (int i =0; i< min; i++){
			System.out.println("Select a card:\n" + toString());
			boolean error = true;
			while(error){
				try{
				Scanner a = new Scanner(System.in);
				input = a.nextInt() - 1; 
				if(input < size() && input >= 0){
					error = false;
					
				}
				else
					System.out.println("The number input is not valid. Make another selection.");
			
				}catch(Exception e){
					System.out.println("The number input is not valid. Make another selection.");
				}
			}
			System.out.println("You have selected "+ this.set.get(input));
			chosenCards.add(draw(input));
			
		}
		return set;
	}
	public Card peek(int n){
		if(n>(size()-1))
			return null;
		return set.get(n);

	}
	
	public Card top(){
		if(size()>0)
			return peek(0);
		return null;
	}
	
	public Card bottom(){
		if(size()>0)
			return peek(size());
		return null;
	}
//	public int value(int n){
//		if(n > (size()-1)){
//			return 0;
//		}
//		return set.get(n).val;
//	}
//	
//	public int suitValue(int n){
//		if(n > (size()-1))
//			return 0;
//		return set.get(n).suitVal[0];
//	}
	
	static public Set merge(Set set1, Set set2){
		Set set= new Set();
		ArrayList<Card> mergeSet = set.set;
		for(Card c: set1.set){
			mergeSet.add(c);
		}
		for(Card c: set2.set){
			mergeSet.add(c);
		}
		
		return set;
	}
	
	public int size(){
		return set.size();
	}
}