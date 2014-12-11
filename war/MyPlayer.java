import java.util.ArrayList;

import javax.smartcardio.Card;


public class MyPlayer implements Player{
	//variables createCard user
	private ArrayList<Card> table;
	//default variable
	private int score;
	
	public MyPlayer(Set card){
		table = card;
	}

	public void drawCard(Card card){
		table.add(card);
	}
	
	public Card playCard(int i){
		return table.remove(i);
	}
}
