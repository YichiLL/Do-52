import java.util.ArrayList;
import java.util.Scanner;


public class MyPlayer implements Player{
	//variables created by user
	private Set table;
	//default variable
	private int score;
	private Set hand;
	
	public MyPlayer(){
		//user defined
		table = new Set();
		
		//default
		hand = new Set();
		score = 0;
	}
	
	/*=========default functions to deal with hand============*/
	public void drawCard(Card card){
		hand.addFront(card);
	}
	
	public void drawCard(Set drawSet){
		hand.merge(drawSet);
	}
	public Card playCard(int i){
		return hand.draw(i);
	}
	
	public int getScore(){
		return score;
	}
	
	public int selectCard(){
		System.out.println("Select a card:\n" + hand.toString());
		Scanner a = new Scanner(System.in);
		int input = a.nextInt();
		return input;
		
	}
	//end of default function
	/*=========user defined functions============*/
	/*public void drawCardFromTable(Ca){
		table.addFront(card);
	}
	
	public Card removeCardFromTable(i){
		;
	}*/
	
	
	//end of user defined functions
}
