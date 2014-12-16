
import java.util.ArrayList;
import java.util.Scanner;


public class Player{
	
	//default variable
	Set hand;
	String playerName;
	
	public Player(String name){
		
		//default
		hand = new Set();
		playerName = name;
	}
	
	public String toString(){
		return playerName;
	}

	public String[] desc(){
		String[] sOutput = new String[1];
		sOutput[0] = toString();
		return sOutput;
	}

	
	
	//end of user defined functions
}