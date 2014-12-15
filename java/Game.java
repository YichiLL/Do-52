import java.util.Scanner;
import java.util.ArrayList;

public class Game {
	//default
	ArrayList<MyPlayer> players;
	Player player1;
	Player player2;
	Set deck;
	int numberOfPlayers = 4;
	int highestCard =12;
	boolean ascendingOrder = true;
	
	//instance variables
	int warCount = 0;
	
	//user defined variables
	//configure
	
	
	
	
	
	public Game(){
		//configure statements
		numberOfPlayers = 2;
		highestCard = 12;
		ascendingOrder = true;
		
		deck = new Deck(highestCard, ascendingOrder);
		players = new ArrayList<MyPlayer>();
	
		for(int i = 0; i < numberOfPlayers; i++){
			players.add(new MyPlayer("Player "+ (i+1) ));
		}
		
		
		
	}
	public void setup(){
		deck.shuffle();
		for(int i = 0; i< 26; i ++)
			Set.append(deck, Set.TOP, players.get(0).hand);
		for(int i = 0; i< 26; i ++)
			Set.append(deck, Set.TOP, players.get(1).hand);
	}
	public void round(){
		turn(players.get(0));
		turn(players.get(1));
		
		evaluate();

	}
	//user defined functions
	private void turn(MyPlayer player){
		System.out.println(player +"'s turn.");
		System.out.println("HAND COUNT: " + player.hand.size());
		if(player.hand.size() == 0){
			if(player==players.get(0)){
				System.out.println(player +" has lost. Player2 Wins!");
			}else{
				System.out.println(player +" has lost. Player1 Wins!");
			}
			System.exit(0);
		}
		System.out.println("Play Card?");
		Scanner a = new Scanner(System.in);
		
		String input = a.nextLine(); 
		if(input.compareTo("y")==0){
			Set.merge(player.table, player.hand.select(1));		
		}
		else{
			System.exit(0);
		}
	}
	
	public void evaluate(){
		if(players.get(0).hand.size()==0 || players.get(1).hand.size()==0){
			if(players.get(0).hand.size()==0 && players.get(1).hand.size()==0){
				System.out.println("It's a tie!");
			}else if(players.get(1).hand.size() == 0){
				System.out.println(players.get(0) +" has lost. Player2 Wins!");
			}else if(players.get(0).hand.size() == 0){
				System.out.println(players.get(1) +" has lost. Player1 Wins!");
			}
			System.exit(0);
		}
		//when we see logical operators, we immediately take the value
		if(players.get(0).table.value(0) > players.get(1).table.value(0)){
			System.out.println("Player 1's card is higher");
			//everytime we see a loop we make a variable of the cards that are taken out
			//draw_hand function takes n number of cards
			
			for(int i = 0; i< players.get(0).table.size(); i ++)
				Set.prepend(players.get(0).table, Set.TOP, players.get(0).hand);
			for(int i = 0; i< players.get(1).table.size(); i ++)
				Set.prepend(players.get(1).table, Set.TOP, players.get(0).hand);
			
		}else if(players.get(0).table.value(0) < players.get(1).table.value(0)){
			System.out.println("Player 2's card is higher");
			
			for(int i = 0; i< players.get(0).table.size(); i ++)
				Set.prepend(players.get(0).table, Set.TOP, players.get(1).hand);
			for(int i = 0; i< players.get(1).table.size(); i ++)
				Set.prepend(players.get(1).table, Set.TOP, players.get(1).hand);
			
			
		}else{
			
			System.out.println("It's a tie. That means WAR!");
			warCount = warCount +1;
			
			for(int i = 0; i< 4; i ++)
				Set.append(players.get(0).hand, Set.TOP, players.get(1).table);
			for(int i = 0; i< 4; i ++)
				Set.append(players.get(1).hand, Set.TOP, players.get(1).table);
			
			evaluate();
		}
		
	}

	
	
	
}
