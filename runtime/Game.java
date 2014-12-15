import java.util.Scanner;
import java.util.ArrayList;

public class Game {
ArrayList<MyPlayer> players;
Set deck;
int numberOfPlayers = 4;
int highestCard = 12;
boolean ascendingOrder = true;

public Game() {
deck = new Deck(highestCard, ascendingOrder);
players = new ArrayList<MyPlayer>();
for(int i = 0; i < numberOfPlayers; i++) {
players.add(new MyPlayer("Player " + (i+1)));
}
}

public void setup()
{
System.out.println("Hello, World!");}

public void round()
{
System.exit(0);}
}