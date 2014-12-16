import java.util.Scanner;


public class Utility {
	public static boolean compareString(String str1, String str2) {
		if(str1.compareTo(str2)==0)
			return true;
		else
			return false;
	}
	public static boolean cardEquals(Card c1, Card c2){
		if(c1.val[0]==c2.val[0])
			return true;
		else
			return false;
	}
	
	public static boolean cardGreaterThan(Card c1, Card c2){
		if(c1.val[0] > c2.val[0])
			return true;
		else if(c1.val == c2.val){
			if(c1.suitVal[0] > c2.suitVal[0])
				return true;
			else
				return false;
		}
			return false;
	}
	
	public static boolean cardGreaterOrEqualThan(Card c1, Card c2){
		if(c1.val[0] >= c2.val[0])
			return true;
		else
			return false;
	}
	
	public static boolean cardLessThan(Card c1, Card c2){
		if(c1.val[0] < c2.val[0])
			return true;
		else if(c1.val == c2.val){
			if(c1.suitVal[0] < c2.suitVal[0])
				return true;
			else
				return false;
		} else
			return false;
	}
	
	public static boolean cardLessOrEqualThan(Card c1, Card c2){
		if(c1.val[0] <= c2.val[0])
			return true;
		else
			return false;
	}
	
	public static boolean cardNotEqual(Card c1, Card c2){
		if(c1.val != c2.val)
			return true;
		else
			return false;
	} 
	
	public static String inputString(){
		Scanner a = new Scanner(System.in);
		return a.nextLine();
	}
	
	public static int inputInt(){
		boolean error = true;
		while(error){
			try{
			error = false;
			Scanner a = new Scanner(System.in);
			int input = a.nextInt(); 

			}catch(Exception e){
				error = true;
				System.out.println("The number input is not valid. Input another number.");
			}
		}
		return 0;
	}
	
	public static boolean inputBool(){
		System.out.println("Y/N?");
		boolean error = true;
		while(error){
			
			error = false;
			Scanner a = new Scanner(System.in);
			String input = a.nextLine();
			
			if (compareString(input,"Y"))
				return true;
			else if(compareString(input,"N"))
				return false;
			else {
				error = true;
				System.out.println("Input not valid. Y/N?");
			}
				
			
		}
		return false;
	}
	
}