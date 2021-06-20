/**
Just to help me understand the algo

Next pos = (i-1 , j+1)

if (-1 , n) then (0 , n-2)

if i == -1 then i = n - 1
if j == n then j = 0

if occupied (i+1, j-2)

We start at 1,2
 
Then (i,j) -> (i-1,j+1) SO now (0,3)

But 3 doesn't exist so if j == n then j = n so now (i,j) = (0,0)

(0,0) -> (-1, 1) but -1 doesn't exist so if i == -1  then i = n - 1. So now (i,j) = (2,1)

(2,1) -> (1,2) But (1,2) is occupied so we do i+= 1 and j=-2 so now (2,0)

(2,0) -> (1,1) 

(1,1) -> (0,2)

(0,2) -> (-1,3) Applying the rules we get (2,0)

(-1,3) == (-1,n) so now it's (0,1)

(0,1) -> (-1,2) applying the rules its (2,2)

(2,2) -> (1,3) applying the rules its (1,0)

We done 
*/

import java.util.Scanner;
import java.util.Arrays;
import java.util.Random;

public class magicSquare
{

	public static void printSquare(int[][] square)
	{
		for (int i = 0; i < square.length; i++)
		{
			System.out.print("  " + (i + 1));
		}
		System.out.println();
		for (int k = 0; k < square.length; k++)
		{
			System.out.print(k + 1);
			System.out.println(Arrays.toString(square[k]));
		}

		System.out.println();
	}
	
	public static int[][] generateSquare(int n)
	{
		int x = n/2;
		int y = n-1;
		int[][] square = new int[n][n]; 	

		//Populating the square with zeros
		for (int xCord = 0; xCord < n; xCord++)
		{
			for (int yCord = 0; yCord < n; yCord++)
			{
				square[xCord][yCord] = 0;
			}
		}
		square[x][y] = 1;
		//printSquare(square);

		for (int i = 2; i < (n * n) + 1; i++)
		{

			x = x - 1;
			y = y + 1;
			//System.out.println("old: "+ x +" "+ y);
			if (x == -1 && y == n)
			{
				//System.out.println("X out of bounds and Y out of bounds");
				x = 0;
				y = n-2;
			}
			else if (x == -1)
			{
				//System.out.println("X out of bounds");
				x = n - 1;
			}
			else if (y == n)
			{
				//System.out.println("Y out of bounds");
				y = 0;
			}
			else if(square[x][y] != 0)
			{
				//System.out.println("Occupied");
				x = x + 1;
				y = y - 2;  
			}
			//System.out.println("new :" + x + " " + y);
			square[x][y] = i;
			//printSquare(square);
		}

		return square;
	}

	public static int[][] shuffleSquare(int[][] square)
	{

		//What I want is for a random square to be picked
		//Then pick a random direction
		//If that move is valid
		//Execute that move
		//Else 
		//Try again 
		int len = square.length;

		Random rand = new Random();
		int xNum = rand.nextInt(square.length);
		int yNum = rand.nextInt(square.length);

		int direction;

		int[][] newSquare = square;

		for (int i = 0; i < len*len; i++)
		{
			xNum = rand.nextInt(square.length);
			yNum = rand.nextInt(square.length);
			
		
			direction = rand.nextInt(5);
			switch (direction)
			{
				case 0:
					if (isValidMove(square, xNum, yNum, 'U'))
					{
						newSquare = performUserMove(square, xNum, yNum, 'U');
					}
					else
					{
						i--;
					}
					break;
				case 1:
					if (isValidMove(square, xNum, yNum, 'R'))
					{
						newSquare = performUserMove(square, xNum, yNum, 'R');
					}
					else
					{
						i--;
					}
					break;
				case 2:
					if (isValidMove(square, xNum,yNum,'D'))
					{
						newSquare = performUserMove(square, xNum, yNum, 'D');
				 	}
				 	else
					{
						i--;
					}
					break;
				case 3:
					if (isValidMove(square, xNum, yNum, 'L'))
					{
						newSquare = performUserMove(square, xNum, yNum, 'L');
					}
					else
					{
						i--;
					}
					break;
			}
		}

		return newSquare;
	}

	public static int[][] swap(int[][] square, int x1, int y1, int x2, int y2)
	{
		//Swapping co-ords
		int origNum = square[x1][y1];
		square[x1][y1] = square[x2][y2];
		square[x2][y2] = origNum;
		return square;
	}

	public static boolean isValidMove(int[][] square, int x, int y, Character direction)
	{	
		//Basically this adds moves up left down or up and tests whether the new postion is out of the box or not.
		int newIndex = 0;
		switch(direction)
		{
			case 'U':
				newIndex = x - 1;
				break;
			case 'R':
				newIndex = y + 1;
				break;
			case 'D':
				newIndex = x + 1;
				break;
			case 'L':
				newIndex = y - 1;
				break;
		}

		if (newIndex > square.length - 1 || newIndex < 0)
		{

			return false;
		}
		else
		{
			return true;
		}
	
	}

	public static int[][] performUserMove(int[][] square, int x, int y, Character direction)
	{

		int [][] newSquare = square;
		switch(direction)
		{
			case 'U':
				newSquare = swap(newSquare, x,y, (x - 1),y);
				break;
			case 'R':
				newSquare = swap(newSquare, x,y, x, (y + 1));
				break;
			case 'D':
				newSquare = swap(newSquare, x,y, (x + 1), y);
				break;
			case 'L':
				newSquare = swap(newSquare, x,y, x, (y-1));
				break;
		}
		return newSquare;
	}

	public static boolean gameWon(int[][]square)
	{
		//What I want is to check all the rows e.g (00,01,02)
		//										   (10, 11,12)...

		//Then columns e.g (00) (01) etc...
		//				  (10) (11)
		//				  (20) (21)	

		//Then diag and anti-diag
		//(00, 11, 22) i++, j++
		//(20, 11  02) i-- j++
		int size = square.length;				
		int base = (size * ((size  * size) + 1)) / 2;
		int sum;

		for (int i = 0; i < size; i++)
		{	
			//Checking rows
			sum = 0;
			for (int k = 0; k < size; k++)
			{
				sum += square[i][k];
				//System.out.println(i+" "+k);
			}
			//System.out.println(sum);
			if (sum != base)
			{
				return false;
			} 

		}
		for (int i = 0; i < size; i++)
		{
			//Checking columns
			sum = 0;
			for (int k = 0; k < size; k++)
			{
				sum += square[k][i];
				//System.out.println(k+" "+i);
			}
			//System.out.println(sum);
			if (sum != base)
			{
				return false;
			} 
		}
		sum = 0;

		for (int i = 0; i < size; i++)
		{
			//diag
			sum+= square[i][i];
			//System.out.println(i+ " " +i);
		}
		if (sum != base)
		{
			return false;
		}
		sum = 0;
		
		for (int i = 0; i < size; i++)
		{
			//anti-diag
			sum += square[i][(size - 1) - i];
			//System.out.println(i + " " + ((size - 1) - i));
		}
		if (sum != base)
		{
			return false;
		}
		return true;
	}

	public static int getNum(String message)
	{

		System.out.println(message);
		Scanner scanner = new Scanner(System.in);
		while(!scanner.hasNextInt())
		{
			scanner.next();
			System.out.println(message);
		}
		int input = scanner.nextInt();

		return input;
	}
	
	public static void main(String[] args) 
	{

		int number = getNum("Enter an odd number");
		while (number % 2 == 0)
		{
			number = getNum("Enter an odd number");
		}

		int [][] square = generateSquare(number);
		//printSquare(square);

		square = shuffleSquare(square);
		printSquare(square);

		int xInput;
		int yInput;
		Character direction;
		int movesTaken = 0;

		Scanner scanner = new Scanner(System.in);
		while (!gameWon(square))
		{
			xInput = getNum("Enter your X value");

			yInput = getNum("Enter your Y value");

			System.out.println("Enter your direction");
			direction = scanner.next().charAt(0);

			if (isValidMove(square, xInput - 1, yInput - 1, direction))
			{
				square = performUserMove(square, xInput - 1, yInput - 1, direction);
				movesTaken += 1;
			}
			else
			{
				System.out.println("INVALID MOVE!!");
			}
			printSquare(square);

		}
		System.out.println("You won congratulations!!!");
		System.out.println("You took " + movesTaken + " moves");
	}
}
