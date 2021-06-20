abstract public class Pet
{
	public static void main(String[] args)
	{
		Springer_Spainel newDog = new Springer_Spainel("dido");
		System.out.println(newDog);
		newDog.setName("fog");
		System.out.println(newDog);
	}

	protected String name;
	protected String breed;
	protected int lifespan;

	public Pet()
	{

	}

	public String getName()
	{
		return name;
	}

	public void setName(String newName)
	{
		name = newName;
	}

	public String getBreed()
	{
		return breed;
	}

	public int getLifespan()
	{
		return lifespan;
	}

	public void setLifespan(int newLifespan)
	{
		lifespan = newLifespan;
	}
}

abstract class Fish extends Pet
{
	protected String species = "Fish";

	public Fish()
	{

	}

	public Fish(String name)
	{
		this.name = name;
	}

	public String getSpecies()
	{
		return species;
	}

	@Override
	public String toString()
	{
		return name + " the " + species;
	}
}

class Guppy extends Fish
{

	private int lifespan;
	private String breed = "Guppy";

	@Override
	public String toString()
	{
		return name + " the " + species + "\n" + "Breed :" + breed + "\nLifespan :" + lifespan;
	}

}


class Tetra extends Fish
{

	private int lifespan;
	private String breed = "Tetra";

	@Override
	public String toString()
	{
		return name + " the " + species + "\n" + "Breed :" + breed + "\nLifespan :" + lifespan;
	}

}



abstract class Dog extends Pet
{
	protected String species = "Dog";

	public Dog()
	{

	}

	public Dog(String name)
	{
		this.name = name;
	}

	@Override
	public String toString()
	{
		return name + " the " + species;
	}

}

class Springer_Spainel extends Dog
{

	private int lifespan;
	private String breed = "Springer Spaniel";

	public Springer_Spainel()
	{

	}

	public Springer_Spainel(String name)
	{
		this.name = name;
	}

	@Override
	public String toString()
	{
		return name + " the " + species + "\n" + "Breed :" + breed + "\nLifespan :" + lifespan;
	}

}

class Boxer extends Dog
{

	private int lifespan;
	private String breed = "Boxer";

	public Boxer()
	{

	}

	public Boxer(String name)
	{
		this.name = name;
	}

	@Override
	public String toString()
	{
		return name + " the " + species + "\n" + "Breed :" + breed + "\nLifespan :" + lifespan;
	}
}
