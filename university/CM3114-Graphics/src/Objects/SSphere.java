package Objects;
public class SSphere extends SObject{
	private float radius;
	private int slices;
	private int stacks;
		
	public SSphere(){	
		super();
		init();
		update();
	}
		
	public SSphere(float radius){
		super();
		init();
		this.radius = radius;
		update();
	}
	
	public SSphere(float radius, int slices, int stacks){
		super();
		this.radius = radius;
		this.slices = slices;
		this.stacks = stacks;
		update();
	}
	
	private void init(){
		this.radius = 1;
		this.slices = 20;
		this.stacks = 20;
	}

	@Override
	protected void genData() {
		int i,j,k;
		
		double deltaLong=PI*2/slices;
		double deltaLat= PI/stacks;

		// Generate vertices coordinates, normal values, and texture coordinates
		numVertices = (slices+1)*(stacks-1)+2; 
		vertices = new float[numVertices*3];
		normals = new float[numVertices*3];
		textures = new float[numVertices*2];
		
		//North pole point
		normals[0] = 0; normals[1] = 0; normals[2] = 1;
		vertices[0] = 0; vertices[1] = 0; vertices[2] = radius;
		textures[0]= 0.5f; textures[1] = 1.0f;

		k = 1;
		//vertices on the main body
		for(i=1;i<stacks;i++){
			for(j=0;j<=slices;j++){
				normals[3*k] = sin(deltaLat*i)*cos(deltaLong*j);
				normals[3*k+1] = sin(deltaLat*i)*sin(deltaLong*j);
				normals[3*k+2] = cos(deltaLat*i);
				vertices[3*k] = radius*normals[3*k];
				vertices[3*k+1] = radius*normals[3*k+1];
				vertices[3*k+2] = radius*normals[3*k+2];
				textures[2*k] = (float) j/slices;
				textures[2*k+1] = 1-(float) i/stacks;
				k++;
			}
		}

		//South pole point
		normals[3*k] = 0; normals[3*k+1] = 0; normals[3*k+2] = -1;
		vertices[3*k] = 0; vertices[3*k+1] = 0; vertices[3*k+2] = -radius;		
		textures[2*k] = 0.5f; textures[2*k+1] = 0.0f;  k++;

		
		// Generate indices for triangular mesh
		numIndices = (stacks-1)*slices*6;
		indices = new int[numIndices];
		
		k = 0;
		//North Pole, numElement:slices*3 	
		for(j=1;j<=slices;j++){
			indices[k++] = 0;
			indices[k++] = j;
			indices[k++] = j+1;
		}

		//South Pole, numElement:slices*3 
		int temp = numVertices-1;
		for(j=temp-1;j>temp-slices-1;j--){
			indices[k++] = temp;
			indices[k++] = j;
			indices[k++] = j-1;
		}

		//Main body, numElement:(stacks-2)*slices*6 
		for(i=1;i<stacks-1;i++){
			for(j=1;j<=slices;j++){
				//each quad gives two triangles
				//triangle one
				indices[k++] = (i-1)*(slices+1)+j;
				indices[k++] = i*(slices+1)+j;
				indices[k++] = i*(slices+1)+j+1;
				//triangle two
				indices[k++] = (i-1)*(slices+1)+j;
				indices[k++] = i*(slices+1)+j+1;
				indices[k++] = (i-1)*(slices+1)+j+1;
			}
		}
	}	
	
	public void setRadius(float radius){
		this.radius = radius;
		updated = false;
	}
		
	public void setSlices(int slices){
		this.slices = slices;
		updated = false;
	}

	public void setStacks(int stacks){
		this.stacks = stacks;
		updated = false;
	}
		
	public float getRadius(){
		return radius;
	}
		
	public int getSlices(){
		return slices;
	}

	public int getStacks(){
		return stacks;
	}	
}