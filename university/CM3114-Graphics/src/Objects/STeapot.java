package Objects;
import Basic.Vec3;

/** Draw the Utah teapot
Copyright (c) Xianfang Sun, 2015. 
*/
/**
Patch data from Mark J. Kilgard
Copyright (c) Mark J. Kilgard, 1994. 
*/

public class STeapot extends SObject {
	private final double EPS = 1E-8;
	private int level; // division level

	public STeapot() {
		super();
		level = 0;
		update();
	}

	public STeapot(int level) {
		super();
		this.level = level;
		update();
	}

	protected void genData() {
		int numPointsPerEdge = (int) Math.pow(2, level) * 3 + 1;
		int numPointsPerPatch = numPointsPerEdge*numPointsPerEdge;
		int numSubPatches = (numPointsPerEdge-1) * (numPointsPerEdge-1);

		float step = 1.0f / (numPointsPerEdge - 1);
		numVertices = 32 * numPointsPerPatch;
		numIndices = 6*32 * numSubPatches;
		vertices = new float[numVertices* 3];
		normals = new float[numVertices * 3];
		textures = new float[numVertices * 2];
		indices = new int[numIndices];

		int i, j, k, m, n, ii, jj;
		float u;
		Vec3 v0, v1, v2;

		float[][] weights = new float[numPointsPerEdge][4];
		weights[0][0] = 1; weights[0][1] = 0;
		weights[0][2] = 0; weights[0][3] = 0;
		weights[numPointsPerEdge - 1][0] = 0;
		weights[numPointsPerEdge - 1][1] = 0;
		weights[numPointsPerEdge - 1][2] = 0;
		weights[numPointsPerEdge - 1][3] = 1;
		for (k = 1; k < numPointsPerEdge - 1; k++) {
			u = k * step;
			weights[k][0] = (1 - u) * (1 - u) * (1 - u);
			weights[k][1] = 3 * u * (1 - u) * (1 - u);
			weights[k][2] = 3 * u * u * (1 - u);
			weights[k][3] = u * u * u;
		}

		// dweights represent the weights for computing the derivatives
		// They are used for normal computation
		// They have been divided by 3 for simplicity
		float[][] dweights = new float[numPointsPerEdge][4];
		dweights[0][0] = -1;
		dweights[0][1] = 1;
		dweights[0][2] = 0;
		dweights[0][3] = 0;
		dweights[numPointsPerEdge - 1][0] = 0;
		dweights[numPointsPerEdge - 1][1] = 0;
		dweights[numPointsPerEdge - 1][2] = -1;
		dweights[numPointsPerEdge - 1][3] = 1;
		for (k = 1; k < numPointsPerEdge - 1; k++) {
			u = k * step;
			dweights[k][0] = (u - 1) * (1 - u);
			dweights[k][1] = (1 - u) * (1 - 3 * u);
			dweights[k][2] = u * (2 - 3 * u);
			dweights[k][3] = u * u;
		}

		
		//main data: vertices, normals, and texture coordinates
		m = 0;
		for (k = 0; k < 10; k++) {
			for (i = 0; i < numPointsPerEdge; i++)
			for (j = 0; j < numPointsPerEdge; j++) {
				
				// compute vertex coordinates
				for (n = 0; n < 3; n++) {
					vertices[3 * m + n] = 0;
					for (ii = 0; ii < 4; ii++)
					for (jj = 0; jj < 4; jj++) {
						vertices[3 * m + n] += cpdata[patches[k][ii][jj]][n]
								* weights[i][ii] * weights[j][jj];
					}
				}
				vertices[3 * m + 2] -= 1.575f; // centralize the teapot

				// y axis reflection
				vertices[30*numPointsPerPatch+3*m] = vertices[3*m];
				vertices[30*numPointsPerPatch+3*m+1] = -vertices[3*m+1];
				vertices[30*numPointsPerPatch+3*m+2] = vertices[3*m+2];
				
				// x and y axis reflection: the first 6 patches
				if(k<6){
					vertices[60*numPointsPerPatch+3*m] = - vertices[3*m];
					vertices[60*numPointsPerPatch+3*m+1] = vertices[3*m+1];
					vertices[60*numPointsPerPatch+3*m+2] = vertices[3*m+2];
					vertices[78*numPointsPerPatch+3*m] = - vertices[3*m];
					vertices[78*numPointsPerPatch+3*m+1] = -vertices[3*m+1];
					vertices[78*numPointsPerPatch+3*m+2] = vertices[3*m+2];			
				}
				
				// compute texture coordinates based on 
				// the orientation of the vertices
				v0 = new Vec3(vertices[3*m], vertices[3*m+1], vertices[3*m+2]);
				v0.normalize();
				textures[2 * m + 1] = (float) 
						(Math.acos(v0.getZ()) / Math.PI);				
				textures[2 * m] = (float) (0.5 + 0.5
						* Math.atan2(v0.getY(), v0.getX()) 
						/ Math.PI);
				// modify texture coordinates to remedy 
				// the texture holes 
				if(Math.abs(textures[2*m])>1-EPS)
					textures[2*m]=0;
				
			
				// y axis reflection
				textures[20*numPointsPerPatch+2*m+1] 
						= textures[2*m+1];
				textures[20*numPointsPerPatch+2*m] = (float) (0.5 
						+ 0.5 * Math.atan2(-v0.getY(), v0.getX())
						/ Math.PI);
				//modify texture coordinates to remedy the texture holes 
				if(Math.abs(textures[20*numPointsPerPatch+2*m])<EPS)
					textures[20*numPointsPerPatch+2*m]=1;
				
				// x and y axis reflection
				if(k<6){ 
					textures[40*numPointsPerPatch+2*m+1] 
							= textures[2*m+1];
					textures[40*numPointsPerPatch+2*m] = (float) 
							(0.5 + 0.5 * Math.atan2(v0.getY(), 
							 -v0.getX()) / Math.PI);;
					//modify texture coordinates to remedy the texture holes 
					if(Math.abs(textures[40*numPointsPerPatch+2*m])>1-EPS)
						textures[40*numPointsPerPatch+2*m]=0;
	
					textures[52*numPointsPerPatch+2*m+1] 
							= textures[2*m+1];
					textures[52*numPointsPerPatch+2*m] = (float) 
							(0.5 + 0.5 * Math.atan2(-v0.getY(), 
							-v0.getX()) / Math.PI);;
					//modify texture coordinates to remedy the texture holes 
					if(Math.abs(textures[52*numPointsPerPatch+2*m])<EPS)
						textures[52*numPointsPerPatch+2*m]=1;
				}
				
				// compute normal direction analytically
				float x = 0, y = 0, z = 0;
				for (ii = 0; ii < 4; ii++)
				for (jj = 0; jj < 4; jj++) {
					x += dweights[i][ii] * weights[j][jj]
						* cpdata[patches[k][ii][jj]][0];
					y += dweights[i][ii] * weights[j][jj]
						* cpdata[patches[k][ii][jj]][1];
					z += dweights[i][ii] * weights[j][jj]
						* cpdata[patches[k][ii][jj]][2];
					}
				v1 = new Vec3(x, y, z); //u-direction gradient

				x = 0; y = 0; z = 0;
				for (ii = 0; ii < 4; ii++)
				for (jj = 0; jj < 4; jj++) {
					x += weights[i][ii] * dweights[j][jj]
						* cpdata[patches[k][ii][jj]][0];
					y += weights[i][ii] * dweights[j][jj]
						* cpdata[patches[k][ii][jj]][1];
					z += weights[i][ii] * dweights[j][jj]
						* cpdata[patches[k][ii][jj]][2];
				}
				v2 = new Vec3(x, y, z); //v-direction gradient

				v0 = v1.cross(v2); //normall direction
				v0.normalize();
				normals[3 * m] = v0.getX();
				normals[3 * m + 1] = v0.getY();
				normals[3 * m + 2] = v0.getZ();

				// y axis reflection
				normals[30*numPointsPerPatch+3*m] = normals[3*m];
				normals[30*numPointsPerPatch+3*m+1] = -normals[3*m+1];
				normals[30*numPointsPerPatch+3*m+2] = normals[3*m+2];

				// x and y axis reflection
				if(k<6){
					normals[60*numPointsPerPatch+3*m] = -normals[3*m];
					normals[60*numPointsPerPatch+3*m+1] = normals[3*m+1];
					normals[60*numPointsPerPatch+3*m+2] = normals[3*m+2];
					normals[78*numPointsPerPatch+3*m] = -normals[3*m];
					normals[78*numPointsPerPatch+3*m+1] = -normals[3*m+1];
					normals[78*numPointsPerPatch+3*m+2] = normals[3*m+2];
				}
				m++;
			}
		}
		
		//compute indices for triangles
		//each subpatch (quad) constructed by two triangles
		m = 0;
		for (k = 0; k < 10; k++) {
			for (i = 0; i < numPointsPerEdge-1; i++)
			for (j = 0; j < numPointsPerEdge-1; j++) {
				indices[6*m]=k*numPointsPerPatch
						+ i*numPointsPerEdge + j;
				indices[6*m+1]=indices[6*m+4]= k*numPointsPerPatch
						+ i*numPointsPerEdge + j+1;
				indices[6*m+2]=indices[6*m+3]=k*numPointsPerPatch
						+ (i+1)*numPointsPerEdge + j;
				indices[6*m+5]=k*numPointsPerPatch 
						+ (i+1)*numPointsPerEdge + j+1;
				
				// y axis reflection
				n = 60*numSubPatches;
				indices[n + 6*m] = 10 * numPointsPerPatch 
						+ indices[6*m];
				indices[n + 6*m+1] = indices[n + 6*m+4] =
						10 * numPointsPerPatch + indices[6*m+2];
				indices[n + 6*m+2] = indices[n + 6*m+3] =
						10 * numPointsPerPatch + indices[6*m+1];
				indices[n + 6*m+5] = 
						10 * numPointsPerPatch + indices[6*m+5];					

				// x and y axis reflection
				if (k<6){
					n = 120*numSubPatches;
					indices[n + 6*m] = 20 * numPointsPerPatch 
							+ indices[6*m];
					indices[n + 6*m+2] = indices[n + 6*m+3] =
							20 * numPointsPerPatch + indices[6*m+1];
					indices[n + 6*m+1] = indices[n + 6*m+4] =
							20 * numPointsPerPatch + indices[6*m+2];
					indices[n + 6*m+5] = 
							20 * numPointsPerPatch + indices[6*m+5];					
						n = 156*numSubPatches;
					indices[n + 6*m] = 26 * numPointsPerPatch 
							+ indices[6*m];
					indices[n + 6*m+1] = indices[n + 6*m+4] =
							26 * numPointsPerPatch + indices[6*m+1];
					indices[n + 6*m+2] = indices[n + 6*m+3] =
							26 * numPointsPerPatch + indices[6*m+2];
					indices[n + 6*m+5] = 
							26 * numPointsPerPatch + indices[6*m+5];											
				}
				m++;
			}
		}
	}


	/* Rim, body, lid, and bottom data must be reflected in x and
    y; handle and spout data across the y axis only.  */
	private int patches[][][] = {

		/* lid */
		{ { 0, 0, 0, 0 }, { 1, 2, 3, 4 }, 
			{ 5, 5, 5, 5 }, { 6, 7, 8, 9 } },
		{ {  6, 7, 8, 9 }, { 10, 11, 12, 13 }, 
			{ 14, 15, 16, 17 }, { 18, 19, 20, 21 } },

		/* rim */
		{ { 22, 23, 24, 25 }, { 26, 27, 28, 29 }, 
			{ 30, 31, 32, 33 }, { 34, 35, 36, 37 } },
		
		/* body */
		{ { 34, 35, 36, 37 }, { 38, 39, 40, 41 }, 
			{ 42, 43, 44, 45 }, { 46, 47, 48, 49 } },
		{ { 46, 47, 48, 49 }, { 50, 51, 52, 53 }, 
				{ 54, 55, 56, 57 }, { 58, 59, 60, 61 } },
		
		/* bottom */
		{ { 58, 59, 60, 61 }, { 62, 63, 64, 65 }, 
			{ 66, 67, 68, 69 }, { 70, 70, 70, 70 } },

		/* handle */
		{ { 71, 72, 73, 74 }, { 75, 76, 77, 78 }, 
			{ 79, 80, 81, 82 }, { 83, 84, 85, 86 } },
		{ { 83, 84, 85, 86 }, { 87, 88, 89, 90 }, 
			{ 91, 92, 93, 94 }, { 95, 96, 97, 98 } },
			
		/* spout */
		{ { 99, 100, 101, 102 }, { 103, 104, 105, 106 }, 
			{ 107, 108, 109, 110 }, { 111, 112, 113, 114 } },
		{ { 111, 112, 113, 114 }, { 115, 116, 117, 118 }, 
			{ 119, 120, 121, 122 }, { 123, 124, 125, 126 } } 
	};

	private float cpdata[][] = {
		/* lid */
		{ 0f, 0f, 3.15f }, 
		{ 0.8f, 0f, 3.15f }, { 0.8f, -0.45f, 3.15f }, 
		{ 0.45f, -0.8f, 3.15f }, { 0f, -0.8f, 3.15f },
		{ 0f, 0f, 2.85f },
		{ 0.2f, 0f, 2.7f }, { 0.2f, -0.112f, 2.7f }, // lid next
		{ 0.112f, -0.2f, 2.7f }, { 0f, -0.2f, 2.7f }, // lid next

		{ 0.4f, 0f, 2.55f }, { 0.4f, -0.224f, 2.55f },
		{ 0.224f, -0.4f, 2.55f }, { 0f, -0.4f, 2.55f },
		{ 1.3f, 0f, 2.55f }, { 1.3f, -0.728f, 2.55f },
		{ 0.728f, -1.3f, 2.55f }, { 0f, -1.3f, 2.55f },
		{ 1.3f, 0f, 2.4f }, { 1.3f, -0.728f, 2.4f },
		{ 0.728f, -1.3f, 2.4f }, { 0f, -1.3f, 2.4f },

		/* rim */
		{ 1.4f, 0f, 2.4f }, { 1.4f, -0.784f, 2.4f },
		{ 0.784f, -1.4f, 2.4f }, { 0f, -1.4f, 2.4f },
		{ 1.3375f, 0f, 2.53125f }, { 1.3375f, -0.749f, 2.53125f },
		{ 0.749f, -1.3375f, 2.53125f }, { 0f, -1.3375f, 2.53125f },
		{ 1.4375f, 0f, 2.53125f }, { 1.4375f, -0.805f, 2.53125f },
		{ 0.805f, -1.4375f, 2.53125f }, { 0f, -1.4375f, 2.53125f },
		{ 1.5f, 0f, 2.4f }, { 1.5f, -0.84f, 2.4f }, // body next
		{ 0.84f, -1.5f, 2.4f }, { 0f, -1.5f, 2.4f }, // body next
		
		/* body */
		{ 1.75f, 0f, 1.875f }, { 1.75f, -0.98f, 1.875f },
		{ 0.98f, -1.75f, 1.875f }, { 0f, -1.75f, 1.875f },
		{ 2f, 0f, 1.35f }, { 2f, -1.12f, 1.35f },
		{ 1.12f, -2f, 1.35f }, { 0f, -2f, 1.35f },
		{ 2f, 0f, 0.9f }, { 2f, -1.12f, 0.9f }, // body next
		{ 1.12f, -2f, 0.9f }, { 0f, -2f, 0.9f }, // body next

		{ 2f, 0f, 0.45f }, { 2f, -1.12f, 0.45f },
		{ 1.12f, -2f, 0.45f }, { 0f, -2f, 0.45f },
		{ 1.5f, 0f, 0.225f }, { 1.5f, -0.84f, 0.225f },
		{ 0.84f, -1.5f, 0.225f }, { 0f, -1.5f, 0.225f },
		{ 1.5f, 0f, 0.15f }, { 1.5f, -0.84f, 0.15f }, // bottom next
		{ 0.84f, -1.5f, 0.15f }, { 0f, -1.5f, 0.15f }, // bottom next
		
		/* bottom */
		{ 1.5f, 0f, 0.075f }, { 1.5f, -0.84f, 0.075f },
		{ 0.84f, -1.5f, 0.075f }, { 0f, -1.5f, 0.075f },
		{ 1.425f, 0f, 0f }, { 1.425f, -0.798f, 0f },
		{ 0.798f, -1.425f, 0f }, { 0f, -1.425f, 0f },
		{ 0f, 0f, 0f },
				
		/* handle */
		{ -1.6f, 0f, 2.025f }, { -1.6f, -0.3f, 2.025f },
		{ -1.5f, -0.3f, 2.25f }, { -1.5f, 0f, 2.25f },
		{ -2.3f, 0f, 2.025f }, { -2.3f, -0.3f, 2.025f },
		{ -2.5f, -0.3f, 2.25f }, { -2.5f, 0f, 2.25f },
		{ -2.7f, 0f, 2.025f }, { -2.7f, -0.3f, 2.025f },
		{ -3f, -0.3f, 2.25f }, { -3f, 0f, 2.25f },
		{ -2.7f, 0f, 1.8f }, { -2.7f, -0.3f, 1.8f }, // handle next
		{ -3f, -0.3f, 1.8f }, { -3f, 0f, 1.8f }, // handle next

		{ -2.7f, 0f, 1.575f }, { -2.7f, -0.3f, 1.575f },
		{ -3f, -0.3f, 1.35f }, { -3f, 0f, 1.35f }, 
		{ -2.5f, 0f, 1.125f }, { -2.5f, -0.3f, 1.125f }, 
		{ -2.65f, -0.3f, 0.9375f }, { -2.65f, 0f, 0.9375f }, 
		{ -2f, 0f, 0.9f }, { -2f, -0.3f, 0.9f },
		{ -1.9f, -0.3f, 0.6f }, { -1.9f, 0f, 0.6f },
		
		/* spout */
		{ 1.7f, 0f, 1.425f }, { 1.7f, -0.66f, 1.425f },
		{ 1.7f, -0.66f, 0.6f }, { 1.7f, 0f, 0.6f },
		{ 2.6f, 0f, 1.425f }, { 2.6f, -0.66f, 1.425f },
		{ 3.1f, -0.66f, 0.825f }, { 3.1f, 0f, 0.825f },
		{ 2.3f, 0f, 2.1f }, { 2.3f, -0.25f, 2.1f },
		{ 2.4f, -0.25f, 2.025f }, { 2.4f, 0f, 2.025f },
		{ 2.7f, 0f, 2.4f }, { 2.7f, -0.25f, 2.4f }, // spout next
		{ 3.3f, -0.25f, 2.4f }, { 3.3f, 0f, 2.4f }, // spout next

		{ 2.8f, 0f, 2.475f }, { 2.8f, -0.25f, 2.475f },
		{ 3.525f, -0.25f, 2.49375f }, { 3.525f, 0f, 2.49375f },
		{ 2.9f, 0f, 2.475f }, { 2.9f, -0.15f, 2.475f },
		{ 3.45f, -0.15f, 2.5125f }, { 3.45f, 0f, 2.5125f },
		{ 2.8f, 0f, 2.4f }, { 2.8f, -0.15f, 2.4f }, 
		{ 3.2f, -0.15f, 2.4f }, { 3.2f, 0f, 2.4f }
	};
	
	public void setLevel(int level){
		this.level = level;
		updated = false;
	}
		
	public float getLevel(){
		return level;
	}
}