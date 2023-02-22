package Basic;

public class Mat4 {
	
	public float M[][] = new float[4][4];
	private final double EPS = 1.0E-8;

	public Mat4(){
		for(int i = 0; i<4; i++){
			for(int j = 0; j<4; j++){
				M[i][j]=0;
			}
		}
	}

	public Mat4(float m00, float m01, float m02, float m03, 
				 float m10, float m11, float m12, float m13, 
				 float m20, float m21, float m22, float m23, 
				 float m30, float m31, float m32, float m33){
		M[0][0] = m00; M[0][1] = m01; M[0][2] = m02; M[0][3] = m03;   
		M[1][0] = m10; M[1][1] = m11; M[1][2] = m12; M[1][3] = m13;   
		M[2][0] = m20; M[2][1] = m21; M[2][2] = m22; M[2][3] = m23;   
		M[3][0] = m30; M[3][1] = m31; M[3][2] = m32; M[3][3] = m33;   		
	}
	
	public Mat4(float[][] mm){
		M[0][0] = mm[0][0]; M[0][1] = mm[0][1]; M[0][2] = mm[0][2]; M[0][3] = mm[0][3];   
		M[1][0] = mm[1][0]; M[1][1] = mm[1][1]; M[1][2] = mm[1][2]; M[1][3] = mm[1][3];   
		M[2][0] = mm[2][0]; M[2][1] = mm[2][1]; M[2][2] = mm[2][2]; M[2][3] = mm[2][3];   
		M[3][0] = mm[3][0]; M[3][1] = mm[3][1]; M[3][2] = mm[3][2]; M[3][3] = mm[3][3];   		
	}
	
	public Mat4(float[] mv){
		int k = 0;
		for(int i = 0; i<4; i++){
			for(int j = 0; j<4; j++){
				M[i][j]=mv[k++];
			}
		}
	}

	public Mat4(Mat4 m){
		for(int i = 0; i<4; i++){
			for(int j = 0; j<4; j++){
				M[i][j]=m.M[i][j];
			}
		}
	}

	public void setMat(float m00, float m01, float m02, float m03, 
				 float m10, float m11, float m12, float m13, 
				 float m20, float m21, float m22, float m23, 
				 float m30, float m31, float m32, float m33){
		M[0][0] = m00; M[0][1] = m01; M[0][2] = m02; M[0][3] = m03;   
		M[1][0] = m10; M[1][1] = m11; M[1][2] = m12; M[1][3] = m13;   
		M[2][0] = m20; M[2][1] = m21; M[2][2] = m22; M[2][3] = m23;   
		M[3][0] = m30; M[3][1] = m31; M[3][2] = m32; M[3][3] = m33;   		
	}	

	public void setMat(float[][] mm){
		M[0][0] = mm[0][0]; M[0][1] = mm[0][1]; M[0][2] = mm[0][2]; M[0][3] = mm[0][3];   
		M[1][0] = mm[1][0]; M[1][1] = mm[1][1]; M[1][2] = mm[1][2]; M[1][3] = mm[1][3];   
		M[2][0] = mm[2][0]; M[2][1] = mm[2][1]; M[2][2] = mm[2][2]; M[2][3] = mm[2][3];   
		M[3][0] = mm[3][0]; M[3][1] = mm[3][1]; M[3][2] = mm[3][2]; M[3][3] = mm[3][3];   		
	}

	// Set matrix using a vector mv (1D array)
	// mv is supposed to be in row-major order 	
	public void setMat(float[] mv){
		int k = 0;
		for(int i = 0; i<4; i++){
			for(int j = 0; j<4; j++){
				M[i][j]=mv[k++];
			}
		}
	}

	public void setMat(Mat4 m){
		for(int i = 0; i<4; i++){
			for(int j = 0; j<4; j++){
				M[i][j]=m.M[i][j];
			}
		}
	}
		
	// return a vector representation of the matrix,
	// which is in row-major order
	public float[] getMatrixv(){
		float [] m = {M[0][0], M[0][1], M[0][2], M[0][3],
				M[1][0], M[1][1], M[1][2], M[1][3],
				M[2][0], M[2][1], M[2][2], M[2][3],
				M[3][0], M[3][1], M[3][2], M[3][3]};
		return m;
	}
	
	// return a vector representation of the transpose 
	// of the matrix, or in column-major order
	public float[] getMatrixTv(){
		float [] m = {M[0][0], M[1][0], M[2][0], M[3][0],
				M[0][1], M[1][1], M[2][1], M[3][1],
				M[0][2], M[1][2], M[2][2], M[3][2],
				M[0][3], M[1][3], M[2][3], M[3][3]};
		return m;
	}
	
	// return a 2d array representation of the matrix
	public float[][] getMatrixm(){
		float [][] m = {{M[0][0], M[0][1], M[0][2], M[0][3]},
				{M[1][0], M[1][1], M[1][2], M[1][3]},
				{M[2][0], M[2][1], M[2][2], M[2][3]},
				{M[3][0], M[3][1], M[3][2], M[3][3]}};
		return m;
	}		
	
	
	// Set the matrix as zero matrix
	public void Zeros(){
		for(int i = 0; i<4; i++){
			for(int j = 0; j<4; j++){
				M[i][j]=0;
			}
		}
	}

	// Set the matrix as Identity matrix
	public void Identity(){
		for(int i = 0; i<4; i++)
			for(int j = 0; j<4; j++){
				M[i][j]=0;
			}

		for(int i = 0; i<4; i++){
			M[i][i] = 1;
		}
	}

	// Post-multiplied by mat, if post is true,
	// otherwise pre-multiplied by mat
	public void multiply(Mat4 mat, boolean post){
		Mat4 temp = new Mat4();
		if(post) // post-multiply by mat
		{
			for (int i =0; i<4; i++)
				for(int j =0; j<4; j++)
					for(int k =0; k<4;k++){
						temp.M[i][j] +=M[i][k]*mat.M[k][j];						
					}
		}
		else { // pre-multiply by mat
			for (int i =0; i<4; i++)
				for(int j =0; j<4; j++)
					for(int k =0; k<4;k++){
						temp.M[i][j] +=mat.M[i][k]*M[k][j];						
					}
		}
		for (int i =0; i<4; i++)
			for(int j =0; j<4; j++){
				M[i][j]=temp.M[i][j];
			}
	}

	//compute the determinant of the matrix
	public float determinant() {
		float det ;
		det  = M[0][0]*(M[1][1]*M[2][2]*M[3][3] - M[1][3]*M[2][2]*M[3][1]
			          + M[1][2]*M[2][3]*M[3][1] - M[1][2]*M[2][1]*M[3][3]
			          + M[1][3]*M[2][1]*M[3][2] - M[1][1]*M[2][3]*M[3][2]);
		det -= M[0][1]*(M[1][0]*M[2][2]*M[3][3] - M[1][3]*M[2][2]*M[3][0]
		              + M[1][2]*M[2][3]*M[3][0] - M[1][2]*M[2][0]*M[3][3]
		              + M[1][3]*M[2][0]*M[3][2] - M[1][0]*M[2][3]*M[3][2]);
		det += M[0][2]*(M[1][0]*M[2][1]*M[3][3] - M[1][3]*M[2][1]*M[3][0]
		          	  + M[1][1]*M[2][3]*M[3][0] - M[1][1]*M[2][0]*M[3][3]
		          	  + M[1][3]*M[2][0]*M[3][1] - M[1][0]*M[2][3]*M[3][1]);
		det -= M[0][3]*(M[1][0]*M[2][1]*M[3][2] - M[1][2]*M[2][1]*M[3][0]
	                  + M[1][1]*M[2][2]*M[3][0] - M[1][1]*M[2][0]*M[3][2]
	                  + M[1][2]*M[2][0]*M[3][1] - M[1][0]*M[2][2]*M[3][1]);
		return det;
	}
	
	//invert the matrix 
	public void inverse(){
		int i, j, k, r, c;
		int [] perm ={0, 1, 2, 3};
		double temp[][] = new double[4][4];
		double result[][] = new double[4][4];
		double t = Math.abs(determinant());
		if(t>EPS){//back substitution to compute inverse matrix
			//Initialise
			for(i = 0; i<4; i++)
				for(j = 0; j<4; j++){
					temp[i][j] = M[i][j];
					result[i][j] = 0;
				}
			for(i = 0; i<4; i++){
				result[i][i] = 1;
			}

			// convert temp to up triangle matrix
			for(k=0; k<4; k++){
				//find the biggest element in the submatrix
				r=k;c=k;
				t = Math.abs(temp[k][k]);
				for(i=k; i<4; i++)
					for(j=k; j<4; j++){
						if(Math.abs(temp[i][j])>t){
							t = Math.abs(temp[i][j]);
							r=i;
							c=j;
						}
					}
				
				//interchange row r and row k
				if (r!=k){
					for(j = k; j<4; j++){
						t = temp[r][j];
						temp[r][j] = temp[k][j];
						temp[k][j] = t;
					}
					for(j = 0; j<4; j++){
						t = result[r][j];
						result[r][j] = result[k][j];
						result[k][j] = t;
					}
				}
				
				//interchange column c and column k
				if (c!=k){
					j = perm[k];
					perm[k] = perm[c];
					perm[c]=j;
					for(i = 0; i<4; i++){
						t = temp[i][c];
						temp[i][c] = temp[i][k];
						temp[i][k] = t;
					}
				}				

				//temp[k][k] becomes 1, and temp[i][k] = 0 for i>k
				t = 1/temp[k][k];
				temp[k][k]=1;
				for(j =k+1; j<4; j++){
					temp[k][j] *= t;
				}										
				for(j =0; j<4; j++){
					result[k][j] *= t;
				}

				for(i=k+1; i<4; i++){
					for(j =k+1; j<4; j++){
						temp[i][j] -= temp[i][k]*temp[k][j];
					}										
					for(j =0; j<4; j++){
						result[i][j] -= temp[i][k]*result[k][j];
					}
				}
				
				for(i=k+1;i<4;i++){
					temp[i][k] =0;
				}				

			}
				
			//convert temp to identity by row transformation			
			for(k=1; k<4; k++) {
				for(i=k-1; i>=0; i--){
					for(j = 0; j<4; j++){
						result[i][j] -= temp[i][k]*result[k][j];
					}
				}
			}
			
			for(i = 0; i<4; i++)
				for(j = 0; j<4; j++){
					M[perm[i]][j] = (float) result[i][j];
				}			

		} else { //not invertible
			System.out.print("Matrix not invertible!!!");
			System.exit(0);
		}
	}
}
