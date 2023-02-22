package Basic;

/*
 * Transformation is implemented by PREMULTIPLY existing transformation matrix,
 * which means the transformations will be performed in the order First Come 
 * First Transform according to their order in the program. This is opposite 
 * to the order of the old version of OpenGL with the fixed-function pipeline.  
 * */

public class Transform {

	private Mat4 T = new Mat4();
	private final double EPS = 1.0E-8;
	
	public Transform() {
		T.Identity();
	}
	
	public void initialize(){
		T.Identity();
	}

	public float[] getTransformv(){
		return T.getMatrixv();
	}

	public float[] getTransformTv(){
		return T.getMatrixTv();
	}
	public Mat4 getTransform(){
		return T;
	}

	public float[] getInvTransformv(){
		Mat4 temp = new Mat4(T);
		temp.inverse();
		return temp.getMatrixv();
	}
	
	public float[] getInvTransformTv(){
		Mat4 temp = new Mat4(T);
		temp.inverse();
		return temp.getMatrixTv();
	}
	public Mat4 getInvTransform(){
		Mat4 temp = new Mat4(T);
		temp.inverse();
		return temp;
	}

	public void scale(float sx, float sy, float sz){
		for(int i=0;i<4;i++) {
			T.M[0][i] = T.M[0][i]*sx;  
			T.M[1][i] = T.M[1][i]*sy;  
			T.M[2][i] = T.M[2][i]*sz;
		}
	}
	public void translate(float dx, float dy, float dz){
		Mat4 temp = new Mat4(T);			
		for(int i=0;i<4;i++){
			temp.M[0][i] = T.M[0][i]+T.M[3][i]*dx;  
			temp.M[1][i] = T.M[1][i]+T.M[3][i]*dy;  
			temp.M[2][i] = T.M[2][i]+T.M[3][i]*dz;  
			}
		for(int i=0; i<4;i++){
			T.M[0][i]=temp.M[0][i]; 
			T.M[1][i]=temp.M[1][i]; 
			T.M[2][i]=temp.M[2][i]; 
		}  

	}

	//rotation around an axis in anticlockwise direction
	public void rotateA(float angle, float x, float y, float z){
		Mat4 temp = new Mat4();
		float ca = (float) Math.cos(angle*Math.PI/180);
		float sa = (float) Math.sin(angle*Math.PI/180);
		float ca1 = 1-ca;
		temp.Identity();
		
		Vec3 v = new Vec3 (x,y,z); //for normalising x, y, z
		if (v.normalize() && Math.abs(angle)>EPS){
			x = v.getX();
			y = v.getY();
			z = v.getZ();

			temp.M[0][0]=ca+x*x*ca1;
			temp.M[0][1]=x*y*ca1-z*sa;
			temp.M[0][2]=x*z*ca1+y*sa;
			temp.M[1][0]=y*x*ca1+z*sa;
			temp.M[1][1]=ca+y*y*ca1;
			temp.M[1][2]=y*z*ca1-x*sa;
			temp.M[2][0]=z*x*ca1-y*sa;
			temp.M[2][1]=z*y*ca1+x*sa;
			temp.M[2][2]=ca+z*z*ca1;
		}
		T.multiply(temp, false);
	}



	//rotation around X-axis in anticlockwise direction
	public void rotateX(float angle){
		Mat4 temp = new Mat4(T);			
		float ca = (float) Math.cos(angle*Math.PI/180);
		float sa = (float) Math.sin(angle*Math.PI/180);
		for(int i=0;i<4;i++){
			temp.M[1][i] = T.M[1][i]*ca-T.M[2][i]*sa;  
			temp.M[2][i] = T.M[1][i]*sa+T.M[2][i]*ca;  
		}
		for(int i=0; i<4;i++){
			T.M[1][i] = temp.M[1][i];
			T.M[2][i] = temp.M[2][i];
		}  

	}		

	//rotation around Y-axis in anticlockwise direction
	public void rotateY(float angle){
		Mat4 temp = new Mat4(T);			
		float ca = (float) Math.cos(angle*Math.PI/180);
		float sa = (float) Math.sin(angle*Math.PI/180);
		for(int i=0;i<4;i++){
			temp.M[0][i] = T.M[0][i]*ca+T.M[2][i]*sa;  
			temp.M[2][i] =-T.M[0][i]*sa+T.M[2][i]*ca;  
		}			
		for(int i=0; i<4;i++){
			T.M[0][i] = temp.M[0][i];
			T.M[2][i] = temp.M[2][i];
		}  
	}		

	
	//rotation around Z-axis in anticlockwise direction
	public void rotateZ(float angle){
		Mat4 temp = new Mat4(T);			
		float ca = (float) Math.cos(angle*Math.PI/180);
		float sa = (float) Math.sin(angle*Math.PI/180);
		for(int i=0;i<4;i++){
			temp.M[0][i] = T.M[0][i]*ca-T.M[1][i]*sa;  
			temp.M[1][i] = T.M[0][i]*sa+T.M[1][i]*ca;  
		}			
		for(int i=0; i<4;i++){
			T.M[0][i] = temp.M[0][i];
			T.M[1][i] = temp.M[1][i];
		}  
	}
	
    // to reverse the Z coordinates, to make it consistent 
    // with our usual Euclidean system (right-hand system)
	public void reverseZ(){
		for(int i=0;i<4;i++){
			T.M[2][i] = -T.M[2][i];  
			}
	}		

	// Camera configuration
	// Default case: eye at (0,0,0), look at (0, 0 , -1), 
	// and y-axis (0, 1, 0) is the up direction
	public void lookAt(float ex,float ey, float ez, 
			           float ax,float ay, float az, 
			           float ux,float uy, float uz){
		Vec3 e = new Vec3(ex,ey,ez); //eye position
		Vec3 a = new Vec3(ax,ay,az); //look at position
		Vec3 u = new Vec3(ux,uy,uz); // up direction 
		
		Vec3 b = e.minus(a); // back direction
		Vec3 r = u.cross(b); //right direction
		u = b.cross(r); // modified up direction 
		r.normalize();  // x direction
		u.normalize();  // y direction
		b.normalize();  // z direciton
		Mat4 m = new Mat4(r.getX(), u.getX(), b.getX(), e.getX(),
							r.getY(), u.getY(), b.getY(), e.getY(),
							r.getZ(), u.getZ(), b.getZ(), e.getZ(),
							0, 				 0,        0,       1 );
		m.inverse();  // Look at transformation matrix
		T.multiply(m, false);
	}


	public void ortho(float left, float right,
                      float bottom, float top, 
                      float near, float far){
		Mat4 m = new Mat4();
		m.M[0][0] = 2/(right-left);
		m.M[0][3] = (right+left)/(left-right);
		m.M[1][1] = 2/(top-bottom);
		m.M[1][3] = (top+bottom)/(bottom-top);
		m.M[2][2] = 2/(far-near);
		m.M[2][3] = (far+near)/(far-near);
		m.M[3][3] = 1;
		T.multiply(m, false);
	}

	public void frustum(float left, float right,
            float bottom, float top, 
            float near, float far){
		Mat4 m = new Mat4();
		m.M[0][0] = 2*near/(right-left);
		m.M[0][2] = (right+left)/(right-left);
		m.M[1][1] = 2*near/(top-bottom);
		m.M[1][2] = (top+bottom)/(top-bottom);
		m.M[2][2] = (far+near)/(far-near);
		m.M[2][3] = 2*far*near/(far-near);
		m.M[3][2] = -1;
		T.multiply(m, false);
		}

	public void perspective(float fovy, float aspect, float near, float far){
		float height2 = near*(float)Math.tan(fovy*Math.PI/360); // half height
		float width2 = aspect * height2; // half width
		frustum(-width2, width2, -height2, height2, near, far);
	}
}
