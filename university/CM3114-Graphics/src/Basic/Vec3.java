package Basic;

public class Vec3 {
	private float x, y, z;
	private final double EPS = 1.0E-8;

	public Vec3() {
		x = 0; y = 0; z = 0;		
	}

	public Vec3(float x, float y, float z) {
		this.x = x;
		this.y = y;
		this.z = z;		
	}

	public Vec3(float[] v) {
		if(v.length==3){
			this.x = v[0];
			this.y = v[1];
			this.z = v[2];
		} else {
			System.out.println("vec3 initialization fail!!!");
		}
	}
	
	public Vec3 minus(Vec3 v){
		Vec3 ret = new Vec3();
		ret.x = x - v.x;
		ret.y = y - v.y;
		ret.z = z - v.z;
		return ret;
	}

	public Vec3 minus(float f){
		Vec3 ret = new Vec3();
		ret.x = x - f;
		ret.y = y - f;
		ret.z = z - f;
		return ret;
	}
	
	public Vec3 add(Vec3 v){
		Vec3 ret = new Vec3();
		ret.x = x + v.x;
		ret.y = y + v.y;
		ret.z = z + v.z;
		return ret;
	}

	public Vec3 add(float f){
		Vec3 ret = new Vec3();
		ret.x = x + f;
		ret.y = y + f;
		ret.z = z + f;
		return ret;
	}
	
	public float dot(Vec3 v){
		return x*v.x + y*v.y + z*v.z; 	
	}
	
	public Vec3 cross(Vec3 v){
		Vec3 ret = new Vec3();
		ret.x = y * v.z - z*v.y;
		ret.y = z * v.x - x*v.z;
		ret.z = x * v.y - y*v.x;
		return ret;
	}
	
	
	public float len(){
		return (float) Math.sqrt(x*x + y*y + z*z); 	
	}

	public boolean normalize(){
		double s =1/ len();
		if(s>EPS){
			x = (float) s*x;
			y = (float) s*y;
			z = (float) s*z;
			return true;
		} else {
			x = 0; y = 0; z = 0;
			return false;
		}
	}
	
	public void setVector(float x, float y, float z){
		this.x = x;
		this.y = y;
		this.z = z;
	}

	public void setVector(float[] fv){
		this.x = fv[0];
		this.y = fv[1];
		this.z = fv[2];
	}

	public void setVector(Vec3 v){
		this.x = v.x;
		this.y = v.y;
		this.z = v.z;
	}

	public float[] getVector(){
		float[] ret = {x,y,z};
		return ret;
	}

	public float getX(){
		return x;
	}

	public float getY(){
		return y;
	}
	
	public float getZ(){
		return z;
	}
}
