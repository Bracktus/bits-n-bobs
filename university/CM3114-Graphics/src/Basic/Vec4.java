package Basic;

public class Vec4 {
	private float x, y, z, w;
	private final double EPS = 1.0E-8;

	public Vec4() {
		x = 0; y = 0; z = 0; w = 1;		
	}

	public Vec4(float x, float y, float z) {
		this.x = x;
		this.y = y;
		this.z = z;
		this.w = 1;
	}

	public Vec4(float x, float y, float z, float w) {
		this.x = x;
		this.y = y;
		this.z = z;
		this.w = w;
	}

	// multiplication element-wise
	public Vec4 times(Vec4 v){
		Vec4 ret = new Vec4();
		ret.x = this.x*v.x;
		ret.y = this.y*v.y;
		ret.z = this.z*v.z;
		ret.w = this.w*v.w;
		return ret;
	}
		
	public void setVector(float x, float y, float z){
		this.x = x;
		this.y = y;
		this.z = z;
	}

	public void setVector(float x, float y, float z, float w) {
		this.x = x;
		this.y = y;
		this.z = z;
		this.w = w;
	}
	
	public void setVector(float[] fv){
		this.x = fv[0];
		this.y = fv[1];
		this.z = fv[2];
		if(fv.length==4){
			this.w =fv[3];
		}
	}

	public void setVector(Vec4 v){
		this.x = v.x;
		this.y = v.y;
		this.z = v.z;
		this.w = v.w;
	}

	public float[] getVector3v(){
		float[] ret = {x,y,z};
		return ret;
	}

	public float[] getVector(){
		float[] ret = {x,y,z,w};
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
	public float getW(){
		return w;
	}
}
