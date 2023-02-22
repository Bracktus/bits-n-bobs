import static com.jogamp.opengl.GL3.*;
import static com.jogamp.opengl.math.FloatUtil.cos;
import static com.jogamp.opengl.math.FloatUtil.sin;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferByte;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.util.logging.Level;
import java.util.logging.Logger;

import Basic.ShaderProg;
import Basic.Transform;
import Basic.Vec4;
import Objects.SCube;
import Objects.SObject;

import Objects.SQuad;
import com.jogamp.nativewindow.WindowClosingProtocol;
import com.jogamp.newt.opengl.GLWindow;
import com.jogamp.opengl.GL3;
import com.jogamp.opengl.GLAutoDrawable;
import com.jogamp.opengl.GLCapabilities;
import com.jogamp.opengl.GLEventListener;
import com.jogamp.opengl.GLProfile;
import com.jogamp.opengl.util.FPSAnimator;

import javax.imageio.ImageIO;

public class CGCW04{

	final GLWindow window; //Define a window
	final FPSAnimator animator=new FPSAnimator(60, true);
	final Renderer renderer = new Renderer();

	public CGCW04() {
        GLProfile glp = GLProfile.get(GLProfile.GL3);
        GLCapabilities caps = new GLCapabilities(glp);
        window = GLWindow.create(caps);

		window.addGLEventListener(renderer); //Set the window to listen GLEvents
		animator.add(window);

		window.setTitle("Coursework 4");
		window.setSize(500,500);
		window.setDefaultCloseOperation(WindowClosingProtocol.WindowClosingMode.DISPOSE_ON_CLOSE);
		window.setVisible(true);

		animator.start();
		}

	public static void main(String[] args) {
		new CGCW04();
	}

	class Renderer implements GLEventListener {
		private Transform T = new Transform();

		//VAOs and VBOs parameters
		private int idPoint=0, numVAOs = 2;
		private int idBuffer=0, numVBOs = 2;
		private int idElement=0, numEBOs = 2;
		private int[] VAOs = new int[numVAOs];
		private int[] VBOs = new int[numVBOs];
		private int[] EBOs = new int[numEBOs];

		//Model parameters
		private int[] numElements = new int[numEBOs];
		private long vertexSize;
		private long normalSize;
		private long textureSize;
		private int vPosition;
		private int vNormal;
		private int vTexture;

		//Transformation parameters
		private int ModelView;
		private int Projection; 
		private int NormalTransform;
		private int LightPosition;
		private int texturePtr;
		private int mapPtr;
		private float theta = 0f;

		private int[] texName = new int[3];
		ByteBuffer texImg;
		ByteBuffer texImg2;
		private int texWidth, texHeight;
		ByteBuffer mapImg;
		
		@Override
		public void display(GLAutoDrawable drawable) {
			GL3 gl = drawable.getGL().getGL3(); // Get the GL pipeline object this 
			
			gl.glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
			gl.glPolygonMode(GL_FRONT_AND_BACK, GL_FILL); //default

			gl.glPointSize(5);
			gl.glLineWidth(5);


			//Transform first cube
			T.initialize();
			T.rotateZ(90);
			T.rotateY(360f*cos(theta/8));
			T.rotateX(360f*cos(theta/16));
			T.translate(0f, 0.5f, 0f);

			gl.glUniformMatrix4fv( ModelView, 1, true, T.getTransformv(), 0 );
			gl.glUniformMatrix4fv( NormalTransform, 1, true, T.getInvTransformTv(), 0 );
			gl.glUniform4fv( LightPosition, 1, new float[] {3f*cos(theta), 3f*sin(theta), -3f}, 0 );

			//Set 2nd object's textures
			gl.glActiveTexture(GL_TEXTURE1);
			gl.glBindTexture(GL_TEXTURE_2D, texName[1]);
			gl.glUniform1i(texturePtr, 1);

			gl.glActiveTexture(GL_TEXTURE2);
			gl.glBindTexture(GL_TEXTURE_2D, texName[2]);
			gl.glUniform1i(mapPtr, 2);

			idPoint = 0;
			idBuffer = 0;
			idElement = 0;
			bindObject(gl);
		    gl.glDrawElements(GL_TRIANGLES, numElements[idElement], GL_UNSIGNED_INT, 0);	//for solid teapot

			//Transform 2nd cube
			T.initialize();
			T.rotateZ(90);
			T.rotateY(360f*cos(theta/8));
			T.rotateX(360f*cos(theta/16));
			T.translate(0, -0.5f, 0);

			gl.glUniformMatrix4fv( ModelView, 1, true, T.getTransformv(), 0 );
			gl.glUniformMatrix4fv( NormalTransform, 1, true, T.getInvTransformTv(), 0 );

			//Set 2nd object's textures
			gl.glActiveTexture(GL_TEXTURE0);
			gl.glBindTexture(GL_TEXTURE_2D, texName[0]);
			gl.glUniform1i(texturePtr, 0);

			gl.glActiveTexture(GL_TEXTURE2);
			gl.glBindTexture(GL_TEXTURE_2D, texName[2]);
			gl.glUniform1i(mapPtr, 2);

			idPoint = 1;
			idBuffer = 1;
			idElement = 1;
			bindObject(gl);
			gl.glDrawElements(GL_TRIANGLES, numElements[idElement], GL_UNSIGNED_INT, 0);

			//Increment time
			theta += 0.03;
		}

		@Override
		public void dispose(GLAutoDrawable drawable) {
			System.exit(0);
		}

		@Override
		public void init(GLAutoDrawable drawable) {
			GL3 gl = drawable.getGL().getGL3(); // Get the GL pipeline object this
			gl.glGenTextures(3, texName, 0);
			// Create welsh dragon texture
			try {
				texImg = readImage("WelshDragon.jpg");
			} catch (IOException ex) {
				Logger.getLogger(getClass().getName()).log(Level.SEVERE, null, ex);
			}

			gl.glActiveTexture(GL_TEXTURE0);
			gl.glBindTexture(GL_TEXTURE_2D, texName[0]);
			gl.glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, texWidth, texHeight,
					0, GL_BGR, GL_UNSIGNED_BYTE, texImg);
			gl.glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

			//Create brick wall texture
			try {
				texImg2 = readImage("brickwall.jpg");
			} catch (IOException ex) {
				Logger.getLogger(getClass().getName()).log(Level.SEVERE, null, ex);
			}

			gl.glActiveTexture(GL_TEXTURE1);
			gl.glBindTexture(GL_TEXTURE_2D, texName[1]);
			gl.glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, texWidth, texHeight,
					0, GL_BGR, GL_UNSIGNED_BYTE, texImg2);
			gl.glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

			//Create normal map texture
			try {
				mapImg = readImage("brickwall_normal.jpg");
			} catch (IOException ex) {
				Logger.getLogger(getClass().getName()).log(Level.SEVERE, null, ex);
			}

			gl.glActiveTexture(GL_TEXTURE2);
			gl.glBindTexture(GL_TEXTURE_2D, texName[2]);
			gl.glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, texWidth, texHeight,
					0, GL_BGR, GL_UNSIGNED_BYTE, mapImg);
			gl.glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

			gl.glEnable(GL_PRIMITIVE_RESTART);
			gl.glPrimitiveRestartIndex(0xFFFF);
			gl.glEnable(GL_CULL_FACE);

			ShaderProg shaderproc = new ShaderProg(gl, "texMap.vert", "texMap.frag");
			int program = shaderproc.getProgram();
			gl.glUseProgram(program);

			// Initialize the vertex position attribute in the vertex shader
			vPosition = gl.glGetAttribLocation( program, "vPosition" );
			vNormal = gl.glGetAttribLocation( program, "vNormal" );
			vTexture = gl.glGetAttribLocation( program, "vTexture" );

			//Get connected with the ModelView matrix in the vertex shader
			ModelView = gl.glGetUniformLocation(program, "ModelView");
			NormalTransform = gl.glGetUniformLocation(program, "NormalTransform");
			Projection = gl.glGetUniformLocation(program, "Projection");
			LightPosition = gl.glGetUniformLocation(program, "LightPosition");

			//obtain uniform locations
			texturePtr = gl.glGetUniformLocation(program, "tex");
			mapPtr = gl.glGetUniformLocation(program, "map");

			// Generate VAOs, VBOs, and EBOs
			gl.glGenVertexArrays(numVAOs,VAOs,0);
			gl.glGenBuffers(numVBOs, VBOs,0);
			gl.glGenBuffers(numEBOs, EBOs,0);


			//create our 2 objects
			SObject cube = new SCube(0.5f);
			idPoint = 0;
			idBuffer = 0;
			idElement = 0;
			createObject(gl, cube);

			SObject cube_2 = new SCube(0.5f);
			idPoint = 1;
			idBuffer = 1;
			idElement = 1;
			createObject(gl, cube_2);

		    // Initialize shader lighting parameters
		    Vec4 lightAmbient = new Vec4(1.0f, 1.0f, 1.0f, 1.0f);
		    Vec4 lightDiffuse = new Vec4(0.8f, 0.8f, 0.8f, 1.0f);
		    Vec4 lightSpecular = new Vec4(1.0f, 1.0f, 1.0f, 1.0f);

		    //White
		    Vec4 materialAmbient = new Vec4(0.6f, 0.6f, 0.6f, 1.0f);
		    Vec4 materialDiffuse = new Vec4(1.0f, 1.0f, 1.0f, 1.0f);
		    Vec4 materialSpecular = new Vec4(1.0f, 1.0f, 1.0f, 1.0f);
		    float  materialShininess = 60.8974f;
		    
		    Vec4 ambientProduct = lightAmbient.times(materialAmbient);
		    float[] ambient = ambientProduct.getVector();
		    Vec4 diffuseProduct = lightDiffuse.times(materialDiffuse);
		    float[] diffuse = diffuseProduct.getVector();
		    Vec4 specularProduct = lightSpecular.times(materialSpecular);
		    float[] specular = specularProduct.getVector();

		    gl.glUniform4fv( gl.glGetUniformLocation(program, "AmbientProduct"),
				  1, ambient,0 );
		    gl.glUniform4fv( gl.glGetUniformLocation(program, "DiffuseProduct"),
				  1, diffuse, 0 );
		    gl.glUniform4fv( gl.glGetUniformLocation(program, "SpecularProduct"),
				  1, specular, 0 );
		    gl.glUniform1f( gl.glGetUniformLocation(program, "Shininess"),
				 materialShininess );
		    // This is necessary. Otherwise, the The color on back face may display 
//		    gl.glDepthFunc(GL_LESS);
		    gl.glEnable(GL_DEPTH_TEST);		    
		}
		
		@Override
		public void reshape(GLAutoDrawable drawable, int x, int y, int w,
				int h) {

			GL3 gl = drawable.getGL().getGL3(); // Get the GL pipeline object
			
			gl.glViewport(x, y, w, h);

			T.initialize();

			//projection
			if(h<1){h=1;}
			if(w<1){w=1;}			
			float a = (float) w/ h;   //aspect 
			if (w < h) {
				T.ortho(-1, 1, -1/a, 1/a, -1, 1);
			}
			else{
				T.ortho(-1*a, 1*a, -1, 1, -1, 1);
			}
			
			// Convert right-hand to left-hand coordinate system
			T.reverseZ();
		    gl.glUniformMatrix4fv( Projection, 1, true, T.getTransformv(), 0 );
		}

		private ByteBuffer readImage(String filename) throws IOException {

			ByteBuffer imgbuf;
			BufferedImage img = ImageIO.read(new FileInputStream(filename));

			texWidth = img.getWidth();
			texHeight = img.getHeight();
			DataBufferByte datbuf = (DataBufferByte) img.getData().getDataBuffer();
			imgbuf = ByteBuffer.wrap(datbuf.getData());
			return imgbuf;
		}

		public void createObject(GL3 gl, SObject obj) {
			float [] vertexArray = obj.getVertices();
			float [] normalArray = obj.getNormals();
			int [] vertexIndexs =obj.getIndices();
			float [] textureArray = obj.getTextures();
			numElements[idElement] = obj.getNumIndices();

			bindObject(gl);

			FloatBuffer vertices = FloatBuffer.wrap(vertexArray);
			FloatBuffer normals = FloatBuffer.wrap(normalArray);
			FloatBuffer textures = FloatBuffer.wrap(textureArray);

			// Create an empty buffer with the size we need
			// and a null pointer for the data values
			vertexSize = vertexArray.length*(Float.SIZE/8);
			normalSize = normalArray.length*(Float.SIZE/8);
			textureSize = textureArray.length*(Float.SIZE/8);
			gl.glBufferData(GL_ARRAY_BUFFER, vertexSize + normalSize + textureSize,
					null, GL_STATIC_DRAW); // pay attention to *Float.SIZE/8

			// Load the real data separately.  We put the colors right after the vertex coordinates,
			// so, the offset for colors is the size of vertices in bytes
			gl.glBufferSubData( GL_ARRAY_BUFFER, 0, vertexSize, vertices );
			gl.glBufferSubData( GL_ARRAY_BUFFER, vertexSize, normalSize, normals );
			gl.glBufferSubData( GL_ARRAY_BUFFER, vertexSize + normalSize, textureSize, textures );

			IntBuffer elements = IntBuffer.wrap(vertexIndexs);
			long indexSize = vertexIndexs.length*(Integer.SIZE/8);
			gl.glBufferData(GL_ELEMENT_ARRAY_BUFFER, indexSize,
					elements, GL_STATIC_DRAW); // pay attention to *Float.SIZE/8

			gl.glEnableVertexAttribArray(vPosition);
			gl.glVertexAttribPointer(vPosition, 3, GL_FLOAT, false, 0, 0L);

			gl.glEnableVertexAttribArray(vNormal);
			gl.glVertexAttribPointer(vNormal, 3, GL_FLOAT, false, 0, vertexSize);

			gl.glEnableVertexAttribArray(vTexture);
			gl.glVertexAttribPointer(vTexture, 2, GL_FLOAT, false, 0, vertexSize + normalSize);
		}

		public void bindObject(GL3 gl){
			gl.glBindVertexArray(VAOs[idPoint]);
			gl.glBindBuffer(GL_ARRAY_BUFFER, VBOs[idBuffer]);
			gl.glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBOs[idElement]);
		}
	}
}