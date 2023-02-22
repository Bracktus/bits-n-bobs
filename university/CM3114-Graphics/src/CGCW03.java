import static com.jogamp.opengl.GL3.*;

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

import com.jogamp.nativewindow.WindowClosingProtocol;
import com.jogamp.newt.opengl.GLWindow;
import com.jogamp.opengl.GL3;
import com.jogamp.opengl.GLAutoDrawable;
import com.jogamp.opengl.GLCapabilities;
import com.jogamp.opengl.GLEventListener;
import com.jogamp.opengl.GLProfile;
import com.jogamp.opengl.util.FPSAnimator;
import com.jogamp.opengl.util.texture.TextureIO;

import javax.imageio.ImageIO;

public class CGCW03{

	final GLWindow window; //Define a window
	final FPSAnimator animator=new FPSAnimator(60, true);
	final Renderer renderer = new Renderer();

	public CGCW03() {
        GLProfile glp = GLProfile.get(GLProfile.GL3);
        GLCapabilities caps = new GLCapabilities(glp);
        window = GLWindow.create(caps);

		window.addGLEventListener(renderer); //Set the window to listen GLEvents
		animator.add(window);

		window.setTitle("Coursework 3");
		window.setSize(500,500);
		window.setDefaultCloseOperation(WindowClosingProtocol.WindowClosingMode.DISPOSE_ON_CLOSE);
		window.setVisible(true);

		animator.start();
		}

	public static void main(String[] args) {
		new CGCW03();

	}

	class Renderer implements GLEventListener {
		private Transform T = new Transform();

		//VAOs and VBOs parameters
		private int idPoint=0, numVAOs = 1;
		private int idBuffer=0, numVBOs = 1;
		private int idElement=0, numEBOs = 1;
		private int[] VAOs = new int[numVAOs];
		private int[] VBOs = new int[numVBOs];
		private int[] EBOs = new int[numEBOs];

		//Model parameters
		private int numElements;
		private int vPosition;
		private int vNormal;
		private int vTexture;

		//Transformation parameters
		private int ModelView;
		private int Projection; 
		private int NormalTransform;

		ByteBuffer texImg;
		private int texWidth, texHeight;
		
		@Override
		public void display(GLAutoDrawable drawable) {
			GL3 gl = drawable.getGL().getGL3(); // Get the GL pipeline object this 
			
			gl.glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);

			gl.glPointSize(5);                                                                                                                                                                                                                                                                                                                                                                                                  
			gl.glLineWidth(5);                                                                                                                                                                                                                                                                                                                                                                                                  

			T.initialize();
			
			//Key control interaction
			/* Task 1.c: Fill in transformation code here*/
			T.rotateZ(270);
			T.rotateX(40);
			T.rotateY(-20);

			//Locate camera
			T.lookAt(0, 0, 0, 0, 0, -1, 0, 1, 0);  	//Default					
			
			//Send model_view and normal transformation matrices to shader. 
			//Here parameter 'true' for transpose means to convert the row-major  
			//matrix to column major one, which is required when vertices'
			//location vectors are pre-multiplied by the model_view matrix.
			//Note that the normal transformation matrix is the inverse-transpose
			//matrix of the vertex transformation matrix
			gl.glUniformMatrix4fv( ModelView, 1, true, T.getTransformv(), 0 );			
			gl.glUniformMatrix4fv( NormalTransform, 1, true, T.getInvTransformTv(), 0 );			

		    gl.glPolygonMode(GL_FRONT_AND_BACK, GL_FILL); //default
		    gl.glDrawElements(GL_TRIANGLES, numElements, GL_UNSIGNED_INT, 0);	//for solid teapot
		}

		@Override
		public void dispose(GLAutoDrawable drawable) {
			System.exit(0);
		}

		@Override
		public void init(GLAutoDrawable drawable) {
			GL3 gl = drawable.getGL().getGL3(); // Get the GL pipeline object this

			gl.glEnable(GL_PRIMITIVE_RESTART);
			gl.glPrimitiveRestartIndex(0xFFFF);

			gl.glEnable(GL_CULL_FACE);


			//Load our image in
			try {
				texImg = readImage("WelshDragon.jpg");
			} catch (IOException ex) {
				Logger.getLogger(getClass().getName()).log(Level.SEVERE, null, ex);
			}

			gl.glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, texWidth, texHeight,
					0, GL_BGR, GL_UNSIGNED_BYTE, texImg);
			gl.glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
			
			SObject cube = new SCube();
			float [] vertexArray = cube.getVertices();
			float [] normalArray = cube.getNormals();
			int [] vertexIndexs = cube.getIndices();
			float [] textureArray = cube.getTextures();
			numElements = cube.getNumIndices();
			
			gl.glGenVertexArrays(numVAOs,VAOs,0);
			gl.glBindVertexArray(VAOs[idPoint]);

			FloatBuffer vertices = FloatBuffer.wrap(vertexArray);
			FloatBuffer normals = FloatBuffer.wrap(normalArray);
			FloatBuffer textures = FloatBuffer.wrap(textureArray);

			gl.glGenBuffers(numVBOs, VBOs,0);
			gl.glBindBuffer(GL_ARRAY_BUFFER, VBOs[idBuffer]);

		    // Create an empty buffer with the size we need 
			// and a null pointer for the data values
			long vertexSize = vertexArray.length*(Float.SIZE/8);
			long normalSize = normalArray.length*(Float.SIZE/8);
			long textureSize = textureArray.length*(Float.SIZE/8);
			gl.glBufferData(GL_ARRAY_BUFFER, vertexSize + normalSize + textureSize,
					null, GL_STATIC_DRAW); // pay attention to *Float.SIZE/8
		    
			// Load the real data separately.  We put the colors right after the vertex coordinates,
		    // so, the offset for colors is the size of vertices in bytes
		    gl.glBufferSubData( GL_ARRAY_BUFFER, 0, vertexSize, vertices );
		    gl.glBufferSubData( GL_ARRAY_BUFFER, vertexSize, normalSize, normals );
			gl.glBufferSubData( GL_ARRAY_BUFFER, vertexSize + normalSize, textureSize, textures );

			IntBuffer elements = IntBuffer.wrap(vertexIndexs);
			
			gl.glGenBuffers(numEBOs, EBOs,0);
			gl.glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBOs[idElement]);

			long indexSize = vertexIndexs.length*(Integer.SIZE/8);
			gl.glBufferData(GL_ELEMENT_ARRAY_BUFFER, indexSize, 
					elements, GL_STATIC_DRAW); // pay attention to *Float.SIZE/8						
			
		    ShaderProg shaderproc = new ShaderProg(gl, "Texture.vert", "Texture.frag");
			int program = shaderproc.getProgram();
			gl.glUseProgram(program);
			
		   // Initialize the vertex position attribute in the vertex shader    
		    vPosition = gl.glGetAttribLocation( program, "vPosition" );
			gl.glEnableVertexAttribArray(vPosition);
			gl.glVertexAttribPointer(vPosition, 3, GL_FLOAT, false, 0, 0L);

		    // Initialize the vertex color attribute in the vertex shader.
		    // The offset is the same as in the glBufferSubData, i.e., vertexSize
			// It is the starting point of the color data
		    vNormal = gl.glGetAttribLocation( program, "vNormal" );
			gl.glEnableVertexAttribArray(vNormal);
		    gl.glVertexAttribPointer(vNormal, 3, GL_FLOAT, false, 0, vertexSize);

			vTexture = gl.glGetAttribLocation( program, "vTexture" );
			gl.glEnableVertexAttribArray(vTexture);
			gl.glVertexAttribPointer(vTexture, 2, GL_FLOAT, false, 0, vertexSize + normalSize);
		    //Get connected with the ModelView matrix in the vertex shader
		    ModelView = gl.glGetUniformLocation(program, "ModelView");
		    NormalTransform = gl.glGetUniformLocation(program, "NormalTransform");
		    Projection = gl.glGetUniformLocation(program, "Projection");

		    // Initialize shader lighting parameters
		    float[] lightPosition = {10.0f, 3.0f, -10.0f, 0.0f};
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
			
		    gl.glUniform4fv( gl.glGetUniformLocation(program, "LightPosition"),
				  1, lightPosition, 0 );

		    gl.glUniform1f( gl.glGetUniformLocation(program, "Shininess"),
				 materialShininess );


			//Set texture uniform
			gl.glUniform1i(gl.glGetUniformLocation(program, "tex"), 0);
				 
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
	}
}