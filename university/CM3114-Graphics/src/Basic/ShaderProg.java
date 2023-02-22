package Basic;

import java.io.BufferedReader;
import java.io.FileReader;
import java.nio.ByteBuffer;
import java.nio.IntBuffer;
import java.nio.charset.Charset;
import com.jogamp.opengl.GL3;

import static com.jogamp.opengl.GL3.*;

/**
 * Basic Class for shader programming
 */

/**
 * @author scmxs1
 *
 */
public class ShaderProg{
	
	private int program;
	private String vFile; 
	private String fFile;
	
	public ShaderProg(GL3 gl, String vFile, String fFile) {
		this.vFile = vFile;
		this.fFile = fFile;
		loadShaders(gl);
	}

	
	public int getProgram(){
		return program;
	}
	
	private void loadShaders(GL3 gl){
		
        String[] vSource = readShader(vFile);
        String[] fSource = readShader(fFile);

        int vShader = gl.glCreateShader(GL_VERTEX_SHADER);
        int fShader = gl.glCreateShader(GL_FRAGMENT_SHADER);

        gl.glShaderSource(vShader, 1, vSource, null);
        gl.glShaderSource(fShader, 1, fSource, null);

        gl.glCompileShader(vShader);
        gl.glCompileShader(fShader);

        program = gl.glCreateProgram();
        gl.glAttachShader(program, fShader);
        gl.glAttachShader(program, vShader);

        gl.glLinkProgram(program);

        
        //catch any errors
        int length[] = {0};
        ByteBuffer buf = null;
        IntBuffer len = null;

        /*
        gl.glGetShaderiv(vShader, GL_INFO_LOG_LENGTH, length, 0);
 
        gl.glGetShaderInfoLog(vShader, length[0] - 1, len, buf);
        System.out.print(vFile + " info:\n");
        buf.rewind();
        System.out.print(Charset.forName("UTF-8" ).decode(buf));
        */         
                
        gl.glGetProgramiv(program, GL_INFO_LOG_LENGTH, length, 0);
        if (length[0] > 1) {
            buf = ByteBuffer.allocateDirect(length[0]);
            len = null;
            gl.glGetProgramInfoLog(program, length[0] - 1, len, buf);
            System.out.print(Charset.forName("UTF-8" ).decode(buf));
            }
        
        gl.glGetProgramiv(program, GL_LINK_STATUS, length, 0);
        if (length[0] == 0) {
            System.out.println("Shader could not be linked\n");
            System.exit(-1);
        }

	}
	
    private String[] readShader (String shader){
        StringBuilder sb = new StringBuilder();
        try {
            BufferedReader br = new BufferedReader(new FileReader(shader));
            String line;
            while ((line = br.readLine()) != null)
            {
                sb.append(line);
                sb.append('\n');
            }
            br.close();
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
         return new String[] { sb.toString() };
    }
}
