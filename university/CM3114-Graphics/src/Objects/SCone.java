package Objects;

import java.util.Arrays;

public class SCone extends SObject{

    private float height;
    private float radius;

    // Slices here refer to the number of faces that make up the side of the cone
    private int slices;

    public SCone() {
        super();
        this.height = 1.0f;
        this.radius = 1.0f;
        this.slices = 100;
        update();
    }

    public SCone(float radius, float height){
        super();
        this.height = height;
        this.radius = radius;
        this.slices = 100;
        update();
    }

    public SCone(float radius, float height, int slices){
        super();
        this.height = height;
        this.radius = radius;
        this.slices = slices;
        update();
    }
    @Override
    protected void genData() {
        int numVertices = this.slices + 2;
        vertices = new float[numVertices * 3];
        normals = new float[numVertices * 3];
        textures = new float[numVertices * 2];

        int k = 1;

        //The center base point
        normals[0] = 0f; normals[1] = -1f; normals[2] = 0f;
        vertices[0] = 0f; vertices[1] = 0f; vertices[2] = 0f;
        textures[0] = 0.5f; textures[1] = 0.0f;

        //The points making up base of cone
        float theta = 0;
        float jump = (float) ((PI*2)/this.slices);
        for (int i = 0; i < slices; i++){
            theta += jump;
            normals[3 * k] = cos(theta);
            normals[3 * k + 1] = 0;
            normals[3 * k + 2] = sin(theta);

            vertices[3 * k] = this.radius * normals[3*k];
            vertices[3 * k + 1] = 0;
            vertices[3 * k + 2] = this.radius * normals[3 * k + 2];

            textures[2 * k] = (k-1)/this.slices;
            textures[2 * k + 1] = 0.0f;
            k++;
        }

        //The point at the tip of cone
        normals[3 * k] = 0f; normals[3 * k + 1] = 1f; normals[3 * k + 2] = 0f;
        vertices[3 * k] = 0f; vertices[3 * k + 1] = this.height; vertices[3 * k + 2] = 0f;
        textures[2 * k] = 0.5f; textures[2 * k + 1] = 1.0f;

        //Generate indices for triangular mesh
        numIndices = this.slices * 2 * 3;
        indices = new int[numIndices];
        k = 0;

        // Generate indices for base of cone
        for (int i = 1; i < this.slices; i++) {
            indices[k++] = 0;
            indices[k++] = i;
            indices[k++] = i + 1;
        }

        //Wrapping around
        indices[k++] = 0;
        indices[k++] = this.slices;
        indices[k++] = 1;

        // Generate indices for side of the cone
        for (int i = 1; i < this.slices; i++) {
            indices[k++] = numVertices - 1;
            indices[k++] = numVertices - i - 1;
            indices[k++] = numVertices - i - 2;
        }

        //Wrapping around
        indices[k++] = numVertices - 1;
        indices[k++] = 1;
        indices[k++] = this.slices;
    }
}
