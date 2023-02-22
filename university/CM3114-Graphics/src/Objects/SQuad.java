package Objects;

public class SQuad extends SObject {

    private float sideLen;

    public SQuad() {
        super();
        this.sideLen = 1.0f;
        update();
    }

    public SQuad(float sideLength) {
        super();
        this.sideLen = sideLength;
        update();
    }

    @Override
    protected void genData() {

        numVertices = 4;
        float hl = sideLen;
        vertices = new float[] {
                 hl,  hl, hl,
                -hl,  hl, hl,
                 hl, -hl, hl,
                -hl, -hl, hl
        };
        normals = new float[] {
                0f,0f,-1f,
                0f,0f,-1f,
                0f,0f,-1f,
                0f,0f,-1f
        };
        textures = new float[]{
                1f, 1f,
                1f, 0f,
                0f, 1f,
                0f, 0f
        };
        numIndices = 6;
        indices = new int[] {
                0, 1, 2,
                3, 2, 1
        };
    }
}