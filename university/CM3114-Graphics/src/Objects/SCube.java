package Objects;

import java.util.ArrayList;

public class SCube extends SObject {

    private float sideLength;

    public SCube() {
        super();
        this.sideLength = 1.0f;
        update();
    }

    public SCube(float sideLength) {
        super();
        this.sideLength = sideLength;
        update();
    }

    private ArrayList<Float> genFace (float[] v1, float[] v2, float[] v3, float[] v4) {
        ArrayList<float[]> acc = new ArrayList<>();
        acc.add(v1);
        acc.add(v2);
        acc.add(v3);
        acc.add(v4);

        ArrayList<Float> res = new ArrayList<>();
        for (float[] fl : acc) {
            for (int i = 0; i < fl.length; i++){
                res.add(fl[i]);
            }
        }
        return res;
    }

    @Override
    protected void genData() {
        int numFaces = 6;
        numVertices = numFaces * 4;
        vertices = new float[numVertices * 3];
        normals = new float[numVertices * 3];
        textures = new float[numVertices * 2];

        float sl = sideLength / 2;
        float[] a = new float[] {sl, sl, sl};
        float[] b = new float[] {sl, sl, -sl};
        float[] c = new float[] {sl, -sl, sl};
        float[] d = new float[] {sl, -sl, -sl};
        float[] e = new float[] {-sl, sl, sl};
        float[] f = new float[] {-sl, sl, -sl};
        float[] g = new float[] {-sl, -sl, sl};
        float[] h = new float[] {-sl, -sl, -sl};

        ArrayList<Float> f1 = genFace(a, b, d, c); // x
        ArrayList<Float> f2 = genFace(e, f, b, a); // y
        ArrayList<Float> f3 = genFace(e, a, c, g); // z
        ArrayList<Float> f4 = genFace(e, f, h, g); // -x
        ArrayList<Float> f5 = genFace(g, h, d, c); // -y
        ArrayList<Float> f6 = genFace(h, f, b, d); // -z

        f1.addAll(f2);
        f1.addAll(f3);
        f1.addAll(f4);
        f1.addAll(f5);
        f1.addAll(f6);

        int k = 0;
        for (final Float value: f1){
            vertices[k++] = value;
        }

        k = 0;
        int j = 0;
        for (int i = 0; i < 4; i++) {
            normals[k++] = 1f;
            normals[k++] = 0f;
            normals[k++] = 0f;

            textures[j++] = i < 2 ? 1f : 0f;
            textures[j++] = (i == 0 || i == 3) ? 1f : 0f;
        }

        for (int i = 0; i < 4; i++) {
            normals[k++] = 0f;
            normals[k++] = 1f;
            normals[k++] = 0f;

            textures[j++] = i < 2 ? 1f : 0f;
            textures[j++] = (i == 0 || i == 3) ? 1f : 0f;
        }

        for (int i = 0; i < 4; i++) {
            normals[k++] = 0f;
            normals[k++] = 0f;
            normals[k++] = 1f;

            textures[j++] = i < 2 ? 1f : 0f;
            textures[j++] = (i == 0 || i == 3) ? 1f : 0f;
        }

        for (int i = 0; i < 4; i++) {
            normals[k++] = -1f;
            normals[k++] = 0f;
            normals[k++] = 0f;

            textures[j++] = i < 2 ? 1f : 0f;
            textures[j++] = (i == 0 || i == 3) ? 1f : 0f;
        }

        for (int i = 0; i < 4; i++) {
            normals[k++] = 0f;
            normals[k++] = -1f;
            normals[k++] = 0f;

            textures[j++] = i < 2 ? 1f : 0f;
            textures[j++] = (i == 0 || i == 3) ? 1f : 0f;
        }

        for (int i = 0; i < 4; i++) {
            normals[k++] = 0f;
            normals[k++] = 0f;
            normals[k++] = -1f;

            textures[j++] = i < 2 ? 1f : 0f;
            textures[j++] = (i == 0 || i == 3) ? 1f : 0f;
        }

        numIndices = numFaces * 6;
        indices = new int[numIndices];
        k = 0;
        for (int i = 0; i < numFaces; i++){
            // CCW vertices for front facing
            // CW  vertices for back facing
            // https://learnopengl.com/Advanced-OpenGL/Face-culling
            if (i < 3) {
                indices[k++] = (i * 4);
                indices[k++] = (i * 4) + 2;
                indices[k++] = (i * 4) + 1;

                indices[k++] = (i * 4);
                indices[k++] = (i * 4) + 3;
                indices[k++] = (i * 4) + 2;
            } else {
                indices[k++] = (i * 4);
                indices[k++] = (i * 4) + 1;
                indices[k++] = (i * 4) + 2;

                indices[k++] = (i * 4);
                indices[k++] = (i * 4) + 2;
                indices[k++] = (i * 4) + 3;
            }
        }
    }
}
