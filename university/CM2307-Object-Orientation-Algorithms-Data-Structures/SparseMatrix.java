

/* C1911627
 *
 * Optionally, if you have any comments regarding your submission, put them here. 
 * For instance, specify here if your program does not generate the proper output or does not do it in the correct manner.
 */

import java.util.*;
import java.io.*;

// A class that represents a dense vector and allows you to read/write its elements
class DenseVector {
    private int[] elements;

    public DenseVector(int n) {
        elements = new int[n];
    }

    public DenseVector(String filename) {
        File file = new File(filename);
        ArrayList<Integer> values = new ArrayList<Integer>();

        try {
            Scanner sc = new Scanner(file);

            while (sc.hasNextInt()) {
                values.add(sc.nextInt());
            }

            sc.close();

            elements = new int[values.size()];
            for (int i = 0; i < values.size(); ++i) {
                elements[i] = values.get(i);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // Read an element of the vector
    public int getElement(int idx) {
        return elements[idx];
    }

    // Modify an element of the vector
    public void setElement(int idx, int value) {
        elements[idx] = value;
    }

    // Return the number of elements
    public int size() {
        return (elements == null) ? 0 : (elements.length);
    }

    // Print all the elements
    public void print() {
        if (elements == null) {
            return;
        }

        for (int i = 0; i < elements.length; ++i) {
            System.out.println(elements[i]);
        }
    }
}

// A class that represents a sparse matrix
public class SparseMatrix {


    // Auxiliary function that prints out the command syntax
    public static void printCommandError() {
        System.err.println("ERROR: use one of the following commands");
        System.err.println(" - Read a matrix and print information: java SparseMatrix -i <MatrixFile>");
        System.err.println(" - Read a matrix and print elements: java SparseMatrix -r <MatrixFile>");
        System.err.println(" - Transpose a matrix: java SparseMatrix -t <MatrixFile>");
        System.err.println(" - Add two matrices: java SparseMatrix -a <MatrixFile1> <MatrixFile2>");
        System.err.println(" - Matrix-vector multiplication: java SparseMatrix -v <MatrixFile> <VectorFile>");
    }

    public static void main(String[] args) throws Exception {
        if (args.length < 2) {
            printCommandError();
            System.exit(-1);
        }

        if (args[0].equals("-i")) {
            if (args.length != 2) {
                printCommandError();
                System.exit(-1);
            }

            SparseMatrix mat = new SparseMatrix();
            mat.loadEntries(args[1]);
            System.out.println("Read matrix from " + args[1]);
            System.out.println("The matrix has " + mat.getNumRows() + " rows and " + mat.getNumColumns() + " columns");
            System.out.println("It has " + mat.numNonZeros() + " non-zeros");
        } else if (args[0].equals("-r")) {
            if (args.length != 2) {
                printCommandError();
                System.exit(-1);
            }

            SparseMatrix mat = new SparseMatrix();
            mat.loadEntries(args[1]);
            System.out.println("Read matrix from " + args[1] + ":");
            mat.print();
        } else if (args[0].equals("-t")) {
            if (args.length != 2) {
                printCommandError();
                System.exit(-1);
            }

            SparseMatrix mat = new SparseMatrix();
            mat.loadEntries(args[1]);
            System.out.println("Read matrix from " + args[1]);
            SparseMatrix transpose_mat = mat.transpose();
            System.out.println();
            System.out.println("Matrix elements:");
            mat.print();
            System.out.println();
            System.out.println("Transposed matrix elements:");
            transpose_mat.print();
        } else if (args[0].equals("-a")) {
            if (args.length != 3) {
                printCommandError();
                System.exit(-1);
            }

            SparseMatrix mat1 = new SparseMatrix();
            mat1.loadEntries(args[1]);
            System.out.println("Read matrix 1 from " + args[1]);
            System.out.println("Matrix elements:");
            mat1.print();

            System.out.println();
            SparseMatrix mat2 = new SparseMatrix();
            mat2.loadEntries(args[2]);
            System.out.println("Read matrix 2 from " + args[2]);
            System.out.println("Matrix elements:");
            mat2.print();
            SparseMatrix mat_sum1 = mat1.add(mat2);

            System.out.println();
            mat1.multiplyBy(2);
            SparseMatrix mat_sum2 = mat1.add(mat2);

            mat1.multiplyBy(5);
            SparseMatrix mat_sum3 = mat1.add(mat2);

            System.out.println("Matrix1 + Matrix2 =");
            mat_sum1.print();
            System.out.println();

            System.out.println("Matrix1 * 2 + Matrix2 =");
            mat_sum2.print();
            System.out.println();

            System.out.println("Matrix1 * 10 + Matrix2 =");
            mat_sum3.print();
        } else if (args[0].equals("-v")) {
            if (args.length != 3) {
                printCommandError();
                System.exit(-1);
            }

            SparseMatrix mat = new SparseMatrix();
            mat.loadEntries(args[1]);
            DenseVector vec = new DenseVector(args[2]);
            DenseVector mv = mat.multiply(vec);

            System.out.println("Read matrix from " + args[1] + ":");
            mat.print();
            System.out.println();

            System.out.println("Read vector from " + args[2] + ":");
            vec.print();
            System.out.println();

            System.out.println("Matrix-vector multiplication:");
            mv.print();
        }
    }

    // Loading matrix entries from a text file
    // You need to complete this implementation
    public void loadEntries(String filename) {
        File file = new File(filename);

        try {
            Scanner sc = new Scanner(file);
            numRows = sc.nextInt();
            numCols = sc.nextInt();
            entries = new ArrayList<Entry>();

            while (sc.hasNextInt()) {
                // Read the row index, column index, and value of an element
                int row = sc.nextInt();
                int col = sc.nextInt();
                int val = sc.nextInt();
                
                int pos = (row * numCols) + col;
                Entry entry = new Entry(pos, val);

                entries.add(entry);

                // Add your code here to add the element into data member entries
            }

            // Add your code here for sorting non-zero elements
            entries = mergeSort(entries);

        } catch (Exception e) {
            e.printStackTrace();
            numRows = 0;
            numCols = 0;
            entries = null;
        }
    }
    
    private static ArrayList<Entry> mergeSort(ArrayList<Entry> entryList){
        int len = entryList.size();
        int mid = len/2;

        if (len < 2){
            return entryList;
        }
        ArrayList<Entry> leftList = new ArrayList<Entry>(entryList.subList(0, mid));
        ArrayList<Entry> rightList = new ArrayList<Entry>(entryList.subList(mid, len));
        
        return merge(mergeSort(leftList), mergeSort(rightList));
    }
    
    private static ArrayList<Entry> merge(ArrayList<Entry> list1, ArrayList<Entry> list2){
        ArrayList<Entry> merged = new ArrayList<Entry>();
        int list1ptr = 0;
        int list2ptr = 0;

        while(list1ptr < list1.size() && list2ptr < list2.size()){
            Entry entry1 = list1.get(list1ptr);
            Entry entry2 = list2.get(list2ptr);
            
            int pos1 = entry1.getPosition();
            int pos2 = entry2.getPosition();
           
            if (pos1 < pos2){
                merged.add(entry1);
                list1ptr++;
            }
            else if (pos2 < pos1){
                merged.add(entry2);
                list2ptr++;
            }
        }
        //Copying over the rest of the array
        if (list1ptr >= list1.size()){
            ArrayList<Entry> remainder = new ArrayList<Entry>(list2.subList(list2ptr, list2.size()));
            merged.addAll(remainder);
        }
        else if (list2ptr >= list2.size()){
            ArrayList<Entry> remainder = new ArrayList<Entry>(list1.subList(list1ptr, list1.size()));
            merged.addAll(remainder);
        }

        return merged;
    }
    
    private static void printEntries(ArrayList<Entry> entries){
        Entry entry;
        System.out.print("pos: ");
        for (int i = 0; i < entries.size(); i++){
            entry = entries.get(i);
            System.out.print(entry.position + " ");
        }
        System.out.println();
        System.out.print("val: ");
        for (int i = 0; i < entries.size(); i++){
            entry = entries.get(i);
            System.out.print(entry.value + " ");
        }
        System.out.println();
        System.out.println();
    }
    
    // Default constructor
    public SparseMatrix() {
        numRows = 0;
        numCols = 0;
        entries = null;
    }

    // New Constructor
    public SparseMatrix(int rows, int cols, ArrayList<Entry> newEntries) {
        numRows = rows;
        numCols = cols;
        entries = newEntries;
    }

    // A class representing a pair of column index and elements
    private class Entry {
        private int position; // Position within row-major full array representation
        private int value; // Element value

        // Constructor using the column index and the element value
        public Entry(int pos, int val) {
            this.position = pos;
            this.value = val;
        }

        // Copy constructor
        public Entry(Entry entry) {
            this(entry.position, entry.value);
        }

        // Read column index
        int getPosition() {
            return position;
        }

        // Set column index
        void setPosition(int pos) {
            this.position = pos;
        }

        // Read element value
        int getValue() {
            return value;
        }

        // Set element value
        void setValue(int val) {
            this.value = val;
        }
    }

    // Adding two matrices
    public SparseMatrix add(SparseMatrix M) {
        // Add your code here

        ArrayList<Entry> list1 = entries;
        ArrayList<Entry> list2 = M.entries;
        ArrayList<Entry> sum = new ArrayList<>();

        int ptr1 = 0;
        int ptr2 = 0;
        Entry entry1;
        Entry entry2;

        int tmpPos;
        int tmpVal;
        Entry tmpEntry;

        int value;
        //based off the merging in mergesort

        //Copy the primitives from entries, otherwise they'll point to the same address and will be affected by other
        //operations on entries e.g multiplication
        while (ptr1 < list1.size() && ptr2 < list2.size()) {
            entry1 = list1.get(ptr1);
            entry2 = list2.get(ptr2);

            if (entry1.getPosition() < entry2.getPosition()) {
                tmpPos = entry1.getPosition();
                tmpVal = entry1.getValue();
                tmpEntry = new Entry(tmpPos, tmpVal);
                sum.add(tmpEntry);
                ptr1++;
            }
            else if (entry1.getPosition() > entry2.getPosition()) {
                tmpPos = entry2.getPosition();
                tmpVal = entry2.getValue();
                tmpEntry = new Entry(tmpPos, tmpVal);
                sum.add(tmpEntry);
                ptr2++;
            }
            else {
                value = entry1.getValue() + entry2.getValue();
                if (value != 0) {
                    tmpEntry = new Entry(entry1.getPosition(), value);
                    sum.add(tmpEntry);
                }
                ptr1++;
                ptr2++;
            }
        }
        //Copying over the rest of the array
        if (ptr1 >= list1.size()){
            for (int i = ptr2; i < list2.size(); i++){
                entry2 = list2.get(ptr2);

                tmpPos = entry2.getPosition();
                tmpVal = entry2.getValue();
                tmpEntry = new Entry(tmpPos, tmpVal);

                sum.add(tmpEntry);
            }
        }
        else if (ptr2 >= list2.size()){
            for (int i = ptr1; i < list1.size(); i++){
                entry1 = list1.get(ptr1);

                tmpPos = entry1.getPosition();
                tmpVal = entry1.getValue();
                tmpEntry = new Entry(tmpPos, tmpVal);

                sum.add(tmpEntry);
            }
        }
        SparseMatrix sumMatrix = new SparseMatrix(numRows, numCols, sum);
        return sumMatrix;
    }

    // Transposing a matrix
    public SparseMatrix transpose() {
        // Add your code here
        int newPos;
        int oldCol;
        int oldRow;

        Entry tmpEntry;
        Entry oldEntry;
        ArrayList<Entry> tmpEntries = new ArrayList<Entry>();

        for (int i = 0; i < entries.size(); i++){
            oldEntry = entries.get(i);

            oldRow = oldEntry.getPosition() / numCols;
            oldCol = oldEntry.getPosition() % numCols;
            newPos = (oldCol * numRows) + oldRow;//transpose by using column-major calculation

            tmpEntry = new Entry(newPos, oldEntry.getValue());
            tmpEntries.add(tmpEntry);
        }
        ArrayList<Entry> newEntries = mergeSort(tmpEntries);
        SparseMatrix transposed = new SparseMatrix(numCols, numRows, newEntries);
        return transposed;
    }

    // Matrix-vector multiplication
    public DenseVector multiply(DenseVector v) {
        // Add your code here
        //Iterate through entries, get row and col.
        //value = entry.value * v[entry.col]
        //Add value to dense vector[n] if row = n

        DenseVector multVec = new DenseVector(numRows);
        int row;
        int col;
        int newVal;

        Entry entry;
        for (int i = 0; i < entries.size(); i++){
            entry = entries.get(i);
            row = entry.getPosition() / numCols;
            col = entry.getPosition() % numCols;

            newVal = entry.getValue() * v.getElement(col);
            multVec.setElement(row, newVal + multVec.getElement(row));
        }
        return multVec;
    }

    // Return the number of non-zeros
    public int numNonZeros() {
        // Add your code here
        return entries.size();
    }

    // Multiply the matrix by a scalar, and update the matrix elements
    public void multiplyBy(int scalar) {
        // Add your code here
        Entry tmpEntry;
        int tmpVal;
        if (scalar == 0){
            entries.clear();
        }
        for (int i = 0; i < this.entries.size(); i++){
            tmpEntry = entries.get(i);
            tmpVal = tmpEntry.getValue();
            tmpEntry.setValue(scalar * tmpVal);
        }
    }

    // Number of rows of the matrix
    public int getNumRows() {
        return this.numRows;
    }

    // Number of columns of the matrix
    public int getNumColumns() {
        return this.numCols;
    }

    // Output the elements of the matrix, including the zeros
    // Do not modify this method
    public void print() {
        int n_elem = numRows * numCols;
        int pos = 0;

        for (int i = 0; i < entries.size(); ++i) {
            int nonzero_pos = entries.get(i).getPosition();

            while (pos <= nonzero_pos) {
                if (pos < nonzero_pos) {
                    System.out.print("0 ");
                } else {
                    System.out.print(entries.get(i).getValue());
                    System.out.print(" ");
                }

                if ((pos + 1) % this.numCols == 0) {
                    System.out.println();
                }

                pos++;
            }
        }

        while (pos < n_elem) {
            System.out.print("0 ");
            if ((pos + 1) % this.numCols == 0) {
                System.out.println();
            }

            pos++;
        }
    }

    private int numRows; // Number of rows
    private int numCols; // Number of columns
    private ArrayList<Entry> entries; // Non-zero elements
}
