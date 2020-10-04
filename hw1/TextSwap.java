import java.io.*;
import java.util.*;
import java.lang.Thread;

public class TextSwap {

    private static String readFile(String filename) throws Exception {
        String line;
        StringBuilder buffer = new StringBuilder();
        File file = new File(filename);
        BufferedReader br = new BufferedReader(new FileReader(file));
        while ((line = br.readLine()) != null) {
            buffer.append(line);
        }
        br.close();
        return buffer.toString();
    }

    private static Interval[] getIntervals(int numChunks, int chunkSize) {
        Interval[] intervals = new Interval[numChunks];
        for(int i = 0; i<numChunks; i++){
            intervals[i] = new Interval(i*chunkSize, (i+1)*chunkSize-1);
        }
        return intervals;
    }

    private static List<Character> getLabels(int numChunks) {
        Scanner scanner = new Scanner(System.in);
        List<Character> labels = new ArrayList<Character>();
        int endChar = numChunks == 0 ? 'a' : 'a' + numChunks - 1;
        System.out.printf("Input %d character(s) (\'%c\' - \'%c\') for the pattern.\n", numChunks, 'a', endChar);
        for (int i = 0; i < numChunks; i++) {
            labels.add(scanner.next().charAt(0));
        }
        scanner.close();
        return labels;
    }

    private static char[] runSwapper(String content, int chunkSize, int numChunks) {
        List<Character> labels = getLabels(numChunks);
        Interval[] intervals = getIntervals(numChunks, chunkSize);
        char[] buffer = new char[content.length()];
        Thread[] swappers = new Thread[numChunks];
        for(int i = 0; i<numChunks; i++){
            int index = labels.get(i) - 'a';
            swappers[i] = new Thread(new Swapper(intervals[index], content, buffer, i*chunkSize));
            swappers[i].start();
        }
        for(Thread t : swappers) {
            try {
                t.join();
            } catch(Exception e){

            }
        }
        return buffer;
    }

    private static void writeToFile(String contents, int chunkSize, int numChunks) throws Exception {
        char[] buff = runSwapper(contents, chunkSize, contents.length() / chunkSize);
        PrintWriter writer = new PrintWriter("output.txt", "UTF-8");
        writer.print(buff);
        writer.close();
    }

    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("Usage: java TextSwap <chunk size> <filename>");
            return;
        }
        String contents = "";
        int chunkSize = Integer.parseInt(args[0]);
        int numChunks = contents.length() / chunkSize;
        if (numChunks > 26) System.out.println("Chunk size too small.");
        else {
            try {
                contents = readFile(args[1]);
                if (contents.length() % chunkSize != 0) System.out.println("File size must be a multiple of chunk size.");
                else writeToFile(contents, chunkSize, numChunks);
            } catch (Exception e) {
                System.out.println("Error with IO.");
                return;
            }
        }
    }
}