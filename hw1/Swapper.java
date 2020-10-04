public class Swapper implements Runnable {
    private int offset;
    private Interval interval;
    private String content;
    private char[] buffer;

    public Swapper(Interval interval, String content, char[] buffer, int offset) {
        this.offset = offset;
        this.interval = interval;
        this.content = content;
        this.buffer = buffer;
    }

    @Override
    public void run() {
        for(int i = 0, start = interval.getX(), end = interval.getY(); start <= end; start++, i++){
            buffer[offset+i] = content.charAt(start);
        }
    }
}