import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.ArrayList;

public class Bakery implements Runnable {
    private static final int TOTAL_CUSTOMERS = 200;
    private static final int ALLOWED_CUSTOMERS = 50;
    private static final int FULL_BREAD = 20;
    private Map<BreadType, Integer> availableBread;
    private ExecutorService executor;
    private float sales = 0;

    // TODO
    private Map<BreadType, Semaphore> shelves;
    private Semaphore cashiers;
    private Semaphore modifyValue;

    /**
     * Remove a loaf from the available breads and restock if necessary
     */
    public void takeBread(BreadType bread) {
        try {
            shelves.get(bread).acquire();
            int breadLeft = availableBread.get(bread);
            if (breadLeft > 0) {
                availableBread.put(bread, breadLeft - 1);
                Thread.sleep(125);
            } else {
                System.out.println("No " + bread.toString() + " bread left! Restocking...");
                // restock by preventing access to the bread stand for some time
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException ie) {
                    ie.printStackTrace();
                }
                availableBread.put(bread, FULL_BREAD - 1);
            }
            shelves.get(bread).release();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

    }

    /**
     * Add to the total sales
     */
    public void addSales(float value) {
        try {
            cashiers.acquire();
            modifyValue.acquire();
            sales += value;
            modifyValue.release();
            Thread.sleep(200);
            cashiers.release();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    /**
     * Run all customers in a fixed thread pool
     */
    public void run() {
        availableBread = new ConcurrentHashMap<BreadType, Integer>();
        availableBread.put(BreadType.RYE, FULL_BREAD);
        availableBread.put(BreadType.SOURDOUGH, FULL_BREAD);
        availableBread.put(BreadType.WONDER, FULL_BREAD);

        shelves = new ConcurrentHashMap<BreadType, Semaphore>();
        shelves.put(BreadType.RYE, new Semaphore(1));
        shelves.put(BreadType.SOURDOUGH, new Semaphore(1));
        shelves.put(BreadType.WONDER, new Semaphore(1));

        modifyValue = new Semaphore(1);
        cashiers = new Semaphore(4);

        executor = Executors.newFixedThreadPool(ALLOWED_CUSTOMERS);

        for (int i = 0; i < TOTAL_CUSTOMERS; i++) {
            executor.execute(new Customer(this));
        }

        executor.shutdown();
        try {
            executor.awaitTermination(60, TimeUnit.MINUTES);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        System.out.printf("Total sales: %.02f\n", sales);
    }
}