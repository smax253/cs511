import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

public class Customer implements Runnable {
    private Bakery bakery;
    private Random rnd;
    private List<BreadType> shoppingCart;
    private int shopTime;
    private int checkoutTime;

    /**
     * Initialize a customer object and randomize its shopping cart
     */
    public Customer(Bakery bakery) {
        rnd = new Random();
        shoppingCart = new ArrayList<>();
        this.bakery = bakery;
    }

    /**
     * Run tasks for the customer
     */
    public void run() {
        int hash = hashCode();
        long startTime = System.currentTimeMillis();
        System.out.printf("Customer %s has started shopping.\n", hash);
        fillShoppingCart();
        for (BreadType bread : shoppingCart) {
            bakery.takeBread(bread);
            System.out.printf("Customer %s has added bread %s to their cart.\n", hash, bread);
        }
        long systemShopTime = System.currentTimeMillis();
        System.out.printf("Customer %s is checking out.\n", hash);
        bakery.addSales(getItemsValue());
        long checkout = System.currentTimeMillis();
        shopTime = (int) (systemShopTime - startTime);
        checkoutTime = (int) (checkout - systemShopTime);
        System.out.println(this.toString());
    }

    /**
     * Return a string representation of the customer
     */
    public String toString() {
        return "Customer " + hashCode() + ": shoppingCart=" + Arrays.toString(shoppingCart.toArray()) + ", shopTime="
                + shopTime + ", checkoutTime=" + checkoutTime;
    }

    /**
     * Add a bread item to the customer's shopping cart
     */
    private boolean addItem(BreadType bread) {
        // do not allow more than 3 items, chooseItems() does not call more than 3 times
        if (shoppingCart.size() >= 3) {
            return false;
        }
        shoppingCart.add(bread);
        return true;
    }

    /**
     * Fill the customer's shopping cart with 1 to 3 random breads
     */
    private void fillShoppingCart() {
        int itemCnt = 1 + rnd.nextInt(3);
        while (itemCnt > 0) {
            addItem(BreadType.values()[rnd.nextInt(BreadType.values().length)]);
            itemCnt--;
        }
    }

    /**
     * Calculate the total value of the items in the customer's shopping cart
     */
    private float getItemsValue() {
        float value = 0;
        for (BreadType bread : shoppingCart) {
            value += bread.getPrice();
        }
        return value;
    }
}