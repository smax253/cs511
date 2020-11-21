import java.util.concurrent.Semaphore;
import java.util.Random;

int N = 4;
Semaphore[] permissionToGetOn = [new Semaphore(0), new Semaphore(0)];
Semaphore occupancy = new Semaphore(0);
Semaphore permissionToGetOff = new Semaphore(0);

Thread.start{
    int floor = 0;
    while (true) {
        
        for(int i = 0; i<N; i++){
            permissionToGetOn[floor].release();
            occupancy.acquire();
        }
        
        floor = (floor + 1) % 2;
        print('elevator moved to floor '+ floor)
        for(int i = 0; i<N; i++){
            permissionToGetOff.release();
        }

    }
}
100.times{
    Thread.start {
        Random rnd = new Random();
        int floor = rnd.nextInt(1);
        permissionToGetOn[floor].acquire();

        occupancy.release();

        permissionToGetOff.acquire();

        occupancy.release();
    }
}