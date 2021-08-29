import java.util.Scanner;
import java.util.concurrent.atomic.AtomicInteger;

public class PlayersAndChairs extends Thread {

    private int player;
    
    public static int players;

    public static AtomicInteger[] chairs;
    public static int sitting = 0;

    public PlayersAndChairs(int i) {
        this.player = i;
    }

    public PlayersAndChairs() {
        // constructor
    }
    
    public void run() {
        if ((sitting < chairs.length) && (chairs[sitting].get() == 0)) {
            chairs[sitting].set(this.player);
            sitting++;
        } else {
            System.out.println("O jogador " + this.player + " foi eliminado!");
        }
    }

    public static void startGame(int players) {       
        chairs = new AtomicInteger[players - 1];
        for (int i = 0; i < chairs.length; i++) {
            chairs[i] = new AtomicInteger(0);
        }

        for (int i = 0; i < players; i++) {
            new PlayersAndChairs(i + 1).start();
        }
    }

    public static void newGame (int players) {
        int[] chairsAux = new int[chairs.length];
        for (int i = 0; i < chairs.length; i++) {
            if (chairs[i].get() != 0) {
                chairsAux[i] = chairs[i].get();
            }
        }

        chairs = new AtomicInteger[players - 1];
        for (int i = 0; i < chairs.length; i++) {
            chairs[i] = new AtomicInteger(0);
        }

        for (int i = 0; i < chairsAux.length; i++) {
            new PlayersAndChairs(chairsAux[i]).start();
        }
    }

    public static void main(String args[]) {
        Scanner keyboard = new Scanner(System.in);

        System.out.println("Insert the amount of players:");
        players = keyboard.nextInt();
        keyboard.close();

        startGame(players);

        try {
            sleep(2000);
        } catch (Exception e) {
            System.out.println(e);
        }

        while (players > 2) {
            sitting = 0;
            players--;
            newGame(players);

            try {
                sleep(2000);
            } catch (Exception e) {
                System.out.println(e);
            }
        }

        if (players == 2) {
            System.out.println("O jogador " + chairs[0].get() + " foi o vencedor!");
        }
    }
}
