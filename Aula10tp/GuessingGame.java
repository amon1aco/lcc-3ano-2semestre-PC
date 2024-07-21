import java.util.ArrayList;
import java.util.Random;
import java.util.concurrent.TimeUnit;

interface Jogo {
    void participa();
}

interface Partida {
    String advinha(int n);
}

class Jogador extends Thread implements Jogo {
    private String name;
    private Partida partida;
    private int tentativas;

    public Jogador(String name, Partida partida) {
        this.name = name;
        this.partida = partida;
        this.tentativas = 0;
    }

    @Override
    public void participa() {
        int guess;
        while (tentativas < 100) { // Limita o número máximo de tentativas a 100
            guess = new Random().nextInt(100) + 1;
            String result = partida.advinha(guess);
            System.out.println(name + ": " + result);
            if (result.equals("Acertou!")) {
                break;
            }
            tentativas++;
        }
        if (tentativas == 100) {
            System.out.println(name + ": Excedeu o limite de tentativas.");
        }
    }

    public void run(){
        participa();
    }
}

class JogoAdvinha implements Partida {
    private ArrayList<Jogador> jogadores;
    private int numberToGuess;
    private boolean gameWon;
    private long startTime;

    public JogoAdvinha(ArrayList<Jogador> jogadores, int numberToGuess) {
        this.jogadores = jogadores;
        this.numberToGuess = numberToGuess;
        this.gameWon = false;
        this.startTime = System.currentTimeMillis();
    }

    @Override
    public String advinha(int guess) {
        if (gameWon == false){
            if (guess == numberToGuess) {
                this.gameWon = true;
                return "Acertou!";
            } else if (System.currentTimeMillis() - startTime >= TimeUnit.MINUTES.toMillis(1)) {
                gameWon = true;
                return "Tempo esgotado.";
            } else {
                return "Errou.";
            }
        }
        else {
            return "Jogo já terminou";
        }
    }

    public void startGame() {
        for (Jogador jogador : jogadores) {
            jogador.start();
        }
    }
}

public class GuessingGame {
    public static void main(String[] args) {
        int numberToGuess = new Random().nextInt(100) + 1;

        System.out.println("Start");

        ArrayList<Jogador> jogadores = new ArrayList<>();
        JogoAdvinha jogo = new JogoAdvinha(jogadores, numberToGuess);

        jogadores.add(new Jogador("Player 1", jogo));
        jogadores.add(new Jogador("Player 2", jogo));
        jogadores.add(new Jogador("Player 3", jogo));
        jogadores.add(new Jogador("Player 4", jogo));

        jogo.startGame();
    }
}


//////////////////////////////////////////////////////////////////// Solucao do stor
class JogoImpl implements jogo {
    PartidaImpl partida = new PartidaImpl();
    int jogadores = 0;

    public Partida participa(){
        jogadores += 1;
        PartidaImpl p = partida;
        if (jogadores == 4){
            notifyAll();
            jogadores = 0;
            partida = new Partida();

            new Thread(()-> {
                try {
                    Thread.sleep(60000);
                } catch (InterruptedException ignored){}
                p.timeout();
            }).start();

        } else {
            while (p == partida) {
                wait();
            }
        }
    }
}

class PartidaImpl implements Partida {
    private int number = new Random().nextInt(100);
    int tentativas = 0;
    boolean timeout = false;
    boolean ganhou = false;

    synchronized void timeout(){
        timeout = true;
    }

    public synchronized String advinha(int n){
        tentativas += 1;
        if(ganhou) return "PERDEU";
        if(timeout) return "TIMEOUT";
        if(tentativas > 100) return "TENTATIVAS";
        if(n == number) {
            ganhou = true;
            return "GANHOU";
        }
        if(number < n) return "MENOR";
        return "MAIOR";
    }
}

// resolucao em erlang noutro file