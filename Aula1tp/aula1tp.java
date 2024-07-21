// aula prática 1

import java.security.PublicKey;

class MyThread extends Thread {
    public void run() {
        for (long i = 0; i < 100000000L; i++)
            ;
        System.out.println("NA THREAD");
    }
}

/*
 * 
 * class aula1tp {
 * public static void main(String[] agrs) {
 * MyThread t = new MyThread();
 * System.out.println("No Main1");
 * t.run();
 * System.out.println("No Main2");
 * }
 * }
 * 
 */

// Procedimento correto com concorrência:
/*
 * class aula1tp {
 * public static void main(String[] agrs) {
 * MyThread t = new MyThread();
 * System.out.println("No Main1");
 * t.start(); // continua a rodar o main enquanto roda o thread
 * System.out.println("No Main2");
 * }
 * }
 */

// Exercicios Guião
// Ex1
/*
 * class Printer extends Thread {
 * private int I;
 * 
 * public Printer(int i) {
 * I = i;
 * }
 * 
 * public void run() {
 * for (long i = 0; i < I; i++)
 * System.out.println(i);
 * }
 * }
 * 
 * class aula1tp {
 * public static void main(String[] args) throws InterruptedException {
 * int N = Integer.parseInt(args[0]);
 * int I = Integer.parseInt(args[1]);
 * 
 * Printer[] a = new Printer[N];
 * 
 * // Criar as threads
 * for (int i = 0; i < N; ++i)
 * a[i] = new Printer(I);
 * 
 * for (int i = 0; i < N; ++i)
 * a[i].start();
 * 
 * for (int i = 0; i < N; ++i)
 * a[i].join();
 * 
 * System.out.println("FIM");
 * 
 * }
 * }
 */

// Exc 2
class Printer extends Thread {
    private int I;
    private Counter contador;

    public Printer(int i, Counter count) {
        I = i;
        contador = count;
    }

    public void run() {
        contador.increment(I);
        // contador.count += i; // Outra opção

        System.out.println(contador.getCount());
    }
}

class Counter extends Thread {
    public int count;

    public Counter() {
        this.count = 0;
    }

    public void increment(int i) {
        this.count += i;
        // Se fosse incrementado 1 valor de cada vez com ciclo for, haveria erro nas
        // threads por erros de acesso à memória
        // acontecia erro tanto acedendo de forma correta à memoria (com metodos) como de forma errada (diretamente à variavel)

        // com o comando "synchronized" antes do método increment já funciona (próxima aula)
        // public synchronized void inc()
    }


    public int getCount() {
        return this.count;
    }
}

class aula1tp {
    public static void main(String[] args) throws InterruptedException {
        int N = Integer.parseInt(args[0]);
        int I = Integer.parseInt(args[1]);

        Printer[] a = new Printer[N];

        Counter contador = new Counter();

        // Criar as threads
        for (int i = 0; i < N; ++i)
            a[i] = new Printer(I, contador);

        for (int i = 0; i < N; ++i)
            a[i].start();

        for (int i = 0; i < N; ++i)
            a[i].join();

        System.out.println("FIM");

    }
}