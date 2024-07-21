import java.util.concurrent.locks.Condition;

class Barrier {

    private final int N;
    private int c = 0;

    private final Condition condition = lock.newCondition();

    Barrier(int N) {
        this.N = N;
    }

    boolean predicado() {
        return cond == false;
    }

    // Exc 1
    public synchronized void await() throws InterruptedException {
        c += 1;
        if (c == N) {
            notifyAll();
        } else {
            while (c < N) {
                wait();
            }
        }

        /*
         * // Outra solução
         * c+=1;
         * while(c<N){
         * wait()
         * }
         * notifyAll();
         */
    }

    // Exc 2
    int excs = 0;

    public synchronized void await2() throws InterruptedException {
        int curr_exc = excs;
        c += 1;

        if (c == N) {
            notifyAll();
            c = 0;
            excs++;

        } else {
            while (excs == curr_exc) {
                wait();
            }
        }
    }

    // Versão stor
    public synchronized void await_stor() throws InterruptedException {

        while (open) { // este ciclo while é para tratar de threads adiantadas que podem causar
                       // problemas
            wait();
        }

        count++;
        if (count == N) {
            notifyAll();
            open = true;
        } else {
            while (count < N && !open) {
                wait();
            }
        }
        count--;
        if (count == 0) {
            open = false;
            notifyAll(); // por causa das threads adiantadas
        }
    }

    // versao do stor com etapas:
    private Etape current = new Etapa();

    public synchronized void await_stor2() throws InterruptedException {

        Etapa e = current;
        c += 1;

        if(c == N){
            notifyAll();
            current = new Etapa();
            c = 0;
        } else {
            while (current == e) {
                wait();
            }
        }
    }
}

// Exc 2 - TPC
// not sure se ta certo
class Agreement {

    private int N;
    private int c = 0;

    class Etapa {
        public int n;

        public void setEtapa(int x) {
            this.n = x;
        }
    }

    Agreement(int N) {
        this.N = N;
    }

    private Etapa current = new Etapa();
    
    public synchronized int propose(int choice) throws InterruptedException {
        Etapa e = current;
        c += 1;
        if (choice > e.n) {
            e.setEtapa(choice);
        }

        if (c == N) {
            notifyAll();
            current = new Etapa();
            c = 0;
        } else {
            while (current == e) {
                wait();
            }
        }
        return current.n; 
    }
}
