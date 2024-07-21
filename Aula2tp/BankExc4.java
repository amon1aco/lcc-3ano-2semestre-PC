public class BankExc4 {

    private static class Account {
        private int balance;
        Account(int balance) { this.balance = balance; }
        synchronized int balance() { return balance; }
        synchronized boolean deposit(int value) {
            balance += value;
            return true;
        }
        synchronized boolean withdraw(int value) {
            if (value > balance)
                return false;
            balance -= value;
            return true;
        }
    }

    // Bank slots and vector of accounts
    private int slots;
    private Account[] av; 

    public BankExc4(int n) {
        slots=n;
        av=new Account[slots];
        for (int i=0; i<slots; i++) av[i]=new Account(0);
    }

    // Account balance
    public int balance(int id) {
        if (id < 0 || id >= slots)
            return 0;
        return av[id].balance();
    }

    // Deposit
    public boolean deposit(int id, int value) {
        if (id < 0 || id >= slots)
            return false;
        return av[id].deposit(value);
    }

    // Withdraw; fails if no such account or insufficient balance
    public boolean withdraw(int id, int value) {
        if (id < 0 || id >= slots)
            return false;
        return av[id].withdraw(value);
    }


    // Exc3
    public boolean transfer (int from, int to, int value){

        Account cfrom = av[from];
        Account cto = av[to];

        /* // DeadLock
        synchronized (cfrom) {
            synchronized(cto){
                if (!cfrom.withdraw(value))
                    return false;
                return cto.deposit(value);
            }
        } */

        /* // Erro de sincronização 
        synchronized (cfrom) {
            if (!cfrom.withdraw(value))
                return false;
        }
        synchronized(cto){
            return cto.deposit(value);
        } */

        // Solução
        Account l1,l2;
        
        if(from < to){
            l1 = cfrom;
            l2 = cto;
        } else {
            l1 = cto;
            l2 = cfrom;
        }

        synchronized (l1) {
            synchronized(l2){
                if (!cfrom.withdraw(value))
                    return false;
                return cto.deposit(value);
            }
        }
    }

    public int totalBalance(){
        return totalBalanceRec(0);
    }

    public synchronized int totalBalanceRec(int i){
        if (i<slots){
            synchronized(av[i]){
                return totalBalanceRec(i+1);
            } 
        } else {
            int sum = 0;

            for(int count = 0; count < slots; count++){
                sum += balance(count);
            }
            
            return sum; 
        }
    }
}