public class Bank {

    private static class Account {
        private int balance;
        Account(int balance) { this.balance = balance; }
        int balance() { return balance; }
        boolean deposit(int value) {
            balance += value;
            return true;
        }
        boolean withdraw(int value) {
            if (value > balance)
                return false;
            balance -= value;
            return true;
        }
    }

    // Bank slots and vector of accounts
    private int slots;
    private Account[] av; 

    public Bank(int n) {
        slots=n;
        av=new Account[slots];
        for (int i=0; i<slots; i++) av[i]=new Account(0);
    }

    // Account balance
    synchronized public int balance(int id) {
        if (id < 0 || id >= slots)
            return 0;
        return av[id].balance();
    }

    // Deposit
    synchronized public boolean deposit(int id, int value) {
        if (id < 0 || id >= slots)
            return false;
        return av[id].deposit(value);
    }

    // Withdraw; fails if no such account or insufficient balance
    synchronized public boolean withdraw(int id, int value) {
        if (id < 0 || id >= slots)
            return false;
        return av[id].withdraw(value);
    }


    // Exc3
    // exc3 sem a correção, basta tirar o synchronized do metodo "transfer" e "totalBalance"
    public synchronized boolean transfer (int from, int to, int value){

        return withdraw(from, value) && deposit(to, value);
        
    }


    public synchronized int totalBalance(){
        int sum = 0;

        for(int i = 0; i < slots; i++){
            sum += balance(i);
        }
        
        return sum;
    }
}