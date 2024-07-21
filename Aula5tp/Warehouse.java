import java.util.*;
import java.util.concurrent.locks.Condition;

class Warehouse {
    private Map<String, Product> map =  new HashMap<String, Product>();

    
    private class Product { 
        int quantity = 0; 
        Condition cond = l.newCondition();
    } 

    private Product get(String item) {
        Product p = map.get(item);
        if (p != null) return p;
        p = new Product();
        map.put(item, p);
        return p;
    }

    public void supply(String item, int quantity) {
        Product p = get(item);
        p.quantity += quantity;
    }

    // Errado se faltar algum produto...
    public void consume(Set<String> items) {
        for (String s : items)
            get(s).quantity--;
    }

    // Exc 1, versao 1
    public void supply_1_1(String item, int quantity) {
        l.lock();
        try{
            Product p = get(item);
            p.quantity += quantity;
            p.cond.signalAll();
        } finally {
            l.unlock();
        }
    }

    public void consume_1_1(Set<String> items) throws InterruptedException {
        l.lock();
        try {
            for (String s : items){
                Product p = get(s);
                while (p.quantity == 0) {
                    p.cond.await();
                }
            }
        } finally {
            l.unlock();
        }
    }

    // Exc 1, versao 2
    public void supply_1_2(String item, int quantity) {
        l.lock();
        try{
            Product p = get(item);
            p.quantity += quantity;
            p.cond.signalAll();
        } finally {
            l.unlock();
        }
    }

    public Product todos_disponiveis(Set<String> items){
        for (String s : items){
            Product p = get(s);
            if (p.quantity == 0) {
                return p;
            }
        }
        return null;
    }

    public void consume_1_2(Set<String> items) throws InterruptedException {    
        l.lock();
        try {
            for(;;){
                Product p = todos_disponiveis(items);
                if (p == null) break;
                p.cond.await();
            }

            for (String s : items){
                Product p = get(s);
                p.quantity--;
            }
        } finally {
            l.unlock();
        }
    }
}
